{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeApplications           #-}
-- |
-- Benchmark framework which uses CPU instruction counting instead of
-- time measurement. This approach is much more deterministic and not
-- subject to variation caused by concurrent execution of other
-- programs.
--
-- Hardware counters are accessed using
-- [PAPI](https://icl.utk.edu/papi/). Thus OS and hardware support
-- inherited from that library.
module Test.Tasty.PAPI
  ( -- * Running benchmarks
    Test.Tasty.PAPI.defaultMain
  , Benchmark
  , Benchmarkable(..)
  , bench
  , bgroup
    -- * Creation of Benchmarkable
  , nf
  , whnf
  , nfIO
  , whnfIO
    -- * Data types
  , Counter(..)
  ) where

import Control.Exception
import Control.Monad
import Control.DeepSeq
import Control.Concurrent.STM
import Data.List       (nub, intercalate)
import Data.Proxy
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import qualified Data.Set           as Set
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import System.Mem
import System.Exit
import System.IO
import Text.Printf

import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Ingredients.ConsoleReporter
import Test.Tasty.Options
import Test.Tasty.Providers
import Test.Tasty.Runners


----------------------------------------------------------------
-- Foreign calls to PAPI
----------------------------------------------------------------

-- PAPI eventset
newtype EventSet = EventSet CInt
  deriving (Show, Storable)

foreign import capi "papi.h value PAPI_OK"          papi_OK          :: CInt
foreign import capi "papi.h value PAPI_NULL"        papi_NULL        :: CInt
foreign import capi "papi.h value PAPI_VER_CURRENT" papi_VER_CURRENT :: CInt

foreign import capi "papi.h value PAPI_TOT_INS" papi_TOT_INS :: CInt
foreign import capi "papi.h value PAPI_FP_INS"  papi_FP_INS  :: CInt
foreign import capi "papi.h value PAPI_BR_INS"  papi_BR_INS  :: CInt
foreign import capi "papi.h value PAPI_BR_MSP"  papi_BR_MSP  :: CInt

foreign import capi "papi.h PAPI_library_init"
  papi_library_init :: CInt -> IO CInt

foreign import capi unsafe "papi.h PAPI_create_eventset"
  papi_create_eventset :: Ptr EventSet -> IO CInt
foreign import capi unsafe "papi.h PAPI_cleanup_eventset"
  papi_cleanup_eventset :: EventSet -> IO CInt
foreign import capi unsafe "papi.h PAPI_destroy_eventset"
  papi_destroy_eventset :: Ptr EventSet -> IO CInt

foreign import capi unsafe "papi.h PAPI_add_event"
  papi_add_event :: EventSet -> CInt -> IO CInt

foreign import capi unsafe "papi.h PAPI_start"
  papi_start :: EventSet -> IO CInt
foreign import capi unsafe "papi.h PAPI_stop"
  papi_stop :: EventSet -> Ptr CLLong -> IO CInt
-- foreign import capi unsafe "papi.h PAPI_read"
--   papi_read :: CInt -> Ptr CLLong -> IO CInt
-- foreign import capi unsafe "papi.h PAPI_reset"
--   papi_reset :: CInt -> IO CInt

-- Call PAPI function and return error code
call :: IO CInt -> IO ()
call f = f >>= \case
  n | n == papi_OK -> pure ()
    | otherwise    -> error $ "PAPI call failed: " ++ show n

-- Create event set for use with PAPI
withPapiEventSet :: (EventSet -> IO a) -> IO a
withPapiEventSet action = do
  -- Initialize library. It seems that calling it multiple times is safe
  do n <- papi_library_init papi_VER_CURRENT
     when (n /= papi_VER_CURRENT) $ error "PAPI init failed"
  bracket ini fini action
  where
    ini = alloca $ \p_evt -> do
      poke p_evt (EventSet papi_NULL)
      call $ papi_create_eventset p_evt
      peek p_evt
    fini evt = do
      call $ papi_cleanup_eventset evt
      alloca $ \p_evt -> do
        poke p_evt evt
        call $ papi_destroy_eventset p_evt

-- | Supported hardware counters
data Counter
  = TOT_INS  -- ^ Total instruction count
  | FP_INS   -- ^ Number of floating point instructions
  | BR_INS   -- ^ Number of branch instructions
  | BR_MSP   -- ^ Number of branch mispredictions
  deriving (Show,Read,Eq,Ord)

toCounter :: Counter -> CInt
toCounter = \case
  TOT_INS -> papi_TOT_INS
  FP_INS  -> papi_FP_INS
  BR_INS  -> papi_BR_INS
  BR_MSP  -> papi_BR_MSP

----------------------------------------------------------------
--
----------------------------------------------------------------



newtype CsvPath = CsvPath FilePath

instance IsOption (Maybe CsvPath) where
  defaultValue = Nothing
  parseValue = Just . Just . CsvPath
  optionName = pure "csv"
  optionHelp = pure "File to write results in CSV format"

-- Set of counters to use
newtype CounterSet = CounterSet { getCounterSet :: [Counter] }

instance IsOption CounterSet where
  defaultValue = CounterSet [TOT_INS, BR_INS, BR_MSP]
  optionName = pure "counters"
  optionHelp = pure "Adjust set of counters to use"
  parseValue = \case
    ('=':s) -> CounterSet <$> parser s
    s       -> CounterSet . nub . (def++) <$> parser s
    where
      CounterSet def = defaultValue
      parser s = case reads s of
        [(c,"")]     -> Just [c]
        [(c,',':s')] -> (c:) <$> parser s'
        _            -> Nothing


----------------------------------------------------------------
-- Running benchmarks
----------------------------------------------------------------

-- | Just a 'TestTree'. This type synonym is provided for source compatibility with
--   @criterion@ and @gauge@.
--
-- @since 0.1
type Benchmark = TestTree

-- | IO action which could be benchmarked. It's created by 'whnf',
--   'nf', 'whnfIO', 'nfIO'.
--
-- @since 0.1
newtype Benchmarkable = Benchmarkable (IO ())

instance IsTest Benchmarkable where
  testOptions = pure []
  run opts (Benchmarkable io) _
    | 1 == n_threads = do
        withPapiEventSet $ \evt -> do
          forM_ counters $ call . papi_add_event evt . toCounter
          allocaArray (length counters) $ \vals -> do
            -- We don't want to GC happen in the middle of benchmark
            -- just because previous benchmarks allocated enough to
            -- trigger it. This could bias measurement a lot since we
            -- run bencmark only once
            performMajorGC
            n1 <- getAllocationCounter
            -- Perform measurement
            call $ papi_start evt
            io
            call $ papi_stop evt vals
            n2 <- getAllocationCounter
            let n_alloc = fromIntegral $ n1-n2
            -- Read data
            measurements <- traverse (peekElemOff vals . fst) $ [0..] `zip` counters
            pure $ testPassed
                 $ show (n_alloc:measurements)
                ++ intercalate "\t"
                     ( ("ALLOC="++showN n_alloc)
                     : [ show c ++ ('=':showN n)
                       | (c,n) <- zip counters measurements])


    | otherwise = pure $ testFailed
        "Benchmarks must not be run concurrently. Please pass -j1 or use single threaded runtime."
    where
      n_threads = getNumThreads $ lookupOption opts
      counters  = getCounterSet $ lookupOption opts

showN :: CLLong -> String
showN n
  | x < 1e4 = show n
  --
  | x < 1e5 = printf "%.2fe3" (x/1e3)
  | x < 1e6 = printf "%.1fe3" (x/1e3)
  --
  | x < 1e7 = printf "%.3fe6" (x/1e6)
  | x < 1e8 = printf "%.2fe6" (x/1e6)
  | x < 1e9 = printf "%.1fe6" (x/1e6)
  --
  | x < 1e10 = printf "%.3fe9" (x/1e9)
  | x < 1e11 = printf "%.2fe9" (x/1e9)
  | x < 1e12 = printf "%.1fe9" (x/1e9)
  | otherwise = printf "%.3e" x
  where x = fromIntegral n :: Double

-- | Create single benchmark. This is just a monomorphization of
--   'singleTest' which provides API compatibility with @criterion@
--   and @gauge@.
--
-- @since 0.1
bench :: String -> Benchmarkable -> TestTree
bench = singleTest

-- | Create single benchmark. This is just a 'testGroup' and it exists
--   to provide API compatibility with @criterion@ and @gauge@.
--
-- @since 0.1
bgroup :: String -> [Benchmark] -> Benchmark
bgroup = testGroup

-- | @nf f x@ measures number of instructions needed to compute normal
--   form of and application of @f@ to @x@.
--
-- @since 0.1
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf f a = Benchmarkable $ do _ <- evaluate $ force (f a)
                            return ()

-- | @nf f x@ measures number of instructions needed to compute weak
--   head normal form of and application of @f@ to @x@.
--
-- @since 0.1
whnf :: (a -> b) -> a -> Benchmarkable
whnf f a = Benchmarkable $ do _ <- evaluate (f a)
                              return ()

-- | @whnfIO a@ measures number of instructions needed to evaluate IO
--   action and reduce value returned by it to weak head normal form.
--
-- @since 0.1
whnfIO :: IO a -> Benchmarkable
whnfIO io = Benchmarkable $ do _ <- evaluate =<< io
                               return ()
-- | @nfIO a@ measures number of instructions needed to evaluate IO
--   action and reduce value returned by it to normal form.
--
-- @since 0.1
nfIO :: NFData a => IO a -> Benchmarkable
nfIO io = Benchmarkable $ do a <- io
                             _ <- evaluate (force a)
                             return ()

-- | Run benchmark suite. It provides API compatible with @criterion@ and @gauge@.
--
-- @since 0.1
defaultMain :: [TestTree] -> IO ()
defaultMain
  = defaultMainWithIngredients [ listingTests
                               , consoleBenchReporter `composeReporters` csvReporter
                               ]
  . testGroup "All"


----------------------------------------------------------------
-- Reporters
----------------------------------------------------------------

consoleBenchReporter :: Ingredient
consoleBenchReporter = consoleTestReporterWithHook $ \_ r -> do
  case reads @[CLLong] $ resultDescription r of
    [(_,s)] -> pure r { resultDescription = s }
    _       -> pure r

csvReporter :: Ingredient
csvReporter = TestReporter [Option (Proxy @(Maybe CsvPath)), Option (Proxy @CounterSet)] $
  \opts tree -> do
    CsvPath path <- lookupOption opts
    let CounterSet counters = lookupOption opts
    let names    = testsNames opts tree
        namesMap = IM.fromDistinctAscList $ zip [0..] names
    pure $ \smap -> do
      case findNonUniqueElement names of
        Nothing -> pure ()
        Just name -> die $ "CSV report cannot proceed, because name '" ++ name
                        ++ "' corresponds to two or more benchmarks. Please disambiguate them."
      withFile path WriteMode $ \h -> do
        hSetBuffering h LineBuffering
        hPutStrLn h $ intercalate "," $ "ALLOC" : (show <$> counters)
        csvOutput h $ IM.intersectionWith (,) namesMap smap
      pure $ \_ -> isSuccessful smap

isSuccessful :: StatusMap -> IO Bool
isSuccessful = go . IM.elems
  where
    go [] = pure True
    go (tv : tvs) = do
      b <- atomically $ readTVar tv >>= \s -> case s of Done r -> pure (resultSuccessful r); _ -> retry
      if b then go tvs else pure False

findNonUniqueElement :: Ord a => [a] -> Maybe a
findNonUniqueElement = go mempty
  where
    go _   [] = Nothing
    go acc (x : xs)
      | x `Set.member` acc = Just x
      | otherwise          = go (Set.insert x acc) xs

csvOutput :: Handle -> IM.IntMap (TestName, TVar Status) -> IO ()
csvOutput h = traverse_ $ \(name, tv) -> do
  r <- atomically $ readTVar tv >>= \case
    Done r -> pure r
    _      -> retry
  case reads @[CLLong] $ resultDescription r of
    [(meas,_)] -> hPutStrLn h $ intercalate "," $ encodeCsv name : (show <$> meas)
    _          -> pure ()

encodeCsv :: String -> String
encodeCsv xs
  | any (`elem` xs) ",\"\n\r" = '"' : go xs
  | otherwise                 = xs
  where
    go []         = '"' : []
    go ('"' : ys) = '"' : '"' : go ys
    go (y   : ys) = y   : go ys

{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE TypeApplications           #-}
{- |
Module:      Test.Tasty.PAPI
Copyright:   (c) 2023 Alexey Khudyakov
Licence:     BSD3

Benchmark framework which uses CPU instruction counting instead of
time measurement. This approach is much more deterministic and not
subject to variation caused by concurrent execution of other
programs.

Hardware counters are accessedusing
[PAPI](https://icl.utk.edu/papi/). Thus OS and hardware support
inherited from that library.


=== How to use

Library uses standard approach for benchmarks. So example benchmarks
looks similar to one using @criterion@, @gauge@ or @test-bench@:

> module Main where
> import Test.Tasty.PAPI
> 
> main :: IO ()
> main = defaultMain
>   [ bench "6" $ whnf fib 6
>   , bench "7" $ whnf fib 7
>   , bench "8" $ whnf fib 8
>   ]
> 
> fib :: Integer -> Integer
> fib 0 = 0
> fib 1 = 1
> fib n = fib (n-1) + fib (n-2)

Its output is:

> All
>   6:                OK
>     ALLOC=528  TOT_INS=4768    TOT_CYC=6114    BR_INS=1128     BR_MSP=74
>   7:                OK
>     ALLOC=864  TOT_INS=7431    TOT_CYC=6631    BR_INS=1744     BR_MSP=70
>   8:                OK
>     ALLOC=1408 TOT_INS=11.75e3 TOT_CYC=8540    BR_INS=2743     BR_MSP=93


=== Command line optiosn

Use @--help@ to list command-line options. Below is list of options
provided by this package:

[@--csv@ @PATH@]

    Write benchmark results into file in CSV format.

[@--counters@ @COUNTER_SET@]

    Adjust set of hardware counters to use. Refer to 'Counter' for
    documentation on supported counters. By default 'TOT_INS', 'TOT_CYC',
    'BR_INS', 'BR_MSP' are measured. @--counters INT_INS,FP_INS@ will
    *add* 'INT_INS' and 'FP_INS' to default set. @--counters
    =INT_INS,FP_INS@ will use only list of provided counters. Note
    that counter may or may not be supported on you CPU and there
    could be limit on number of counters used simultaneously.
-}
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
 
foreign import capi "papi.h value PAPI_L1_DCM"   papi_L1_DCM  :: CInt
foreign import capi "papi.h value PAPI_L1_ICM"   papi_L1_ICM  :: CInt
foreign import capi "papi.h value PAPI_L2_DCM"   papi_L2_DCM  :: CInt
foreign import capi "papi.h value PAPI_L2_ICM"   papi_L2_ICM  :: CInt
foreign import capi "papi.h value PAPI_L3_DCM"   papi_L3_DCM  :: CInt
foreign import capi "papi.h value PAPI_L3_ICM"   papi_L3_ICM  :: CInt
foreign import capi "papi.h value PAPI_L1_TCM"   papi_L1_TCM  :: CInt
foreign import capi "papi.h value PAPI_L2_TCM"   papi_L2_TCM  :: CInt
foreign import capi "papi.h value PAPI_L3_TCM"   papi_L3_TCM  :: CInt
foreign import capi "papi.h value PAPI_CA_SNP"   papi_CA_SNP  :: CInt
foreign import capi "papi.h value PAPI_CA_SHR"   papi_CA_SHR  :: CInt
foreign import capi "papi.h value PAPI_CA_CLN"   papi_CA_CLN  :: CInt
foreign import capi "papi.h value PAPI_CA_INV"   papi_CA_INV  :: CInt
foreign import capi "papi.h value PAPI_CA_ITV"   papi_CA_ITV  :: CInt
foreign import capi "papi.h value PAPI_L3_LDM"   papi_L3_LDM  :: CInt
foreign import capi "papi.h value PAPI_L3_STM"   papi_L3_STM  :: CInt
foreign import capi "papi.h value PAPI_BRU_IDL"  papi_BRU_IDL :: CInt
foreign import capi "papi.h value PAPI_FXU_IDL"  papi_FXU_IDL :: CInt
foreign import capi "papi.h value PAPI_FPU_IDL"  papi_FPU_IDL :: CInt
foreign import capi "papi.h value PAPI_LSU_IDL"  papi_LSU_IDL :: CInt
foreign import capi "papi.h value PAPI_TLB_DM"   papi_TLB_DM  :: CInt
foreign import capi "papi.h value PAPI_TLB_IM"   papi_TLB_IM  :: CInt
foreign import capi "papi.h value PAPI_TLB_TL"   papi_TLB_TL  :: CInt
foreign import capi "papi.h value PAPI_L1_LDM"   papi_L1_LDM  :: CInt
foreign import capi "papi.h value PAPI_L1_STM"   papi_L1_STM  :: CInt
foreign import capi "papi.h value PAPI_L2_LDM"   papi_L2_LDM  :: CInt
foreign import capi "papi.h value PAPI_L2_STM"   papi_L2_STM  :: CInt
foreign import capi "papi.h value PAPI_BTAC_M"   papi_BTAC_M  :: CInt
foreign import capi "papi.h value PAPI_PRF_DM"   papi_PRF_DM  :: CInt
foreign import capi "papi.h value PAPI_L3_DCH"   papi_L3_DCH  :: CInt
foreign import capi "papi.h value PAPI_TLB_SD"   papi_TLB_SD  :: CInt
foreign import capi "papi.h value PAPI_CSR_FAL"  papi_CSR_FAL :: CInt
foreign import capi "papi.h value PAPI_CSR_SUC"  papi_CSR_SUC :: CInt
foreign import capi "papi.h value PAPI_CSR_TOT"  papi_CSR_TOT :: CInt
foreign import capi "papi.h value PAPI_MEM_SCY"  papi_MEM_SCY :: CInt
foreign import capi "papi.h value PAPI_MEM_RCY"  papi_MEM_RCY :: CInt
foreign import capi "papi.h value PAPI_MEM_WCY"  papi_MEM_WCY :: CInt
foreign import capi "papi.h value PAPI_STL_ICY"  papi_STL_ICY :: CInt
foreign import capi "papi.h value PAPI_FUL_ICY"  papi_FUL_ICY :: CInt
foreign import capi "papi.h value PAPI_STL_CCY"  papi_STL_CCY :: CInt
foreign import capi "papi.h value PAPI_FUL_CCY"  papi_FUL_CCY :: CInt
foreign import capi "papi.h value PAPI_HW_INT"   papi_HW_INT  :: CInt
foreign import capi "papi.h value PAPI_BR_UCN"   papi_BR_UCN  :: CInt
foreign import capi "papi.h value PAPI_BR_CN"    papi_BR_CN   :: CInt
foreign import capi "papi.h value PAPI_BR_TKN"   papi_BR_TKN  :: CInt
foreign import capi "papi.h value PAPI_BR_NTK"   papi_BR_NTK  :: CInt
foreign import capi "papi.h value PAPI_BR_MSP"   papi_BR_MSP  :: CInt
foreign import capi "papi.h value PAPI_BR_PRC"   papi_BR_PRC  :: CInt
foreign import capi "papi.h value PAPI_FMA_INS"  papi_FMA_INS :: CInt
foreign import capi "papi.h value PAPI_TOT_IIS"  papi_TOT_IIS :: CInt
foreign import capi "papi.h value PAPI_TOT_INS"  papi_TOT_INS :: CInt
foreign import capi "papi.h value PAPI_INT_INS"  papi_INT_INS :: CInt
foreign import capi "papi.h value PAPI_FP_INS"   papi_FP_INS  :: CInt
foreign import capi "papi.h value PAPI_LD_INS"   papi_LD_INS  :: CInt
foreign import capi "papi.h value PAPI_SR_INS"   papi_SR_INS  :: CInt
foreign import capi "papi.h value PAPI_BR_INS"   papi_BR_INS  :: CInt
foreign import capi "papi.h value PAPI_VEC_INS"  papi_VEC_INS :: CInt
foreign import capi "papi.h value PAPI_RES_STL"  papi_RES_STL :: CInt
foreign import capi "papi.h value PAPI_FP_STAL"  papi_FP_STAL :: CInt
foreign import capi "papi.h value PAPI_TOT_CYC"  papi_TOT_CYC :: CInt
foreign import capi "papi.h value PAPI_LST_INS"  papi_LST_INS :: CInt
foreign import capi "papi.h value PAPI_SYC_INS"  papi_SYC_INS :: CInt
foreign import capi "papi.h value PAPI_L1_DCH"   papi_L1_DCH  :: CInt
foreign import capi "papi.h value PAPI_L2_DCH"   papi_L2_DCH  :: CInt
foreign import capi "papi.h value PAPI_L1_DCA"   papi_L1_DCA  :: CInt
foreign import capi "papi.h value PAPI_L2_DCA"   papi_L2_DCA  :: CInt
foreign import capi "papi.h value PAPI_L3_DCA"   papi_L3_DCA  :: CInt
foreign import capi "papi.h value PAPI_L1_DCR"   papi_L1_DCR  :: CInt
foreign import capi "papi.h value PAPI_L2_DCR"   papi_L2_DCR  :: CInt
foreign import capi "papi.h value PAPI_L3_DCR"   papi_L3_DCR  :: CInt
foreign import capi "papi.h value PAPI_L1_DCW"   papi_L1_DCW  :: CInt
foreign import capi "papi.h value PAPI_L2_DCW"   papi_L2_DCW  :: CInt
foreign import capi "papi.h value PAPI_L3_DCW"   papi_L3_DCW  :: CInt
foreign import capi "papi.h value PAPI_L1_ICH"   papi_L1_ICH  :: CInt
foreign import capi "papi.h value PAPI_L2_ICH"   papi_L2_ICH  :: CInt
foreign import capi "papi.h value PAPI_L3_ICH"   papi_L3_ICH  :: CInt
foreign import capi "papi.h value PAPI_L1_ICA"   papi_L1_ICA  :: CInt
foreign import capi "papi.h value PAPI_L2_ICA"   papi_L2_ICA  :: CInt
foreign import capi "papi.h value PAPI_L3_ICA"   papi_L3_ICA  :: CInt
foreign import capi "papi.h value PAPI_L1_ICR"   papi_L1_ICR  :: CInt
foreign import capi "papi.h value PAPI_L2_ICR"   papi_L2_ICR  :: CInt
foreign import capi "papi.h value PAPI_L3_ICR"   papi_L3_ICR  :: CInt
foreign import capi "papi.h value PAPI_L1_ICW"   papi_L1_ICW  :: CInt
foreign import capi "papi.h value PAPI_L2_ICW"   papi_L2_ICW  :: CInt
foreign import capi "papi.h value PAPI_L3_ICW"   papi_L3_ICW  :: CInt
foreign import capi "papi.h value PAPI_L1_TCH"   papi_L1_TCH  :: CInt
foreign import capi "papi.h value PAPI_L2_TCH"   papi_L2_TCH  :: CInt
foreign import capi "papi.h value PAPI_L3_TCH"   papi_L3_TCH  :: CInt
foreign import capi "papi.h value PAPI_L1_TCA"   papi_L1_TCA  :: CInt
foreign import capi "papi.h value PAPI_L2_TCA"   papi_L2_TCA  :: CInt
foreign import capi "papi.h value PAPI_L3_TCA"   papi_L3_TCA  :: CInt
foreign import capi "papi.h value PAPI_L1_TCR"   papi_L1_TCR  :: CInt
foreign import capi "papi.h value PAPI_L2_TCR"   papi_L2_TCR  :: CInt
foreign import capi "papi.h value PAPI_L3_TCR"   papi_L3_TCR  :: CInt
foreign import capi "papi.h value PAPI_L1_TCW"   papi_L1_TCW  :: CInt
foreign import capi "papi.h value PAPI_L2_TCW"   papi_L2_TCW  :: CInt
foreign import capi "papi.h value PAPI_L3_TCW"   papi_L3_TCW  :: CInt
foreign import capi "papi.h value PAPI_FML_INS"  papi_FML_INS :: CInt
foreign import capi "papi.h value PAPI_FAD_INS"  papi_FAD_INS :: CInt
foreign import capi "papi.h value PAPI_FDV_INS"  papi_FDV_INS :: CInt
foreign import capi "papi.h value PAPI_FSQ_INS"  papi_FSQ_INS :: CInt
foreign import capi "papi.h value PAPI_FNV_INS"  papi_FNV_INS :: CInt

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

foreign import capi unsafe "papi.h PAPI_strerror"
  papi_strerror :: CInt -> IO CString

-- Call PAPI function and return error code
call :: String -> IO CInt -> IO ()
call msg f = f >>= \case
  n | n == papi_OK -> pure ()
    | otherwise    -> do
        c_str <- papi_strerror n
        str   <- if | c_str == nullPtr -> pure "UNKNOWN ERROR"
                    | otherwise        -> peekCString c_str
        error $ printf "PAPI: %s: %s [%s]" msg str (show n)

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
      call "Failed to create eventset" $ papi_create_eventset p_evt
      peek p_evt
    fini evt = do
      call "Failed to cleanup eventset" $ papi_cleanup_eventset evt
      alloca $ \p_evt -> do
        poke p_evt evt
        call "Failed to destroy eventset" $ papi_destroy_eventset p_evt

-- | Supported hardware counters
-- 
-- Documentation is taken from rather outdated manual:
-- https://icl.utk.edu/projects/papi/files/documentation/PAPI_USER_GUIDE_23.htm
data Counter
  = L1_DCM  -- ^ Level 1 data cache misses
  | L1_ICM  -- ^ Level 1 instruction cache misses
  | L2_DCM  -- ^ Level 2 data cache misses
  | L2_ICM  -- ^ Level 2 instruction cache misses
  | L3_DCM  -- ^ Level 3 data cache misses
  | L3_ICM  -- ^ Level 3 instruction cache misses
  | L1_TCM  -- ^ Level 1 total cache misses
  | L2_TCM  -- ^ Level 2 total cache misses
  | L3_TCM  -- ^ Level 3 total cache misses
  | CA_SNP  -- ^ Requests for a Snoop
  | CA_SHR  -- ^ Requests for access to shared cache line (SMP)
  | CA_CLN  -- ^ Requests for access to clean cache line (SMP)
  | CA_INV  -- ^ Cache Line Invalidation (SMP)
  | CA_ITV  -- ^ Cache Line Intervention (SMP)
  | L3_LDM  -- ^ Level 3 load misses
  | L3_STM  -- ^ Level 3 store misses
  | BRU_IDL -- ^ Cycles branch units are idle
  | FXU_IDL -- ^ Cycles integer units are idle
  | FPU_IDL -- ^ Cycles floating point units are idle
  | LSU_IDL -- ^ Cycles load/store units are idle
  | TLB_DM  -- ^ Data translation lookaside buffer misses
  | TLB_IM  -- ^ Instruction translation lookaside buffer misses
  | TLB_TL  -- ^ Total translation lookaside buffer misses
  | L1_LDM  -- ^ Level 1 load misses
  | L1_STM  -- ^ Level 1 store misses
  | L2_LDM  -- ^ Level 2 load misses
  | L2_STM  -- ^ Level 2 store misses
  | BTAC_M  -- ^ Branch target address cache (BTAC) misses
  | PRF_DM  -- ^ Pre-fetch data instruction caused a miss
  | L3_DCH  -- ^ Level 3 Data Cache Hit
  | TLB_SD  -- ^ Translation lookaside buffer shootdowns (SMP)
  | CSR_FAL -- ^ Failed store conditional instructions
  | CSR_SUC -- ^ Successful store conditional instructions
  | CSR_TOT -- ^ Total store conditional instructions
  | MEM_SCY -- ^ Cycles Stalled Waiting for Memory Access
  | MEM_RCY -- ^ Cycles Stalled Waiting for Memory Read
  | MEM_WCY -- ^ Cycles Stalled Waiting for Memory Write
  | STL_ICY -- ^ Cycles with No Instruction Issue
  | FUL_ICY -- ^ Cycles with Maximum Instruction Issue
  | STL_CCY -- ^ Cycles with No Instruction Completion
  | FUL_CCY -- ^ Cycles with Maximum Instruction Completion
  | HW_INT  -- ^ Hardware interrupts
  | BR_UCN  -- ^ Unconditional branch instructions executed
  | BR_CN   -- ^ Conditional branch instructions executed
  | BR_TKN  -- ^ Conditional branch instructions taken
  | BR_NTK  -- ^ Conditional branch instructions not taken
  | BR_MSP  -- ^ Conditional branch instructions mispredicted
  | BR_PRC  -- ^ Conditional branch instructions correctly predicted
  | FMA_INS -- ^ FMA instructions completed
  | TOT_IIS -- ^ Total instructions issued
  | TOT_INS -- ^ Total instructions executed
  | INT_INS -- ^ Integer instructions executed
  | FP_INS  -- ^ Floating point instructions executed
  | LD_INS  -- ^ Load instructions executed
  | SR_INS  -- ^ Store instructions executed
  | BR_INS  -- ^ Total branch instructions executed
  | VEC_INS -- ^ Vector/SIMD instructions executed
  | RES_STL -- ^ Cycles processor is stalled on resource
  | FP_STAL -- ^ Cycles any FP units are stalled
  | TOT_CYC -- ^ Total cycles
  | LST_INS -- ^ Total load/store instructions executed
  | SYC_INS -- ^ Synchronization instructions executed
  | L1_DCH  -- ^ L1 data cache hits
  | L2_DCH  -- ^ L2 data cache hits
  | L1_DCA  -- ^ L1 data cache accesses
  | L2_DCA  -- ^ L2 data cache accesses
  | L3_DCA  -- ^ L3 data cache accesses
  | L1_DCR  -- ^ L1 data cache reads
  | L2_DCR  -- ^ L2 data cache reads
  | L3_DCR  -- ^ L3 data cache reads
  | L1_DCW  -- ^ L1 data cache writes
  | L2_DCW  -- ^ L2 data cache writes
  | L3_DCW  -- ^ L3 data cache writes
  | L1_ICH  -- ^ L1 instruction cache hits
  | L2_ICH  -- ^ L2 instruction cache hits
  | L3_ICH  -- ^ L3 instruction cache hits
  | L1_ICA  -- ^ L1 instruction cache accesses
  | L2_ICA  -- ^ L2 instruction cache accesses
  | L3_ICA  -- ^ L3 instruction cache accesses
  | L1_ICR  -- ^ L1 instruction cache reads
  | L2_ICR  -- ^ L2 instruction cache reads
  | L3_ICR  -- ^ L3 instruction cache reads
  | L1_ICW  -- ^ L1 instruction cache writes
  | L2_ICW  -- ^ L2 instruction cache writes
  | L3_ICW  -- ^ L3 instruction cache writes
  | L1_TCH  -- ^ L1 total cache hits
  | L2_TCH  -- ^ L2 total cache hits
  | L3_TCH  -- ^ L3 total cache hits
  | L1_TCA  -- ^ L1 total cache accesses
  | L2_TCA  -- ^ L2 total cache accesses
  | L3_TCA  -- ^ L3 total cache accesses
  | L1_TCR  -- ^ L1 total cache reads
  | L2_TCR  -- ^ L2 total cache reads
  | L3_TCR  -- ^ L3 total cache reads
  | L1_TCW  -- ^ L1 total cache writes
  | L2_TCW  -- ^ L2 total cache writes
  | L3_TCW  -- ^ L3 total cache writes
  | FML_INS -- ^ Floating Multiply instructions
  | FAD_INS -- ^ Floating Add instructions
  | FDV_INS -- ^ Floating Divide instructions
  | FSQ_INS -- ^ Floating Square Root instructions
  | FNV_INS -- ^ Floating Inverse instructions
  deriving (Show,Read,Eq,Ord)


toCounter :: Counter -> CInt
toCounter = \case
  L1_DCM  -> papi_L1_DCM
  L1_ICM  -> papi_L1_ICM
  L2_DCM  -> papi_L2_DCM
  L2_ICM  -> papi_L2_ICM
  L3_DCM  -> papi_L3_DCM
  L3_ICM  -> papi_L3_ICM
  L1_TCM  -> papi_L1_TCM
  L2_TCM  -> papi_L2_TCM
  L3_TCM  -> papi_L3_TCM
  CA_SNP  -> papi_CA_SNP
  CA_SHR  -> papi_CA_SHR
  CA_CLN  -> papi_CA_CLN
  CA_INV  -> papi_CA_INV
  CA_ITV  -> papi_CA_ITV
  L3_LDM  -> papi_L3_LDM
  L3_STM  -> papi_L3_STM
  BRU_IDL -> papi_BRU_IDL
  FXU_IDL -> papi_FXU_IDL
  FPU_IDL -> papi_FPU_IDL
  LSU_IDL -> papi_LSU_IDL
  TLB_DM  -> papi_TLB_DM
  TLB_IM  -> papi_TLB_IM
  TLB_TL  -> papi_TLB_TL
  L1_LDM  -> papi_L1_LDM
  L1_STM  -> papi_L1_STM
  L2_LDM  -> papi_L2_LDM
  L2_STM  -> papi_L2_STM
  BTAC_M  -> papi_BTAC_M
  PRF_DM  -> papi_PRF_DM
  L3_DCH  -> papi_L3_DCH
  TLB_SD  -> papi_TLB_SD
  CSR_FAL -> papi_CSR_FAL
  CSR_SUC -> papi_CSR_SUC
  CSR_TOT -> papi_CSR_TOT
  MEM_SCY -> papi_MEM_SCY
  MEM_RCY -> papi_MEM_RCY
  MEM_WCY -> papi_MEM_WCY
  STL_ICY -> papi_STL_ICY
  FUL_ICY -> papi_FUL_ICY
  STL_CCY -> papi_STL_CCY
  FUL_CCY -> papi_FUL_CCY
  HW_INT  -> papi_HW_INT
  BR_UCN  -> papi_BR_UCN
  BR_CN   -> papi_BR_CN
  BR_TKN  -> papi_BR_TKN
  BR_NTK  -> papi_BR_NTK
  BR_MSP  -> papi_BR_MSP
  BR_PRC  -> papi_BR_PRC
  FMA_INS -> papi_FMA_INS
  TOT_IIS -> papi_TOT_IIS
  TOT_INS -> papi_TOT_INS
  INT_INS -> papi_INT_INS
  FP_INS  -> papi_FP_INS
  LD_INS  -> papi_LD_INS
  SR_INS  -> papi_SR_INS
  BR_INS  -> papi_BR_INS
  VEC_INS -> papi_VEC_INS
  RES_STL -> papi_RES_STL
  FP_STAL -> papi_FP_STAL
  TOT_CYC -> papi_TOT_CYC
  LST_INS -> papi_LST_INS
  SYC_INS -> papi_SYC_INS
  L1_DCH  -> papi_L1_DCH
  L2_DCH  -> papi_L2_DCH
  L1_DCA  -> papi_L1_DCA
  L2_DCA  -> papi_L2_DCA
  L3_DCA  -> papi_L3_DCA
  L1_DCR  -> papi_L1_DCR
  L2_DCR  -> papi_L2_DCR
  L3_DCR  -> papi_L3_DCR
  L1_DCW  -> papi_L1_DCW
  L2_DCW  -> papi_L2_DCW
  L3_DCW  -> papi_L3_DCW
  L1_ICH  -> papi_L1_ICH
  L2_ICH  -> papi_L2_ICH
  L3_ICH  -> papi_L3_ICH
  L1_ICA  -> papi_L1_ICA
  L2_ICA  -> papi_L2_ICA
  L3_ICA  -> papi_L3_ICA
  L1_ICR  -> papi_L1_ICR
  L2_ICR  -> papi_L2_ICR
  L3_ICR  -> papi_L3_ICR
  L1_ICW  -> papi_L1_ICW
  L2_ICW  -> papi_L2_ICW
  L3_ICW  -> papi_L3_ICW
  L1_TCH  -> papi_L1_TCH
  L2_TCH  -> papi_L2_TCH
  L3_TCH  -> papi_L3_TCH
  L1_TCA  -> papi_L1_TCA
  L2_TCA  -> papi_L2_TCA
  L3_TCA  -> papi_L3_TCA
  L1_TCR  -> papi_L1_TCR
  L2_TCR  -> papi_L2_TCR
  L3_TCR  -> papi_L3_TCR
  L1_TCW  -> papi_L1_TCW
  L2_TCW  -> papi_L2_TCW
  L3_TCW  -> papi_L3_TCW
  FML_INS -> papi_FML_INS
  FAD_INS -> papi_FAD_INS
  FDV_INS -> papi_FDV_INS
  FSQ_INS -> papi_FSQ_INS
  FNV_INS -> papi_FNV_INS


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
  defaultValue = CounterSet [TOT_INS, TOT_CYC, BR_INS, BR_MSP]
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
          forM_ counters $ \c -> do
            call ("Failed to add counter " ++ show c) $ papi_add_event evt $ toCounter c
          allocaArray (length counters) $ \vals -> do
            -- Evaluate benchmark once in order to ensure that all
            -- parameters are evaluated. Consider benchmarks
            --
            -- > bench "A" $ nf funA xs
            -- > bench "B" $ nf funB xs
            --
            -- Without calling `io' we'll count instruction needed to
            -- evaluate xs as well!
            io
            -- We don't want to GC happen in the middle of benchmark
            -- just because previous benchmarks allocated enough to
            -- trigger it. This could bias measurement a lot since we
            -- run bencmark only once
            performMajorGC
            n1 <- getAllocationCounter
            -- Perform measurement
            call "Failed to start measurements" $ papi_start evt
            io
            call "Failed to stop measurements" $ papi_stop evt vals
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
{-# NOINLINE nf #-}
nf f a = Benchmarkable $ do _ <- evaluate $ force (f a)
                            return ()

-- | @nf f x@ measures number of instructions needed to compute weak
--   head normal form of and application of @f@ to @x@.
--
-- @since 0.1
whnf :: (a -> b) -> a -> Benchmarkable
{-# NOINLINE whnf #-}
whnf f a = Benchmarkable $ do _ <- evaluate (f a)
                              return ()

-- | @whnfIO a@ measures number of instructions needed to evaluate IO
--   action and reduce value returned by it to weak head normal form.
--
-- @since 0.1
whnfIO :: IO a -> Benchmarkable
{-# NOINLINE whnfIO #-}
whnfIO io = Benchmarkable $ do _ <- evaluate =<< io
                               return ()
-- | @nfIO a@ measures number of instructions needed to evaluate IO
--   action and reduce value returned by it to normal form.
--
-- @since 0.1
nfIO :: NFData a => IO a -> Benchmarkable
{-# NOINLINE nfIO #-}
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
        hPutStrLn h $ intercalate "," $ "benchmark" : "ALLOC" : (show <$> counters)
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

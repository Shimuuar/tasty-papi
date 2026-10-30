* Unreleased

  - Other Haskell threads, such as the finalizers that the major GC before
    a measurement schedules, run before counting starts rather than inside
    the counted region, where PAPI counted their work as the benchmark's.


* Changes in 0.1.2.0

  - `benchIngredients`, `consoleBenchReporter`, `csvReporter` are exported.


* Changes in 0.1.1.0

  - Human readable error messages not just bare error codes

  - Fixed header in CSV output


* Version 0.1

  Initial release

# tasti-papi

This is another approach to benchmarking which uses counting of CPU instructions
instead of time measurements. Underlying hardware counters are accessed using
[PAPI](https://icl.utk.edu/papi/) library so all hardware and OS support is
inherited from there.

This approach is suitable for benchmarking CPU bound code. Instruction count is
only proxy for code performance but it has advantage of being
deterministic. Timing measurements are affected by concurrent workloads and require
dedicated hardware in order to get precise measurements. Instruction counting is
not affected by this so it could be run as part of CI and used to detect
performance regressions.

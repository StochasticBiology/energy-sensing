# energy-sensing
How do different sensing abilities affect performance in changing environments?

`sensing-sim.c` and `sensing-plot.R` are the updated versions of simulated and plotting code, reflecting the most general model structure (environment frequency and phase, PID+c sensing parameters, cost scaling)

Compile with 

`gcc -o3 sensing-sim.c -lm -o sensing-sim.ce`

(or similar), then run with

`./sensing-sim.ce`

The code will loop through parameterisations and print lines to the screen periodically to demonstrate output. It should take maybe 45 minutes on a 2024 machine. It will produce a collection of time series snapshots and a large file (~380Mb) `redo-out-rk.csv` containing statistics of all the parameter sets. This is used by `sensing-plot.R` to produce figures.


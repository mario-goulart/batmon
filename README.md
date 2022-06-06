# batmon

A simple battery monitor for Linux in CHICKEN Scheme.

The `batmon` executable collects data on the available batteries every
two minutes and saves it under `~/.cache/batmon-data` by default.

The `batmon-plot` program can be used to plot the collected data.  It
requires Gnuplot.

Police Pursuit Crash Trends (FARS via rfars)

This repository provides a clean, reproducible R workflow to quantify and visualize police-pursuit crashes, injuries, and deaths in the U.S. using NHTSA’s Fatality Analysis Reporting System (FARS).

What the script does:
1. Sets global parameters (yrs = 2014:2023, metrics = c("crashes","injuries","fatalities")).
2. Uses rfars::annual_counts to build national and state annual panels for police pursuit involvement.
3. Produces ggplot2 figures with a colorblind-safe palette and exact point labels.
4. Batch-exports a national figure and all states to output/ as high-resolution PNG and PDF.

Key functions:
1. plot_nat_annual(...) — national annual time series with exact point labels.
2. plot_pursuit_state_precomp(state, years, overlay_national = FALSE) — state-specific annual series (precomputed).
3. export_all_state_plots(fars, years, overlay_national, out_root) — batch export for every state plus national.

Batch export writes to:
1. output/national/pursuit_national.png and .pdf
2. output/states/pursuit_<state>.png and .pdf

Note: FARS is fatal crashes only. So “injuries” is not the total injuries that occur during all pursuits, but “non fatal injuries that occur when there’s at least one fatality involved” – think pursued driver is killed, but passenger only injured.
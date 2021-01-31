# dualR Dual Cycling Power Analysis in R

This repository supports a shiny app that accepts two overlapping .fit files and generates a "critical power" comparison and plots of the data.

## Running

Locally, checkout and run the `dualR.Rproj` in RStudio. Run the app via `app.R`. Access the app via `http://127.0.0.1:7450/`

Docker, `docker run --rm -p 3838:3838 dblodgett/dualr`. Access the app via `http://localhost:3838/`

## Vision

The vision for this work was originally curiosity and a need to generate dual power analyses with data that are not natively 1hz recording rate. The work has evolved and may be used/reused for a variety of applications in the future.

## Contributing

Thanks for considering contributing to the project. I'd be excited for this project to grow through community contriubtion. Please get in touch via the issues with ideas.

## License: CC BY 4.0 [![License: CC BY 4.0](https://licensebuttons.net/l/by/4.0/80x15.png)](https://creativecommons.org/licenses/by/4.0/)
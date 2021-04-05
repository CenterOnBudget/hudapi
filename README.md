# hudapi

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

## Overview

Load data from the HUD User Data API into R, including Fair Market Rents and Income Limits.

Note: hudapi uses the HUD User Data API but is not endorsed or certified by HUD User.

## Installation

Install hudapi by running the following code:

```r
devtools::install_github("CenterOnBudget/hudapi")
```

Next, if you don't already have one, sign up for a HUD API [access token](https://www.huduser.gov/portal/dataset/fmr-api.html). Once you have your token, store it in your `.Renviron` file for easy-use by running the following code:

```r
usethis::edit_r_environ()
```

And adding your HUD API access token like so:

```r
HUD_API_TOKEN="your_token_here"
```

This allows hudapi to automatically load your access token when pulling data from the HUD API, and saves you from having to manually copy-paste your token around or openly include your token in scripts you may share with others (or post on GitHub).


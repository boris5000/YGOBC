# YGOBC

## Installation 
A recent-ish version of R is required to make this work, please install the latest version from the following link:
https://cloud.r-project.org/
Pick whichever OS you have and follow the prompts to install.

Launch R and install some of the required packages using the following:
```R
install.packages(c('curl', 'jsonlite', 'shiny', 'shinydashboard', 'DT'))
```

## Running the code

If you are running from the terminal you can use to launch the application
```bash
Rscript ./app.R
# Alternatively
R -e "source('https://raw.githubusercontent.com/boris5000/YGOBC/main/app.R')"
# Or
R -e "source('./app.R')"
```

Or from inside an R session
```R
source('https://raw.githubusercontent.com/boris5000/YGOBC/main/app.R')
```

## Using the App.
Give the package some time to bake, once it you see the red error text on the app. Click the browse button and select a collection.csv file as obtained by exporting from db.ygoprodeck.com/collection/

## To Do:
Add support for .ydk files.

# YGOCB

## Installation 
A recent-ish version of R is required to make this work, please install the latest version from the following link:
https://cloud.r-project.org/
Pick whichever OS you have and follow the prompts to install.

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
Give the package some time to bake. Once it you see the red error text on the app you are good to go!. 
Click the browse button and select a .csv file as obtained by exporting from db.ygoprodeck.com/collection/

###
If you run into any dependencies errors, just run the following bit of code inside an R session.
```R
install.packages(c('curl', 'jsonlite', 'shiny', 'shinydashboard', 'DT'))
```

## To Do:
Add support for .ydk files.

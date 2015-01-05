## Introduction
This package parses and consolidates ebola situation reports into a
machine-readable format. Currently it knows how to process all PDF situation reports from the Ministries of Health for <a href="http://www.mohsw.gov.lr/content_display.php?sub=report2">Liberia</a> and <a href="http://health.gov.sl/?page_id=583">Sierra Leone</a>. These situation reports are the most immediate source of data as they are typically updated daily. In contrast, the <a href="http://www.who.int/csr/disease/ebola/situation-reports/archive/en/">WHO data</a> is published weekly.

Anyone following the ebola outbreak knows how challenging it is to get quality data in a machine-readable format. I assume this is because scarce resources are (rightly) being spent fighting the outbreak as opposed to serving data scientists half a world away. At any rate, obtaining quality data is a significant challenge, where the solution has mostly been manual transcription of the data, as seen in <a href="https://github.com/cmrivers/ebola">Caitlin River's now stale repository</a>. Generosity and herculean efforts aside, the challenge with this approach is that human labor is costly and people have other things to do (like finish a dissertation). Clearly, automation is a better solution, but one that is non-trivial considering the situation reports are tables embedded in a PDF file. With all the publicity around Ebola and various <a href="http://www.eboladata.org/">hackathons</a> making contributions, I'm surprised that nobody has tackled the real issue of providing reliable and timely data. Personally, I think access to information is only useful if the data are readily usable by <em>both</em> humans and machines.

The motivation for this ebola.sitrep package, therefore, is to do the hard work of parsing a bunch of PDFs to provide timely machine-readable data on the ebola outbreak. Clean, regular data is the first step in conducting an analysis. A side motivation is that I wanted to use a real-world case study for my book, <em>Modeling Data With Functional Programming In R</em>. Hence, all the code is written in a functional style with minimal external dependencies. The goal is to demonstrate the power of the small set of higher order functions (map, fold/reduce, filter) that exist in any functional language.

As I need to finish my book, I'm actively looking for people to contribute to the codebase and add parse rules to clean the data further. Please see the github page for more information.

## Basic usage
If all you care about is the raw consolidated data, you can find that in the `data` directory.

If you want to analyze the data in R, I suggest installing the package as well.
```R
library(devtools)
install_github('ebola.sitrep','muxspace')
library(ebola.sitrep)
```

Note that you'll still need to load the data manually.
```R
data.sl <- read.csv('PACKAGE_HOME/data/sitrep_sl.csv')
data.lr <- read.csv('PACKAGE_HOME/data/sitrep_lr.csv')
```

## Measures and Counties
Each country reports a different set of metrics. While there is overlap at a high level, individual measures do not necessarily correspond with each other. Before comparing, be sure to understand what they mean.

### Sierra Leone
Parsed situation reports from Sierra Leone span 13 August - present. Note that the data date of a situation report usually lags the current day by 1.

The available measures can be accessed by looking at the column names of the `data.frame`. Be sure to cross reference these measures with the source PDFs to ensure data integrity.

```R
> data.sl <- read.csv('data/sitrep_sl.csv')
> colnames(data.sl)
 [1] "date"                   "county"                 "county.1"
 [4] "date.1"                 "total.contacts"         "completed.21d.total"
 [7] "incomplete.dead"        "under.followup"         "new.on.day"
[10] "healthy.on.day"         "ill.on.day"             "unvisited.on.day"
[13] "unseen.on.day"          "completed.21d.followup" "tracers.on.day"
[16] "population"             "alive.non.case"         "alive.suspect"
[19] "alive.probable"         "alive.confirm"          "cum.non.case"
[22] "cum.suspect"            "cum.probable"           "cum.confirm"
[25] "cum.dead.suspect"       "cum.dead.probable"      "cum.dead.confirmed"
[28] "CFR"
```

Notice that there are some artifacts from data merging (e.g. `"date.1"`) that can be safely ignored.

All reporting counties can be identified by getting the unique set.

```R
> unique(data.sl$county)
 [1] Bo                 Bombali            Bonthe             Kailahun
 [5] Kambia             Kenema             Koinadugu          Kono
 [9] Moyamba            National           Port Loko          Pujehun
[13] Tonkolili          Western area rural Western area urban
15 Levels: Bo Bombali Bonthe Kailahun Kambia Kenema Koinadugu Kono ... Western area urban
```

All measures can be plotted at county-level granularity. Here is an example using the number of people under followup.

```R
> counties <- as.character(unique(data.sl$county))
> with(data.sl[data.sl$county=='National',], 
+   plot(date, under.followup, type='l'))
> sapply(counties[counties != 'National'], function(cty) 
+   with(data.sl[data.sl$county==cty,], lines(date, under.followup, col='grey')))
```

<img src="https://cartesianfaith.files.wordpress.com/2015/01/sl-county.png?w=529" alt="sl.county" width="529" height="455" class="aligncenter size-large wp-image-1735" />

Note that the data have numerous gaps as the reporting is inconsistent.

### Liberia
The Liberian situation reports are a bit trickier to work with. Unlike Sierra Leone, the Liberian Ministry of Health only publishes a rolling window of historical situation reports. Hence, a smaller set is available, from 6 November - present. Anyone with source PDF files prior to that, please let me know (Twitter: <a href="https://twitter.com/cartesianfaith">@cartesianfaith</a>) or submit a pull request with the files added.

You can use the same approach to identify the counties for Liberia.
```R
>  unique(data.lr$county)
 [1] "Bomi"             "Bong"             "Gbarpolu"         "Grand Bassa"
 [5] "Grand Cape Mount" "Grand Gedeh"      "Grand Kru"        "Lofa"
 [9] "Margibi"          "Maryland"         "Montserrado"      "National"
[13] "Nimba"            "River Cess"       "River Gee"        "Sinoe"
```

And here are the measures. Again, you can safely ignore the merge artifacts.
```R
> colnames(data.lr)
 [1] "date"                   "county"                 "county.1"
 [4] "date.1"                 "alive.total"            "alive.suspect"
 [7] "alive.probable"         "dead.total"             "cum.total"
[10] "cum.suspect"            "cum.probable"           "cum.confirm"
[13] "cum.death"              "new.hcw.cases"          "new.hcw.deaths"
[16] "cum.hcw.cases"          "cum.hcw.deaths"         "new.contacts"
[19] "under.followup"         "seen.on.day"            "completed.21d.followup"
[22] "lost.followup"          "alive.confirm"          "dead.suspect"
[25] "dead.probable"          "dead.community"         "dead.patients"
```

## Data validation
In addition to downloading and parsing the situation reports, the package provides a function for validating the parsed data. Since the source data comes in a PDF, parsing errors are bound to happen. Worse, data can be entered into the source tables incorrectly. To ensure data integrity and identify inconsistent data, the validate function will aggregate county-level data and compare it to national data. In the following example, data for 2014-12-12 is inconsistent.

```R
> validate(data.lr, 'alive.total')
           counties national
2014-11-06       48       48
2014-11-07       50       50
2014-11-08       28       28
2014-11-14       20       20
...
2014-12-11       15       15
2014-12-12       25       26
2014-12-13       11       11
2014-12-14        6        6
```

It's a good idea to run this function on the measure you are interested before doing an analysis. That way you know in advance how reliable the data are. Also note that the parsing is imperfect, so big jumps in data are typically an indication of a poor parse. Use the source PDF to cross reference the data.

## Forecasting
Included in the package is a simple forecasting function that forecasts a given time series for the given county. By default, it uses the `'National'` county and selects the measure `'cum.dead.total'`. Currently the function assumes a log-linear relationship, so series that don't fit that behavior will have a crappy forecast.

```R
fc <- forecast(data.sl, measure='cum.dead.total')
```

<img class="aligncenter size-large wp-image-1733" src="https://cartesianfaith.files.wordpress.com/2015/01/sl-cum-dead.png?w=529" alt="sl.cum.dead" width="529" height="455" />

The result of the function is the actual forecast for the measure in `data.frame` format.

## Contribute to the code
As hinted earlier, I welcome any help in adding parse rules to improve the data quality. Someone more intrepid could even add a new country to the dataset or figure out how to consolidate the measures across countries. It's easiest to reach out over Twitter: <a href="https://twitter.com/cartesianfaith">@cartesianfaith</a>.

### Building the package
It is recommended to use <a href="https://github.com/muxspace/crant">crant</a> for building. Once you've cloned and added crant to your path, simply run `crant` from the shell.

```bash
cd ebola.sitrep
crant
```

During development, you might want build against the modified source (`-S`),
install the latest code (`-i`) or update the documentation (`-x`).
```bash
crant -Six
```

Pull requests must pass CRAN checks.

### Code layout
High-level code and generic functions are in `ebola.R`. Country specific code
is in the country files (`sl.R` and `lr.R`). I use ISO-2 country codes as
abbreviations. Please stick to that convention.

Parse rules are in specific country files and are named "parse_" + ISO-2.
They are big functions with numerous closures embedded.
The key parts to look at are the `get_config` functions,
which manage the different configurations (regular expressions
to demarcate different tables and rows). These functions also contain
regular expressions that fix typos and other syntax issues in the raw
data.

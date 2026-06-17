# Purpose

This folder contains my solutions to the Data Science for Economics and
Finance Practical Exam. Each question is contained in its own subfolder,
with functions stored in the respective `code/` folders and data stored
in the respective `data/` folders (gitignored).

------------------------------------------------------------------------

# Question 1: Coffee Hub

## Approach

A coffee entrepreneur wants to know which origin, roast strength, and
supplier to stock for a new shop, and how that lines up with what
Stellenbosch students say they like in their coffee.

I approached this by building one function per plot, namely origin vs
cost, roast strength, top suppliers, and a keyword-match check against
the student survey wordcloud. I cleaned the data first (checked for
missing values and tidied a few wide-format columns into long form),
then built each plot to answer one specific part of the brief rather
than trying to cram everything into a single chart.

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
list.files('Question1/code/', full.names = T, recursive = T) %>%
    .[grepl('.R', .)] %>%
    as.list() %>%
    walk(~source(.))

Coffee <- silent_read(path = "Question1/data/Coffee/Coffee.csv")
```

### Data prep

Checked for missing values and a few messy duplicate names (e.g. roaster
names with curly vs straight apostrophes) before tidying
`origin_1`/`origin_2` and `desc_1`/`desc_2`/`desc_3` from wide to long
format, since each is really one variable spread across multiple
columns.

``` r
# Check for missing values/NA's
Coffee %>% summarise(across(everything(), ~sum(is.na(.))))
```

    ## # A tibble: 1 × 12
    ##    name roaster roast loc_country origin_1 origin_2 Cost_Per_100g Rating
    ##   <int>   <int> <int>       <int>    <int>    <int>         <int>  <int>
    ## 1     0       0    15           0        0        0             0      0
    ## # ℹ 4 more variables: review_date <int>, desc_1 <int>, desc_2 <int>,
    ## #   desc_3 <int>

``` r
Coffee_long <- Coffee %>%
    gather(origin_rank, origin, origin_1, origin_2) %>%
    gather(desc_rank, desc, desc_1, desc_2, desc_3) %>%
    filter(!is.na(desc))  # two coffees are missing a third review so those rows are dropped

# Chiriquí was splitting into 2 rows under different spellings, messing up the "best rated" label - fixed here so it applies to all the origin plots
Coffee_long <- Coffee_long %>%
    mutate(origin = gsub(" Growing Region| Growing District| District| Department| County| Province| Zone| Region", "", origin)) %>%
    mutate(origin = gsub("í", "i", origin))
```

### Origin vs cost

I plotted cost against rating to see if price actually tracks quality —
bubble size shows review count, so a region’s position isn’t trusted
equally regardless of how many reviews back it up.

Northern Sumatra is both the highest-rated origin and inexpensive, while
Cobán shows a well-rated coffee can be found for under $4 — high rating
doesn’t require a high price here.

``` r
g <- origin_cost_scatter(Coffee_long)
```

    ## Warning: The `<scale>` argument of `guides()` cannot be `FALSE`. Use "none" instead as
    ## of ggplot2 3.3.4.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning in loadfonts_win(quiet = quiet): OS is not Windows. No fonts registered
    ## with windowsFonts().

    ## Warning: The `size` argument of `element_rect()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## ℹ The deprecated feature was likely used in the fmxdat package.
    ##   Please report the issue at <https://github.com/nicktz/fmxdat/issues>.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## ℹ The deprecated feature was likely used in the fmxdat package.
    ##   Please report the issue at <https://github.com/nicktz/fmxdat/issues>.
    ## This warning is displayed once per session.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
g
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

### Roast strength

A boxplot with individual reviews jittered on top shows both the typical
rating per roast and how much it varies. This is useful since a roast
could have a good median but huge spread.

Light and Medium-Light roasts consistently outscore Medium, Medium-Dark,
and especially Dark roasts.

``` r
g <- roast_plot(Coffee)
```

    ## Warning in loadfonts_win(quiet = quiet): OS is not Windows. No fonts registered
    ## with windowsFonts().

``` r
g
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

### Top suppliers

Ranked suppliers by rating directly, since that’s the simplest way to
answer “who should she buy from”. I filtered to a review-count minimum
so high ratings (with a low number of them) don’t dominate, and a cost
ceiling so the list stays realistic for a student-shop having to compete
with places like MyVrew which sell relatively cheap coffee.

Top-rated suppliers under $20/100g with at least 5 reviews each.

``` r
g <- supplier_plot(Coffee)
```

    ## Warning in loadfonts_win(quiet = quiet): OS is not Windows. No fonts registered
    ## with windowsFonts().

``` r
g
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

### Matching student preferences

The brief gave a wordcloud of words Stellenbosch students used to
describe coffee they liked. I picked the words that stood out most and
checked which origins’ reviews mention them the most.

Reviews from Yemen, Tarrazu, and Boquete mention student-preferred
flavour words (chocolate, aroma, savory, toned, sweetly, tart) most
often.

``` r
g <- keyword_plot(Coffee_long)
```

    ## Warning in loadfonts_win(quiet = quiet): OS is not Windows. No fonts registered
    ## with windowsFonts().

``` r
g
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

------------------------------------------------------------------------

# Question 2: Baby Names

## Approach

Brief description of approach.

## Figure 1

Interpretation.

## Figure 2

Interpretation.

------------------------------------------------------------------------

# Question 3: Loans and Credit

## Approach

Brief description of approach.

------------------------------------------------------------------------

# Question 4: Netflix

## Approach

Brief description. Note which aspects of the data were chosen to explore
and why.

------------------------------------------------------------------------

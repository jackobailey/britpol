
<!-- README.md is generated from README.Rmd. Please edit that file -->

# britpol

<img src="https://raw.githubusercontent.com/jackobailey/britpol/master/documentation/_assets/hex.png" alt="The britpol hexlogo" align="right" width="200" style="padding: 0 15px; float: right;"/>

The `{britpol}` package makes analysing British political data quick and
simple. It contains two pre-formatted datasets, plus a host of useful
functions. The first dataset, `pollbase`, is a long-format version of
Mark Pack’s dataset of historic British public opinion polls combined
with more recent data from Wikipedia. The second, `pollbasepro`,
provides 24,307 daily estimates of voting intention figures for each of
Britain’s three largest parties from 26 May 1955 to 11 December 2021.

To install the latest version of `{britpol}`, run the following code in
`R`:

``` r
devtools::install_github("jackobailey/britpol")
```

## Latest Polling Estimates from PollBasePro

<img src="https://raw.githubusercontent.com/jackobailey/britpol/master/documentation/_assets/timeplot_gh.png" alt="Recent polling figures" align="right" width="400" style="padding: 0 15px; float: right;"/>

**British Poll of Polls, 11 December 2021:**

-   **Labour Party:** 40% (37% to 42%)
-   **Conservative Party:** 32% (30% to 35%)
-   **Liberal Democrats:** 11% (10% to 13%)

`pollbasepro` suggests that the Labour Party is the largest party in
Britain. They hold a lead over the Conservative Party of around 7pp (4pp
to 10pp). This puts the Conservative Party in second and the Liberal
Democrats in third.

## Notes, Usage, and Attribution

`{britpol}`, `pollbase`, and `pollbasepro` will change over time as
elections come and go. Users should use only the most recent version of
the package when conducting their analyses. Like any project, some minor
mistakes might have crept into the code. If you think that you have
found an error or would like to make a recommendation for a future
update, please [raise an
issue](https://github.com/jackobailey/britpol/issues).

You may also use the `{britpol}` codebase for your own purposes in line
with [its
license](https://github.com/jackobailey/britpol/blob/master/LICENSE.md).
But you must do so *with attribution*. That is, you may reproduce,
reuse, and adapt the code as you see fit, but must state in each case
that you used `{britpol}` to produce your work. The relevant citations
are as follows:

### britpol

-   **Documentation:** Bailey, J. (2021) britpol v0.1.0: User Guide and
    Data Codebook. Retrieved from
    <https://doi.org/10.17605/OSF.IO/2M9GB>.

### PollBasePro

-   **Data:** Bailey, J., M. Pack, and L. Mansillo (2021) PollBasePro:
    Daily Estimates of Aggregate Voting Intention in Great Britain from
    1955 to 2021 v.0.1.0 \[computer file\], December 2021. Retrieved
    from <https://doi.org/10.7910/DVN/3POIQW>.

-   **Paper:** Bailey, J., M. Pack, and L. Mansillo (2021) PollBasePro:
    Daily Estimates of Aggregate Voting Intention in Great Britain from
    1955 to 2021. Retrieved from doi.

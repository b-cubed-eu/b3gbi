# Extract Years With Observations from an Indicator Map

Takes an "indicator_map", "indicator_ts", or "processed_cube" object and
determines the years for which observation data exists.

## Usage

``` r
get_observed_years(x)
```

## Arguments

- x:

  An "indicator_map" object containing calculated indicator values
  associated with grid cells.

## Value

A data frame with two columns:

- `years`: A sequence of years covering the range of observations.

- `occurrences`: A logical vector indicating if observations exist for
  each year (`TRUE` if present, `FALSE` if absent).

## Examples

``` r
total_occ_mammals_denmark <- total_occ_map(example_cube_1, level = "country",
                                           region = "Denmark")
get_observed_years(total_occ_mammals_denmark)
#>     years occurrences
#> 1    1862        TRUE
#> 2    1863        TRUE
#> 3    1864       FALSE
#> 4    1865       FALSE
#> 5    1866       FALSE
#> 6    1867       FALSE
#> 7    1868       FALSE
#> 8    1869       FALSE
#> 9    1870        TRUE
#> 10   1871       FALSE
#> 11   1872       FALSE
#> 12   1873       FALSE
#> 13   1874        TRUE
#> 14   1875       FALSE
#> 15   1876       FALSE
#> 16   1877       FALSE
#> 17   1878       FALSE
#> 18   1879        TRUE
#> 19   1880       FALSE
#> 20   1881        TRUE
#> 21   1882       FALSE
#> 22   1883       FALSE
#> 23   1884        TRUE
#> 24   1885       FALSE
#> 25   1886        TRUE
#> 26   1887       FALSE
#> 27   1888       FALSE
#> 28   1889       FALSE
#> 29   1890       FALSE
#> 30   1891       FALSE
#> 31   1892       FALSE
#> 32   1893       FALSE
#> 33   1894        TRUE
#> 34   1895       FALSE
#> 35   1896        TRUE
#> 36   1897        TRUE
#> 37   1898        TRUE
#> 38   1899       FALSE
#> 39   1900       FALSE
#> 40   1901        TRUE
#> 41   1902        TRUE
#> 42   1903        TRUE
#> 43   1904       FALSE
#> 44   1905        TRUE
#> 45   1906       FALSE
#> 46   1907        TRUE
#> 47   1908        TRUE
#> 48   1909       FALSE
#> 49   1910        TRUE
#> 50   1911        TRUE
#> 51   1912       FALSE
#> 52   1913        TRUE
#> 53   1914        TRUE
#> 54   1915       FALSE
#> 55   1916       FALSE
#> 56   1917        TRUE
#> 57   1918       FALSE
#> 58   1919       FALSE
#> 59   1920        TRUE
#> 60   1921        TRUE
#> 61   1922       FALSE
#> 62   1923        TRUE
#> 63   1924        TRUE
#> 64   1925        TRUE
#> 65   1926        TRUE
#> 66   1927        TRUE
#> 67   1928       FALSE
#> 68   1929        TRUE
#> 69   1930        TRUE
#> 70   1931        TRUE
#> 71   1932        TRUE
#> 72   1933        TRUE
#> 73   1934        TRUE
#> 74   1935        TRUE
#> 75   1936        TRUE
#> 76   1937        TRUE
#> 77   1938        TRUE
#> 78   1939        TRUE
#> 79   1940        TRUE
#> 80   1941        TRUE
#> 81   1942        TRUE
#> 82   1943        TRUE
#> 83   1944        TRUE
#> 84   1945        TRUE
#> 85   1946        TRUE
#> 86   1947        TRUE
#> 87   1948        TRUE
#> 88   1949        TRUE
#> 89   1950        TRUE
#> 90   1951        TRUE
#> 91   1952        TRUE
#> 92   1953        TRUE
#> 93   1954        TRUE
#> 94   1955        TRUE
#> 95   1956        TRUE
#> 96   1957        TRUE
#> 97   1958        TRUE
#> 98   1959        TRUE
#> 99   1960        TRUE
#> 100  1961        TRUE
#> 101  1962        TRUE
#> 102  1963        TRUE
#> 103  1964        TRUE
#> 104  1965        TRUE
#> 105  1966        TRUE
#> 106  1967        TRUE
#> 107  1968        TRUE
#> 108  1969        TRUE
#> 109  1970        TRUE
#> 110  1971        TRUE
#> 111  1972        TRUE
#> 112  1973        TRUE
#> 113  1974        TRUE
#> 114  1975        TRUE
#> 115  1976        TRUE
#> 116  1977        TRUE
#> 117  1978        TRUE
#> 118  1979        TRUE
#> 119  1980        TRUE
#> 120  1981        TRUE
#> 121  1982        TRUE
#> 122  1983        TRUE
#> 123  1984        TRUE
#> 124  1985        TRUE
#> 125  1986        TRUE
#> 126  1987        TRUE
#> 127  1988        TRUE
#> 128  1989        TRUE
#> 129  1990        TRUE
#> 130  1991        TRUE
#> 131  1992        TRUE
#> 132  1993        TRUE
#> 133  1994        TRUE
#> 134  1995        TRUE
#> 135  1996        TRUE
#> 136  1997        TRUE
#> 137  1998        TRUE
#> 138  1999        TRUE
#> 139  2000        TRUE
#> 140  2001        TRUE
#> 141  2002        TRUE
#> 142  2003        TRUE
#> 143  2004        TRUE
#> 144  2005        TRUE
#> 145  2006        TRUE
#> 146  2007        TRUE
#> 147  2008        TRUE
#> 148  2009        TRUE
#> 149  2010        TRUE
#> 150  2011        TRUE
#> 151  2012        TRUE
#> 152  2013        TRUE
#> 153  2014        TRUE
#> 154  2015        TRUE
#> 155  2016        TRUE
#> 156  2017        TRUE
#> 157  2018        TRUE
#> 158  2019        TRUE
#> 159  2020        TRUE
#> 160  2021        TRUE
#> 161  2022        TRUE
#> 162  2023        TRUE
#> 163  2024        TRUE
```

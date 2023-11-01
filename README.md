FAERS-Pharmacovigilance
================

## Installation

You can install the development version of `faers` from
[GitHub](https://github.com/) with:

``` r
if (!requireNamespace("pak")) {
  install.packages("pak",
    repos = sprintf(
      "https://r-lib.github.io/p/pak/devel/%s/%s/%s",
      .Platform$pkgType, R.Version()$os, R.Version()$arch
    )
  )
}
pak::pkg_install("Yunuuuu/faers")
```

## Pharmacovigilance Analysis using FAERS

FAERS is a database for the spontaneous reporting of adverse events and
medication errors involving human drugs and therapeutic biological
products. This package accelarate the process of Pharmacovigilance
Analysis using FAERS.

``` r
library(faers)
```

### check metadata of FAERS

This will return a data.table reporting years, period, quarter, and file
urls and file sizes. By default, this will use the cached file in
`rappdirs::user_cache_dir("faers")`. If it doesn’t exist, the internal
will parse metadata in
<https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html>

``` r
faers_meta()
#> → Using FAERS metadata from cached
#>   '~/.cache/faers/metadata/faers_meta_data.rds'
#>   Snapshot time: 2023-11-01 15:44:07.444066
#>      year quarter             period
#>     <int>  <char>             <char>
#>  1:  2023      q2       April - June
#>  2:  2023      q1    January - March
#>  3:  2022      q4 October - December
#>  4:  2022      q3   July - September
#>  5:  2022      q2       April - June
#>  6:  2022      q1    January - March
#>  7:  2021      q4 October - December
#>  8:  2021      q3   July - September
#>  9:  2021      q2       April - June
#> 10:  2021      q1    January - March
#> 11:  2020      q4 October - December
#> 12:  2020      q3   July - September
#> 13:  2020      q2       April - June
#> 14:  2020      q1    January - March
#> 15:  2019      q4 October - December
#> 16:  2019      q3   July - September
#> 17:  2019      q2       April - June
#> 18:  2019      q1    January - March
#> 19:  2018      q4 October - December
#> 20:  2018      q3   July - September
#> 21:  2018      q2       April - June
#> 22:  2018      q1    January - March
#> 23:  2017      q4 October - December
#> 24:  2017      q3   July - September
#> 25:  2017      q2       April - June
#> 26:  2017      q1    January - March
#> 27:  2016      q4 October - December
#> 28:  2016      q3   July - September
#> 29:  2016      q2       April - June
#> 30:  2016      q1    January - March
#> 31:  2015      q4 October - December
#> 32:  2015      q3   July - September
#> 33:  2015      q2       April - June
#> 34:  2015      q1    January - March
#> 35:  2014      q4 October - December
#> 36:  2014      q3   July - September
#> 37:  2014      q2       April - June
#> 38:  2014      q1    January - March
#> 39:  2013      q4 October - December
#> 40:  2013      q3   July - September
#> 41:  2013      q2       April - June
#> 42:  2013      q1    January - March
#> 43:  2012      q4 October - December
#> 44:  2012      q3   July - September
#> 45:  2012      q2       April - June
#> 46:  2012      q1    January - March
#> 47:  2011      q4 October - December
#> 48:  2011      q3   July - September
#> 49:  2011      q2       April - June
#> 50:  2011      q1    January - March
#> 51:  2010      q4 October - December
#> 52:  2010      q3   July - September
#> 53:  2010      q2       April - June
#> 54:  2010      q1    January - March
#> 55:  2009      q4 October - December
#> 56:  2009      q3   July - September
#> 57:  2009      q2       April - June
#> 58:  2009      q1    January - March
#> 59:  2008      q4 October - December
#> 60:  2008      q3   July - September
#> 61:  2008      q2       April - June
#> 62:  2008      q1    January - March
#> 63:  2007      q4 October - December
#> 64:  2007      q3   July - September
#> 65:  2007      q2       April - June
#> 66:  2007      q1    January - March
#> 67:  2006      q4 October - December
#> 68:  2006      q3   July - September
#> 69:  2006      q2       April - June
#> 70:  2006      q1    January - March
#>                                                     ascii_urls ascii_file_size
#>                                                         <char>          <char>
#>  1: https://fis.fda.gov/content/Exports/faers_ascii_2023q2.zip          64.5MB
#>  2: https://fis.fda.gov/content/Exports/faers_ascii_2023q1.zip          64.3MB
#>  3: https://fis.fda.gov/content/Exports/faers_ascii_2022Q4.zip            69MB
#>  4: https://fis.fda.gov/content/Exports/faers_ascii_2022Q3.zip          63.2MB
#>  5: https://fis.fda.gov/content/Exports/faers_ascii_2022q2.zip            63MB
#>  6: https://fis.fda.gov/content/Exports/faers_ascii_2022q1.zip          64.7MB
#>  7: https://fis.fda.gov/content/Exports/faers_ascii_2021Q4.zip            59MB
#>  8: https://fis.fda.gov/content/Exports/faers_ascii_2021Q3.zip            70MB
#>  9: https://fis.fda.gov/content/Exports/faers_ascii_2021Q2.zip            66MB
#> 10: https://fis.fda.gov/content/Exports/faers_ascii_2021Q1.zip            69MB
#> 11: https://fis.fda.gov/content/Exports/faers_ascii_2020Q4.zip            71MB
#> 12: https://fis.fda.gov/content/Exports/faers_ascii_2020Q3.zip            64MB
#> 13: https://fis.fda.gov/content/Exports/faers_ascii_2020Q2.zip            66MB
#> 14: https://fis.fda.gov/content/Exports/faers_ascii_2020Q1.zip            65MB
#> 15: https://fis.fda.gov/content/Exports/faers_ascii_2019Q4.zip            60MB
#> 16: https://fis.fda.gov/content/Exports/faers_ascii_2019Q3.zip            62MB
#> 17: https://fis.fda.gov/content/Exports/faers_ascii_2019Q2.zip            62MB
#> 18: https://fis.fda.gov/content/Exports/faers_ascii_2019Q1.zip            56MB
#> 19: https://fis.fda.gov/content/Exports/faers_ascii_2018q4.zip            60MB
#> 20: https://fis.fda.gov/content/Exports/faers_ascii_2018q3.zip            60MB
#> 21: https://fis.fda.gov/content/Exports/faers_ascii_2018q2.zip            60MB
#> 22: https://fis.fda.gov/content/Exports/faers_ascii_2018q1.zip            52MB
#> 23: https://fis.fda.gov/content/Exports/faers_ascii_2017q4.zip            41MB
#> 24: https://fis.fda.gov/content/Exports/faers_ascii_2017q3.zip            48MB
#> 25: https://fis.fda.gov/content/Exports/faers_ascii_2017q2.zip            46MB
#> 26: https://fis.fda.gov/content/Exports/faers_ascii_2017q1.zip            48MB
#> 27: https://fis.fda.gov/content/Exports/faers_ascii_2016q4.zip            44MB
#> 28: https://fis.fda.gov/content/Exports/faers_ascii_2016q3.zip            46MB
#> 29: https://fis.fda.gov/content/Exports/faers_ascii_2016q2.zip            44MB
#> 30: https://fis.fda.gov/content/Exports/faers_ascii_2016q1.zip            46MB
#> 31: https://fis.fda.gov/content/Exports/faers_ascii_2015q4.zip            42MB
#> 32: https://fis.fda.gov/content/Exports/faers_ascii_2015q3.zip            47MB
#> 33: https://fis.fda.gov/content/Exports/faers_ascii_2015q2.zip            38MB
#> 34: https://fis.fda.gov/content/Exports/faers_ascii_2015q1.zip            39MB
#> 35: https://fis.fda.gov/content/Exports/faers_ascii_2014q4.zip            28MB
#> 36: https://fis.fda.gov/content/Exports/faers_ascii_2014q3.zip            28MB
#> 37: https://fis.fda.gov/content/Exports/faers_ascii_2014q2.zip            25MB
#> 38: https://fis.fda.gov/content/Exports/faers_ascii_2014q1.zip            30MB
#> 39: https://fis.fda.gov/content/Exports/faers_ascii_2013q4.zip            26MB
#> 40: https://fis.fda.gov/content/Exports/faers_ascii_2013q3.zip            22MB
#> 41: https://fis.fda.gov/content/Exports/faers_ascii_2013q2.zip            21MB
#> 42: https://fis.fda.gov/content/Exports/faers_ascii_2013q1.zip            25MB
#> 43: https://fis.fda.gov/content/Exports/faers_ascii_2012q4.zip            28MB
#> 44:  https://fis.fda.gov/content/Exports/aers_ascii_2012q3.zip            16MB
#> 45:  https://fis.fda.gov/content/Exports/aers_ascii_2012q2.zip            25MB
#> 46:  https://fis.fda.gov/content/Exports/aers_ascii_2012q1.zip            26MB
#> 47:  https://fis.fda.gov/content/Exports/aers_ascii_2011q4.zip            23MB
#> 48:  https://fis.fda.gov/content/Exports/aers_ascii_2011q3.zip            23MB
#> 49:  https://fis.fda.gov/content/Exports/aers_ascii_2011q2.zip            23MB
#> 50:  https://fis.fda.gov/content/Exports/aers_ascii_2011q1.zip            21MB
#> 51:  https://fis.fda.gov/content/Exports/aers_ascii_2010q4.zip            20MB
#> 52:  https://fis.fda.gov/content/Exports/aers_ascii_2010q3.zip            22MB
#> 53:  https://fis.fda.gov/content/Exports/aers_ascii_2010q2.zip            17MB
#> 54:  https://fis.fda.gov/content/Exports/aers_ascii_2010q1.zip            16MB
#> 55:  https://fis.fda.gov/content/Exports/aers_ascii_2009q4.zip            16MB
#> 56:  https://fis.fda.gov/content/Exports/aers_ascii_2009q3.zip            16MB
#> 57:  https://fis.fda.gov/content/Exports/aers_ascii_2009q2.zip            14MB
#> 58:  https://fis.fda.gov/content/Exports/aers_ascii_2009q1.zip            13MB
#> 59:  https://fis.fda.gov/content/Exports/aers_ascii_2008q4.zip            13MB
#> 60:  https://fis.fda.gov/content/Exports/aers_ascii_2008q3.zip            13MB
#> 61:  https://fis.fda.gov/content/Exports/aers_ascii_2008q2.zip            12MB
#> 62:  https://fis.fda.gov/content/Exports/aers_ascii_2008q1.zip            12MB
#> 63:  https://fis.fda.gov/content/Exports/aers_ascii_2007q4.zip            12MB
#> 64:  https://fis.fda.gov/content/Exports/aers_ascii_2007q3.zip           9.9MB
#> 65:  https://fis.fda.gov/content/Exports/aers_ascii_2007q2.zip           9.5MB
#> 66:  https://fis.fda.gov/content/Exports/aers_ascii_2007q1.zip           9.6MB
#> 67:  https://fis.fda.gov/content/Exports/aers_ascii_2006q4.zip           9.1MB
#> 68:  https://fis.fda.gov/content/Exports/aers_ascii_2006q3.zip           8.5MB
#> 69:  https://fis.fda.gov/content/Exports/aers_ascii_2006q2.zip           9.7MB
#> 70:  https://fis.fda.gov/content/Exports/aers_ascii_2006q1.zip            11MB
#>                                                     xml_urls xml_file_size
#>                                                       <char>        <char>
#>  1: https://fis.fda.gov/content/Exports/faers_xml_2023q2.zip         130MB
#>  2: https://fis.fda.gov/content/Exports/faers_xml_2023q1.zip         133MB
#>  3: https://fis.fda.gov/content/Exports/faers_xml_2022Q4.zip         144MB
#>  4: https://fis.fda.gov/content/Exports/faers_xml_2022Q3.zip         132MB
#>  5: https://fis.fda.gov/content/Exports/faers_xml_2022q2.zip         140MB
#>  6: https://fis.fda.gov/content/Exports/faers_xml_2022q1.zip         136MB
#>  7: https://fis.fda.gov/content/Exports/faers_xml_2021Q4.zip         123MB
#>  8: https://fis.fda.gov/content/Exports/faers_xml_2021Q3.zip         132MB
#>  9: https://fis.fda.gov/content/Exports/faers_xml_2021Q2.zip         123MB
#> 10: https://fis.fda.gov/content/Exports/faers_xml_2021Q1.zip         130MB
#> 11: https://fis.fda.gov/content/Exports/faers_xml_2020Q4.zip         131MB
#> 12: https://fis.fda.gov/content/Exports/faers_xml_2020Q3.zip         121MB
#> 13: https://fis.fda.gov/content/Exports/faers_xml_2020Q2.zip         123MB
#> 14: https://fis.fda.gov/content/Exports/faers_xml_2020Q1.zip         125MB
#> 15: https://fis.fda.gov/content/Exports/faers_xml_2019Q4.zip         113MB
#> 16: https://fis.fda.gov/content/Exports/faers_xml_2019Q3.zip         118MB
#> 17: https://fis.fda.gov/content/Exports/faers_xml_2019Q2.zip         118MB
#> 18: https://fis.fda.gov/content/Exports/faers_xml_2019Q1.zip         103MB
#> 19: https://fis.fda.gov/content/Exports/faers_xml_2018q4.zip         112MB
#> 20: https://fis.fda.gov/content/Exports/faers_xml_2018q3.zip         112MB
#> 21: https://fis.fda.gov/content/Exports/faers_xml_2018q2.zip         112MB
#> 22: https://fis.fda.gov/content/Exports/faers_xml_2018q1.zip          94MB
#> 23: https://fis.fda.gov/content/Exports/faers_xml_2017q4.zip          76MB
#> 24: https://fis.fda.gov/content/Exports/faers_xml_2017q3.zip          91MB
#> 25: https://fis.fda.gov/content/Exports/faers_xml_2017q2.zip          86MB
#> 26: https://fis.fda.gov/content/Exports/faers_xml_2017q1.zip          91MB
#> 27: https://fis.fda.gov/content/Exports/faers_xml_2016q4.zip          82MB
#> 28: https://fis.fda.gov/content/Exports/faers_xml_2016q3.zip          87MB
#> 29: https://fis.fda.gov/content/Exports/faers_xml_2016q2.zip          81MB
#> 30: https://fis.fda.gov/content/Exports/faers_xml_2016q1.zip          84MB
#> 31: https://fis.fda.gov/content/Exports/faers_xml_2015q4.zip          77MB
#> 32: https://fis.fda.gov/content/Exports/faers_xml_2015q3.zip          88MB
#> 33: https://fis.fda.gov/content/Exports/faers_xml_2015q2.zip          70MB
#> 34: https://fis.fda.gov/content/Exports/faers_xml_2015q1.zip          72MB
#> 35: https://fis.fda.gov/content/Exports/faers_xml_2014q4.zip          53MB
#> 36: https://fis.fda.gov/content/Exports/faers_xml_2014q3.zip          54MB
#> 37: https://fis.fda.gov/content/Exports/faers_xml_2014q2.zip          44MB
#> 38: https://fis.fda.gov/content/Exports/faers_xml_2014q1.zip          52MB
#> 39: https://fis.fda.gov/content/Exports/faers_xml_2013q4.zip          46MB
#> 40: https://fis.fda.gov/content/Exports/faers_xml_2013q3.zip          40MB
#> 41: https://fis.fda.gov/content/Exports/faers_xml_2013q2.zip          38MB
#> 42: https://fis.fda.gov/content/Exports/faers_xml_2013q1.zip          44MB
#> 43: https://fis.fda.gov/content/Exports/faers_xml_2012q4.zip          50MB
#> 44: https://fis.fda.gov/content/Exports/aers_sgml_2012q3.zip          21MB
#> 45: https://fis.fda.gov/content/Exports/aers_sgml_2012q2.zip          32MB
#> 46: https://fis.fda.gov/content/Exports/aers_sgml_2012q1.zip          33MB
#> 47: https://fis.fda.gov/content/Exports/aers_sgml_2011q4.zip          29MB
#> 48: https://fis.fda.gov/content/Exports/aers_sgml_2011q3.zip          29MB
#> 49: https://fis.fda.gov/content/Exports/aers_sgml_2011q2.zip          29MB
#> 50: https://fis.fda.gov/content/Exports/aers_sgml_2011q1.zip          26MB
#> 51: https://fis.fda.gov/content/Exports/aers_sgml_2010q4.zip          25MB
#> 52: https://fis.fda.gov/content/Exports/aers_sgml_2010q3.zip          28MB
#> 53: https://fis.fda.gov/content/Exports/aers_sgml_2010q2.zip          22MB
#> 54: https://fis.fda.gov/content/Exports/aers_sgml_2010q1.zip          20MB
#> 55: https://fis.fda.gov/content/Exports/aers_sgml_2009q4.zip          20MB
#> 56: https://fis.fda.gov/content/Exports/aers_sgml_2009q3.zip          19MB
#> 57: https://fis.fda.gov/content/Exports/aers_sgml_2009q2.zip          18MB
#> 58: https://fis.fda.gov/content/Exports/aers_sgml_2009q1.zip          16MB
#> 59: https://fis.fda.gov/content/Exports/aers_sgml_2008q4.zip          16MB
#> 60: https://fis.fda.gov/content/Exports/aers_sgml_2008q3.zip          16MB
#> 61: https://fis.fda.gov/content/Exports/aers_sgml_2008q2.zip          16MB
#> 62: https://fis.fda.gov/content/Exports/aers_sgml_2008q1.zip          15MB
#> 63: https://fis.fda.gov/content/Exports/aers_sgml_2007q4.zip          14MB
#> 64: https://fis.fda.gov/content/Exports/aers_sgml_2007q3.zip          13MB
#> 65: https://fis.fda.gov/content/Exports/aers_sgml_2007q2.zip          12MB
#> 66: https://fis.fda.gov/content/Exports/aers_sgml_2007q1.zip          12MB
#> 67: https://fis.fda.gov/content/Exports/aers_sgml_2006q4.zip          12MB
#> 68: https://fis.fda.gov/content/Exports/aers_sgml_2006q3.zip          11MB
#> 69: https://fis.fda.gov/content/Exports/aers_sgml_2006q2.zip          13MB
#> 70: https://fis.fda.gov/content/Exports/aers_sgml_2006q1.zip          13MB
#>  [ reached getOption("max.print") -- omitted 9 rows ]
```

An metadata copy was associated with the package, just set `internal =
TRUE`.

``` r
faers_meta(internal = TRUE)
#>      year quarter             period
#>     <int>  <char>             <char>
#>  1:  2023      q2       April - June
#>  2:  2023      q1    January - March
#>  3:  2022      q4 October - December
#>  4:  2022      q3   July - September
#>  5:  2022      q2       April - June
#>  6:  2022      q1    January - March
#>  7:  2021      q4 October - December
#>  8:  2021      q3   July - September
#>  9:  2021      q2       April - June
#> 10:  2021      q1    January - March
#> 11:  2020      q4 October - December
#> 12:  2020      q3   July - September
#> 13:  2020      q2       April - June
#> 14:  2020      q1    January - March
#> 15:  2019      q4 October - December
#> 16:  2019      q3   July - September
#> 17:  2019      q2       April - June
#> 18:  2019      q1    January - March
#> 19:  2018      q4 October - December
#> 20:  2018      q3   July - September
#> 21:  2018      q2       April - June
#> 22:  2018      q1    January - March
#> 23:  2017      q4 October - December
#> 24:  2017      q3   July - September
#> 25:  2017      q2       April - June
#> 26:  2017      q1    January - March
#> 27:  2016      q4 October - December
#> 28:  2016      q3   July - September
#> 29:  2016      q2       April - June
#> 30:  2016      q1    January - March
#> 31:  2015      q4 October - December
#> 32:  2015      q3   July - September
#> 33:  2015      q2       April - June
#> 34:  2015      q1    January - March
#> 35:  2014      q4 October - December
#> 36:  2014      q3   July - September
#> 37:  2014      q2       April - June
#> 38:  2014      q1    January - March
#> 39:  2013      q4 October - December
#> 40:  2013      q3   July - September
#> 41:  2013      q2       April - June
#> 42:  2013      q1    January - March
#> 43:  2012      q4 October - December
#> 44:  2012      q3   July - September
#> 45:  2012      q2       April - June
#> 46:  2012      q1    January - March
#> 47:  2011      q4 October - December
#> 48:  2011      q3   July - September
#> 49:  2011      q2       April - June
#> 50:  2011      q1    January - March
#> 51:  2010      q4 October - December
#> 52:  2010      q3   July - September
#> 53:  2010      q2       April - June
#> 54:  2010      q1    January - March
#> 55:  2009      q4 October - December
#> 56:  2009      q3   July - September
#> 57:  2009      q2       April - June
#> 58:  2009      q1    January - March
#> 59:  2008      q4 October - December
#> 60:  2008      q3   July - September
#> 61:  2008      q2       April - June
#> 62:  2008      q1    January - March
#> 63:  2007      q4 October - December
#> 64:  2007      q3   July - September
#> 65:  2007      q2       April - June
#> 66:  2007      q1    January - March
#> 67:  2006      q4 October - December
#> 68:  2006      q3   July - September
#> 69:  2006      q2       April - June
#> 70:  2006      q1    January - March
#>                                                     ascii_urls ascii_file_size
#>                                                         <char>          <char>
#>  1: https://fis.fda.gov/content/Exports/faers_ascii_2023q2.zip          64.5MB
#>  2: https://fis.fda.gov/content/Exports/faers_ascii_2023q1.zip          64.3MB
#>  3: https://fis.fda.gov/content/Exports/faers_ascii_2022Q4.zip            69MB
#>  4: https://fis.fda.gov/content/Exports/faers_ascii_2022Q3.zip          63.2MB
#>  5: https://fis.fda.gov/content/Exports/faers_ascii_2022q2.zip            63MB
#>  6: https://fis.fda.gov/content/Exports/faers_ascii_2022q1.zip          64.7MB
#>  7: https://fis.fda.gov/content/Exports/faers_ascii_2021Q4.zip            59MB
#>  8: https://fis.fda.gov/content/Exports/faers_ascii_2021Q3.zip            70MB
#>  9: https://fis.fda.gov/content/Exports/faers_ascii_2021Q2.zip            66MB
#> 10: https://fis.fda.gov/content/Exports/faers_ascii_2021Q1.zip            69MB
#> 11: https://fis.fda.gov/content/Exports/faers_ascii_2020Q4.zip            71MB
#> 12: https://fis.fda.gov/content/Exports/faers_ascii_2020Q3.zip            64MB
#> 13: https://fis.fda.gov/content/Exports/faers_ascii_2020Q2.zip            66MB
#> 14: https://fis.fda.gov/content/Exports/faers_ascii_2020Q1.zip            65MB
#> 15: https://fis.fda.gov/content/Exports/faers_ascii_2019Q4.zip            60MB
#> 16: https://fis.fda.gov/content/Exports/faers_ascii_2019Q3.zip            62MB
#> 17: https://fis.fda.gov/content/Exports/faers_ascii_2019Q2.zip            62MB
#> 18: https://fis.fda.gov/content/Exports/faers_ascii_2019Q1.zip            56MB
#> 19: https://fis.fda.gov/content/Exports/faers_ascii_2018q4.zip            60MB
#> 20: https://fis.fda.gov/content/Exports/faers_ascii_2018q3.zip            60MB
#> 21: https://fis.fda.gov/content/Exports/faers_ascii_2018q2.zip            60MB
#> 22: https://fis.fda.gov/content/Exports/faers_ascii_2018q1.zip            52MB
#> 23: https://fis.fda.gov/content/Exports/faers_ascii_2017q4.zip            41MB
#> 24: https://fis.fda.gov/content/Exports/faers_ascii_2017q3.zip            48MB
#> 25: https://fis.fda.gov/content/Exports/faers_ascii_2017q2.zip            46MB
#> 26: https://fis.fda.gov/content/Exports/faers_ascii_2017q1.zip            48MB
#> 27: https://fis.fda.gov/content/Exports/faers_ascii_2016q4.zip            44MB
#> 28: https://fis.fda.gov/content/Exports/faers_ascii_2016q3.zip            46MB
#> 29: https://fis.fda.gov/content/Exports/faers_ascii_2016q2.zip            44MB
#> 30: https://fis.fda.gov/content/Exports/faers_ascii_2016q1.zip            46MB
#> 31: https://fis.fda.gov/content/Exports/faers_ascii_2015q4.zip            42MB
#> 32: https://fis.fda.gov/content/Exports/faers_ascii_2015q3.zip            47MB
#> 33: https://fis.fda.gov/content/Exports/faers_ascii_2015q2.zip            38MB
#> 34: https://fis.fda.gov/content/Exports/faers_ascii_2015q1.zip            39MB
#> 35: https://fis.fda.gov/content/Exports/faers_ascii_2014q4.zip            28MB
#> 36: https://fis.fda.gov/content/Exports/faers_ascii_2014q3.zip            28MB
#> 37: https://fis.fda.gov/content/Exports/faers_ascii_2014q2.zip            25MB
#> 38: https://fis.fda.gov/content/Exports/faers_ascii_2014q1.zip            30MB
#> 39: https://fis.fda.gov/content/Exports/faers_ascii_2013q4.zip            26MB
#> 40: https://fis.fda.gov/content/Exports/faers_ascii_2013q3.zip            22MB
#> 41: https://fis.fda.gov/content/Exports/faers_ascii_2013q2.zip            21MB
#> 42: https://fis.fda.gov/content/Exports/faers_ascii_2013q1.zip            25MB
#> 43: https://fis.fda.gov/content/Exports/faers_ascii_2012q4.zip            28MB
#> 44:  https://fis.fda.gov/content/Exports/aers_ascii_2012q3.zip            16MB
#> 45:  https://fis.fda.gov/content/Exports/aers_ascii_2012q2.zip            25MB
#> 46:  https://fis.fda.gov/content/Exports/aers_ascii_2012q1.zip            26MB
#> 47:  https://fis.fda.gov/content/Exports/aers_ascii_2011q4.zip            23MB
#> 48:  https://fis.fda.gov/content/Exports/aers_ascii_2011q3.zip            23MB
#> 49:  https://fis.fda.gov/content/Exports/aers_ascii_2011q2.zip            23MB
#> 50:  https://fis.fda.gov/content/Exports/aers_ascii_2011q1.zip            21MB
#> 51:  https://fis.fda.gov/content/Exports/aers_ascii_2010q4.zip            20MB
#> 52:  https://fis.fda.gov/content/Exports/aers_ascii_2010q3.zip            22MB
#> 53:  https://fis.fda.gov/content/Exports/aers_ascii_2010q2.zip            17MB
#> 54:  https://fis.fda.gov/content/Exports/aers_ascii_2010q1.zip            16MB
#> 55:  https://fis.fda.gov/content/Exports/aers_ascii_2009q4.zip            16MB
#> 56:  https://fis.fda.gov/content/Exports/aers_ascii_2009q3.zip            16MB
#> 57:  https://fis.fda.gov/content/Exports/aers_ascii_2009q2.zip            14MB
#> 58:  https://fis.fda.gov/content/Exports/aers_ascii_2009q1.zip            13MB
#> 59:  https://fis.fda.gov/content/Exports/aers_ascii_2008q4.zip            13MB
#> 60:  https://fis.fda.gov/content/Exports/aers_ascii_2008q3.zip            13MB
#> 61:  https://fis.fda.gov/content/Exports/aers_ascii_2008q2.zip            12MB
#> 62:  https://fis.fda.gov/content/Exports/aers_ascii_2008q1.zip            12MB
#> 63:  https://fis.fda.gov/content/Exports/aers_ascii_2007q4.zip            12MB
#> 64:  https://fis.fda.gov/content/Exports/aers_ascii_2007q3.zip           9.9MB
#> 65:  https://fis.fda.gov/content/Exports/aers_ascii_2007q2.zip           9.5MB
#> 66:  https://fis.fda.gov/content/Exports/aers_ascii_2007q1.zip           9.6MB
#> 67:  https://fis.fda.gov/content/Exports/aers_ascii_2006q4.zip           9.1MB
#> 68:  https://fis.fda.gov/content/Exports/aers_ascii_2006q3.zip           8.5MB
#> 69:  https://fis.fda.gov/content/Exports/aers_ascii_2006q2.zip           9.7MB
#> 70:  https://fis.fda.gov/content/Exports/aers_ascii_2006q1.zip            11MB
#>                                                     xml_urls xml_file_size
#>                                                       <char>        <char>
#>  1: https://fis.fda.gov/content/Exports/faers_xml_2023q2.zip         130MB
#>  2: https://fis.fda.gov/content/Exports/faers_xml_2023q1.zip         133MB
#>  3: https://fis.fda.gov/content/Exports/faers_xml_2022Q4.zip         144MB
#>  4: https://fis.fda.gov/content/Exports/faers_xml_2022Q3.zip         132MB
#>  5: https://fis.fda.gov/content/Exports/faers_xml_2022q2.zip         140MB
#>  6: https://fis.fda.gov/content/Exports/faers_xml_2022q1.zip         136MB
#>  7: https://fis.fda.gov/content/Exports/faers_xml_2021Q4.zip         123MB
#>  8: https://fis.fda.gov/content/Exports/faers_xml_2021Q3.zip         132MB
#>  9: https://fis.fda.gov/content/Exports/faers_xml_2021Q2.zip         123MB
#> 10: https://fis.fda.gov/content/Exports/faers_xml_2021Q1.zip         130MB
#> 11: https://fis.fda.gov/content/Exports/faers_xml_2020Q4.zip         131MB
#> 12: https://fis.fda.gov/content/Exports/faers_xml_2020Q3.zip         121MB
#> 13: https://fis.fda.gov/content/Exports/faers_xml_2020Q2.zip         123MB
#> 14: https://fis.fda.gov/content/Exports/faers_xml_2020Q1.zip         125MB
#> 15: https://fis.fda.gov/content/Exports/faers_xml_2019Q4.zip         113MB
#> 16: https://fis.fda.gov/content/Exports/faers_xml_2019Q3.zip         118MB
#> 17: https://fis.fda.gov/content/Exports/faers_xml_2019Q2.zip         118MB
#> 18: https://fis.fda.gov/content/Exports/faers_xml_2019Q1.zip         103MB
#> 19: https://fis.fda.gov/content/Exports/faers_xml_2018q4.zip         112MB
#> 20: https://fis.fda.gov/content/Exports/faers_xml_2018q3.zip         112MB
#> 21: https://fis.fda.gov/content/Exports/faers_xml_2018q2.zip         112MB
#> 22: https://fis.fda.gov/content/Exports/faers_xml_2018q1.zip          94MB
#> 23: https://fis.fda.gov/content/Exports/faers_xml_2017q4.zip          76MB
#> 24: https://fis.fda.gov/content/Exports/faers_xml_2017q3.zip          91MB
#> 25: https://fis.fda.gov/content/Exports/faers_xml_2017q2.zip          86MB
#> 26: https://fis.fda.gov/content/Exports/faers_xml_2017q1.zip          91MB
#> 27: https://fis.fda.gov/content/Exports/faers_xml_2016q4.zip          82MB
#> 28: https://fis.fda.gov/content/Exports/faers_xml_2016q3.zip          87MB
#> 29: https://fis.fda.gov/content/Exports/faers_xml_2016q2.zip          81MB
#> 30: https://fis.fda.gov/content/Exports/faers_xml_2016q1.zip          84MB
#> 31: https://fis.fda.gov/content/Exports/faers_xml_2015q4.zip          77MB
#> 32: https://fis.fda.gov/content/Exports/faers_xml_2015q3.zip          88MB
#> 33: https://fis.fda.gov/content/Exports/faers_xml_2015q2.zip          70MB
#> 34: https://fis.fda.gov/content/Exports/faers_xml_2015q1.zip          72MB
#> 35: https://fis.fda.gov/content/Exports/faers_xml_2014q4.zip          53MB
#> 36: https://fis.fda.gov/content/Exports/faers_xml_2014q3.zip          54MB
#> 37: https://fis.fda.gov/content/Exports/faers_xml_2014q2.zip          44MB
#> 38: https://fis.fda.gov/content/Exports/faers_xml_2014q1.zip          52MB
#> 39: https://fis.fda.gov/content/Exports/faers_xml_2013q4.zip          46MB
#> 40: https://fis.fda.gov/content/Exports/faers_xml_2013q3.zip          40MB
#> 41: https://fis.fda.gov/content/Exports/faers_xml_2013q2.zip          38MB
#> 42: https://fis.fda.gov/content/Exports/faers_xml_2013q1.zip          44MB
#> 43: https://fis.fda.gov/content/Exports/faers_xml_2012q4.zip          50MB
#> 44: https://fis.fda.gov/content/Exports/aers_sgml_2012q3.zip          21MB
#> 45: https://fis.fda.gov/content/Exports/aers_sgml_2012q2.zip          32MB
#> 46: https://fis.fda.gov/content/Exports/aers_sgml_2012q1.zip          33MB
#> 47: https://fis.fda.gov/content/Exports/aers_sgml_2011q4.zip          29MB
#> 48: https://fis.fda.gov/content/Exports/aers_sgml_2011q3.zip          29MB
#> 49: https://fis.fda.gov/content/Exports/aers_sgml_2011q2.zip          29MB
#> 50: https://fis.fda.gov/content/Exports/aers_sgml_2011q1.zip          26MB
#> 51: https://fis.fda.gov/content/Exports/aers_sgml_2010q4.zip          25MB
#> 52: https://fis.fda.gov/content/Exports/aers_sgml_2010q3.zip          28MB
#> 53: https://fis.fda.gov/content/Exports/aers_sgml_2010q2.zip          22MB
#> 54: https://fis.fda.gov/content/Exports/aers_sgml_2010q1.zip          20MB
#> 55: https://fis.fda.gov/content/Exports/aers_sgml_2009q4.zip          20MB
#> 56: https://fis.fda.gov/content/Exports/aers_sgml_2009q3.zip          19MB
#> 57: https://fis.fda.gov/content/Exports/aers_sgml_2009q2.zip          18MB
#> 58: https://fis.fda.gov/content/Exports/aers_sgml_2009q1.zip          16MB
#> 59: https://fis.fda.gov/content/Exports/aers_sgml_2008q4.zip          16MB
#> 60: https://fis.fda.gov/content/Exports/aers_sgml_2008q3.zip          16MB
#> 61: https://fis.fda.gov/content/Exports/aers_sgml_2008q2.zip          16MB
#> 62: https://fis.fda.gov/content/Exports/aers_sgml_2008q1.zip          15MB
#> 63: https://fis.fda.gov/content/Exports/aers_sgml_2007q4.zip          14MB
#> 64: https://fis.fda.gov/content/Exports/aers_sgml_2007q3.zip          13MB
#> 65: https://fis.fda.gov/content/Exports/aers_sgml_2007q2.zip          12MB
#> 66: https://fis.fda.gov/content/Exports/aers_sgml_2007q1.zip          12MB
#> 67: https://fis.fda.gov/content/Exports/aers_sgml_2006q4.zip          12MB
#> 68: https://fis.fda.gov/content/Exports/aers_sgml_2006q3.zip          11MB
#> 69: https://fis.fda.gov/content/Exports/aers_sgml_2006q2.zip          13MB
#> 70: https://fis.fda.gov/content/Exports/aers_sgml_2006q1.zip          13MB
#>  [ reached getOption("max.print") -- omitted 9 rows ]
```

#### download and parse quarterly data files from FAERS

The FAERS Quarterly Data files contain raw data extracted from the AERS
database for the indicated time ranges. The quarterly data files, which
are available in ASCII or SGML formats, include:

  - demographic and administrative information (demo);
  - drug information from the case reports (drug);
  - reaction information from the reports (reac);
  - patient outcome information from the reports (outc);
  - information on the source of the reports (rpsr);
  - drug therapy start dates and end dates for the reported drugs
    (ther);
  - Medical Dictionary for Regulatory Activities (indi)

Generally, we can use `faers()` function to download and parse all
quarterly data files from FAERS. Internally, the `faers()` function
seamlessly utilizes `faers_download()` and `faers_parse()` to preprocess
each quarterly data file from the FAERS repository. The default `format`
was `ascii` and will return a `FAERSascii` object. (xml format would
also be okay , but presently, the XML file receives only minimal support
in the following process.)

``` r
# # you must change `dir`, as the file included in the package is sampled
data1 <- faers(2004, "q1", dir = system.file("extdata", package = "faers"))
#> Finding 1 file already downloaded: 'aers_ascii_2004q1.zip'
data1
#> FAERS data from 1 Quarterly ascii file
#>   Total reports: 100 (with duplicates)
```

Furthermore, in cases where multiple quarterly data files are requisite,
the `faers_combine()` function is judiciously employed.

``` r
data2 <- faers(c(2004, 2017), c("q1", "q2"),
  dir = system.file("extdata", package = "faers")
)
#> Finding 2 files already downloaded: 'aers_ascii_2004q1.zip' and
#> 'faers_ascii_2017q2.zip'
#> → Combining all 2 FAERS Quarterly ascii Data files
data2
#> FAERS data from 2 Quarterly ascii files
#>   Total reports: 200 (with duplicates)
```

#### standardize and De-duplication

The `reac` file provides the adverse drug reactions, where it includes
the “P.T.” field or the “Preferred Term” level terminology from the
Medical Dictionary for Regulatory Activities (MedDRA). The `indi` file
contains the drug indications, which also uses the “P.T.” level of
MedDRA as a descriptor for the drug indication. In this way, `MedDRA`
was necessary to standardize this field and add additional informations,
such as `System Organ Classes`.

One limitation of FAERS database is Duplicate and incomplete reports.
There are many instances of duplicative reports and some reports do not
contain all the necessary information. We deemed two cases to be
identical if they exhibited a full concordance across drugs
administered, and adverse reactions and but showed discrepancies in one
or none of the following fields: gender, age, reporting country, event
date, start date, and drug indications.

``` r
# you should replace `meddra_path` with the directory of meddra data
data <- faers_standardize(data2, meddra_path)
data <- faers_dedup(data)
```

#### Pharmacovigilance analysis

Pharmacovigilance is the science and activities relating to the
detection, assessment, understanding and prevention of adverse effects
or any other medicine/vaccine related problem.

``` r
faers_phv_signal(data,
  filter_params = list(field = "demo", .fn = function(x) {
    sample(x$primaryid, 100L)
  })
)
```

#### sessionInfo

``` r
sessionInfo()
#> R version 4.3.1 (2023-06-16)
#> Platform: x86_64-pc-linux-gnu (64-bit)
#> Running under: Ubuntu 22.04.3 LTS
#> 
#> Matrix products: default
#> BLAS/LAPACK: /usr/lib/x86_64-linux-gnu/libmkl_rt.so;  LAPACK version 3.8.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: Asia/Shanghai
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] faers_0.0.0.9000
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_4.3.1    fastmap_1.1.1     cli_3.6.1         tools_4.3.1      
#>  [5] htmltools_0.5.5   rstudioapi_0.15.0 rappdirs_0.3.3    yaml_2.3.7       
#>  [9] rmarkdown_2.23    data.table_1.14.9 knitr_1.43        xfun_0.39        
#> [13] digest_0.6.33     rlang_1.1.1       evaluate_0.21
```

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
#> → Reading html:
#>   <https://fis.fda.gov/extensions/FPD-QDE-FAERS/FPD-QDE-FAERS.html>
#> → Writing FAERS metadata into cache file
#>   '~/.cache/R/faers/metadata/faers_meta_data.rds'
#>      year quarter             period
#>     <int>  <char>             <char>
#>  1:  2023      q3   July - September
#>  2:  2023      q2       April - June
#>  3:  2023      q1    January - March
#>  4:  2022      q4 October - December
#>  5:  2022      q3   July - September
#>  6:  2022      q2       April - June
#>  7:  2022      q1    January - March
#>  8:  2021      q4 October - December
#>  9:  2021      q3   July - September
#> 10:  2021      q2       April - June
#> 11:  2021      q1    January - March
#> 12:  2020      q4 October - December
#> 13:  2020      q3   July - September
#> 14:  2020      q2       April - June
#> 15:  2020      q1    January - March
#> 16:  2019      q4 October - December
#> 17:  2019      q3   July - September
#> 18:  2019      q2       April - June
#> 19:  2019      q1    January - March
#> 20:  2018      q4 October - December
#> 21:  2018      q3   July - September
#> 22:  2018      q2       April - June
#> 23:  2018      q1    January - March
#> 24:  2017      q4 October - December
#> 25:  2017      q3   July - September
#> 26:  2017      q2       April - June
#> 27:  2017      q1    January - March
#> 28:  2016      q4 October - December
#> 29:  2016      q3   July - September
#> 30:  2016      q2       April - June
#> 31:  2016      q1    January - March
#> 32:  2015      q4 October - December
#> 33:  2015      q3   July - September
#> 34:  2015      q2       April - June
#> 35:  2015      q1    January - March
#> 36:  2014      q4 October - December
#> 37:  2014      q3   July - September
#> 38:  2014      q2       April - June
#> 39:  2014      q1    January - March
#> 40:  2013      q4 October - December
#> 41:  2013      q3   July - September
#> 42:  2013      q2       April - June
#> 43:  2013      q1    January - March
#> 44:  2012      q4 October - December
#> 45:  2012      q3   July - September
#> 46:  2012      q2       April - June
#> 47:  2012      q1    January - March
#> 48:  2011      q4 October - December
#> 49:  2011      q3   July - September
#> 50:  2011      q2       April - June
#> 51:  2011      q1    January - March
#> 52:  2010      q4 October - December
#> 53:  2010      q3   July - September
#> 54:  2010      q2       April - June
#> 55:  2010      q1    January - March
#> 56:  2009      q4 October - December
#> 57:  2009      q3   July - September
#> 58:  2009      q2       April - June
#> 59:  2009      q1    January - March
#> 60:  2008      q4 October - December
#> 61:  2008      q3   July - September
#> 62:  2008      q2       April - June
#> 63:  2008      q1    January - March
#> 64:  2007      q4 October - December
#> 65:  2007      q3   July - September
#> 66:  2007      q2       April - June
#> 67:  2007      q1    January - March
#> 68:  2006      q4 October - December
#> 69:  2006      q3   July - September
#> 70:  2006      q2       April - June
#>                                                     ascii_urls ascii_file_size
#>                                                         <char>          <char>
#>  1: https://fis.fda.gov/content/Exports/faers_ascii_2023Q3.zip          60.1MB
#>  2: https://fis.fda.gov/content/Exports/faers_ascii_2023q2.zip          64.5MB
#>  3: https://fis.fda.gov/content/Exports/faers_ascii_2023q1.zip          64.3MB
#>  4: https://fis.fda.gov/content/Exports/faers_ascii_2022Q4.zip            69MB
#>  5: https://fis.fda.gov/content/Exports/faers_ascii_2022Q3.zip          63.2MB
#>  6: https://fis.fda.gov/content/Exports/faers_ascii_2022q2.zip            63MB
#>  7: https://fis.fda.gov/content/Exports/faers_ascii_2022q1.zip          64.7MB
#>  8: https://fis.fda.gov/content/Exports/faers_ascii_2021Q4.zip            59MB
#>  9: https://fis.fda.gov/content/Exports/faers_ascii_2021Q3.zip            70MB
#> 10: https://fis.fda.gov/content/Exports/faers_ascii_2021Q2.zip            66MB
#> 11: https://fis.fda.gov/content/Exports/faers_ascii_2021Q1.zip            69MB
#> 12: https://fis.fda.gov/content/Exports/faers_ascii_2020Q4.zip            71MB
#> 13: https://fis.fda.gov/content/Exports/faers_ascii_2020Q3.zip            64MB
#> 14: https://fis.fda.gov/content/Exports/faers_ascii_2020Q2.zip            66MB
#> 15: https://fis.fda.gov/content/Exports/faers_ascii_2020Q1.zip            65MB
#> 16: https://fis.fda.gov/content/Exports/faers_ascii_2019Q4.zip            60MB
#> 17: https://fis.fda.gov/content/Exports/faers_ascii_2019Q3.zip            62MB
#> 18: https://fis.fda.gov/content/Exports/faers_ascii_2019Q2.zip            62MB
#> 19: https://fis.fda.gov/content/Exports/faers_ascii_2019Q1.zip            56MB
#> 20: https://fis.fda.gov/content/Exports/faers_ascii_2018q4.zip            60MB
#> 21: https://fis.fda.gov/content/Exports/faers_ascii_2018q3.zip            60MB
#> 22: https://fis.fda.gov/content/Exports/faers_ascii_2018q2.zip            60MB
#> 23: https://fis.fda.gov/content/Exports/faers_ascii_2018q1.zip            52MB
#> 24: https://fis.fda.gov/content/Exports/faers_ascii_2017q4.zip            41MB
#> 25: https://fis.fda.gov/content/Exports/faers_ascii_2017q3.zip            48MB
#> 26: https://fis.fda.gov/content/Exports/faers_ascii_2017q2.zip            46MB
#> 27: https://fis.fda.gov/content/Exports/faers_ascii_2017q1.zip            48MB
#> 28: https://fis.fda.gov/content/Exports/faers_ascii_2016q4.zip            44MB
#> 29: https://fis.fda.gov/content/Exports/faers_ascii_2016q3.zip            46MB
#> 30: https://fis.fda.gov/content/Exports/faers_ascii_2016q2.zip            44MB
#> 31: https://fis.fda.gov/content/Exports/faers_ascii_2016q1.zip            46MB
#> 32: https://fis.fda.gov/content/Exports/faers_ascii_2015q4.zip            42MB
#> 33: https://fis.fda.gov/content/Exports/faers_ascii_2015q3.zip            47MB
#> 34: https://fis.fda.gov/content/Exports/faers_ascii_2015q2.zip            38MB
#> 35: https://fis.fda.gov/content/Exports/faers_ascii_2015q1.zip            39MB
#> 36: https://fis.fda.gov/content/Exports/faers_ascii_2014q4.zip            28MB
#> 37: https://fis.fda.gov/content/Exports/faers_ascii_2014q3.zip            28MB
#> 38: https://fis.fda.gov/content/Exports/faers_ascii_2014q2.zip            25MB
#> 39: https://fis.fda.gov/content/Exports/faers_ascii_2014q1.zip            30MB
#> 40: https://fis.fda.gov/content/Exports/faers_ascii_2013q4.zip            26MB
#> 41: https://fis.fda.gov/content/Exports/faers_ascii_2013q3.zip            22MB
#> 42: https://fis.fda.gov/content/Exports/faers_ascii_2013q2.zip            21MB
#> 43: https://fis.fda.gov/content/Exports/faers_ascii_2013q1.zip            25MB
#> 44: https://fis.fda.gov/content/Exports/faers_ascii_2012q4.zip            28MB
#> 45:  https://fis.fda.gov/content/Exports/aers_ascii_2012q3.zip            16MB
#> 46:  https://fis.fda.gov/content/Exports/aers_ascii_2012q2.zip            25MB
#> 47:  https://fis.fda.gov/content/Exports/aers_ascii_2012q1.zip            26MB
#> 48:  https://fis.fda.gov/content/Exports/aers_ascii_2011q4.zip            23MB
#> 49:  https://fis.fda.gov/content/Exports/aers_ascii_2011q3.zip            23MB
#> 50:  https://fis.fda.gov/content/Exports/aers_ascii_2011q2.zip            23MB
#> 51:  https://fis.fda.gov/content/Exports/aers_ascii_2011q1.zip            21MB
#> 52:  https://fis.fda.gov/content/Exports/aers_ascii_2010q4.zip            20MB
#> 53:  https://fis.fda.gov/content/Exports/aers_ascii_2010q3.zip            22MB
#> 54:  https://fis.fda.gov/content/Exports/aers_ascii_2010q2.zip            17MB
#> 55:  https://fis.fda.gov/content/Exports/aers_ascii_2010q1.zip            16MB
#> 56:  https://fis.fda.gov/content/Exports/aers_ascii_2009q4.zip            16MB
#> 57:  https://fis.fda.gov/content/Exports/aers_ascii_2009q3.zip            16MB
#> 58:  https://fis.fda.gov/content/Exports/aers_ascii_2009q2.zip            14MB
#> 59:  https://fis.fda.gov/content/Exports/aers_ascii_2009q1.zip            13MB
#> 60:  https://fis.fda.gov/content/Exports/aers_ascii_2008q4.zip            13MB
#> 61:  https://fis.fda.gov/content/Exports/aers_ascii_2008q3.zip            13MB
#> 62:  https://fis.fda.gov/content/Exports/aers_ascii_2008q2.zip            12MB
#> 63:  https://fis.fda.gov/content/Exports/aers_ascii_2008q1.zip            12MB
#> 64:  https://fis.fda.gov/content/Exports/aers_ascii_2007q4.zip            12MB
#> 65:  https://fis.fda.gov/content/Exports/aers_ascii_2007q3.zip           9.9MB
#> 66:  https://fis.fda.gov/content/Exports/aers_ascii_2007q2.zip           9.5MB
#> 67:  https://fis.fda.gov/content/Exports/aers_ascii_2007q1.zip           9.6MB
#> 68:  https://fis.fda.gov/content/Exports/aers_ascii_2006q4.zip           9.1MB
#> 69:  https://fis.fda.gov/content/Exports/aers_ascii_2006q3.zip           8.5MB
#> 70:  https://fis.fda.gov/content/Exports/aers_ascii_2006q2.zip           9.7MB
#>                                                     xml_urls xml_file_size
#>                                                       <char>        <char>
#>  1: https://fis.fda.gov/content/Exports/faers_xml_2023Q3.zip         123MB
#>  2: https://fis.fda.gov/content/Exports/faers_xml_2023q2.zip         130MB
#>  3: https://fis.fda.gov/content/Exports/faers_xml_2023q1.zip         133MB
#>  4: https://fis.fda.gov/content/Exports/faers_xml_2022Q4.zip         144MB
#>  5: https://fis.fda.gov/content/Exports/faers_xml_2022Q3.zip         132MB
#>  6: https://fis.fda.gov/content/Exports/faers_xml_2022q2.zip         140MB
#>  7: https://fis.fda.gov/content/Exports/faers_xml_2022q1.zip         136MB
#>  8: https://fis.fda.gov/content/Exports/faers_xml_2021Q4.zip         123MB
#>  9: https://fis.fda.gov/content/Exports/faers_xml_2021Q3.zip         132MB
#> 10: https://fis.fda.gov/content/Exports/faers_xml_2021Q2.zip         123MB
#> 11: https://fis.fda.gov/content/Exports/faers_xml_2021Q1.zip         130MB
#> 12: https://fis.fda.gov/content/Exports/faers_xml_2020Q4.zip         131MB
#> 13: https://fis.fda.gov/content/Exports/faers_xml_2020Q3.zip         121MB
#> 14: https://fis.fda.gov/content/Exports/faers_xml_2020Q2.zip         123MB
#> 15: https://fis.fda.gov/content/Exports/faers_xml_2020Q1.zip         125MB
#> 16: https://fis.fda.gov/content/Exports/faers_xml_2019Q4.zip         113MB
#> 17: https://fis.fda.gov/content/Exports/faers_xml_2019Q3.zip         118MB
#> 18: https://fis.fda.gov/content/Exports/faers_xml_2019Q2.zip         118MB
#> 19: https://fis.fda.gov/content/Exports/faers_xml_2019Q1.zip         103MB
#> 20: https://fis.fda.gov/content/Exports/faers_xml_2018q4.zip         112MB
#> 21: https://fis.fda.gov/content/Exports/faers_xml_2018q3.zip         112MB
#> 22: https://fis.fda.gov/content/Exports/faers_xml_2018q2.zip         112MB
#> 23: https://fis.fda.gov/content/Exports/faers_xml_2018q1.zip          94MB
#> 24: https://fis.fda.gov/content/Exports/faers_xml_2017q4.zip          76MB
#> 25: https://fis.fda.gov/content/Exports/faers_xml_2017q3.zip          91MB
#> 26: https://fis.fda.gov/content/Exports/faers_xml_2017q2.zip          86MB
#> 27: https://fis.fda.gov/content/Exports/faers_xml_2017q1.zip          91MB
#> 28: https://fis.fda.gov/content/Exports/faers_xml_2016q4.zip          82MB
#> 29: https://fis.fda.gov/content/Exports/faers_xml_2016q3.zip          87MB
#> 30: https://fis.fda.gov/content/Exports/faers_xml_2016q2.zip          81MB
#> 31: https://fis.fda.gov/content/Exports/faers_xml_2016q1.zip          84MB
#> 32: https://fis.fda.gov/content/Exports/faers_xml_2015q4.zip          77MB
#> 33: https://fis.fda.gov/content/Exports/faers_xml_2015q3.zip          88MB
#> 34: https://fis.fda.gov/content/Exports/faers_xml_2015q2.zip          70MB
#> 35: https://fis.fda.gov/content/Exports/faers_xml_2015q1.zip          72MB
#> 36: https://fis.fda.gov/content/Exports/faers_xml_2014q4.zip          53MB
#> 37: https://fis.fda.gov/content/Exports/faers_xml_2014q3.zip          54MB
#> 38: https://fis.fda.gov/content/Exports/faers_xml_2014q2.zip          44MB
#> 39: https://fis.fda.gov/content/Exports/faers_xml_2014q1.zip          52MB
#> 40: https://fis.fda.gov/content/Exports/faers_xml_2013q4.zip          46MB
#> 41: https://fis.fda.gov/content/Exports/faers_xml_2013q3.zip          40MB
#> 42: https://fis.fda.gov/content/Exports/faers_xml_2013q2.zip          38MB
#> 43: https://fis.fda.gov/content/Exports/faers_xml_2013q1.zip          44MB
#> 44: https://fis.fda.gov/content/Exports/faers_xml_2012q4.zip          50MB
#> 45: https://fis.fda.gov/content/Exports/aers_sgml_2012q3.zip          21MB
#> 46: https://fis.fda.gov/content/Exports/aers_sgml_2012q2.zip          32MB
#> 47: https://fis.fda.gov/content/Exports/aers_sgml_2012q1.zip          33MB
#> 48: https://fis.fda.gov/content/Exports/aers_sgml_2011q4.zip          29MB
#> 49: https://fis.fda.gov/content/Exports/aers_sgml_2011q3.zip          29MB
#> 50: https://fis.fda.gov/content/Exports/aers_sgml_2011q2.zip          29MB
#> 51: https://fis.fda.gov/content/Exports/aers_sgml_2011q1.zip          26MB
#> 52: https://fis.fda.gov/content/Exports/aers_sgml_2010q4.zip          25MB
#> 53: https://fis.fda.gov/content/Exports/aers_sgml_2010q3.zip          28MB
#> 54: https://fis.fda.gov/content/Exports/aers_sgml_2010q2.zip          22MB
#> 55: https://fis.fda.gov/content/Exports/aers_sgml_2010q1.zip          20MB
#> 56: https://fis.fda.gov/content/Exports/aers_sgml_2009q4.zip          20MB
#> 57: https://fis.fda.gov/content/Exports/aers_sgml_2009q3.zip          19MB
#> 58: https://fis.fda.gov/content/Exports/aers_sgml_2009q2.zip          18MB
#> 59: https://fis.fda.gov/content/Exports/aers_sgml_2009q1.zip          16MB
#> 60: https://fis.fda.gov/content/Exports/aers_sgml_2008q4.zip          16MB
#> 61: https://fis.fda.gov/content/Exports/aers_sgml_2008q3.zip          16MB
#> 62: https://fis.fda.gov/content/Exports/aers_sgml_2008q2.zip          16MB
#> 63: https://fis.fda.gov/content/Exports/aers_sgml_2008q1.zip          15MB
#> 64: https://fis.fda.gov/content/Exports/aers_sgml_2007q4.zip          14MB
#> 65: https://fis.fda.gov/content/Exports/aers_sgml_2007q3.zip          13MB
#> 66: https://fis.fda.gov/content/Exports/aers_sgml_2007q2.zip          12MB
#> 67: https://fis.fda.gov/content/Exports/aers_sgml_2007q1.zip          12MB
#> 68: https://fis.fda.gov/content/Exports/aers_sgml_2006q4.zip          12MB
#> 69: https://fis.fda.gov/content/Exports/aers_sgml_2006q3.zip          11MB
#> 70: https://fis.fda.gov/content/Exports/aers_sgml_2006q2.zip          13MB
#>  [ reached getOption("max.print") -- omitted 10 rows ]
```

An metadata copy was associated with the package, just set `internal =
TRUE`. However, this copy will only be used if the cached file on your
computer cannot be found as the cached file on your computer should be
more up-to-date than this metadata copy.

``` r
faers_clearcache("metadata")
#> ✔ Removing '~/.cache/R/faers/metadata' successfully
faers_meta(internal = TRUE)
#> → Using internal FAERS metadata
#>   Snapshot time: 2023-11-08 13:06:35.726011
#>      year quarter             period
#>     <int>  <char>             <char>
#>  1:  2023      q3   July - September
#>  2:  2023      q2       April - June
#>  3:  2023      q1    January - March
#>  4:  2022      q4 October - December
#>  5:  2022      q3   July - September
#>  6:  2022      q2       April - June
#>  7:  2022      q1    January - March
#>  8:  2021      q4 October - December
#>  9:  2021      q3   July - September
#> 10:  2021      q2       April - June
#> 11:  2021      q1    January - March
#> 12:  2020      q4 October - December
#> 13:  2020      q3   July - September
#> 14:  2020      q2       April - June
#> 15:  2020      q1    January - March
#> 16:  2019      q4 October - December
#> 17:  2019      q3   July - September
#> 18:  2019      q2       April - June
#> 19:  2019      q1    January - March
#> 20:  2018      q4 October - December
#> 21:  2018      q3   July - September
#> 22:  2018      q2       April - June
#> 23:  2018      q1    January - March
#> 24:  2017      q4 October - December
#> 25:  2017      q3   July - September
#> 26:  2017      q2       April - June
#> 27:  2017      q1    January - March
#> 28:  2016      q4 October - December
#> 29:  2016      q3   July - September
#> 30:  2016      q2       April - June
#> 31:  2016      q1    January - March
#> 32:  2015      q4 October - December
#> 33:  2015      q3   July - September
#> 34:  2015      q2       April - June
#> 35:  2015      q1    January - March
#> 36:  2014      q4 October - December
#> 37:  2014      q3   July - September
#> 38:  2014      q2       April - June
#> 39:  2014      q1    January - March
#> 40:  2013      q4 October - December
#> 41:  2013      q3   July - September
#> 42:  2013      q2       April - June
#> 43:  2013      q1    January - March
#> 44:  2012      q4 October - December
#> 45:  2012      q3   July - September
#> 46:  2012      q2       April - June
#> 47:  2012      q1    January - March
#> 48:  2011      q4 October - December
#> 49:  2011      q3   July - September
#> 50:  2011      q2       April - June
#> 51:  2011      q1    January - March
#> 52:  2010      q4 October - December
#> 53:  2010      q3   July - September
#> 54:  2010      q2       April - June
#> 55:  2010      q1    January - March
#> 56:  2009      q4 October - December
#> 57:  2009      q3   July - September
#> 58:  2009      q2       April - June
#> 59:  2009      q1    January - March
#> 60:  2008      q4 October - December
#> 61:  2008      q3   July - September
#> 62:  2008      q2       April - June
#> 63:  2008      q1    January - March
#> 64:  2007      q4 October - December
#> 65:  2007      q3   July - September
#> 66:  2007      q2       April - June
#> 67:  2007      q1    January - March
#> 68:  2006      q4 October - December
#> 69:  2006      q3   July - September
#> 70:  2006      q2       April - June
#>                                                     ascii_urls ascii_file_size
#>                                                         <char>          <char>
#>  1: https://fis.fda.gov/content/Exports/faers_ascii_2023Q3.zip          60.1MB
#>  2: https://fis.fda.gov/content/Exports/faers_ascii_2023q2.zip          64.5MB
#>  3: https://fis.fda.gov/content/Exports/faers_ascii_2023q1.zip          64.3MB
#>  4: https://fis.fda.gov/content/Exports/faers_ascii_2022Q4.zip            69MB
#>  5: https://fis.fda.gov/content/Exports/faers_ascii_2022Q3.zip          63.2MB
#>  6: https://fis.fda.gov/content/Exports/faers_ascii_2022q2.zip            63MB
#>  7: https://fis.fda.gov/content/Exports/faers_ascii_2022q1.zip          64.7MB
#>  8: https://fis.fda.gov/content/Exports/faers_ascii_2021Q4.zip            59MB
#>  9: https://fis.fda.gov/content/Exports/faers_ascii_2021Q3.zip            70MB
#> 10: https://fis.fda.gov/content/Exports/faers_ascii_2021Q2.zip            66MB
#> 11: https://fis.fda.gov/content/Exports/faers_ascii_2021Q1.zip            69MB
#> 12: https://fis.fda.gov/content/Exports/faers_ascii_2020Q4.zip            71MB
#> 13: https://fis.fda.gov/content/Exports/faers_ascii_2020Q3.zip            64MB
#> 14: https://fis.fda.gov/content/Exports/faers_ascii_2020Q2.zip            66MB
#> 15: https://fis.fda.gov/content/Exports/faers_ascii_2020Q1.zip            65MB
#> 16: https://fis.fda.gov/content/Exports/faers_ascii_2019Q4.zip            60MB
#> 17: https://fis.fda.gov/content/Exports/faers_ascii_2019Q3.zip            62MB
#> 18: https://fis.fda.gov/content/Exports/faers_ascii_2019Q2.zip            62MB
#> 19: https://fis.fda.gov/content/Exports/faers_ascii_2019Q1.zip            56MB
#> 20: https://fis.fda.gov/content/Exports/faers_ascii_2018q4.zip            60MB
#> 21: https://fis.fda.gov/content/Exports/faers_ascii_2018q3.zip            60MB
#> 22: https://fis.fda.gov/content/Exports/faers_ascii_2018q2.zip            60MB
#> 23: https://fis.fda.gov/content/Exports/faers_ascii_2018q1.zip            52MB
#> 24: https://fis.fda.gov/content/Exports/faers_ascii_2017q4.zip            41MB
#> 25: https://fis.fda.gov/content/Exports/faers_ascii_2017q3.zip            48MB
#> 26: https://fis.fda.gov/content/Exports/faers_ascii_2017q2.zip            46MB
#> 27: https://fis.fda.gov/content/Exports/faers_ascii_2017q1.zip            48MB
#> 28: https://fis.fda.gov/content/Exports/faers_ascii_2016q4.zip            44MB
#> 29: https://fis.fda.gov/content/Exports/faers_ascii_2016q3.zip            46MB
#> 30: https://fis.fda.gov/content/Exports/faers_ascii_2016q2.zip            44MB
#> 31: https://fis.fda.gov/content/Exports/faers_ascii_2016q1.zip            46MB
#> 32: https://fis.fda.gov/content/Exports/faers_ascii_2015q4.zip            42MB
#> 33: https://fis.fda.gov/content/Exports/faers_ascii_2015q3.zip            47MB
#> 34: https://fis.fda.gov/content/Exports/faers_ascii_2015q2.zip            38MB
#> 35: https://fis.fda.gov/content/Exports/faers_ascii_2015q1.zip            39MB
#> 36: https://fis.fda.gov/content/Exports/faers_ascii_2014q4.zip            28MB
#> 37: https://fis.fda.gov/content/Exports/faers_ascii_2014q3.zip            28MB
#> 38: https://fis.fda.gov/content/Exports/faers_ascii_2014q2.zip            25MB
#> 39: https://fis.fda.gov/content/Exports/faers_ascii_2014q1.zip            30MB
#> 40: https://fis.fda.gov/content/Exports/faers_ascii_2013q4.zip            26MB
#> 41: https://fis.fda.gov/content/Exports/faers_ascii_2013q3.zip            22MB
#> 42: https://fis.fda.gov/content/Exports/faers_ascii_2013q2.zip            21MB
#> 43: https://fis.fda.gov/content/Exports/faers_ascii_2013q1.zip            25MB
#> 44: https://fis.fda.gov/content/Exports/faers_ascii_2012q4.zip            28MB
#> 45:  https://fis.fda.gov/content/Exports/aers_ascii_2012q3.zip            16MB
#> 46:  https://fis.fda.gov/content/Exports/aers_ascii_2012q2.zip            25MB
#> 47:  https://fis.fda.gov/content/Exports/aers_ascii_2012q1.zip            26MB
#> 48:  https://fis.fda.gov/content/Exports/aers_ascii_2011q4.zip            23MB
#> 49:  https://fis.fda.gov/content/Exports/aers_ascii_2011q3.zip            23MB
#> 50:  https://fis.fda.gov/content/Exports/aers_ascii_2011q2.zip            23MB
#> 51:  https://fis.fda.gov/content/Exports/aers_ascii_2011q1.zip            21MB
#> 52:  https://fis.fda.gov/content/Exports/aers_ascii_2010q4.zip            20MB
#> 53:  https://fis.fda.gov/content/Exports/aers_ascii_2010q3.zip            22MB
#> 54:  https://fis.fda.gov/content/Exports/aers_ascii_2010q2.zip            17MB
#> 55:  https://fis.fda.gov/content/Exports/aers_ascii_2010q1.zip            16MB
#> 56:  https://fis.fda.gov/content/Exports/aers_ascii_2009q4.zip            16MB
#> 57:  https://fis.fda.gov/content/Exports/aers_ascii_2009q3.zip            16MB
#> 58:  https://fis.fda.gov/content/Exports/aers_ascii_2009q2.zip            14MB
#> 59:  https://fis.fda.gov/content/Exports/aers_ascii_2009q1.zip            13MB
#> 60:  https://fis.fda.gov/content/Exports/aers_ascii_2008q4.zip            13MB
#> 61:  https://fis.fda.gov/content/Exports/aers_ascii_2008q3.zip            13MB
#> 62:  https://fis.fda.gov/content/Exports/aers_ascii_2008q2.zip            12MB
#> 63:  https://fis.fda.gov/content/Exports/aers_ascii_2008q1.zip            12MB
#> 64:  https://fis.fda.gov/content/Exports/aers_ascii_2007q4.zip            12MB
#> 65:  https://fis.fda.gov/content/Exports/aers_ascii_2007q3.zip           9.9MB
#> 66:  https://fis.fda.gov/content/Exports/aers_ascii_2007q2.zip           9.5MB
#> 67:  https://fis.fda.gov/content/Exports/aers_ascii_2007q1.zip           9.6MB
#> 68:  https://fis.fda.gov/content/Exports/aers_ascii_2006q4.zip           9.1MB
#> 69:  https://fis.fda.gov/content/Exports/aers_ascii_2006q3.zip           8.5MB
#> 70:  https://fis.fda.gov/content/Exports/aers_ascii_2006q2.zip           9.7MB
#>                                                     xml_urls xml_file_size
#>                                                       <char>        <char>
#>  1: https://fis.fda.gov/content/Exports/faers_xml_2023Q3.zip         123MB
#>  2: https://fis.fda.gov/content/Exports/faers_xml_2023q2.zip         130MB
#>  3: https://fis.fda.gov/content/Exports/faers_xml_2023q1.zip         133MB
#>  4: https://fis.fda.gov/content/Exports/faers_xml_2022Q4.zip         144MB
#>  5: https://fis.fda.gov/content/Exports/faers_xml_2022Q3.zip         132MB
#>  6: https://fis.fda.gov/content/Exports/faers_xml_2022q2.zip         140MB
#>  7: https://fis.fda.gov/content/Exports/faers_xml_2022q1.zip         136MB
#>  8: https://fis.fda.gov/content/Exports/faers_xml_2021Q4.zip         123MB
#>  9: https://fis.fda.gov/content/Exports/faers_xml_2021Q3.zip         132MB
#> 10: https://fis.fda.gov/content/Exports/faers_xml_2021Q2.zip         123MB
#> 11: https://fis.fda.gov/content/Exports/faers_xml_2021Q1.zip         130MB
#> 12: https://fis.fda.gov/content/Exports/faers_xml_2020Q4.zip         131MB
#> 13: https://fis.fda.gov/content/Exports/faers_xml_2020Q3.zip         121MB
#> 14: https://fis.fda.gov/content/Exports/faers_xml_2020Q2.zip         123MB
#> 15: https://fis.fda.gov/content/Exports/faers_xml_2020Q1.zip         125MB
#> 16: https://fis.fda.gov/content/Exports/faers_xml_2019Q4.zip         113MB
#> 17: https://fis.fda.gov/content/Exports/faers_xml_2019Q3.zip         118MB
#> 18: https://fis.fda.gov/content/Exports/faers_xml_2019Q2.zip         118MB
#> 19: https://fis.fda.gov/content/Exports/faers_xml_2019Q1.zip         103MB
#> 20: https://fis.fda.gov/content/Exports/faers_xml_2018q4.zip         112MB
#> 21: https://fis.fda.gov/content/Exports/faers_xml_2018q3.zip         112MB
#> 22: https://fis.fda.gov/content/Exports/faers_xml_2018q2.zip         112MB
#> 23: https://fis.fda.gov/content/Exports/faers_xml_2018q1.zip          94MB
#> 24: https://fis.fda.gov/content/Exports/faers_xml_2017q4.zip          76MB
#> 25: https://fis.fda.gov/content/Exports/faers_xml_2017q3.zip          91MB
#> 26: https://fis.fda.gov/content/Exports/faers_xml_2017q2.zip          86MB
#> 27: https://fis.fda.gov/content/Exports/faers_xml_2017q1.zip          91MB
#> 28: https://fis.fda.gov/content/Exports/faers_xml_2016q4.zip          82MB
#> 29: https://fis.fda.gov/content/Exports/faers_xml_2016q3.zip          87MB
#> 30: https://fis.fda.gov/content/Exports/faers_xml_2016q2.zip          81MB
#> 31: https://fis.fda.gov/content/Exports/faers_xml_2016q1.zip          84MB
#> 32: https://fis.fda.gov/content/Exports/faers_xml_2015q4.zip          77MB
#> 33: https://fis.fda.gov/content/Exports/faers_xml_2015q3.zip          88MB
#> 34: https://fis.fda.gov/content/Exports/faers_xml_2015q2.zip          70MB
#> 35: https://fis.fda.gov/content/Exports/faers_xml_2015q1.zip          72MB
#> 36: https://fis.fda.gov/content/Exports/faers_xml_2014q4.zip          53MB
#> 37: https://fis.fda.gov/content/Exports/faers_xml_2014q3.zip          54MB
#> 38: https://fis.fda.gov/content/Exports/faers_xml_2014q2.zip          44MB
#> 39: https://fis.fda.gov/content/Exports/faers_xml_2014q1.zip          52MB
#> 40: https://fis.fda.gov/content/Exports/faers_xml_2013q4.zip          46MB
#> 41: https://fis.fda.gov/content/Exports/faers_xml_2013q3.zip          40MB
#> 42: https://fis.fda.gov/content/Exports/faers_xml_2013q2.zip          38MB
#> 43: https://fis.fda.gov/content/Exports/faers_xml_2013q1.zip          44MB
#> 44: https://fis.fda.gov/content/Exports/faers_xml_2012q4.zip          50MB
#> 45: https://fis.fda.gov/content/Exports/aers_sgml_2012q3.zip          21MB
#> 46: https://fis.fda.gov/content/Exports/aers_sgml_2012q2.zip          32MB
#> 47: https://fis.fda.gov/content/Exports/aers_sgml_2012q1.zip          33MB
#> 48: https://fis.fda.gov/content/Exports/aers_sgml_2011q4.zip          29MB
#> 49: https://fis.fda.gov/content/Exports/aers_sgml_2011q3.zip          29MB
#> 50: https://fis.fda.gov/content/Exports/aers_sgml_2011q2.zip          29MB
#> 51: https://fis.fda.gov/content/Exports/aers_sgml_2011q1.zip          26MB
#> 52: https://fis.fda.gov/content/Exports/aers_sgml_2010q4.zip          25MB
#> 53: https://fis.fda.gov/content/Exports/aers_sgml_2010q3.zip          28MB
#> 54: https://fis.fda.gov/content/Exports/aers_sgml_2010q2.zip          22MB
#> 55: https://fis.fda.gov/content/Exports/aers_sgml_2010q1.zip          20MB
#> 56: https://fis.fda.gov/content/Exports/aers_sgml_2009q4.zip          20MB
#> 57: https://fis.fda.gov/content/Exports/aers_sgml_2009q3.zip          19MB
#> 58: https://fis.fda.gov/content/Exports/aers_sgml_2009q2.zip          18MB
#> 59: https://fis.fda.gov/content/Exports/aers_sgml_2009q1.zip          16MB
#> 60: https://fis.fda.gov/content/Exports/aers_sgml_2008q4.zip          16MB
#> 61: https://fis.fda.gov/content/Exports/aers_sgml_2008q3.zip          16MB
#> 62: https://fis.fda.gov/content/Exports/aers_sgml_2008q2.zip          16MB
#> 63: https://fis.fda.gov/content/Exports/aers_sgml_2008q1.zip          15MB
#> 64: https://fis.fda.gov/content/Exports/aers_sgml_2007q4.zip          14MB
#> 65: https://fis.fda.gov/content/Exports/aers_sgml_2007q3.zip          13MB
#> 66: https://fis.fda.gov/content/Exports/aers_sgml_2007q2.zip          12MB
#> 67: https://fis.fda.gov/content/Exports/aers_sgml_2007q1.zip          12MB
#> 68: https://fis.fda.gov/content/Exports/aers_sgml_2006q4.zip          12MB
#> 69: https://fis.fda.gov/content/Exports/aers_sgml_2006q3.zip          11MB
#> 70: https://fis.fda.gov/content/Exports/aers_sgml_2006q2.zip          13MB
#>  [ reached getOption("max.print") -- omitted 10 rows ]
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
data1 <- faers(2004, "q1",
  dir = system.file("extdata", package = "faers"),
  compress_dir = tempdir()
)
#> Finding 1 file already downloaded: 'aers_ascii_2004q1.zip'
data1
#> FAERS data from 1 Quarterly ascii file
#>   Total reports: 100 (with duplicates)
```

Furthermore, in cases where multiple quarterly data files are requisite,
the `faers_combine()` function is judiciously employed.

``` r
data2 <- faers(c(2004, 2017), c("q1", "q2"),
  dir = system.file("extdata", package = "faers"),
  compress_dir = tempdir()
)
#> Finding 2 files already downloaded: 'aers_ascii_2004q1.zip' and
#> 'faers_ascii_2017q2.zip'
#> → Combining all 2 <FAERS> Datas
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
# we use faers_filter() to extract data we are interested
# here, we just sample 100 reports. You should do it based on your purpose.
faers_phv_signal(
  faers_filter(data, .fn = ~ sample(faers_primaryid(.x), 100L)),
  .full = data
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
#> [1] faers_0.99.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.33     R6_2.5.1          fastmap_1.1.1     rvest_1.0.3      
#>  [5] xfun_0.39         magrittr_2.0.3    rappdirs_0.3.3    glue_1.6.2       
#>  [9] stringr_1.5.0     knitr_1.43        htmltools_0.5.5   rmarkdown_2.23   
#> [13] lifecycle_1.0.3   xml2_1.3.5        cli_3.6.1         vctrs_0.6.3      
#> [17] data.table_1.14.9 compiler_4.3.1    httr_1.4.6        rstudioapi_0.15.0
#> [21] tools_4.3.1       curl_5.0.1        evaluate_0.21     yaml_2.3.7       
#> [25] rlang_1.1.1       stringi_1.7.12    selectr_0.4-2
```

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

### Check metadata of FAERS

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

#### Download and Parse quarterly data files from FAERS

The FAERS Quarterly Data files contain raw data extracted from the AERS
database for the indicated time ranges. The quarterly data files, which
are available in ASCII or SGML formats, include:

  - `demo`: demographic and administrative information
  - `drug`: drug information from the case reports
  - `reac`: reaction information from the reports
  - `outc`: patient outcome information from the reports
  - `rpsr`: information on the source of the reports
  - `ther`: drug therapy start dates and end dates for the reported
    drugs
  - `indi`: contains all “Medical Dictionary for Regulatory Activities”
    (MedDRA) terms coded for the indications for use (diagnoses) for the
    reported drugs

Generally, we can use `faers()` function to download and parse all
quarterly data files from FAERS. Internally, the `faers()` function
seamlessly utilizes `faers_download()` and `faers_parse()` to preprocess
each quarterly data file from the FAERS repository. The default `format`
was `ascii` and will return a `FAERSascii` object. (xml format would
also be okay , but presently, the XML file receives only minimal support
in the following process.)

Some variables has been added into specific field. See `?faers_parse`
for details.

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

#### Standardize and De-duplication

The `reac` file provides the adverse drug reactions, where it includes
the “P.T.” field or the “Preferred Term” level terminology from the
Medical Dictionary for Regulatory Activities (MedDRA). The `indi` file
contains the drug indications, which also uses the “P.T.” level of
MedDRA as a descriptor for the drug indication. In this way, `MedDRA`
was necessary to standardize this field and add additional informations,
such as `System Organ Classes`.

``` r
# you must replace `meddra_path` with the path of uncompressed meddra data
data <- faers_standardize(data2, meddra_path)
```

To proceed following steps, we just read a standardized data.

``` r
data <- readRDS(system.file("extdata", "standardized_data.rds",
  package = "faers"
))
data
#> Standardized FAERS data from 2 Quarterly ascii files
#>   Total reports: 200 (with duplicates)
```

One limitation of FAERS database is Duplicate and incomplete reports.
There are many instances of duplicative reports and some reports do not
contain all the necessary information. We deemed two cases to be
identical if they exhibited a full concordance across drugs
administered, and adverse reactions and but showed discrepancies in one
or none of the following fields: gender, age, reporting country, event
date, start date, and drug indications.

``` r
data <- faers_dedup(data)
#> → deduplication from the same source by retain the most recent report
#> → merging `drug`, `indi`, `ther`, and `reac` data
#> → deduplication from multiple sources by matching gender, age, reporting country, event date, start date, drug indications, drugs administered, and adverse reactions
data
#> Standardized and De-duplicated FAERS data from 2 Quarterly ascii files
#>   Total unique reports: 200
```

#### Pharmacovigilance analysis

Pharmacovigilance is the science and activities relating to the
detection, assessment, understanding and prevention of adverse effects
or any other medicine/vaccine related problem.

To mine the signals of “insulin”, we start by using the `faers_filter()`
function. In this function, the `.fn` argument should be a function that
accepts data specified in `.field`. It is important to note that `.fn`
should always return the `primaryid` that you want to keep.

To enhance our analysis, it would be advantageous to include all drug
synonym names for `insulin`. These synonyms can be obtained by querying
sources such as <https://go.drugbank.com/> or alternative databases.
Furthermore, we extract the brand names of insulin from the
[Drugs@FDA](https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files)
dataset, which can be easily obtained using the `fda_drugs()` function.

``` r
insulin_pattern <- "insulin"
insulin_pattern <- paste(insulin_pattern, collapse = "|")
fda_insulin <- fda_drugs()[
  grepl(insulin_pattern, ActiveIngredient, ignore.case = TRUE)
]
#> → Using Drugs@FDA data from cached
#>   '/home/yun/.cache/R/faers/fdadrugs/fda_drugs_data_2023-12-16.zip'
#>   Snapshot date: 2023-12-16
#> Warning: One or more parsing issues, call `problems()` on your data frame for details,
#> e.g.:
#>   dat <- vroom(...)
#>   problems(dat)
insulin_pattern <- paste0(
  unique(tolower(c(insulin_pattern, fda_insulin$DrugName))),
  collapse = "|"
)
insulin_data <- faers_filter(data, .fn = function(x) {
  idx <- grepl(insulin_pattern, x$drugname, ignore.case = TRUE) |
    grepl(insulin_pattern, x$prod_ai, ignore.case = TRUE)
  x[idx, primaryid]
}, .field = "drug")
insulin_data
#> Standardized and De-duplicated FAERS data from 2 Quarterly ascii files
#>   Total unique reports: 3
```

Then, signal can be easily obtained with `faers_phv_signal()` which
internally use `faers_phv_table()` to create a contingency table and use
`phv_signal()` to do signal analysis specified in `.methods` argument.
By default, all supported signal analysis methods will be run, including
“ror”, “prr”, “chisq”, “bcpnn\_norm”, “bcpnn\_mcmc”, “obsexp\_shrink”,
“fisher”, and “ebgm”.

The most important argument for this function is `.object`, which should
be a de-duplicated FAERSascii object containing the data for the drugs
or traits of interest. Additionally, you must specify either `.full`,
which represents the background distributions data (usually the entire
FAERS data), or you can specify `.object2`, which should be the control
data or another drug of interest for comparison.

``` r
insulin_signals <- faers_phv_signal(insulin_data, .full = data)
insulin_signals
#> Key: <soc_name>
#>                                                                soc_name     a
#>                                                                  <char> <int>
#>  1:                                Blood and lymphatic system disorders     0
#>  2:                                                   Cardiac disorders     1
#>  3:                          Congenital, familial and genetic disorders     0
#>  4:                                         Ear and labyrinth disorders     0
#>  5:                                                 Endocrine disorders     0
#>  6:                                                       Eye disorders     1
#>  7:                                          Gastrointestinal disorders     1
#>  8:                General disorders and administration site conditions     1
#>  9:                                             Hepatobiliary disorders     0
#> 10:                                             Immune system disorders     0
#> 11:                                         Infections and infestations     2
#> 12:                      Injury, poisoning and procedural complications     1
#> 13:                                                      Investigations     2
#> 14:                                  Metabolism and nutrition disorders     2
#> 15:                     Musculoskeletal and connective tissue disorders     0
#>         b     c     d expected        ror ror_ci_low ror_ci_high        prr
#>     <int> <int> <int>    <num>      <num>      <num>       <num>      <num>
#>  1:     3    14   183    0.210  0.0000000 0.00000000         NaN  0.0000000
#>  2:     2     7   190    0.120 13.5714286 1.09612571  168.031524  9.3809524
#>  3:     3     1   196    0.015  0.0000000 0.00000000         NaN  0.0000000
#>  4:     3     4   193    0.060  0.0000000 0.00000000         NaN  0.0000000
#>  5:     3     1   196    0.015  0.0000000 0.00000000         NaN  0.0000000
#>  6:     2     9   188    0.150 10.4444444 0.86432480  126.209985  7.2962963
#>  7:     2    33   164    0.510  2.4848485 0.21888790   28.208376  1.9898990
#>  8:     2    79   118    1.200  0.7468354 0.06658895    8.376212  0.8312236
#>  9:     3     9   188    0.135  0.0000000 0.00000000         NaN  0.0000000
#> 10:     3     4   193    0.060  0.0000000 0.00000000         NaN  0.0000000
#> 11:     1    30   167    0.480 11.1333333 0.97846354  126.679335  4.3777778
#> 12:     2    30   167    0.465  2.7833333 0.24461589   31.669834  2.1888889
#> 13:     1    30   167    0.480 11.1333333 0.97846354  126.679335  4.3777778
#> 14:     1    10   187    0.180 37.4000000 3.12161662  448.088336 13.1333333
#> 15:     3    19   178    0.285  0.0000000 0.00000000         NaN  0.0000000
#>     prr_ci_low prr_ci_high        chisq chisq_pvalue bcpnn_norm_ic
#>          <num>       <num>        <num>        <num>         <num>
#>  1:  0.0000000         NaN 4.096414e-31  1.000000000   -0.97458175
#>  2:  1.6173190   54.412435 1.272561e+00  0.259286856    0.63833534
#>  3:  0.0000000         NaN 1.265951e-26  1.000000000   -0.31739140
#>  4:  0.0000000         NaN 2.866117e-32  1.000000000   -0.63762418
#>  5:  0.0000000         NaN 1.265951e-26  1.000000000   -0.31739140
#>  6:  1.3027676   40.863727 8.727402e-01  0.350197814    0.57607008
#>  7:  0.3897628   10.159250 6.459270e-31  1.000000000    0.05900067
#>  8:  0.1662546    4.155871 1.762340e-31  1.000000000   -0.57013768
#>  9:  0.0000000         NaN 6.503567e-28  1.000000000   -0.83645056
#> 10:  0.0000000         NaN 2.866117e-32  1.000000000   -0.63762418
#> 11:  1.8426662   10.400656 2.619652e+00  0.105547591    0.81550390
#> 12:  0.4272128   11.215100 3.165120e-03  0.955135161    0.11209704
#> 13:  1.8426662   10.400656 2.619652e+00  0.105547591    0.81550390
#> 14:  4.8197010   35.787374 1.045469e+01  0.001223383    1.24133970
#> 15:  0.0000000         NaN 4.555566e-29  1.000000000   -1.09198678
#>     bcpnn_norm_ic_ci_low bcpnn_norm_ic_ci_high bcpnn_mcmc_ic
#>                    <num>                 <num>         <num>
#>  1:            -4.960069              3.010906   -0.50449401
#>  2:            -2.239177              3.515848    1.27660894
#>  3:            -4.844759              4.209977   -0.03947607
#>  4:            -4.773808              3.498560   -0.16092798
#>  5:            -4.844759              4.209977   -0.03947607
#>  6:            -2.270036              3.422176    1.20821022
#>  7:            -2.690902              2.808903    0.57112278
#>  8:            -3.294337              2.154062   -0.18051848
#>  9:            -4.858428              3.185527   -0.34296004
#> 10:            -4.773808              3.498560   -0.16092798
#> 11:            -1.546257              3.177265    1.35163975
#> 12:            -2.641970              2.866165    0.63695385
#> 13:            -1.546257              3.177265    1.35163975
#> 14:            -1.203821              3.686501    1.87988617
#> 15:            -5.059438              2.875465   -0.64969772
#>     bcpnn_mcmc_ic_ci_low bcpnn_mcmc_ic_ci_high    oe_ratio oe_ratio_ci_low
#>                    <num>                 <num>       <num>           <num>
#>  1:          -10.3227280             1.7097008 -0.50589093     -10.3882898
#>  2:           -2.3655391             2.7746712  1.27462238      -2.5084784
#>  3:          -10.0215171             2.2542258 -0.04264434      -9.9520669
#>  4:          -10.0098340             2.1264994 -0.16349873     -10.0243790
#>  5:           -9.9353969             2.2481277 -0.04264434      -9.9976267
#>  6:           -2.4113479             2.6932162  1.20645088      -2.5766499
#>  7:           -2.8700834             1.8915814  0.57060721      -3.2124936
#>  8:           -3.5041218             0.9905238 -0.18057225      -3.9636731
#>  9:          -10.2883764             1.9221028 -0.34482850     -10.2279901
#> 10:           -9.9715096             2.0990512 -0.16349873     -10.1426249
#> 11:           -0.7880866             2.3055106  1.35107444      -1.2419932
#> 12:           -2.8025074             1.9833057  0.63636165      -3.1467392
#> 13:           -0.7691758             2.3151794  1.35107444      -1.2419932
#> 14:           -0.4359694             3.0233355  1.87832144      -0.7147462
#> 15:          -10.5114367             1.5589373 -0.65076456     -10.4720362
#>     oe_ratio_ci_high odds_ratio odds_ratio_ci_low odds_ratio_ci_high
#>                <num>      <num>             <num>              <num>
#>  1:         1.717446  0.0000000        0.00000000           33.68585
#>  2:         2.962049 13.0303800        0.20150028          279.21542
#>  3:         2.257209  0.0000000        0.00000000         2462.50000
#>  4:         2.120288  0.0000000        0.00000000          145.21133
#>  5:         2.259757  0.0000000        0.00000000         2462.50000
#>  6:         2.893877 10.1233187        0.15947006          211.87117
#>  7:         2.258033  2.4701675        0.04089140           48.73410
#>  8:         1.506854  0.7478897        0.01250834           14.59249
#>  9:         1.901241  0.0000000        0.00000000           55.53664
#> 10:         2.127870  0.0000000        0.00000000          145.21133
#> 11:         2.742477 10.9151800        0.55237709          657.45882
#> 12:         2.323788  2.7642969        0.04567010           54.61951
#> 13:         2.742477 10.9151800        0.55237709          657.45882
#> 14:         3.269724 35.2158110        1.70522393         2176.34560
#> 15:         1.575781  0.0000000        0.00000000           23.77817
#>     fisher_pvalue     ebgm ebgm_ci_low ebgm_ci_high
#>             <num>    <num>       <num>        <num>
#>  1:    1.00000000 2.421502        2.38         2.46
#>  2:    0.11582153 2.421743        2.38         2.46
#>  3:    1.00000000 2.421595        2.38         2.46
#>  4:    1.00000000 2.421574        2.38         2.46
#>  5:    1.00000000 2.421595        2.38         2.46
#>  6:    0.14330745 2.421729        2.38         2.46
#>  7:    0.42998325 2.421556        2.38         2.46
#>  8:    1.00000000 2.421225        2.38         2.46
#>  9:    1.00000000 2.421538        2.38         2.46
#> 10:    1.00000000 2.421574        2.38         2.46
#> 11:    0.06722095 2.421769        2.38         2.46
#> 12:    0.39832191 2.421578        2.38         2.46
#> 13:    0.06722095 2.421769        2.38         2.46
#> 14:    0.00961474 2.421913        2.38         2.46
#> 15:    1.00000000 2.421466        2.38         2.46
#>  [ reached getOption("max.print") -- omitted 12 rows ]
```

The column containing the events of interest can be specified using an
atomic character in the `.events` (default: “soc\_name”) argument. The
combination of all specified columns will define the unique event.
Additionally, we can control which field data to find the columns in the
`.field` (default: “reac”) argument.

``` r
insulin_signals_hlgt <- faers_phv_signal(
  insulin_data,
  .events = "hlgt_name",
  .full = data
)
insulin_signals_hlgt
#> Key: <hlgt_name>
#>                                                       hlgt_name     a     b
#>                                                          <char> <int> <int>
#>   1:                                        Acid-base disorders     0     3
#>   2:                              Administration site reactions     0     3
#>   3:                                        Allergic conditions     0     3
#>   4:               Anaemias nonhaemolytic and marrow depression     0     3
#>   5:                                   Angioedema and urticaria     0     3
#>  ---                                                                       
#> 138:                                 Viral infectious disorders     0     3
#> 139:                                           Vision disorders     1     2
#> 140: Vulvovaginal disorders (excl infections and inflammations)     0     3
#> 141:              Water, electrolyte and mineral investigations     1     2
#> 142:                                 White blood cell disorders     0     3
#>          c     d expected   ror ror_ci_low ror_ci_high      prr prr_ci_low
#>      <int> <int>    <num> <num>      <num>       <num>    <num>      <num>
#>   1:     1   196    0.015  0.00   0.000000         NaN  0.00000   0.000000
#>   2:     9   188    0.135  0.00   0.000000         NaN  0.00000   0.000000
#>   3:     3   194    0.045  0.00   0.000000         NaN  0.00000   0.000000
#>   4:     2   195    0.030  0.00   0.000000         NaN  0.00000   0.000000
#>   5:     1   196    0.015  0.00   0.000000         NaN  0.00000   0.000000
#>  ---                                                                      
#> 138:     6   191    0.090  0.00   0.000000         NaN  0.00000   0.000000
#> 139:     2   195    0.045 48.75   3.038446    782.1638 32.83333   3.971134
#> 140:     1   196    0.015  0.00   0.000000         NaN  0.00000   0.000000
#> 141:     1   196    0.030 98.00   4.405403   2180.0503 65.66667   5.249564
#> 142:     6   191    0.090  0.00   0.000000         NaN  0.00000   0.000000
#>      prr_ci_high        chisq chisq_pvalue bcpnn_norm_ic bcpnn_norm_ic_ci_low
#>            <num>        <num>        <num>         <num>                <num>
#>   1:         NaN 1.265951e-26  1.000000000    -0.3173914            -4.844759
#>   2:         NaN 6.503567e-28  1.000000000    -0.8364506            -4.858428
#>   3:         NaN 3.579029e-27  1.000000000    -0.5729454            -4.769182
#>   4:         NaN 1.292402e-30  1.000000000    -0.4806767            -4.781546
#>   5:         NaN 1.265951e-26  1.000000000    -0.3173914            -4.844759
#>  ---                                                                         
#> 138:         NaN 2.800437e-31  1.000000000    -0.7316939            -4.801687
#> 139:    271.4660 4.741741e+00  0.029439266     0.8697497            -2.230719
#> 140:         NaN 1.265951e-26  1.000000000    -0.3173914            -4.844759
#> 141:    821.4227 7.550975e+00  0.005997761     0.9620184            -2.278658
#> 142:         NaN 2.800437e-31  1.000000000    -0.7316939            -4.801687
#>      bcpnn_norm_ic_ci_high bcpnn_mcmc_ic bcpnn_mcmc_ic_ci_low
#>                      <num>         <num>                <num>
#>   1:              4.209977   -0.03947607            -9.936350
#>   2:              3.185527   -0.34296004           -10.324187
#>   3:              3.623291   -0.12157646           -10.144909
#>   4:              3.820193   -0.08111417           -10.044329
#>   5:              4.209977   -0.03947607            -9.937904
#>  ---                                                         
#> 138:              3.338300   -0.23653305           -10.164893
#> 139:              3.970219    1.46338605            -2.217522
#> 140:              4.209977   -0.03947607           -10.033860
#> 141:              4.202695    1.50384833            -2.188318
#> 142:              3.338300   -0.23653305           -10.066599
#>      bcpnn_mcmc_ic_ci_high    oe_ratio oe_ratio_ci_low oe_ratio_ci_high
#>                      <num>       <num>           <num>            <num>
#>   1:              2.267840 -0.04264434       -9.987751         2.263439
#>   2:              1.911937 -0.34482850      -10.335599         1.908490
#>   3:              2.172273 -0.12432814      -10.042727         2.166006
#>   4:              2.206658 -0.08406426      -10.083733         2.223030
#>   5:              2.260174 -0.04264434       -9.921420         2.248651
#>  ---                                                                   
#> 138:              2.015313 -0.23878686      -10.169003         2.036207
#> 139:              3.008917  1.46063437       -2.322466         3.148061
#> 140:              2.250537 -0.04264434      -10.065103         2.242616
#> 141:              3.051391  1.50089824       -2.282203         3.188325
#> 142:              2.012159 -0.23878686      -10.133274         2.022040
#>      odds_ratio odds_ratio_ci_low odds_ratio_ci_high fisher_pvalue     ebgm
#>           <num>             <num>              <num>         <num>    <num>
#>   1:    0.00000         0.0000000         2462.50000    1.00000000 3.670936
#>   2:    0.00000         0.0000000           55.53664    1.00000000 3.459110
#>   3:    0.00000         0.0000000          213.09160    1.00000000 3.615584
#>   4:    0.00000         0.0000000          408.94448    1.00000000 3.643050
#>   5:    0.00000         0.0000000         2462.50000    1.00000000 3.670936
#>  ---                                                                       
#> 138:    0.00000         0.0000000           88.65547    1.00000000 3.535617
#> 139:   43.06704         0.5524356         1247.00745    0.04454850 4.117845
#> 140:    0.00000         0.0000000         2462.50000    1.00000000 3.670936
#> 141:   80.16649         0.8340751         7069.12541    0.02984925 4.149126
#> 142:    0.00000         0.0000000           88.65547    1.00000000 3.535617
#>      ebgm_ci_low ebgm_ci_high
#>            <num>        <num>
#>   1:        1.66         7.14
#>   2:        1.56         6.73
#>   3:        1.63         7.03
#>   4:        1.65         7.09
#>   5:        1.66         7.14
#>  ---                         
#> 138:        1.60         6.88
#> 139:        1.96         7.71
#> 140:        1.66         7.14
#> 141:        1.98         7.77
#> 142:        1.60         6.88
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
#> [1] faers_0.99.3
#> 
#> loaded via a namespace (and not attached):
#>  [1] generics_0.1.3     rappdirs_0.3.3     utf8_1.2.4         xml2_1.3.6        
#>  [5] stringi_1.8.3      openEBGM_0.9.1     lattice_0.22-5     digest_0.6.33     
#>  [9] magrittr_2.0.3     evaluate_0.23      grid_4.3.1         MCMCpack_1.6-3    
#> [13] fastmap_1.1.1      Matrix_1.6-4       survival_3.5-7     mcmc_0.9-7        
#> [17] httr_1.4.7         rvest_1.0.3        fansi_1.0.6        selectr_0.4-2     
#> [21] scales_1.3.0       cli_3.6.2          rlang_1.1.2        crayon_1.5.2      
#> [25] munsell_0.5.0      bit64_4.0.5        splines_4.3.1      yaml_2.3.8        
#> [29] tools_4.3.1        parallel_4.3.1     SparseM_1.81       tzdb_0.4.0        
#> [33] MatrixModels_0.5-2 coda_0.19-4        dplyr_1.1.4        colorspace_2.1-0  
#> [37] ggplot2_3.4.4      curl_5.2.0         vctrs_0.6.5        R6_2.5.1          
#> [41] lifecycle_1.0.4    stringr_1.5.1      bit_4.0.5          vroom_1.6.5       
#> [45] MASS_7.3-60        pkgconfig_2.0.3    archive_1.1.6      pillar_1.9.0      
#> [49] gtable_0.3.4       data.table_1.14.9  glue_1.6.2         xfun_0.41         
#> [53] tibble_3.2.1       tidyselect_1.2.0   knitr_1.45         htmltools_0.5.7   
#> [57] rmarkdown_2.25     compiler_4.3.1     quantreg_5.96
```

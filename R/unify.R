#' Used in internal function, unify data
#' @noRd
unify_ascii <- function(data, field, year, quarter) {
    data.table::setnames(data, tolower)
    if (is_from_laers(year, quarter)) {
        data.table::setnames(data, "isr", "primaryid")
    } else {
        data[, caseid := as.character(caseid)]
    }
    data[, primaryid := as.character(primaryid)]
    switch(field,
        demo = unify_ascii_demo(data, year, quarter),
        ther = unify_ascii_ther(data, year, quarter),
        indi = unify_ascii_indi(data, year, quarter),
        drug = unify_ascii_drug(data, year, quarter),
        outc = unify_ascii_outc(data, year, quarter)
    )
    old_nms <- data.table::copy(names(data))
    data[, c("year", "quarter") := list(year, quarter)]
    data.table::setcolorder(data, c("year", "quarter", old_nms))
    data[!is.na(primaryid)]
}

unify_ascii_demo <- function(data, year, quarter) {
    if (is_before_period(year, quarter, 2014L, "q2")) {
        data.table::setnames(data, "gndr_cod", "sex")
    }
    if (is_from_laers(year, quarter)) {
        data.table::setnames(
            data, c("case", "i_f_cod"),
            c("caseid", "i_f_code")
        )
        # nolint start
        data[, caseversion := 0L]
        data[, caseid := as.character(caseid)]
    }
    # AGE FILED TO YEARS
    data[, age := as.numeric(age)]
    data[, age_in_years := data.table::fcase(
        age_cod == "DEC", age * 12L,
        age_cod == "YR" | age_cod == "YEAR", age,
        age_cod == "MON", age / 12L,
        age_cod == "WK" | age_cod == "WEEK", age / 52L,
        age_cod == "DY" | age_cod == "DAY", age / 365L,
        age_cod == "HR" | age_cod == "HOUR", age / 8760L,
        rep_len(TRUE, .N), age
    )]
    # before 05q2, there is not a reporter_country column
    if (is_before_period(year, quarter, 2005L, "q2")) {
        data[, country_code := NA_character_]
    } else {
        # COUNTRY CODE
        data[, country_code := data.table::fcase(
            nchar(reporter_country) == 2L, reporter_country,
            reporter_country == "AFGHANISTAN", "AF",
            reporter_country == "ALAND ISLANDS", "AX",
            reporter_country == "ALBANIA", "AL",
            reporter_country == "ALGERIA", "DZ",
            reporter_country == "AMERICAN SAMOA", "AS",
            reporter_country == "ANDORRA", "AD",
            reporter_country == "ANGOLA", "AO",
            reporter_country == "ANGUILLA", "AI",
            reporter_country == "ANTIGUA AND BARBUDA", "AG",
            reporter_country == "ARGENTINA", "AR",
            reporter_country == "ARMENIA", "AM",
            reporter_country == "ARUBA", "AW",
            reporter_country == "AUSTRALIA", "AU",
            reporter_country == "AUSTRIA", "AT",
            reporter_country == "AZERBAIJAN", "AZ",
            reporter_country == "BAHAMAS", "BS",
            reporter_country == "BAHRAIN", "BH",
            reporter_country == "BANGLADESH", "BD",
            reporter_country == "BARBADOS", "BB",
            reporter_country == "BELARUS", "BY",
            reporter_country == "BELGIUM", "BE",
            reporter_country == "BELIZE", "BZ",
            reporter_country == "BENIN", "BJ",
            reporter_country == "BERMUDA", "BM",
            reporter_country == "BOLIVIA", "BO",
            reporter_country == "BOSNIA AND HERZEGOWINA", "BA",
            reporter_country == "BOTSWANA", "BW",
            reporter_country == "BRAZIL", "BR",
            reporter_country == "BRUNEI DARUSSALAM", "BN",
            reporter_country == "BULGARIA", "BG",
            reporter_country == "BURKINA FASO", "BF",
            reporter_country == "BURUNDI", "BI",
            reporter_country == "CAMBODIA", "KH",
            reporter_country == "CAMEROON", "CM",
            reporter_country == "CANADA", "CA",
            reporter_country == "CAPE VERDE", "CV",
            reporter_country == "CAYMAN ISLANDS", "KY",
            reporter_country == "CHILE", "CL",
            reporter_country == "CHINA", "CN",
            reporter_country == "COLOMBIA", "CO",
            reporter_country == "CONGO", "CG",
            reporter_country == "CONGO, THE DEMOCRATIC REPUBLIC OF THE", "CD",
            reporter_country == "COSTA RICA", "CR",
            reporter_country == "COTE D'IVOIRE", "CI",
            reporter_country == "COUNTRY NOT SPECIFIED", NA_character_,
            reporter_country == "CROATIA (local name: Hrvatska)", "HR",
            reporter_country == "CUBA", "CU",
            reporter_country == "CURACAO", "CW",
            reporter_country == "CYPRUS", "CY",
            reporter_country == "CZECH REPUBLIC", "CZ",
            reporter_country == "DENMARK", "DK",
            reporter_country == "DOMINICA", "DM",
            reporter_country == "DOMINICAN REPUBLIC", "DO",
            reporter_country == "ECUADOR", "EC",
            reporter_country == "EGYPT", "EG",
            reporter_country == "EL SALVADOR", "SV",
            reporter_country == "ESTONIA", "EE",
            reporter_country == "ETHIOPIA", "ET",
            reporter_country == "European Union", "EU",
            reporter_country == "FAROE ISLANDS", "FO",
            reporter_country == "FIJI", "FJ",
            reporter_country == "FINLAND", "FI",
            reporter_country == "FRANCE", "FR",
            reporter_country == "FRANCE, METROPOLITAN", "FX",
            reporter_country == "FRENCH GUIANA", "GF",
            reporter_country == "FRENCH POLYNESIA", "PF",
            reporter_country == "FRENCH SOUTHERN TERRITORIES", "TF",
            reporter_country == "GABON", "GA",
            reporter_country == "GAMBIA", "GM",
            reporter_country == "GEORGIA", "GE",
            reporter_country == "GERMANY", "DE",
            reporter_country == "GHANA", "GH",
            reporter_country == "GIBRALTAR", "GI",
            reporter_country == "GREECE", "GR",
            reporter_country == "GREENLAND", "GL",
            reporter_country == "GRENADA", "GD",
            reporter_country == "GUADELOUPE", "GP",
            reporter_country == "GUAM", "GU",
            reporter_country == "GUATEMALA", "GT",
            reporter_country == "GUINEA-BISSAU", "GW",
            reporter_country == "GUYANA", "GY",
            reporter_country == "HAITI", "HT",
            reporter_country == "HONDURAS", "HN",
            reporter_country == "HONG KONG", "HK",
            reporter_country == "HUNGARY", "HU",
            reporter_country == "ICELAND", "IS",
            reporter_country == "INDIA", "IN",
            reporter_country == "INDONESIA", "ID",
            reporter_country == "IRAN (ISLAMIC REPUBLIC OF)", "IR",
            reporter_country == "IRAQ", "IQ",
            reporter_country == "IRELAND", "IE",
            reporter_country == "ISLE OF MAN", "IM",
            reporter_country == "ISRAEL", "IL",
            reporter_country == "ITALY", "IT",
            reporter_country == "JAMAICA", "JM",
            reporter_country == "JAPAN", "JP",
            reporter_country == "JORDAN", "JO",
            reporter_country == "KAZAKHSTAN", "KZ",
            reporter_country == "KENYA", "KE",
            reporter_country == "KIRIBATI", "KI",
            reporter_country == "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF", "KP",
            reporter_country == "KOREA, REPUBLIC OF", "KR",
            reporter_country == "KUWAIT", "KW",
            reporter_country == "KYRGYZSTAN", "KG",
            reporter_country == "LAO PEOPLE'S DEMOCRATIC REPUBLIC", "LA",
            reporter_country == "LATVIA", "LV",
            reporter_country == "LEBANON", "LB",
            reporter_country == "LESOTHO", "LS",
            reporter_country == "LIBERIA", "LR",
            reporter_country == "LIBYAN ARAB JAMAHIRIYA", "LY",
            reporter_country == "LIECHTENSTEIN", "LI",
            reporter_country == "LITHUANIA", "LT",
            reporter_country == "LUXEMBOURG", "LU",
            reporter_country == "MACAU", "MO",
            reporter_country == "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF", "MK",
            reporter_country == "MADAGASCAR", "MG",
            reporter_country == "MALAWI", "MW",
            reporter_country == "MALAYSIA", "MY",
            reporter_country == "MALI", "ML",
            reporter_country == "MALTA", "MT",
            reporter_country == "MARTINIQUE", "MQ",
            reporter_country == "MAURITANIA", "MR",
            reporter_country == "MAURITIUS", "MU",
            reporter_country == "MAYOTTE", "YT",
            reporter_country == "MEXICO", "MX",
            reporter_country == "MICRONESIA, FEDERATED STATES OF", "FM",
            reporter_country == "MOLDOVA, REPUBLIC OF", "MD",
            reporter_country == "MONACO", "MC",
            reporter_country == "MONGOLIA", "MN",
            reporter_country == "MONTENEGRO", "ME",
            reporter_country == "MONTSERRAT", "MS",
            reporter_country == "MOROCCO", "MA",
            reporter_country == "MYANMAR", "MM",
            reporter_country == "NAMIBIA", "NA",
            reporter_country == "NEPAL", "NP",
            reporter_country == "NETHERLANDS", "NL",
            reporter_country == "NETHERLANDS ANTILLES", "AN",
            reporter_country == "NETHERLANDS ANTILLES (retired code)", "AN",
            reporter_country == "NEW CALEDONIA", "NC",
            reporter_country == "NEW ZEALAND", "NZ",
            reporter_country == "NICARAGUA", "NI",
            reporter_country == "NIGER", "NE",
            reporter_country == "NIGERIA", "NG",
            reporter_country == "NORFOLK ISLAND", "NF",
            reporter_country == "NORWAY", "NO",
            reporter_country == "OMAN", "OM",
            reporter_country == "PAKISTAN", "PK",
            reporter_country == "PALESTINIAN TERRITORY, OCCUPIED", "PS",
            reporter_country == "PANAMA", "PA",
            reporter_country == "PAPUA NEW GUINEA", "PG",
            reporter_country == "PARAGUAY", "PY",
            reporter_country == "PERU", "PE",
            reporter_country == "PHILIPPINES", "PH",
            reporter_country == "POLAND", "PL",
            reporter_country == "PORTUGAL", "PT",
            reporter_country == "PUERTO RICO", "PR",
            reporter_country == "QATAR", "QA",
            reporter_country == "REUNION", "RE",
            reporter_country == "ROMANIA", "RO",
            reporter_country == "RUSSIAN FEDERATION", "RU",
            reporter_country == "RWANDA", "RW",
            reporter_country == "SAINT KITTS AND NEVIS", "KN",
            reporter_country == "SAMOA", "WS",
            reporter_country == "SAUDI ARABIA", "SA",
            reporter_country == "SENEGAL", "SN",
            reporter_country == "SERBIA", "RS",
            reporter_country == "SERBIA AND MONTENEGRO", "CS",
            reporter_country == "SERBIA AND MONTENEGRO (see individual countries)", "CS",
            reporter_country == "SIERRA LEONE", "SL",
            reporter_country == "SINGAPORE", "SG",
            reporter_country == "SLOVAKIA (Slovak Republic)", "SK",
            reporter_country == "SLOVENIA", "SI",
            reporter_country == "SOLOMON ISLANDS", "SB",
            reporter_country == "SOUTH AFRICA", "ZA",
            reporter_country == "SPAIN", "ES",
            reporter_country == "SRI LANKA", "LK",
            reporter_country == "SUDAN", "SD",
            reporter_country == "SURINAME", "SR",
            reporter_country == "SVALBARD AND JAN MAYEN ISLANDS", "SJ",
            reporter_country == "SWAZILAND", "SZ",
            reporter_country == "SWEDEN", "SE",
            reporter_country == "SWITZERLAND", "CH",
            reporter_country == "SYRIAN ARAB REPUBLIC", "SY",
            reporter_country == "TAIWAN, PROVINCE OF CHINA", "TW",
            reporter_country == "TANZANIA, UNITED REPUBLIC OF", "TZ",
            reporter_country == "THAILAND", "TH",
            reporter_country == "TOGO", "TG",
            reporter_country == "TOKELAU", "TK",
            reporter_country == "TRINIDAD AND TOBAGO", "TT",
            reporter_country == "TUNISIA", "TN",
            reporter_country == "TURKEY", "TR",
            reporter_country == "UGANDA", "UG",
            reporter_country == "UKRAINE", "UA",
            reporter_country == "UNITED ARAB EMIRATES", "AE",
            reporter_country == "UNITED KINGDOM", "GB",
            reporter_country == "UNITED STATES", "US",
            reporter_country == "UNITED STATES MINOR OUTLYING ISLANDS", "UM",
            reporter_country == "URUGUAY", "UY",
            reporter_country == "UZBEKISTAN", "UZ",
            reporter_country == "VATICAN CITY STATE (HOLY SEE)", "VA",
            reporter_country == "VENEZUELA", "VE",
            reporter_country == "VIET NAM", "VN",
            reporter_country == "VIRGIN ISLANDS (U.S.)", "VI",
            reporter_country == "WALLIS AND FUTUNA ISLANDS", "WF",
            reporter_country == "YEMEN", "YE",
            reporter_country == "YUGOSLAVIA", "YU",
            reporter_country == "ZAIRE", "ZR",
            reporter_country == "ZAMBIA", "ZM",
            reporter_country == "ZIMBABWE", "ZW"
        )]
        missed_reporter_country <- data[is.na(country_code)][
            !is.na(reporter_country) &
                !reporter_country == "COUNTRY NOT SPECIFIED",
            unique(reporter_country)
        ]
        if (length(missed_reporter_country)) {
            cli::cli_warn("Cannot map {.val {missed_reporter_country}} into country_code")
        }
    }
    # Adding a new column gender
    data[, gender := data.table::fifelse(
        sex %in% c("UNK", "NS", "YR"), NA_character_, sex
    )]
    # nolint end
}

unify_ascii_drug <- function(data, year, quarter) {
    # Always use character to store nda_num
    # As data.table will parse nda_num as integer64 or character,
    # preventing the rbindlist from working.
    data[, nda_num := str_trim(as.character(nda_num))] # nolint
}

unify_ascii_ther <- function(data, year, quarter) {
    if (is_from_laers(year, quarter)) {
        data.table::setnames(data, "drug_seq", "dsg_drug_seq")
    }
}
unify_ascii_indi <- function(data, year, quarter) {
    if (is_from_laers(year, quarter)) {
        data.table::setnames(data, "drug_seq", "indi_drug_seq")
    }
}
unify_ascii_outc <- function(data, year, quarter) {
    if (year == 2012L && quarter == "q4") {
        data.table::setnames(data, "outc_code", "outc_cod")
    }
}
utils::globalVariables(c(
    "age_in_years", "age_cod", "age",
    "country_code", "reporter_country",
    "gender", "sex", "nda_num"
))

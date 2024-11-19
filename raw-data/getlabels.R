# 2001
> get_labels(l.data[[1]]$ASBH0TIM,values="n")
       #                   1                          2                          3                          4
       # "MORE THAN 2 YEARS"                  "2 YEARS"    "BETWEEN 1 AND 2 YEARS"                   "1 YEAR"
       #                   5                          6                          9
       #  "LESS THAN 1 YEAR" "LOGICALLY NOT APPLICABLE"                  "OMITTED"

> get_labels(l.data[[1]]$ASBGLANH,values="n")
# 1                         2                         3                         9
# "ALWAYS OR ALMOST ALWAYS"               "SOMETIMES"                   "NEVER"                 "OMITTED"

#2006
> get_labels(l.data[[2]]$ASBH0HLO, values="n")
# 1                          2                          3                          4
# "3 YEARS OR MORE"    "BETWEEN 2 AND 3 YEARS"                  "2 YEARS"    "BETWEEN 1 AND 2 YEARS"
# 5                          6                          9
# "1 YEAR OR LESS" "LOGICALLY NOT APPLICABLE"                  "OMITTED"

> get_labels(l.data[[2]]$ASBGLNGH,values="n")
# 1                                          2
# "ALWAYS SPEAK <LANGUAGE OF TEST>" "SOMETIMES YES AND SOMETIMES ANOTHER LANG"
# 3                                          9
# "I NEVER SPEAK <LANGUAGE OF TEST>"                                  "OMITTED"

#2011
> get_labels(l.data[[3]]$ASDHAPS ,values="n")
# 1                                        2                                        3
# "3 YEARS OR MORE" "LESS THAN 3 YEARS BUT MORE THAN 1 YEAR"                         "1 YEAR OR LESS"
# 4                                        6                                        9
# "DID NOT ATTEND"               "LOGICALLY NOT APPLICABLE"                     "OMITTED OR INVALID"

> get_labels(l.data[[3]]$ASBG03,values="n")
# 1                         2                         3                         9
# "ALWAYS OR ALMOST ALWAYS"               "SOMETIMES"                   "NEVER"      "OMITTED OR INVALID"

# variables literacy and numeracy: ASBHELA and ASBHENA
> get_labels(l.data[[3]]$ASBHENA,values="n")
# 999996                     999999
# "LOGICALLY NOT APPLICABLE"       "OMITTED OR INVALID"

# 2016
> get_labels(l.data[[4]]$ASDHAPS ,values="n")
# 0                    1                    2                    3                    9
# "Did Not Attend"     "1 Year or Less"            "2 Years"    "3 Years or More" "Omitted or invalid"

> get_labels(l.data[[4]]$ASBG03,values="n")
# 1
# "I always speak <language of test> at home"
# 2
# "I almost always speak <language of test> at home"
# 3
# "I sometimes speak <language of test> and sometimes speak another language at home"
# 4
# "I never speak <language of test> at home"
# 9
# "Omitted or invalid"

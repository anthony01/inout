DATA.month <- "12"
DATA.year  <- "2014"

mask.trn   <- "../data/turn"                                    # Turnstile data file mask
mask.crm   <- "../data/crm_"                                    # CRM data file mask
mask.red   <- "../data/red_"                                    # Redmine data file mask
mask.day   <- "../data/day_"                                    # Workdays file mask (1C)
mask.obj   <- "../data/obj_"                                    # Preprocessed data file mask (Intermediate result)
mask.out   <- "../data/"                                        # File out mask (final output)

FILE.hld   <- "../data/holidays.RData"                          # Holidays
FILE.sub   <- "../data/subst_emp.csv"                           # Employee name substitutions
FILE.mrk   <- "../data/emp_mark.csv"                            # Marking employees to be included in output
FILE.trn   <- paste(mask.trn, DATA.year, DATA.month, ".csv",  sep = "")
FILE.crm   <- paste(mask.crm, DATA.year, DATA.month, ".csv",  sep = "")
FILE.red   <- paste(mask.red, DATA.year, DATA.month, ".csv",  sep = "")
FILE.day   <- paste(mask.day, DATA.year, DATA.month, ".xlsx", sep = "")
FILE.obj   <- paste(mask.obj, DATA.year, DATA.month, ".bz2",  sep = "")
FILE.out   <- paste(mask.out, DATA.year, DATA.month, ".html", sep = "")

load(FILE.hld)                                                  # Read DAYS.off

lastDayInMonth <- function(first.day) {
        next.mon <- seq(as.Date(first.day), length=2, by='1 month')[2]
        last.day <- seq(next.mon, length=2, by='-1 day')[2]
        return(last.day)
}
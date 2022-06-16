# Ordered Probit Models with Random Effects: Social Equity of Bridge Management
## Panel of National Bridge Inventory (NBI), census tract, & NOAA climate data
## Author: Cari Gandy <cgandy@andrew.cmu.edu>

library(pglm)
library(texreg)

memory.limit(size=100000000000) # Maximum setting
options(scipen=999)

# Load Panel as data.frame for PGLM (data.table causes issues)
## Panel created in "Panel_Prep_NBI_Census_Climate_Gandy2022.R"
panel<-read.csv("NBI_Census_Climate_J40_Panel_min3_100yrs_small_avg_50_60.csv")


## Ensure DVs are numeric
panel$DECK_COND_058<-as.numeric(panel$DECK_COND_058)
panel$SUPERSTRUCTURE_COND_059<-as.numeric(panel$SUPERSTRUCTURE_COND_059)
panel$SUBSTRUCTURE_COND_060<-as.numeric(panel$SUBSTRUCTURE_COND_060)
panel$BRIDGE_COND<-as.numeric(panel$BRIDGE_COND)

# Ordered probit models using PGLM, see:
## Croissant, Y., & Millo, G. (2018). Panel Data Econometrics with R. Panel 
## Data Econometrics with R. Hoboken, NJ: Wiley.

### Troubleshooting: 
#### Error in eigen: infinite or missing values in 'x' - Decrease "R"
#### Inf or NaN values SE - Increase iterlim or try a simpler model

## Deck
deck_cejst <- pglm(DECK_COND_STATE ~ AGE + URBAN + 
                     INTERSTATE + ADT_029 +  PERCENT_ADT_TRUCK_109 + 
                     DETOUR_KILOS_019 + DECK_PROTECTION + WARM_REGION + 
                     HIGH_FREEZE_THAW + HIGH_PRECIP + DISADVANTAGED,
                   panel,
                   family=ordinal('probit'),
                   R=4,
                   print.level=0,
                   method='bfgs',
                   index = c('BRIDGE_KEY','INSPECT_YR'),
                   iterlim=5,
                   model="random")

deck_cejst_blk_hol_50_2020 <- pglm(DECK_COND_STATE ~ AGE + URBAN +
                                     INTERSTATE + ADT_029 +
                                     PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                     DECK_PROTECTION + WARM_REGION +
                                     HIGH_FREEZE_THAW + HIGH_PRECIP +
                                     DISADVANTAGED +
                                     BLACK_AFAM_ABOVE_50_PERCENT_2020 +
                                     HISP_LATINO_ABOVE_50_PERCENT_2020,
                                   panel,
                                   family=ordinal('probit'),
                                   R=4,
                                   print.level=0,
                                   method='bfgs',
                                   index = c('BRIDGE_KEY','INSPECT_YR'),
                                   iterlim=5,
                                   model="random")

deck_cejst_min_50_2020 <- pglm(DECK_COND_STATE ~ AGE + URBAN +
                                 INTERSTATE + ADT_029 +
                                 PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                 DECK_PROTECTION + WARM_REGION +
                                 HIGH_FREEZE_THAW + HIGH_PRECIP +
                                 DISADVANTAGED +
                                 MINORITY_ABOVE_50_PERCENT_2020,
                               panel,
                               family=ordinal('probit'),
                               R=4,
                               print.level=0,
                               method='bfgs',
                               index = c('BRIDGE_KEY','INSPECT_YR'),
                               iterlim=5,
                               model="random")

deck_income_min_50_2020 <- pglm(DECK_COND_STATE ~ AGE + URBAN +
                                  INTERSTATE + ADT_029 +
                                  PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                  DECK_PROTECTION + WARM_REGION +
                                  HIGH_FREEZE_THAW + HIGH_PRECIP +
                                  log(MED_INCOME.2020.acs) +
                                  MINORITY_ABOVE_50_PERCENT_2020,
                                panel,
                                family=ordinal('probit'),
                                R=5,
                                print.level=0,
                                method='bfgs',
                                index = c('BRIDGE_KEY','INSPECT_YR'),
                                iterlim=5,
                                model="random")


deck_income_blk_hol_50_2020 <- pglm(DECK_COND_STATE ~ AGE + URBAN +
                                      INTERSTATE + ADT_029 +
                                      PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                      DECK_PROTECTION + WARM_REGION +
                                      HIGH_FREEZE_THAW + HIGH_PRECIP +
                                      log(MED_INCOME.2020.acs) +
                                      BLACK_AFAM_ABOVE_50_PERCENT_2020 +
                                      HISP_LATINO_ABOVE_50_PERCENT_2020,
                                    panel,
                                    family=ordinal('probit'),
                                    R=5,
                                    print.level=0,
                                    method='bfgs',
                                    index = c('BRIDGE_KEY','INSPECT_YR'),
                                    iterlim=6,
                                    model="random")

deck_income_blk_hol_50_2010 <- pglm(DECK_COND_STATE ~ AGE + URBAN +
                                      INTERSTATE + ADT_029 +
                                      PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                      DECK_PROTECTION + WARM_REGION +
                                      HIGH_FREEZE_THAW + HIGH_PRECIP +
                                      log(MED_INCOME.2010.acs) +
                                      BLACK_AFAM_ABOVE_50_PERCENT_2010 +
                                      HISP_LATINO_ABOVE_50_PERCENT_2010,
                                    panel,
                                    family=ordinal('probit'),
                                    R=5,
                                    print.level=0,
                                    method='bfgs',
                                    index = c('BRIDGE_KEY','INSPECT_YR'),
                                    iterlim=6,
                                    model="random")

deck_income_blk_hol_50_2000 <- pglm(DECK_COND_STATE ~ AGE + URBAN +
                                      INTERSTATE + ADT_029 +
                                      PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                      DECK_PROTECTION + WARM_REGION +
                                      HIGH_FREEZE_THAW + HIGH_PRECIP +
                                      log(MED_INCOME.2000.dec) +
                                      BLACK_AFAM_ABOVE_50_PERCENT_2000 +
                                      HISP_LATINO_ABOVE_50_PERCENT_2000,
                                    panel,
                                    family=ordinal('probit'),
                                    R=5,
                                    print.level=0,
                                    method='bfgs',
                                    index = c('BRIDGE_KEY','INSPECT_YR'),
                                    iterlim=6,
                                    model="random")

deck_income_blk_hol_natavg_2020 <- pglm(DECK_COND_STATE ~ AGE + URBAN +
                                          INTERSTATE + ADT_029 +
                                          PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                          DECK_PROTECTION + WARM_REGION +
                                          HIGH_FREEZE_THAW + HIGH_PRECIP +
                                          log(MED_INCOME.2020.acs) +
                                          BLACK_AFAM_ABOVE_NAT_AVG_2020 +
                                          HISP_LATINO_ABOVE_NAT_AVG_2020,
                                        panel,
                                        family=ordinal('probit'),
                                        R=5,
                                        print.level=0,
                                        method='bfgs',
                                        index = c('BRIDGE_KEY','INSPECT_YR'),
                                        iterlim=6,
                                        model="random")

deck_income_blk_hol_60_2020 <- pglm(DECK_COND_STATE ~ AGE + URBAN +
                                      INTERSTATE + ADT_029 +
                                      PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                      DECK_PROTECTION + WARM_REGION +
                                      HIGH_FREEZE_THAW + HIGH_PRECIP +
                                      log(MED_INCOME.2020.acs) +
                                      BLACK_AFAM_ABOVE_60_PERCENT_2020 +
                                      HISP_LATINO_ABOVE_60_PERCENT_2020,
                                    panel,
                                    family=ordinal('probit'),
                                    R=5,
                                    print.level=0,
                                    method='bfgs',
                                    index = c('BRIDGE_KEY','INSPECT_YR'),
                                    iterlim=6,
                                    model="random")

## Superstructure
super_cejst <- pglm(SUPERSTRUCTURE_COND_STATE ~ AGE + URBAN + 
                      INTERSTATE + ADT_029 +  PERCENT_ADT_TRUCK_109 + 
                      DETOUR_KILOS_019 + STEEL_STRUCT + WATERWAY + WARM_REGION + 
                      HIGH_FREEZE_THAW + HIGH_PRECIP + DISADVANTAGED,
                    panel,
                    family=ordinal('probit'),
                    R=4,
                    print.level=0,
                    method='bfgs',
                    index = c('BRIDGE_KEY','INSPECT_YR'),
                    iterlim=5,
                    model="random")

super_cejst_blk_hol_50_2020 <- pglm(SUPERSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                      INTERSTATE + ADT_029 +
                                      PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                      STEEL_STRUCT + WATERWAY + WARM_REGION +
                                      HIGH_FREEZE_THAW + HIGH_PRECIP +
                                      DISADVANTAGED +
                                      BLACK_AFAM_ABOVE_50_PERCENT_2020 +
                                      HISP_LATINO_ABOVE_50_PERCENT_2020,
                                    panel,
                                    family=ordinal('probit'),
                                    R=4,
                                    print.level=0,
                                    method='bfgs',
                                    index = c('BRIDGE_KEY','INSPECT_YR'),
                                    iterlim=5,
                                    model="random")

super_cejst_min_50_2020 <- pglm(SUPERSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                  INTERSTATE + ADT_029 +
                                  PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                  STEEL_STRUCT + WATERWAY + WARM_REGION +
                                  HIGH_FREEZE_THAW + HIGH_PRECIP +
                                  DISADVANTAGED +
                                  MINORITY_ABOVE_50_PERCENT_2020,
                                panel,
                                family=ordinal('probit'),
                                R=4,
                                print.level=0,
                                method='bfgs',
                                index = c('BRIDGE_KEY','INSPECT_YR'),
                                iterlim=5,
                                model="random")

super_income_min_50_2020 <- pglm(SUPERSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                   INTERSTATE + ADT_029 +
                                   PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                   STEEL_STRUCT + WATERWAY + WARM_REGION +
                                   HIGH_FREEZE_THAW + HIGH_PRECIP +
                                   log(MED_INCOME.2020.acs) +
                                   MINORITY_ABOVE_50_PERCENT_2020,
                                 panel,
                                 family=ordinal('probit'),
                                 R=5,
                                 print.level=0,
                                 method='bfgs',
                                 index = c('BRIDGE_KEY','INSPECT_YR'),
                                 iterlim=5,
                                 model="random")


super_income_blk_hol_50_2020 <- pglm(SUPERSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                       INTERSTATE + ADT_029 +
                                       PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                       STEEL_STRUCT + WATERWAY + WARM_REGION +
                                       HIGH_FREEZE_THAW + HIGH_PRECIP +
                                       log(MED_INCOME.2020.acs) +
                                       BLACK_AFAM_ABOVE_50_PERCENT_2020 +
                                       HISP_LATINO_ABOVE_50_PERCENT_2020,
                                     panel,
                                     family=ordinal('probit'),
                                     R=5,
                                     print.level=0,
                                     method='bfgs',
                                     index = c('BRIDGE_KEY','INSPECT_YR'),
                                     iterlim=6,
                                     model="random")

super_income_blk_hol_50_2010 <- pglm(SUPERSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                       INTERSTATE + ADT_029 +
                                       PERCENT_ADT_TRUCK_109 + 
                                       DETOUR_KILOS_019 +
                                       STEEL_STRUCT + WATERWAY + WARM_REGION +
                                       HIGH_FREEZE_THAW + HIGH_PRECIP +
                                       log(MED_INCOME.2010.acs) +
                                       BLACK_AFAM_ABOVE_50_PERCENT_2010 +
                                       HISP_LATINO_ABOVE_50_PERCENT_2010,
                                     panel,
                                     family=ordinal('probit'),
                                     R=5,
                                     print.level=0,
                                     method='bfgs',
                                     index = c('BRIDGE_KEY','INSPECT_YR'),
                                     iterlim=6,
                                     model="random")

super_income_blk_hol_50_2000 <- pglm(SUPERSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                       INTERSTATE + ADT_029 +
                                       PERCENT_ADT_TRUCK_109 + 
                                       DETOUR_KILOS_019 +
                                       STEEL_STRUCT + WATERWAY + 
                                       WARM_REGION +
                                       HIGH_FREEZE_THAW + HIGH_PRECIP +
                                       log(MED_INCOME.2000.dec) +
                                       BLACK_AFAM_ABOVE_50_PERCENT_2000 +
                                       HISP_LATINO_ABOVE_50_PERCENT_2000,
                                     panel,
                                     family=ordinal('probit'),
                                     R=5,
                                     print.level=0,
                                     method='bfgs',
                                     index = c('BRIDGE_KEY','INSPECT_YR'),
                                     iterlim=6,
                                     model="random")

super_income_blk_hol_natavg_2020 <- pglm(SUPERSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                           INTERSTATE + ADT_029 +
                                           PERCENT_ADT_TRUCK_109 + 
                                           DETOUR_KILOS_019 +
                                           STEEL_STRUCT + WATERWAY + 
                                           WARM_REGION +
                                           HIGH_FREEZE_THAW + HIGH_PRECIP +
                                           log(MED_INCOME.2020.acs) +
                                           BLACK_AFAM_ABOVE_NAT_AVG_2020 +
                                           HISP_LATINO_ABOVE_NAT_AVG_2020,
                                         panel,
                                         family=ordinal('probit'),
                                         R=5,
                                         print.level=0,
                                         method='bfgs',
                                         index = c('BRIDGE_KEY','INSPECT_YR'),
                                         iterlim=6,
                                         model="random")

super_income_blk_hol_60_2020 <- pglm(SUPERSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                       INTERSTATE + ADT_029 +
                                       PERCENT_ADT_TRUCK_109 + 
                                       DETOUR_KILOS_019 +
                                       STEEL_STRUCT + WATERWAY + 
                                       WARM_REGION +
                                       HIGH_FREEZE_THAW + HIGH_PRECIP +
                                       log(MED_INCOME.2020.acs) +
                                       BLACK_AFAM_ABOVE_60_PERCENT_2020 +
                                       HISP_LATINO_ABOVE_60_PERCENT_2020,
                                     panel,
                                     family=ordinal('probit'),
                                     R=5,
                                     print.level=0,
                                     method='bfgs',
                                     index = c('BRIDGE_KEY','INSPECT_YR'),
                                     iterlim=6,
                                     model="random")

## Substructure
sub_cejst <- pglm(SUBSTRUCTURE_COND_STATE ~ AGE + URBAN + 
                    INTERSTATE + ADT_029 +  PERCENT_ADT_TRUCK_109 + 
                    DETOUR_KILOS_019 + STEEL_STRUCT + WATERWAY + WARM_REGION + 
                    HIGH_FREEZE_THAW + HIGH_PRECIP + DISADVANTAGED,
                  panel,
                  family=ordinal('probit'),
                  R=4,
                  print.level=0,
                  method='bfgs',
                  index = c('BRIDGE_KEY','INSPECT_YR'),
                  iterlim=5,
                  model="random")

sub_cejst_blk_hol_50_2020 <- pglm(SUBSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                    INTERSTATE + ADT_029 +
                                    PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                    STEEL_STRUCT + WATERWAY + WARM_REGION +
                                    HIGH_FREEZE_THAW + HIGH_PRECIP +
                                    DISADVANTAGED +
                                    BLACK_AFAM_ABOVE_50_PERCENT_2020 +
                                    HISP_LATINO_ABOVE_50_PERCENT_2020,
                                  panel,
                                  family=ordinal('probit'),
                                  R=4,
                                  print.level=0,
                                  method='bfgs',
                                  index = c('BRIDGE_KEY','INSPECT_YR'),
                                  iterlim=5,
                                  model="random")

sub_cejst_min_50_2020 <- pglm(SUBSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                INTERSTATE + ADT_029 +
                                PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                STEEL_STRUCT + WATERWAY + WARM_REGION +
                                HIGH_FREEZE_THAW + HIGH_PRECIP +
                                DISADVANTAGED +
                                MINORITY_ABOVE_50_PERCENT_2020,
                              panel,
                              family=ordinal('probit'),
                              R=4,
                              print.level=0,
                              method='bfgs',
                              index = c('BRIDGE_KEY','INSPECT_YR'),
                              iterlim=5,
                              model="random")

sub_income_min_50_2020 <- pglm(SUBSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                 INTERSTATE + ADT_029 +
                                 PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                 STEEL_STRUCT + WATERWAY + WARM_REGION +
                                 HIGH_FREEZE_THAW + HIGH_PRECIP +
                                 log(MED_INCOME.2020.acs) +
                                 MINORITY_ABOVE_50_PERCENT_2020,
                               panel,
                               family=ordinal('probit'),
                               R=5,
                               print.level=0,
                               method='bfgs',
                               index = c('BRIDGE_KEY','INSPECT_YR'),
                               iterlim=5,
                               model="random")


sub_income_blk_hol_50_2020 <- pglm(SUBSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                     INTERSTATE + ADT_029 +
                                     PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                     STEEL_STRUCT + WATERWAY + WARM_REGION +
                                     HIGH_FREEZE_THAW + HIGH_PRECIP +
                                     log(MED_INCOME.2020.acs) +
                                     BLACK_AFAM_ABOVE_50_PERCENT_2020 +
                                     HISP_LATINO_ABOVE_50_PERCENT_2020,
                                   panel,
                                   family=ordinal('probit'),
                                   R=5,
                                   print.level=0,
                                   method='bfgs',
                                   index = c('BRIDGE_KEY','INSPECT_YR'),
                                   iterlim=6,
                                   model="random")

sub_income_blk_hol_50_2010 <- pglm(SUBSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                     INTERSTATE + ADT_029 +
                                     PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                     STEEL_STRUCT + WATERWAY + WARM_REGION +
                                     HIGH_FREEZE_THAW + HIGH_PRECIP +
                                     log(MED_INCOME.2010.acs) +
                                     BLACK_AFAM_ABOVE_50_PERCENT_2010 +
                                     HISP_LATINO_ABOVE_50_PERCENT_2010,
                                   panel,
                                   family=ordinal('probit'),
                                   R=5,
                                   print.level=0,
                                   method='bfgs',
                                   index = c('BRIDGE_KEY','INSPECT_YR'),
                                   iterlim=6,
                                   model="random")

sub_income_blk_hol_50_2000 <- pglm(SUBSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                     INTERSTATE + ADT_029 +
                                     PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                     STEEL_STRUCT + WATERWAY + WARM_REGION +
                                     HIGH_FREEZE_THAW + HIGH_PRECIP +
                                     log(MED_INCOME.2000.dec) +
                                     BLACK_AFAM_ABOVE_50_PERCENT_2000 +
                                     HISP_LATINO_ABOVE_50_PERCENT_2000,
                                   panel,
                                   family=ordinal('probit'),
                                   R=4,
                                   print.level=0,
                                   method='bfgs',
                                   index = c('BRIDGE_KEY','INSPECT_YR'),
                                   iterlim=6,
                                   model="random")

sub_income_blk_hol_natavg_2020 <- pglm(SUBSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                         INTERSTATE + ADT_029 +
                                         PERCENT_ADT_TRUCK_109 + 
                                         DETOUR_KILOS_019 +
                                         STEEL_STRUCT + WATERWAY + 
                                         WARM_REGION +
                                         HIGH_FREEZE_THAW + HIGH_PRECIP +
                                         log(MED_INCOME.2020.acs) +
                                         BLACK_AFAM_ABOVE_NAT_AVG_2020 +
                                         HISP_LATINO_ABOVE_NAT_AVG_2020,
                                       panel,
                                       family=ordinal('probit'),
                                       R=5,
                                       print.level=0,
                                       method='bfgs',
                                       index = c('BRIDGE_KEY','INSPECT_YR'),
                                       iterlim=6,
                                       model="random")

sub_income_blk_hol_60_2020 <- pglm(SUBSTRUCTURE_COND_STATE ~ AGE + URBAN +
                                     INTERSTATE + ADT_029 +
                                     PERCENT_ADT_TRUCK_109 + DETOUR_KILOS_019 +
                                     STEEL_STRUCT + WATERWAY + WARM_REGION +
                                     HIGH_FREEZE_THAW + HIGH_PRECIP +
                                     log(MED_INCOME.2020.acs) +
                                     BLACK_AFAM_ABOVE_60_PERCENT_2020 +
                                     HISP_LATINO_ABOVE_60_PERCENT_2020,
                                   panel,
                                   family=ordinal('probit'),
                                   R=5,
                                   print.level=0,
                                   method='bfgs',
                                   index = c('BRIDGE_KEY','INSPECT_YR'),
                                   iterlim=6,
                                   model="random")

# Create Latex results tables with texreg:  Leifeld, P. (2013). Texreg: 
# Conversion of statistical model output in R to LATEX and HTML tables. Journal of Statistical 
# Software, 55(8). https://doi.org/10.18637/jss.v055.i08

options(scipen=0)

main_min_compare_3 <- texreg(list(deck_income_min_50_2020,
                                  super_income_min_50_2020,
                                  sub_income_min_50_2020),
                             single.row = TRUE, digits = 3,
                             caption = "Comparison of Ordinal Probit Random Effects Models, Component Condition States 1-4, 2020 Income, Race, and Ethnicity Indicators",
                             caption.above = TRUE,
                             custom.model.names = c("Deck",
                                                    "Superstructure",
                                                    "Substructure"),
                             custom.coef.names = c("Intercept",
                                                   "Age, years",
                                                   "Urban indicator",
                                                   "Interstate indicator",
                                                   "Average Daily Traffic (ADT)",
                                                   "% ADT trucks",
                                                   "Detour length, kilometers",
                                                   "Deck protection indicator",
                                                   "Average temperature $>$ 64 deg F",
                                                   "Annual freeze-thaw cycles $>$ 60",
                                                   "Annual precipitation $>$ 50 inches",
                                                   "Logarithm of median income",
                                                   "% Minority > 50%",
                                                   "mu_1",
                                                   "mu_2",
                                                   "sigma",
                                                   "Steel structure indicator",
                                                   "Bridge over waterway indicator"))

main_blk_hol_compare_3 <- texreg(list(deck_income_blk_hol_50_2020,
                                      super_income_blk_hol_50_2020,
                                      sub_income_blk_hol_50_2020),
                                 single.row = TRUE, digits = 3,
                                 caption = "Comparison of Ordinal Probit Random Effects Models, Component Condition States 1-4, 2020 Income, Race, and Ethnicity Indicators",
                                 caption.above = TRUE,
                                 custom.model.names = c("Deck",
                                                        "Superstructure",
                                                        "Substructure"),
                                 custom.coef.names = c("Intercept",
                                                       "Age, years",
                                                       "Urban indicator",
                                                       "Interstate indicator",
                                                       "Average Daily Traffic (ADT)",
                                                       "% ADT trucks",
                                                       "Detour length, kilometers",
                                                       "Deck protection indicator",
                                                       "Average temperature $>$ 64 deg F",
                                                       "Annual freeze-thaw cycles $>$ 60",
                                                       "Annual precipitation $>$ 50 inches",
                                                       "Logarithm of median income",
                                                       "% Black or African American > 50%",
                                                       "% Hispanic or Latino > 50%",
                                                       "mu_1",
                                                       "mu_2",
                                                       "sigma",
                                                       "Steel structure indicator",
                                                       "Bridge over waterway indicator"))

main_cejst_compare_3 <- texreg(list(deck_cejst_blk_hol_50_2020,
                                    super_cejst_blk_hol_50_2020,
                                    sub_cejst_blk_hol_50_2020),
                               single.row = TRUE, digits = 3,
                               caption = "Comparison of Ordinal Probit Random Effects Models, Component Condition States 1-4, 2020 Income, Race, and Ethnicity Indicators",
                               caption.above = TRUE,
                               custom.model.names = c("Deck",
                                                      "Superstructure",
                                                      "Substructure"),
                               custom.coef.names = c("Intercept",
                                                     "Age, years",
                                                     "Urban indicator",
                                                     "Interstate indicator",
                                                     "Average Daily Traffic (ADT)",
                                                     "% ADT trucks",
                                                     "Detour length, kilometers",
                                                     "Deck protection indicator",
                                                     "Average temperature $>$ 64 deg F",
                                                     "Annual freeze-thaw cycles $>$ 60",
                                                     "Annual precipitation $>$ 50 inches",
                                                     "Disadvantaged community",
                                                     "% Black or African American > 50%",
                                                     "% Hispanic or Latino > 50%",
                                                     "mu_1",
                                                     "mu_2",
                                                     "sigma",
                                                     "Steel structure indicator",
                                                     "Bridge over waterway indicator"))

natavg_blk_hol_compare_3 <- texreg(list(deck_income_blk_hol_natavg_2020,
                                        super_income_blk_hol_natavg_2020,
                                        sub_income_blk_hol_natavg_2020),
                                   single.row = TRUE, digits = 3,
                                   caption = "Comparison of Ordinal Probit Random Effects Models, Component Condition States 1-4, 2020 Income, Race, and Ethnicity Indicators, National Average Majority Threshold",
                                   caption.above = TRUE,
                                   custom.model.names = c("Deck",
                                                          "Superstructure",
                                                          "Substructure"),
                                   custom.coef.names = c("Intercept",
                                                         "Age, years",
                                                         "Urban indicator",
                                                         "Interstate indicator",
                                                         "Average Daily Traffic (ADT)",
                                                         "% ADT trucks",
                                                         "Detour length, kilometers",
                                                         "Deck protection indicator",
                                                         "Average temperature $>$ 64 deg F",
                                                         "Annual freeze-thaw cycles $>$ 60",
                                                         "Annual precipitation $>$ 60 inches",
                                                         "Logarithm of median income",
                                                         "% Black or African American > average",
                                                         "% Hispanic or Latino > average",
                                                         "mu_1",
                                                         "mu_2",
                                                         "sigma",
                                                         "Steel structure indicator",
                                                         "Bridge over waterway indicator"))

thresh60_blk_hol_compare_3 <- texreg(list(deck_income_blk_hol_60_2020,
                                          super_income_blk_hol_60_2020,
                                          sub_income_blk_hol_60_2020),
                                     single.row = TRUE, digits = 3,
                                     caption = "Comparison of Ordinal Probit Random Effects Models, Component Condition States 1-4, 2020 Income, Race, and Ethnicity Indicators, 60% Majority Threshold",
                                     caption.above = TRUE,
                                     custom.model.names = c("Deck",
                                                            "Superstructure",
                                                            "Substructure"),
                                     custom.coef.names = c("Intercept",
                                                           "Age, years",
                                                           "Urban indicator",
                                                           "Interstate indicator",
                                                           "Average Daily Traffic (ADT)",
                                                           "% ADT trucks",
                                                           "Detour length, kilometers",
                                                           "Deck protection indicator",
                                                           "Average temperature $>$ 64 deg F",
                                                           "Annual freeze-thaw cycles $>$ 60",
                                                           "Annual precipitation $>$ 60 inches",
                                                           "Logarithm of median income",
                                                           "% Black or African American > 60%",
                                                           "% Hispanic or Latino > 60%",
                                                           "mu_1",
                                                           "mu_2",
                                                           "sigma",
                                                           "Steel structure indicator",
                                                           "Bridge over waterway indicator"))

compare_yrs_deck_blk_hol_50_3 <- texreg(list(deck_income_blk_hol_50_2000,
                                             deck_income_blk_hol_50_2010,
                                             deck_income_blk_hol_50_2020),
                                        single.row = TRUE, digits = 3,
                                        caption = "Comparison of Census Years, Ordinal Probit Random Effects Models, Deck Condition States 1-4",
                                        caption.above = TRUE,
                                        custom.model.names = c("2000",
                                                               "2010",
                                                               "2020"),
                                        custom.coef.names = c("Intercept",
                                                              "Age, years",
                                                              "Urban indicator",
                                                              "Interstate indicator",
                                                              "Average Daily Traffic (ADT)",
                                                              "% ADT trucks",
                                                              "Detour length, kilometers",
                                                              "Deck protection indicator",
                                                              "Average temperature $>$ 64 deg F",
                                                              "Annual freeze-thaw cycles $>$ 60",
                                                              "Annual precipitation $>$ 60 inches",
                                                              "Logarithm of median income",
                                                              "Black or African American $>$ 50%",
                                                              "Hispanic or Latino $>$ 50%",
                                                              "mu_1",
                                                              "mu_2",
                                                              "sigma",
                                                              "Logarithm of median income",
                                                              "Black or African American $>$ 50%",
                                                              "Hispanic or Latino $>$ 50%",
                                                              "Logarithm of median income",
                                                              "Black or African American $>$ 50%",
                                                              "Hispanic or Latino $>$ 50%"))

compare_yrs_super_blk_hol_50_3 <- texreg(list(super_income_blk_hol_50_2000,
                                              super_income_blk_hol_50_2010,
                                              super_income_blk_hol_50_2020),
                                         single.row = TRUE, digits = 3,
                                         caption = "Comparison of Census Years, Ordinal Probit Random Effects Models, Superstructure Condition States 1-4",
                                         caption.above = TRUE,
                                         custom.model.names = c("2000",
                                                                "2010",
                                                                "2020"),
                                         custom.coef.names = c("Intercept",
                                                               "Age, years",
                                                               "Urban indicator",
                                                               "Interstate indicator",
                                                               "Average Daily Traffic (ADT)",
                                                               "% ADT trucks",
                                                               "Detour length, kilometers",
                                                               "Steel structure indicator",
                                                               "Bridge over waterway indicator",
                                                               "Average temperature $>$ 64 deg F",
                                                               "Annual freeze-thaw cycles $>$ 60",
                                                               "Annual precipitation $>$ 60 inches",
                                                               "Logarithm of median income",
                                                               "Black or African American $>$ 50%",
                                                               "Hispanic or Latino $>$ 50%",
                                                               "mu_1",
                                                               "mu_2",
                                                               "sigma",
                                                               "Logarithm of median income",
                                                               "Black or African American $>$ 50%",
                                                               "Hispanic or Latino $>$ 50%",
                                                               "Logarithm of median income",
                                                               "Black or African American $>$ 50%",
                                                               "Hispanic or Latino $>$ 50%"))

compare_yrs_sub_blk_hol_50_3 <- texreg(list(sub_income_blk_hol_50_2000,
                                            sub_income_blk_hol_50_2010,
                                            sub_income_blk_hol_50_2020),
                                       single.row = TRUE, digits = 3,
                                       caption = "Comparison of Census Years, Ordinal Probit Random Effects Models, Substructure Condition States 1-4",
                                       caption.above = TRUE,
                                       custom.model.names = c("2000",
                                                              "2010",
                                                              "2020"),
                                       custom.coef.names = c("Intercept",
                                                             "Age, years",
                                                             "Urban indicator",
                                                             "Interstate indicator",
                                                             "Average Daily Traffic (ADT)",
                                                             "% ADT trucks",
                                                             "Detour length, kilometers",
                                                             "Steel structure indicator",
                                                             "Bridge over waterway indicator",
                                                             "Average temperature $>$ 64 deg F",
                                                             "Annual freeze-thaw cycles $>$ 60",
                                                             "Annual precipitation $>$ 60 inches",
                                                             "Logarithm of median income",
                                                             "Black or African American $>$ 50%",
                                                             "Hispanic or Latino $>$ 50%",
                                                             "mu_1",
                                                             "mu_2",
                                                             "sigma",
                                                             "Logarithm of median income",
                                                             "Black or African American $>$ 50%",
                                                             "Hispanic or Latino $>$ 50%",
                                                             "Logarithm of median income",
                                                             "Black or African American $>$ 50%",
                                                             "Hispanic or Latino $>$ 50%"))


## Capture Latex tables with 3 digits and model summaries in a text file
capture.output("Main Results, 3 digits:", 
               main_min_compare_3,
               main_blk_hol_compare_3, 
               main_cejst_compare_3,
              "Main results: Deck model summaries (for scientific notation)",
               summary(deck_income_min_50_2020),
               summary(deck_income_blk_hol_50_2020),
               summary(deck_cejst_blk_hol_50_2020),
               "Main results: Superstructure model summaries (for scientific notation)",
               summary(super_income_min_50_2020),
               summary(super_income_blk_hol_50_2020),
               summary(super_cejst_blk_hol_50_2020),
               "Main results: Substructure model summaries (for scientific notation)",
               summary(sub_income_min_50_2020),
               summary(sub_income_blk_hol_50_2020),
               summary(sub_cejst_blk_hol_50_2020),
              "Natl Avg Threshold Results, 3 digits:", 
              natavg_blk_hol_compare_3, 
              "Natl Avg Threshold: Deck model summary (for scientific notation)",
              summary(deck_income_blk_hol_natavg_2020),
              "Natl Avg Threshold: Superstructure model summaries (for scientific notation)",
              summary(super_income_blk_hol_natavg_2020),
              "Natl Avg Threshold: Substructure model summaries (for scientific notation)",
              summary(sub_income_blk_hol_natavg_2020),
              "60% Threshold Results, 3 digits:", 
              thresh60_blk_hol_compare_3, 
              "60% Threshold: Deck model summary (for scientific notation)",
              summary(deck_income_blk_hol_60_2020),
              "60% Threshold: Superstructure model summary (for scientific notation)",
              summary(super_income_blk_hol_60_2020),
              "60% Threshold: Substructure model summary (for scientific notation)",
              summary(sub_income_blk_hol_60_2020),
              "Deck, Alternate Census Years, 3 digits:", 
              compare_yrs_deck_blk_hol_50_3, 
              "Superstructure, Alternate Census Years, 3 digits:", 
              compare_yrs_super_blk_hol_50_3,
              "Substructure, Alternate Census Years, 3 digits:", 
              compare_yrs_sub_blk_hol_50_3,
              "Alternate Census Years: Deck model summaries (for scientific notation)",
              summary(deck_income_blk_hol_50_2000),
              summary(deck_income_blk_hol_50_2010),
              summary(deck_income_blk_hol_50_2020),
              "Alternate Census Years: Superstructure model summaries (for scientific notation)",
              summary(super_income_blk_hol_50_2000),
              summary(super_income_blk_hol_50_2010),
              summary(super_income_blk_hol_50_2020),
              "Alternate Census Years: Substructure model summaries (for scientific notation)",
              summary(sub_income_blk_hol_50_2000),
              summary(sub_income_blk_hol_50_2010),
              summary(sub_income_blk_hol_50_2020),
               file="Results_PGLM_NBI_3dig_summaries.txt")


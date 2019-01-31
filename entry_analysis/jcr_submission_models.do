*/ Commands to run models for JCR initial submission */

* import data
use "/Users/david/OneDrive - PennO365/entry_manuscript/entry_analysis/country_year.dta"
xtset GWNoA Year, yearly

* create some variables
by GWNoA: gen xconst_lag = xconst[_n-1]
by GWNoA: gen youth_bulge_diff = youth_bulge_lag - youth_bulge_lag2
by GWNoA: gen xconst_diff = xconst_lag - xconst[_n-2]

* fixed-effects logit models
quietly xtlogit new_joiner latentmean_diff, fe
eststo m1

quietly xtlogit new_joiner latentmean_diff n_conflicts territory maxint postcw polity2 multireb, fe


* run this
quietly xtlogit new_joiner latentmean_diff territory, fe
eststo m2

quietly xtlogit new_joiner latentmean_diff maxint, fe
eststo m3

quietly xtlogit new_joiner latentmean_diff postcw, fe
eststo m4

quietly xtlogit new_joiner latentmean_diff n_conflicts_lag, fe
eststo m5

quietly xtlogit new_joiner latentmean_diff new_conflict, fe
eststo m6

quietly xtlogit new_joiner latentmean_diff lagged_rebels, fe
eststo m7

quietly xtlogit new_joiner latentmean_diff epnum, fe
eststo m8

quietly xtlogit new_joiner latentmean_diff lpop, fe
eststo m9

quietly xtlogit new_joiner latentmean_diff larea, fe
eststo m10

quietly xtlogit new_joiner latentmean_diff lgdp, fe
eststo m11

esttab m2 m3 m4 m5 m6 m7 m8 m9 m10 m11

*continued
quietly xtlogit new_joiner latentmean_diff ethfrac
eststo m12

quietly xtlogit new_joiner latentmean_diff relfrac
eststo m13

quietly xtlogit new_joiner latentmean_diff mtnest
eststo m14

quietly xtlogit new_joiner latentmean_diff cont_civil
eststo m15

quietly xtlogit new_joiner latentmean_diff oil_pc
eststo m16

esttab m12 m13 m14 m15 m16

* multivariate
quietly xtlogit new_joiner latentmean_diff postcw n_conflicts_lag, fe
eststo c1

quietly xtlogit new_joiner latentmean_diff postcw n_conflicts_lag territory maxint cont_civil, fe
eststo c2

esttab c1 c2
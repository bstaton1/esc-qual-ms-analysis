# 1-data-prep

This subdirectory houses the raw data used to calculate composition by age/sex/fate (escapement, or harvest by the subsistence or commercial fishery) and mean length-at-age-and-sex from escapement sampling.

## a-age-data

Contains three subdirectories, one for each fate. For each fate, a temporally stratified weighted average scheme is applied where the sampled composition in each two-week period is weighted by an estimate of the number of fish that experienced that fate that week. For escapement data, the weighting factor is the number of fish that passed each weir each period, for commercial data the weighting factor is the number of fish harvested each period (from trip tickets), and for subsistence data the weighting factor comes from the "subsistence harvest calendars" - which provide an indication of harvest timing at different locations in the basin.

For subsistence data, data from 6 weirs were used. After obtaining temporally-stratified weighted averages for each year and weir, the estimates were averaged again weighted by the number of fish that passed each weir each year. This provided a single composition vector for each year.

## b-length data

Performs the same calculations as those for age/sex composition, but only for escapement samples and for mean length-at-age-and-sex. Values are imputed where necessary using a linear interpolation rule.

## Data Sources

* All files in any `*/inputs` directories labeled ASL came from the [AYKDMBS](<https://www.adfg.alaska.gov/CF_R3/external/sites/aykdbms_website/Default.aspx>), maintained by the Alaska Department of Fish and Game
* All files in any `*/inputs/daily-esc` directories came from the  [AYKDMBS](<https://www.adfg.alaska.gov/CF_R3/external/sites/aykdbms_website/Default.aspx>), maintained by the Alaska Department of Fish and Game
* The files labeled `weir_counts.csv` was created using values from Appendix C3 in Liller et al. ([2018](<http://www.adfg.alaska.gov/FedAidPDFs/RIR.3A.2018.04.pdf>))
* The file labeled `weekly commercial harvest.csv` was created using values from Appendix C6 in Liller et al. ([2018](<http://www.adfg.alaska.gov/FedAidPDFs/RIR.3A.2018.04.pdf>))
* The file labeled `calendar data.csv` was supplied directly by the Alaska Department of Fish and Game
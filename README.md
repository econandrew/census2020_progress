# Model of 2020 census deadline: September 30 versus October 31

This repository contains code and data supporting my _New York Times_ opinion piece with Gus Wezerek (September 23, 2020). The model is based on Jonathan Auerbach and Steve Pierson's American Statistical Association [Technical Report](https://www.amstat.org/asa/files/pdfs/POL-2020%20Census%20Deadline%20Extension.pdf) (September 17, 2020, hereafter "the ASA model/paper/authors"). Although I outline the general model here, it would be helpful to read this with a copy of that paper to hand.

The master branch of this repository will be frozen at the point of publication. Future updates will be on another branch (linked here if and when it exists).

For questions, contact me@andrewwwhitby.id.au.

## Data for the figures in the article

1. "Share of uncounted addresses" [[PNG - note shows _counted_ addresses here]](outputs/plt_progress_proj_final.png)[[CSV]](outputs/plt_progress_proj_final.csv)
2. "13 states will lose more than $5 million a year in Medicaid funding" [[PNG]](outputs/plt_medicaid_impact.png)[[CSV]](outputs/plt_medicaid_impact.csv)

The third figure is external data unrelated to this model.

## Non-technical summary

This is a mathematical model of the effects of ending counting for the 2020 census on September 30, compared with an alternative scenario in which counting continues until October 31. It is based on data about the Census Bureau's enumeration progress, but as with any model it makes various assumptions. These assumptions are supported by data, but are nonetheless still assumptions. The most important of these are:

1. That enumeration progress will continue in all states, but at a slowing rate.
2. That populations for any housing units (roughly, addresses) left unenumerated at the end date are estimated by a process that results in higher undercount than would otherwise be the case.

From these assumptions, we can determine what the undercount percentage is for each state at either a September 30 or October 31 end date. Applying those percentages to our best estimate of the true population on April 1, 2020, we can derive a census count for each state, under either scenario. From this we can calculate both apportionment of the House of Representatives and federal medicaid funding, since both are formulaic and depend on the census count.

All of this is the same general approach taken in the ASA paper. The key differences between their model and ours are:

1. We use a different model of enumeration progress which projects faster progress than the ASA model
2. We use a different model of how incomplete enumeration translates into errors/undercounts. Roughly, our approach is top down whereas theirs is bottom-up.
3. We use (a) six days of additional data on enumeration progress released after they finalized their model; and (b) FY2018 Medicaid expenditure data rather than the FY2015 data they use.

## Technical summary

This section explains and gives brief justifications for some of the modelling assumptions. It is not intended to cover everything in the code, which is comprehensively commented:

[nrfu.R](nrfu.R)

### Enumeration progress model

The ASA authors describe two models of enumeration progress, linear and logistic (curved), in which the enumeration percentage is predicted by the date. They reject the linear model on the basis that it is unrealistic to expect linear progress, and that a decreasing rate of progress is already evident in the data for many more advanced states. This is reinforced by [testimony](reports/Mihm Testimony.pdf) given by the GAO's Christopher Mihm to the House Oversight Committee: "Officials said they expect productivity to slow towards the end of the operation when the most difficult cases remain."

The ASA model does, however, tend to underestimate progress (with the benefit of almost a week's additional data). Moreover by modelling the enumeration percentage, it is possible for the prediction model to underpredict already-observed data—a situation that is generally impossible.

So, instead, we return to the principle behind the ASA authors' model—that "the growth rate is... proportional to the percent of uncounted households"—and model this directly. We regress _daily incremental progress_, the first difference of the the enumeration percentage, on the lagged unresolved housing units (100 - enumerated percentage).

For the form of that relationship, we adopt a curve which is itself logistic, the reasoning being that it must pass through the origin (with no unresolved housing units there can be no incremental progress) and that, given a fixed level of effort, there should be an upper bound (asymptote) to daily incremental progress (ignoring self-response, which is negligible at this point). To fit this function state-by-state, a small number of outliers must be discarded:

![Daily increment versus lagged unresolved housing units](/outputs/plt_increments_v_unresolved.png)

These functions are then used to iteratively forecast the enumeration progress through October 31. The result of this is shown below (red solid = observation; red dashed = forecast), in comparison with the ASA simple logistic fit (thin orange line; updated to the same observations we use).

![Enumeration progress over time, our model vs ASA model](/outputs/plt_progress_proj_final_vs_ASA.png)

### Unresolved housing units to undercount model

The ASA authors derive their bottom-line results on count completion by assuming that different error rates apply to housing units (a) resolved by self-response; (b) resolved by interview in NRFU; and (c) unresolved at the stopping date. This bottom-up approach has merit but is quite complex as raises some conceptual issues (e.g. the authors have to model omissions across the three categories).

We adopt the same general assumption here, namely that accuracy of enumeration declines from self-response to NRFU interview, and from NRFU interview to dealing with unresolved cases (through lesser-quality administrative records matches or imputation). In practice we model only the latter difference, as self-response is increasing only very slowly at this point.

However than the bottom-up approach, take a top-down approach to applying this assumption. First, we assume that an October 31 end-date, having been selected originally by the Census Bureau in response to COVID-19, results in an enumeration unconstrained by time: a "good" enumeration. Then, we assume a state-by-state undercount for such an enumeration based on the "low risk" scenario from the Urban Institute [model of 2020 census undercount](https://www.urban.org/sites/default/files/publication/100324/assessing_miscounts_in_the_2020_census_1.pdf) (Table 2). They authors of that study describe the low risk scenario as one that accounts for "the effect of demographic change over the last decade, holding census performance constant between 2010 and 2020." Choosing this scenario may appear optimistic, even for an October 31 end date, but we do so on the basis that self-response has—against expectations—matched 2010.

We then adjust this state-by-state undercount backward from October 31 by establishing a relationship between percentage of housing units imputed in 2010 (which we assume to be those unresolved) and net undercount in 2010. We do this based on data and at the state level. While a lower level of geography (e.g. county) might seem to offer more information, it is not easy to relate county-level undercount to state-level undercount, due to different counting rules (in particular, it is possible to be enumerated in the wrong county but the right state).

This relationship is shown below. Since it drives the main results, this is a critical part of the model. It should be considered an assumption, albeit one informed by data. The relationship suggest that a 1 percentage point increase in imputation will result in around 0.33 percentage point increase in undercount:

![Undercount % vs imputation %, by state](/outputs/plt_undercount_model.png)

From this we obtain a projected undercount percentage for each state, for each date from September 30 to October 31, if counting were to stop at that date:

![Undercount % by date, boxplot of states](/outputs/plt_undercount_by_month.png)

We apply the undercount projection for September 30 and October 31 to a projection of true population for April 1, 2020, to obtain a final census count for each potential end date. We use the same projection of population as the ASA model—from the same Urban Institute paper cited above (also Table 2).

### Apportionment impact

Based on the projected census counts under each of the two scenarios, we can easily calculate apportionment under the equal-proportions method. We use the code kindly supplied by the ASA authors (checked by comparison with the [apportR](https://github.com/jalapic/apportR) package).

### Medicaid impact

For Medicaid impact, we again use the code supplied by the ASA authors, which derives personal income per capita and applies the FMAP formula. The only change we make here is to update the Medicaid expenditure data, as the author use FY15 data. We use the very latest data from the Medicaid [Financial Management Report FY2018](!https://www.medicaid.gov/medicaid/financial-management/state-expenditure-reporting-for-medicaid-chip/expenditure-reports-mbescbes/index.html
). We include the net program expenditure but exclude administrative costs.

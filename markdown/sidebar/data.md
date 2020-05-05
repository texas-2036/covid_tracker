# About The Data

<hr>

# Reopening Analysis

## Data Sources

- Symptom data (CLI & ILI) state & county level comes from [Texas DSHS](https://www.dshs.state.tx.us/coronavirus/additionaldata/)
- Hospital Data (state and trauma-service area level) comes from [Texas DSHS](https://www.dshs.state.tx.us/coronavirus/additionaldata/)
- Daily case data (state and county level) comes from the [NYTimes](https://github.com/nytimes/covid-19-data).
- Testing data (state only) from the [COVID Tracking Project](https://covidtracking.com/).


## Statistical Model

Due to potential [sources of error](#error) in the data, we chose to use a robust regression[^1] to model the 14-day linear trend. Robust linear models are well suited to this problem since they can account for outliers in the data. For all of the 14-day trend lines shown in the report, we used the `statsmodels`^[2] implementation of robust linear models with the Huber M-estimator^[3]. The [Huber loss](https://en.wikipedia.org/wiki/Huber_loss) results in a squared error for inliers and an absolute error for outliers (up to a constant factor and as specified by the threshold and scale parameter). We chose this function since it does not completely ignore the effect of outliers (like [Tukey’s biweight](https://mathworld.wolfram.com/TukeysBiweight.html)) but instead just downweights their influence.

[^1]: [Robust Statistics](https://conference.scipy.org/proceedings/scipy2010/pdfs/seabold.pdf), Peter J. Huber. John Wiley and Sons, Inc. 1981.
^[2]: [Statsmodels](https://conference.scipy.org/proceedings/scipy2010/pdfs/seabold.pdf): Econometric and Statistical Modeling with Python, Skipper Seabold and Josef Perktold. Proceedings of the 9th Python in Science Conference. 2010.
^[3]: [Robust Estimation of a Location Parameter](https://projecteuclid.org/euclid.aoms/1177703732), Peter J. Huber. Annals of Mathematical Statistics. 1964.

```python
import statsmodels.api as sm
import numpy as np

def fit(X, y):
    features = sm.add_constant(np.arange(len(X)))
    rlm_model = sm.RLM(y, features, M=sm.robust.norms.HuberT())
    model = rlm_model.fit()
```

We only create models for counties which have had at least 28 reported cases. For counties with
low case counts, the fit of a linear trend may not be meaningful.

## Sources of Error

#### Sampling error

Due to a [shortage of tests](https://www.vox.com/2020/5/1/21242589/coronavirus-testing-swab-reagent-supply-shortage) and [backlog of results](https://www.theatlantic.com/health/archive/2020/03/next-covid-19-testing-crisis/609193/), the **total number of COVID-19 cases** may be severely underestimated. However, since at-risk or suggestively symptomatic individuals are [prioritized first](https://www.npr.org/2020/04/03/826044608/many-who-need-testing-for-covid-19-fail-to-get-access), the **percentage of positive cases** may also be an overestimate. Additionally, the biased sampling of who is tested may skew the demographics those diagnosed.

#### Measurement error

Due to inherent uncertainty in RT-PCR [^4] as well as serological antibody [^5] tests, even multiple repeated tests for a positive patient can give an erroneous diagnosis [^6]. For COVID-19, the most dangerous of these errors are the false negative tests [^7], since these individuals may not receive required medical treatment but also continue to spread the virus.

#### Asymptomatic and presymptomatic cases

In addition to the methodological sources of error above, reliable COVID-19 diagnoses are complicated by the temporal dynamics of the disease.[^8] Not only are there exogenous errors that can arise during sample collection (nose and throat swabs), especially if a location is understaffed for the amount of samples they need to collect, but specificity of detection can also fluctuate with the time since infection and the severerity of a case.

One of the earliest natural experiments for studying COVID-19 that occurred during the Diamond Princess cruise ship quarantine in Japan[^9][^10] has shown that many asymptomatic individuals at the time of testing can in fact be positive and contagious. These silent spreaders thus can have an outsized effect on the spread of the virus and often evade testing.

[^4]: [Stability Issues of RT‐PCR Testing of SARS‐CoV‐2 for Hospitalized Patients Clinically Diagnosed with COVID‐19](https://doi.org/10.1002/jmv.25786), Li, Yafang, Lin Yao, Jiawei Li, Lei Chen, Yiyan Song, Zhifang Cai, and Chunhua Yang. Journal of Medical Virology. March 26, 2020.
[^5]: [Test performance evaluation of SARS-CoV-2 serological assays](https://doi.org/10.1101/2020.04.25.20074856), Whitman, Jeffrey D., Joseph Hiatt, Cody T. Mowery, Brian R. Shy, Ruby Yu, Tori N. Yamamoto, Ujjwal Rathore et al. medRxiv. April 29, 2020.
[^6]: [False‐negative of RT‐PCR and prolonged nucleic acid conversion in COVID‐19: Rather than recurrence](https://doi.org/10.1002/jmv.25855), Xiao, Ai Tang, Yi Xin Tong, and Sheng Zhang. Journal of Medical Virology. April 9, 2020.
[^7]: [A case report of COVID-19 with false negative RT-PCR test: necessity of chest CT](https://dx.doi.org/10.1007%2Fs11604-020-00967-9), Feng, Hao, Yujian Liu, Minli Lv, and Jianquan Zhong. Japanese Journal of Radiology. April 7, 2020.
[^8]: [Temporal dynamics in viral shedding and transmissibility of COVID-19](https://doi.org/10.1038/s41591-020-0869-5), He, Xi, Eric HY Lau, Peng Wu, Xilong Deng, Jian Wang, Xinxin Hao, Yiu Chung Lau et al. Nature medicine. April 15, 2020.
[^9]: [Chronology of COVID-19 Cases on the Diamond Princess Cruise Ship and Ethical Considerations: A Report From Japan](https://doi.org/10.1017/dmp.2020.50), Nakazawa, Eisuke, Hiroyasu Ino, and Akira Akabayashi. Disaster Medicine and Public Health Preparedness. March 24, 2020.
[^10]: [Public health responses to COVID-19 outbreaks on cruise ships—worldwide, February–March 2020](http://dx.doi.org/10.15585/mmwr.mm6912e3), Moriarty LF, Plucinski MM, Marston BJ, et al. MMWR Morbidity and mortality weekly report. March 27, 2020.


## America's Health Rankings

[America's Health Rankings](https://www.americashealthrankings.org): The mission of America's Health Rankings (AHR) is to provide a wide variety of health and health-related information to help policymakers, advocates and individuals understand a population’s health in a holistic, inclusive manner. 

  * **Where you can see this data:** It exists [here.](https://features.texas2036.org/caseforaction/intro/13)
  * **How we used their data:** We used the latest version of each of the following three datasets in our narrative to compare Texas health outcomes to health outcomes in other states to understand where Texas fell in the bottom half of state rankings across the various measures that America's Health Rankings Reports on throughh their Annual Report, Health of Women and Children, and their Senior Report.
  
### The AHR 2018 Annual Report
  
  - [**The AHR 2018 Annual Report**](https://www.americashealthrankings.org/explore/annual/measure/Overall/state/TX) is the longest running annual assessment of the nation’s health on a state-by-state basis. For nearly three decades, America’s Health Rankings® Annual Report has analyzed a comprehensive set of behaviors, public and health policies, community and environmental conditions, and clinical care data to provide a holistic view of the health of the people in the nation. 
  * The following measures came from AHR's Annual Report: _["Adverse Childhood Experiences"](https://www.americashealthrankings.org/explore/annual/measure/ACEs/state/TX), "[Cardiovascular Deaths"](https://www.americashealthrankings.org/explore/annual/measure/CVDDeaths/state/TX), ["Diabetes"](https://www.americashealthrankings.org/explore/annual/measure/Diabetes/state/TX), ["Infectious Disease"](https://www.americashealthrankings.org/explore/annual/measure/infectiousdisease/state/TX), ["Low Birthweight"](https://www.americashealthrankings.org/explore/annual/measure/birthweight/state/TX), ["Mental Health Providers"](https://www.americashealthrankings.org/explore/annual/measure/MHP/state/TX), ["Obesity"](https://www.americashealthrankings.org/explore/annual/measure/Obesity/state/TX), ["Preventable Hospitalizations"](https://www.americashealthrankings.org/explore/annual/measure/preventable/state/TX), ["Primary Care Physicians"](https://www.americashealthrankings.org/explore/annual/measure/PCP/state/TX), ["Stroke"](https://www.americashealthrankings.org/explore/annual/measure/Stroke/state/TX) and ["Uninsured"](https://www.americashealthrankings.org/explore/annual/measure/HealthInsurance/state/TX)_.
  - [**The AHR 2018 Health of Women and Children Report**](https://www.americashealthrankings.org/explore/health-of-women-and-children/measure/overall_mch/state/TX?edition-year=2018) is the newest state rankings report. With a focus on women of reproductive age, infants and children under age 18, this report emphasizes the population groups where change can make generational differences.
      * The following measures came from AHR's Health of Women and Children Report: _["Immunizations - Children"](https://www.americashealthrankings.org/explore/health-of-women-and-children/measure/Immunize/state/TX?edition-year=2018), ["Maternal Mortality"](https://www.americashealthrankings.org/explore/health-of-women-and-children/measure/maternal_mortality/state/TX?edition-year=2018), and ["Supportive Neighborhoods"](https://www.americashealthrankings.org/explore/health-of-women-and-children/measure/supportiveneighborhood/state/TX?edition-year=2018)_.  
      * IMPORTANT NOTE: These datasets were pulled in during the first week of September. In the weeks since September 5th, the AHR 2019 Health of Women and Children Report was released. As we did not have sufficient time to include this recently released data, the 2019 AHR Health of Women and Children report will be integrated into these rankings as soon as possible: 
    - [**The AHR 2019 Senior Report**](https://www.americashealthrankings.org/explore/senior/measure/overall_sr_2/state/TX) recently completed its fifth year of presenting a comparative health report for the U.S. population aged 65 and older. 
      * The following measures came from AHR's Senior Report: _"[Frequent Mental Distress - Seniors"](https://www.americashealthrankings.org/explore/senior/measure/mental_disress_sr/state/TX)_.
      
### Income To Poverty Ratios - 200%

Dillon Ipsum Julie Taylor, will you marry me? Clear Eyes. Full Hearts. Saracen, quarterback's a captain. CHAMPIONS DON'T COMPLAIN! Why don't you take your Members Only jacket off and hang it on the coat rack? Blood, sweat and tears - it all stays right here on this field, right now! This is our dirt! This is our mud! This is ours, baby!

Using the census population variables for people whose poverty status was determined ("C17002_001E"), we can see understand that total population in mathematical form like this like this: 

$$ Tot_{PopulationSurveyed} = C17002\_001E$$

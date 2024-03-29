---
bibliography: references.bib
---

# About

I designed this app for two reasons: first, to clarify the differences between vaccine efficacy rates and protection probabilities. Second, I want to provide a human-friendly interface to the clinical data.

In that spirit, please consider this app as my attempt to communicate the science contained in the COVID-19 vaccine research. I'm an analyst, not a doctor, so contact your physician for medical advice or if you have specific questions about vaccines.

I'm a data scientist and educator, somewhat fresh out of graduate school, and I've worked for non-profits large and small. I'm not affiliated with any of the vaccine research personally or professionally. This project is entirely my own independent initiative.

A bit about key terms and methodology is below in [Notes on the Data] and [Can I look at your code?]. While I'm confident in the product, there are surely places to improve -- please let me know.

## Data Sources

All of the data come directly from each of the relevant COVID-19 Vaccine Clinical Studies published via the National Institute of Health and the New England journal of medicine. As of the time of app production, these studies were published as open-access articles. Individual publication citation details are below. Data on daily infections come from the CDC, John's Hopkins University, and other sources, which I access via the `covid19` r package. Full citations below.

## License and Citing

I make a distinction between my creative work and the clinical trial data that I present in this project.

The information presented and cited from clinical papers was accessed via the "PMC Open Access Subset" at the time of publishing, so if you plan to use or cite anything related to the papers I recommend going directly to the source.

If you want to use or refer to anything I've done, i.e. code or methodology, basically you can, as long as you keep it open source. Refer to the [the repository](https://github.com/buscandoaverroes/vaccine-efficacies) for more.

## Can I look at your code?

Yes, please do. I designed the app to be entirely open-source so that every step and calculation is transparent. I encourage those who are learning, curious, or even slightly skeptical to consult [the repository](https://github.com/buscandoaverroes/vaccine-efficacies).

# Notes on the Data

## Calculations

**Person-years** `population infection rate` and `treatment infection rate` are both recorded in "1000 person-years." This is admittedly not a natural way to think about rates, but it resolves a standardization problem described below in [Time]. Think about it this way: 1000 person-years is the equivalent to the total collective covid "risk" of 1000 people going about their daily lives for a year. If your general population has an infection rate of, say, `40` in `thousand person-years` , then on average 40 people for every 1,000 got infected with covid over a single year.

**Population Infection Rate:** The rate at which unvaccinated people test positive for COVID-19, on average, in `1000 person-years`. Person-year data are reported in the clinical papers The population infection rate is conceptually defined as: $\frac{\text{No. Covid Positives in Placebo Group}}{\text{Surveillance Period}}$ @polack2020. This figure excludes those who tested positive for Covid before receiving placebo or treatment and both mRNA vaccines studies used serologic or PCR-based tests to measure for COVID-19 [@baden2021; @polack2020]. The CDC paper reports their figures in person-days, so I scale by 365 to get the equivalent figure in person years @thompson2021.

**Treatment Infection Rate:** The rate at which people with the vaccine people test positive for COVID-19, on average, in `1000 person-years`. Person-year data are reported in the clinical papers The population infection rate is conceptually defined as: $\frac{\text{No. Covid Positives in Vaccine Group}}{\text{No. Surveillance Period}}$ . This figure excludes those who tested positive for Covid before receiving placebo or treatment and both mRNA vaccines studies used serologic or PCR-based tests to measure for COVID-19 [@baden2021; @polack2020].

**Efficacy Rate:** A multiplication factor that reduces your chances of getting the virus, also known as the hazard-ratio. The efficacy calculation I use rate is $1 - \frac{\text{Treatment Infection Rate}}{\text{Population Infection Rate}}$, following that of @polack2020. My code includes automated checks to ensure that the numbers you see are within `0.001` of the stated efficacy rate for all papers.

**Chance of Protection:** is the probability that an average participant in the vaccine group did not test positive for COVID-19. If `chance of protection` is created through hypothetical data, it is the simply $(\text{population infection rate})*(\text{efficacy rate})$.

The two calculation methods are equivalent -- but in hypothetical data, since we obviously can't measure a vaccine trial that didn't happen, we propose a hypothetical `efficacy rate` for demonstration purposes.

## Assumptions

### Cross-Study Comparability

The clinical trials occurred over different times and places during the pandemic. And while this raises valid questions on comparability, the reality is that everyone in the world is comparing the final numbers. I recognize the problems in comparing the data from studies that are not identical in design, and I've done the following to steer users toward more constructive interaction with the data:

-   There are no "full" side-by-side comparisons between studies. I want to convey the sense that each study flies-in to view screen under slightly different circumstances -- I think there can be a false sense of equivalency when you see side-by-side panel after panel.

-   The user sees the entire data flow -- that is, from population infection rate, to vaccine infection rate, to protection rate. All three key numbers change as the user moves from study to study to reflect the reality of the contingent circumstances. The final graph intentionally highlights the different `x` and `y` coordinates to this end.

-   There is a clear demarcation in user experience between exploring the clinical data and exploring hypothetical data. I do not want the user to confuse aspects of one for the other.

# Contact

Here's my GitHub <https://github.com/buscandoaverroes>. You can also reach me at my icloud.com email address with the same name as my GitHub username. (Read more about Averroes here: <https://en.wikipedia.org/wiki/Averroes>)

# Sources

Baden, Lindsey R et al. "Efficacy and Safety of the mRNA-1273 SARS-CoV-2 Vaccine." The New England journal of medicine vol. 384,5 (2021): 403-416. <doi:10.1056/NEJMoa2035389>

Centers for Disease Control and Prevention. "Covid-19 Vaccines Work." Updated on 7 April, 2021. URL: <https://www.cdc.gov/coronavirus/2019-ncov/vaccines/effectiveness/work.html>

Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open Source Software 5(51):2376, doi: 10.21105/joss.02376.\@guidotti2020

Johns Hopkins Center for Systems Science and Engineering (2020), <https://github.com/CSSEGISandData/COVID-19>

The New York Times (2020), <https://github.com/nytimes/covid-19-data>

Polack, Fernando P et al. "Safety and Efficacy of the BNT162b2 mRNA Covid-19 Vaccine." The New England journal of medicine vol. 383,27 (2020): 2603-2615. <doi:10.1056/NEJMoa2034577>

Sadoff, Jerald et al. "Interim Results of a Phase 1-2a Trial of Ad26.COV2.S Covid-19 Vaccine." The New England journal of medicine, NEJMoa2034201. 13 Jan. 2021, <doi:10.1056/NEJMoa2034201>

Hale Thomas, Sam Webster, Anna Petherick, Toby Phillips, and Beatriz Kira (2020). Oxford COVID-19 Government Response Tracker, Blavatnik School of Government.

Thompson MG, Burgess JL, Naleway AL, et al. Interim Estimates of Vaccine Effectiveness of BNT162b2 and mRNA-1273 COVID-19 Vaccines in Preventing SARS-CoV-2 Infection Among Health Care Personnel, First Responders, and Other Essential and Frontline Workers --- Eight U.S. Locations, December 2020--March 2021. MMWR Morb Mortal Wkly Rep 2021;70:495--500. DOI: <http://dx.doi.org/10.15585/mmwr.mm7013e3>

World Bank Open Data (2018), <https://data.worldbank.org/indicator/SP.POP.TOTL>

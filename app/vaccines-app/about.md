---
bibliography: references.bib
---

# About

I designed this app for two reasons: first, to clarify the differences between vaccine efficacy rates and protection probabilities. Second, I want to provide a human-friendly interface to the clinical data.

In that spirit, please consider this app as my attempt to communicate the science contained in the COVID-19 vaccine research. I'm an analyst, not a doctor, so contact your physician for medical advice or if you have specific questions about vaccines.

I'm a data scientist and educator, somewhat fresh out of graduate school, and I've worked for non-profits large and small. I'm not affiliated with any of the vaccine research personally or professionally. This project is entirely my own independent initiative.

A bit about key terms and methodology is below in [Notes on the Data] and [Can I look at your code?]. While I'm confident in the product, there are surely places to improve -- please let me know!

## Inspiration

Our professor's strategy proved quite fortuitous. It was all a game of mental Jiu-Jitsu. Read 20 pages of theoretical math, code it, stay up all night, and then, we show up to class:

"Tell me the units".

"HUuUuUh?" None of us ever remembered the single most important word in the whole paper. His point was simple: we, as future storytellers of data and empirics, will invariably tell our story to people -- and no matter how conceptual or math-y our methodology gets, we need a firm grasp on what our final results mean in practicality. Always know what the units mean.

Fast forward to December 2020, and numbers start popping up on the news: 95, 94, then later, 66. People start noticing differences, and of course we want the best quality medicine available, and of course discussions ensue of vaccine equity, as they should.

But I noticed that one very essential line was missing: the unit. The reported figures are not the "final figures" or the chances of protection as is reasonable to assume -- these figures are the "hazard ratios" and I figured that getting it right was

## Data Sources

All of the data come directly from each of the relevant COVID-19 Vaccine Clinical Studies published via the National Institute of Health and the New England journal of medicine. As of the time of app production, these studies were published as open-access articles. Individual publication citation details are below.

## Can I look at your code?

Yes -- please do. I designed the app to be entirely open-source so that every step and calculation is transparent. I encourage those who are learning, curious, or even slightly skeptical to consult [the repository](https://github.com/buscandoaverroes/vaccine-efficacies).

# Notes on the Data

## Calculations

**Population Infection Rate:** The rate at which unvaccinated people test positive for COVID-19, on average. This is determined from data in each clinical trial's placebo group: $\frac{\text{No. Covid Positives in Placebo Group}}{\text{No. Placebo Participants}}$. Both mRNA vaccines used PCR-based tests to measure for COVID-19, and this figure excludes those who tested positive for Covid before receiving placebo or treatment [@baden2021; @polack2020].

**Treatment Infection Rate:** The rate at which people with the vaccine people test positive for COVID-19, on average. This is determined from data in each clinical trial's placebo group: $\frac{\text{No. Covid Positives in Vaccine Group}}{\text{No. Vaccine Participants}}$ . All studies used serologic or PCR-based tests to measure for COVID-19, and this figure excludes those who tested positive for Covid before receiving placebo or treatment

**Efficacy Rate:** A multiplication factor that reduces your chances of getting the virus, also known as the hazard-ratio. (%% citation.) The efficacy rate is $1 - \frac{\text{Treatment Infection Rate}}{\text{Population Infection Rate}}$.

**Chance of Protection:** is the probability that an average participant in the vaccine group did not test positive for COVID-19. If `chance of protection` is created through hypothetical data, it is the simply $(\text{population infection rate})*(\text{efficacy rate})$.

The two calculation methods are equivalent -- but in hypothetical data, since we obviously can't measure a vaccine trial that didn't happen, we propose a hypothetical `efficacy rate` for demonstration purposes.

## Assumptions

### Time

Each clinical trial conducted vaccines studies over fixed periods of time -- so we should interpret the data under these time constraints for optimal validity. For example, the Pfizer trial timeline was about []. It would be correct to say something like:

> The Pfizer data show that, during the latter part of 2020, the average infection rate was about [] over the period of [] in the places where participants were.

The time aspect is paramount because one's chances of infection do not stay the same over time.

-   There could be an outbreak in your area, thereby increasing your chances.

-   New strains could appear that evade standard protections (i.e. single masks).

-   In theory, if one remained unvaccinated for long enough (say like 1,000 years), your chances of contracting the virus would likely increase simply because the sheer number of contacts with other people brings about many more chances at spread.

To help account for this, some of the papers ([][]) have introduced a standardized way to account for rates over the short intervals of time, called 1000-person-years. But not all papers have done this and I find this somewhat difficult to interpret anyway.

In lieu trying to standardize everyone's data, I decided to simply report the raw numbers and present the time issue as a caveat for readers to be aware of.

### Cross-Study Comparability

The clinical trials occurred over different times and places during the pandemic. And while this raises valid questions on comparability, the reality is that everyone in the world is comparing the final numbers. I recognize the problems in comparing the data from studies that are not identical in design, and I've done the following to steer users toward more constructive interaction with the data:

-   There are no "full" side-by-side comparisons between studies. I want to convey the sense that each study flies-in to view screen under slightly different circumstances -- I think there can be a false sense of equivalency when you see side-by-side panel after panel.

-   The user sees the entire data flow -- that is, from population infection rate, to vaccine infection rate, to protection rate. All three key numbers change as the user moves from study to study to reflect the reality of the contingent circumstances. The final graph intentionally highlights the different `x` and `y` coordinates to this end.

-   There is a clear demarcation in user experience between exploring the clinical data and exploring hypothetical data. I do not want the user to confuse aspects of one for the other.

# Sources

Baden, Lindsey R et al. "Efficacy and Safety of the mRNA-1273 SARS-CoV-2 Vaccine." The New England journal of medicine vol. 384,5 (2021): 403-416. <doi:10.1056/NEJMoa2035389>

Polack, Fernando P et al. "Safety and Efficacy of the BNT162b2 mRNA Covid-19 Vaccine." The New England journal of medicine vol. 383,27 (2020): 2603-2615. <doi:10.1056/NEJMoa2034577>

Sadoff, Jerald et al. "Interim Results of a Phase 1-2a Trial of Ad26.COV2.S Covid-19 Vaccine." The New England journal of medicine, NEJMoa2034201. 13 Jan. 2021, <doi:10.1056/NEJMoa2034201>

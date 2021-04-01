# About

I designed this app for two reasons: first, to help users clear up any confusion between vaccine efficacy rates and protection probabilities. Second, I wanted to provide a human-friendly interface to the clinical data.

In that spirit, please consider this app as my attempt to communicate the science contained in the COVID-19 vaccine research. I'm an analyst, not a doctor, so contact your physician for medical advice or if you have specific questions about vaccines.

A bit about key terms and methodology is below in [Notes on the Data] and [Can I look at your code?]. While I'm confident in the product, there will surely be errors and places to improve -- please let me know!

I'm a data scientist and educator, somewhat fresh out of graduate school, and I've worked for non-profits large and small.

## Inspiration

Our professor's strategy proved quite fortuitous. It was all a game of mental Jiu-Jitsu. Read 20 pages of theoretical math, code it, stay up all night, and then, we show up to class:

"Tell me the units".

"HUuUuUh?" None of us ever remembered the single most important word in the whole paper. His point was simple: we, as future storytellers of data and empirics, will invariably tell our story to people -- and no matter how conceptual or math-y our methodology gets, we need a firm grasp on what our final results mean in practicality. Always know what the units mean.

Fast forward to December 2020, and numbers start popping up on the news: 95, 94, then later, 66. People start noticing differences, and of course we want the best quality medicine available, and of course discussions ensue of vaccine equity, as they should.

But I noticed that one very essential line was missing: the unit. The reported figures are not the "final figures" or the chances of protection as is reasonable to assume -- these figures are the "hazard ratios" and I figured that getting it right was

## Data Sources

All of the data come directly from each of the relevant COVID-19 Vaccine Clinical Studies published via the National Institute of Health and the New England journal of medicine. As of the time of app production, these studies were published as open-access. Individual publication citation details are below.

## Can I look at your code?

Yes -- please do. I designed the app to be entirely open-source so that every step and calculation is transparent. I encourage those who are learning, curious, or even slightly skeptical to consult the code.

# Notes on the Data

## Calculations

### Population Infection Rate

### Efficacy Rate

### Chance of Protection

## Assumptions

### Time

### Cross-Study Comparability

There are numerous ...

# Sources

Baden, Lindsey R et al. "Efficacy and Safety of the mRNA-1273 SARS-CoV-2 Vaccine." The New England journal of medicine vol. 384,5 (2021): 403-416. doi:10.1056/NEJMoa2035389

Polack, Fernando P et al. "Safety and Efficacy of the BNT162b2 mRNA Covid-19 Vaccine." The New England journal of medicine vol. 383,27 (2020): 2603-2615. doi:10.1056/NEJMoa2034577

Sadoff, Jerald et al. "Interim Results of a Phase 1-2a Trial of Ad26.COV2.S Covid-19 Vaccine." The New England journal of medicine, NEJMoa2034201. 13 Jan. 2021, doi:10.1056/NEJMoa2034201

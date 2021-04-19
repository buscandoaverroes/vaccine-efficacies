---
bibliography: references.bib
---

## Vaccine Questions

### Why did the CDC show a higher infection rate among non-vaccinated people?

To be entirely transparent, I'll provide my opinion as a data scientist, not a doctor. My guess is that the study found a higher rate of covid infection among **non-vaccinated** people primarily because their study only included frontline workers, which are normally at higher risk than most people. (Specifically, "frontline workers" includes doctors, nurse practitioners, first responders, teachers and hospitality workers, among others @thompson2021.) Also, the CDC conducted its study very recently, from December 2020 to March 2021, and variants in the US were likely more prevalent during this time period than the summer and fall of 2020, when Pfizer and Moderna conducted their trials.

Most importantly, the CDC study confirmed that the Pfizer and Moderna vaccines provided very high rates of protection for vaccinated participants, despite risky frontline conditions.

### Which study is should I trust the most?

All three studies provide valid, accurate, and high-quality data that work together to tell a clear story: mRNA vaccines continue to be highly effective against Covid-19. The CDC says that it's findings "in real-world conditions compliment and expand upon the vaccines effectiveness estimates from other recent studies" @thompson2021.

### Do the results chance across age, race, or status of underlying health conditions?

It's too early to say for sure. The clinical trials did test across subgroups of age, race, and risk of covid, but measuring for differences in vaccines efficacy was not the primary goal of the studies and more data is needed before any preliminary conclusions are drawn @baden2021 @polack2020. This is because large numbers of participants are required to say with confidence that very small differences in measured outcomes are probably not due to random chance.

### Why are protection chances better with Moderna even though Pfizer has a higher efficacy rate?

Math :) because protection chances depend on both efficacy rate and baseline risk of getting covid. The Pfizer clinical trials showed a higher "baseline" population infection rate in the placebo group compared to the data in the Moderna trials. Essentially, the Pfizer data measured a greater risk of catching covid, and this makes up for the difference in a higher efficacy rate.

These differences are so minor and the conditions in the clinical trials are unlikely to hold *exactly* every person in eternity. The takeaway is that both vaccines leave the average person with extremely good chances of protection from covid across a range of scenarios.

## Very Nerdy Questions

### The rainbow-looking graph has curves on it. Does that mean you have non-linear relationships in your model?

Yes. This is the equation I present in the app, strategically isolated for protection rate.

$$
\text{Protect Rate} = 1 - (\frac{\text{Infect. Rate}}{1000}*(1 - \text{Efficacy Rate}))
$$

And this is the equivalent, that @baden2021 and @polack2020 use. Note that this is a conceptual model and so I don't include the question of person-years for simplicity, but in reality these are taken into account when I do the calculations.

$$
\text{Efficacy Rate} = 1 - (\frac{\text{Treatment Infection Rate}}{\text{Placebo Infection Rate}})
$$

The assumption I believe the papers to be making is that the Placebo Infection Rate is representative of a non-vaccinated comparison group.

Since $\text{Protect Rate}$ is $(1 - \text{Treatment Infection Rate})$, we we note that:

1.  The $\text{Efficacy Rate}$ and $\text{Protect Rate}$ have a linear relationship, holding the $\text{Placebo Infection Rate}$ constant.

2.  The $\text{Efficacy Rate}$ and $\text{Placebo Infection Rate}$ or the population infection rate, have an inverse relationship, holding the $\text{Treatment Infection Rate}$ constant.

### Is this model still valid in a near-future scenario where much of the population is vaccinated?

Honestly I'm not sure, but I don't see why not --- at least on a basic conceptual level. The three components --- efficacy, population infection rate, and protection rate -- still should in theory remain operationally the same in a scenario where most (or even all) of the population is vaccinated. We see from the clinical data that vaccinated people can, at a *very* low rate, become infected with covid, so I would imagine that we could model "herd immunity" with this graph by moving the number of covid cases per 1,000 person-years to a lower number. This would be great news for a number of reasons, but in terms of vaccinations, vaccinated peoples' chances of protection would go up, and we established above the relationship is non-linear.

Say, with infection rate is reduced from 100 to 15 per 1000 person-years, with a vaccine efficacy of 80%:

$$
\text{Protect Rate} = 1 - (\frac{\text{Infect. Rate}}{1000}*(1 - \text{Efficacy Rate}))
$$

$$
0.98 = 1 - (0.1*(1 - 0.80))
$$

$$
0.997 = 1 - (0.015*(1 - 0.80))
$$

Cool, we've increased our communal estimated protection chances by about 1.7 percentage points. That may not seem like a lot, but depending on how literally we want to take these estimates, that small increase could save a lot of lives in aggregate.

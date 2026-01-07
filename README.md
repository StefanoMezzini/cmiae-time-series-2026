# Time Series Trend Analysis and Quantitative Environmental Impact Assessments

This repo contains the `R` scripts and data for the 2026 [CMIAE](https://cmiae.org/) courses on Time Series Trend Analysis and Quantitative Environmental Impact Assessments:

[Iteration 1](https://cmiae.org/event/time-series-trend-analysis-and-quantitative-environmental-impact-assessments/): 2026-01-27 to 2026-02-24; 09:00-12:00 PT

[Iteration 2](https://cmiae.org/event/time-series-trend-analysis-and-quantitative-environmental-impact-assessments-march/): 2026-03-03 to 2026-03-31; 09:00-12:00 PT

After downloading or cloning the repo, make sure you have installed all the packages listed in `necessary-packages.R`. Feel free to use the folder structure to organize files related to the scripts for each day.

# Suggested materials

This course is aimed at early-stage professionals or professionals looking to refresh and expand their skills, recent university or college graduates, and graduate students. Attendees should have basic proficiency with `R` and RStudio and have a general understanding of the following key statistical concepts, although I will go over them during the first day:

> Populations, samples, sample statistics, estimates, uncertainty (standard deviation, variance, standard error), confidence intervals, p-values, bias, precision, accuracy, power, hypothesis testing, statistical significance vs biological relevance, dealing with missing data.

# Additional materials

The following materials may be of interest to attendees who would like to familiarize themselves with Generalized Additive Models before the course. In particular, I suggest looking at 1, 2, 3 or 4, and 7. Resource 8 may be of interest to those working with multivariate models, while 9 may be of interest to those working on system dynamics and state changes. 10 is an excellent lecture series on Bayesian Statistics, and 11 is the accompanying book. Course adaptations for different `R` packages and other languages (including `Python`) are available at [https://xcelab.net/rm/](https://xcelab.net/rm/). Finally, 11 explores common issues regarding low-power studies during Environmental Risk Assessments.

## GAMs

1. Simpson G.L. (2020). Introduction to Generalized Additive Models with `R` and `mgcv`. https://www.youtube.com/watch?v=sgw4cu8hrZM
2. Simpson G.L. (2025). Visualizing generalized additive models in `R` using gratia and `conditional_values()`. https://www.youtube.com/watch?v=iKG0aHWpTAo
3. Simpson G.L. (2018). Modelling Palaeoecological Time Series Using Generalised Additive Models. Frontiers in Ecology and Evolution 6, 149. https://doi.org/10.3389/fevo.2018.00149
4. Simpson G.L. (2025). Method: Using generalized additive models in the livestock animal sciences. arXiv. https://doi.org/10.48550/arXiv.2507.06281
5. Simpson G.L. (2024). `gratia`: An `R` package for exploring generalized additive models. Journal of Open Source Software 9, (104), 6962, https://doi.org/10.21105/joss.06962
6. Wood S.N. (2017). Generalized additive models: an introduction with R, Second edition. CRC Press/Taylor & Francis Group, Boca Raton.

## Introduction to HGAMs

7. Pedersen E.J., Miller D.L., Simpson G.L. & Ross N. (2019). Hierarchical generalized additive models in ecology: an introduction with mgcv. PeerJ 7, e6876. https://doi.org/10.7717/peerj.6876

## Applications of GAMs in Ecology

8. Polazzo F., Limberger R., Pennekamp F., Ross S.R.P. ‐J., Simpson G.L. & Petchey O.L. (2024). Measuring the Response Diversity of Ecological Communities Experiencing Multifarious Environmental Change. Global Change Biology 30, e17594. https://doi.org/10.1111/gcb.17594
9. Gushulak C.A.C., Mezzini S., Moir K.E., Simpson G.L., Bunting L., Wissel B., et al. (2024). Impacts of a century of land‐use change on the eutrophication of large, shallow, prairie Lake Manitoba in relation to adjacent Lake Winnipeg (Manitoba, Canada). Freshwater Biology 69, 47–63. https://doi.org/10.1111/fwb.14192

## Additional resources on statistics

10. McElreath R. (2023). Statistical Rethinking YouTube playlist. https://www.youtube.com/playlist?list=PLDcUM9US4XdPz-KxHM4XHt7uUVGWWVSus
11. McElreath R. (2020). Statistical rethinking: a Bayesian course with examples in R and Stan, Second edition. CRC Press, Boca Raton London New York.
12. Rosenfeld J.S. (2025). Ratchet effects revisited: power effects and systematic bias in natural resource management. FACETS 10, 1–10. https://doi.org/10.1139/facets-2024-0099

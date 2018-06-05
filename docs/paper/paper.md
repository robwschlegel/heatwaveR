---
title: 'heatwaveR: A central algorithm for the detection of heatwaves and cold-spells'
tags:
- R
- heatwaves
- cold-spells
- extreme events
- ocean
- atmosphere
authors:
- affiliation: 1
  name: Robert W. Schlegel
  orcid: 0000-0002-0705-1287
- affiliation: 1
  name: Author Albertus J. Smit
  orcid: 0000-0002-3799-6126
date: "5 June 2018"
output: pdf_document
bibliography: JOSS.bib
affiliations:
- index: 1
  name: Department of Biodiversity and Conservation Biology, University of the Western Cape
---

# Summary

As the world continues to warm, we see not only a steady increase in mean temperatures [@IPCC2014], but an increase in the count and duration of extreme events, known as 'marine heatwaves' [MHW; @Oliver2018]. These events may decimate ecosystems [@Wernberg2016] and impact the health of fisheries [@Oliver2017]. It is therefore necessary that a standard definition for these events be provided for researchers that allows for the comparison of events at a global scale. The first framework that allowed for the measurement and comparison of events globally was first outlined by @Perkins2013 for atmospheric events. Based on this work, @Hobday2016 then developed a definition for MHWs. A publication by @Schlegel2017a then explored the concept of 'marine cold-spells' (MCSs).

The __`heatwaveR`__ package was developed and released in order to provide one central repository for the definition and visualisation of atmospheric and marine heatwaves and cold-spells. It also contains the functionality to calculate and visualise the categories of events as outlined in @Hobday2018. The __`heatwaveR`__ package is a project-wide update to the [__`RmarineHeatWaves`__](https://github.com/ajsmit/RmarineHeatWaves) package, which is itself a translation of the original [Python code](https://github.com/ecjoliver/marineHeatWaves) written by Eric C. J. Oliver. The __`heatwaveR`__ package has brought the inputs and outputs of the R code more in line with the Python code while also introducing substantial speed improvements over the previous R version by deconstructing and modularising it. The slow portions of the code have now been implemented in C++. The modular nature of the code allows for the use of custom baselines and climatologies in the calculations of events. This means that as the techniques for the detection of events change and improve over time, this package will be able to grow with them.

# Acknowledgements
(RWS: I've not acknowledge Eric here as that is done in the main body of the text)

# References

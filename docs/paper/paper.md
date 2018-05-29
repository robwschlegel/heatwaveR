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
  - name: Robert W. Schlegel
    orcid: 0000-0002-0705-1287
    affiliation: 1
  - name: Author Albertus J. Smit
    orcid: 0000-0002-3799-6126
    affiliation: 1
affiliations:
 - name: Department of Biodiversity and Conservation Biology, University of the Western Cape
   index: 1
date: 29 May 2018
bibliography: JOSS.bib
---

# Summary

As the world continues to warm, we see not only a steady increase in mean temperatures [@IPCC2014], but an increase in the count and duration of 'heatwaves' [@Oliver2018]. When extreme temperatures occur continuously for days or months they may pose a threat to both humans and ecosystems around the world []. It is therefore necessary to have a definition for heatwaves that allows for their measurement and comparison globally. The framework to do so was first outlined by [@Perkins-Kirkpatrick] for events in the atmosphere. Based on this work, [@Hobday2016] then developed a definition for 'marine heatwaves'. This definition differed from [@Perkins-Kirkpatrick] by setting the minimum number of  consecutive days of temperatures that exceed the 90th percentile of temperatures found on those Julian dates, allowing for a maximum gap of two days between  those days found in exceedance of the threshold. This differed from the atmopsheric definition in that it did not account for the excess heat factor (EHF; []), as the focus of this definition is how may extreme temperatures affect ecosystems, rather than human health. A publication by [@Schlegel2017a] then explored the concept of 'cold-spells', which are effectively the inverse of heatwaves.

This pacakge allows the user to apply the definitions for heatwaves outlined in [@Hobday2016] to their time series data. It contains three example time series, but is not designed to provide the user with data. The package does however provide a vignette on how to acquire data and calculate heatwaves from them.


The __`heatwaveR`__ package is a project-wide update to the [__`RmarineHeatWaves`__](https://github.com/ajsmit/RmarineHeatWaves) package, which is itself a translation of the original [Python code](https://github.com/ecjoliver/marineHeatWaves) written by Eric C. J. Oliver. The Python and R code have both been used in a number of publications in the past. This package seeks to bring the inputs and outputs of the R code more in line with the Python code. It also introduces substantial speed improvements over the previous R version. It differs from the __`futureheatwaves`__ R package in how it defines these events.

# Acknowledgements

# References

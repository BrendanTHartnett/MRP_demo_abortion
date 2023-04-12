# MRP_demo_abortion

This is the repository for a tutorial using the 2022 CES to predict state-level support for allowing abortion in all circumstances as a matter of choice.

**Using Multilevel Regression with Post-Stratification to Estimate State-Level Public Opinion.pdf** is an annotated demonstration of using mulitlevel regression with post-stratification and the 2022 Cooperative Elections Study to predict state-level support for allowing abortion in all circumstances as a matter of choice.

**MrP_demo_markdown.rmd** is the replication markdown for the above tutorial. 

The national survey data used is from the 2022 Cooperative Election Study, which can be downloaded here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/PR4L8P.

**tidycensus_get_census_data.R** uses Tidycensus to scrape and organize state-level census data. The results of this code (used in the tutorial) can be installed without scraping via **state_census_data.csv.**

**fipstostates.csv** converts state FIPS codes (as used in both the CES and Census) to an index of 1-50 for all states. This drops D.C., but allows us to use a loop to get state-level random effect.

**Create_map.R** uses leaflet to plot the state-level estimates obtained in the tutorial. 

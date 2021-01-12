# Hypothesis-Testing-Report-and-Shiny-Application

The data-set was obtained by conducting a survey with a sample of 174 out of 572 DATA2X02 students from USYD. The questionnaire collects information on a variety of topics ranging from dental practices to physical characteristics.

The data is cleaned and limitations are discussed with regards to bias and validity. One variable in the data set `shoe_size` was unable to be cleaned due major inconsistency issues (this is explained in detail in the report). Thus, inferences using this variable in the shiny app would be invalid.

Three hypothesis tests were conducted to test: (1) do the number of covid counts follow a Poisson distribution?, (2) the frequency of dental visits independent of whether students live with their parents?, and (3) the heights of male and female DATA2X02 students consistent with the statistics reported by the ABS?. These questions are addressed using chi-squared (goodness of fit and independence) tests and one-sample t-tests.

Link to shiny web application that dynamically conducts hypothesis tests: https://sourish27.shinyapps.io/Hypothesis_Testing_App/

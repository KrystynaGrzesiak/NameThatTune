# NameThatTune

## Spotify Compatible App For Playing 'Name That Tune?'.

### About...

This R-package provides a fully compatible with Spotify shiny app that allows you 
to play a competitive game Name That Tune? with your family and 
friends and will make you opening R after working hours.

### How to start?

To install the package execute the following code:

``` r
install.packages("devtools")
devtools::install_github("KrystynaGrzesiak/NameThatTune", upgrade = "always", dependencies = TRUE)
```
Then, you can run the app using the code:

``` r
NameThatTune::run_name_that_tune(client_id = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX",
                                 client_secret = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
```

where `client_id` and `client_secret` are character strings of length 32 generated as explained [here](https://developer.spotify.com/documentation/general/guides/authorization/app-settings/).

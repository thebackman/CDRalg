<!-- README.md is generated from README.Rmd. Please edit that file -->
CDRalg
======

The purpose of this package is to implement the *Clinical Dementia Rating (CDR)* algorithm in R.

### Usage

The package basically contains one workhorse function that does the actual coding. This function takes a data frame as input, applies the algorithm to the data and outputs the same data frame but with the global coding added. Additional functionality is added for debugging purposes.

See detailed notes in the vignette included which can be accessed after installation via:

``` r
vignette("CDRalg")
```

To install from GitHub use:

``` r
devtools::install_github("thebackman/CDRalg")
```

### Notes

To the best of my knowledge the algorithm should give the intended results. I have done my best to test it in different situations but as always there is the potential for errors and therefore I invite anyone to double-check my work. Needless to say, I cannot take responsibility for such coding errors.

Further I did not invent the algorithm nor develop the scale but as many researchers work with R I believe that such a tool might come in handy as it has in my own research group at the University of Gothenburg.

There are at present some "inefficiences" in the code which I know can be fixed. They do not however affect the end results and are therefore not my number one priority. :-)

### References

Literature, scoring rules and links to online tools can be found here. This is the source I used for the implementation of the algorithm.

<http://alzheimer.wustl.edu/cdr/cdr.htm>

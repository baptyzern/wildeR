# Your Package Name

The `wildeR` proposes 4 functions to help you better apprend numeric variables.

## Installation

You can install the package from GitHub using the `devtools` package:

```
devtools::install_github("bouh40/wildeR")
```

## Functions

### distrib

The `distrib` function provides summary statistics and visualizations for a numeric variable.

```
# Example Usage
distrib(your_data$variable)
```

 
### distribCroisee
The `distribCroisee` function generates cross-tabulations and summary statistics for two variables, one numeric, one categorial.

```
# Example Usage
distribCroisee(var1 = your_data$variable1, var2 = your_data$variable2)
```


### graphDistrib
The `graphDistrib` function creates a density plot to visualize the distribution of a numeric variable.

```
# Example Usage
graphDistrib(your_data$variable)
```


### graphDistribCroisee
The `graphDistribCroisee` function produces a crossed density plot for two variables, one numeric, one categorial.

```
# Example Usage
graphDistribCroisee(var1 = your_data$variable1, var2 = your_data$variable2)
```

# wildeR package

The `wildeR` package's main purpose is to proposes functions to help you better apprehend numeric variables.

It also adds random functions that I needed on some ... random projects

## Installation

You can install the package from GitHub using the `remotes` package.

```         
remotes::install_github("baptyzern/wildeR")
```

## Functions

### distrib

The `distrib` function computes various descriptive statistics for the given variable, and put these statistics into a `data.frame`.

It is possible to compute statistics by groups (using a qualitative vector).

The precision of the statistics given can be changed using the `precision` argument.

A `filter` may be applied using an argument to restrict the data used to compute the statistics.

```         
# Example using the default precision ("summary")
data_ex <- rnorm(100)
distrib(data_ex)

# Example using precision "edges"
data_ex <- rnorm(100)
distrib(data_ex, precision = "edges")

# Example using the `var2` argument
data_ex1 <- rnorm(100)
data_ex2 <- sample(c("A", "B", "C"), 100, replace = TRUE)
distrib(data_ex1, data_ex2)

# Example using the `filter` argument
data_ex <- rnorm(100)
distrib(data_ex, filter = data_ex > 0)
```

The `distribCroisee` function generates cross-tabulations and summary statistics for two variables, one numeric, one categorial.\
It was made useless when its functionality (grouped statistics) was implement into `distrib`.

```         
# Example
distribCroisee(var1 = your_data$variable1, var2 = your_data$variable2)
```

### graphDistrib

The `graphDistrib` function creates a density plot to visualize the distribution of a numeric variable.

It is possible to compute statistics by groups (using a qualitative vector).

A `filter` may be applied using an argument to restrict the data used to compute the statistics.

The type of the output can be changed using the `output` argument.

```         
# Example using a numeric vector
data_ex <- rnorm(100)
graphDistrib(data_ex)

# Example using the `var2` argument
data_ex1 <- rnorm(100)
data_ex2 <- sample(c("A", "B", "C"), 100, replace = TRUE)
graphDistrib(data_ex1, data_ex2)

# Example using the `filter` argument
data_ex <- rnorm(100)
graphDistrib(data_ex, filter = data_ex > 0)

# Example using the `output` argument set to layer
library(ggplot2)
data_ex <- rnorm(100)
ggplot() + graphDistrib(data_ex, filter = data_ex > 0, output = "layer")
```

As `distribCroisee`, the `graphDistribCroisee` was made useless when its functionality (grouped statistics) was implement into `graphDistrib`.

```         
# Example
graphDistribCroisee(var1 = your_data$variable1, var2 = your_data$variable2)
```

### pandoc_command

This function helps you generate a pandoc command to be executed in a terminal.

Concretely, it allows you to convert documents from one format to another.

The idea behind this function was to provide a tool to easily convert `docx` (Word) documents to the `markdown` format, in order to make the `Rmarkdown` / `Quarto` workflow easier, especially when redacting in Word and then inserting your text into a `md` file.

```         
# It is possible to force the file extension of the output.
# Pandoc only knows about markdown, but qmd file use the same syntax as markdown
# so there is no issue when outputting to a qmd file.
pandoc_command(input_file = "blabla.docx", output_format = "markdown", output_file = "blabla.qmd")

# You can combine the pandoc_command call with the system() function to send the output directly to the terminal.
system(pandoc_command(input_file = "blabla.docx", output_format = "markdown", output_file = "blabla.qmd"))
pandoc_command(input_file = "blabla.docx", output_format = "markdown", output_file = "blabla.qmd") |> system()
```

### country_name

This function helps you convert country name in English, French or German into their 2 or 3 letter ISO codes, and vice versa.

It works using a data base publicly available through Eurostat and included in this package (`world_seen_by_eu`).

Formats should be provided in these forms: "name_fr", "name_en", "name_de", "iso2", "iso3".

```         
# Example
country_name(c("FR", "PT"), from = "iso2", to = "name_en")
```

### apply_labels_from_df

This function helps apply labels to a `data.frame` using another `data.frame`.

```         
# Example
tt <- apply_labels_from_df(world_seen_by_eu[, -12],
                           variables_world_seen_by_eu,
                           col_label = "VariableLabel_FR")
# tt
# labelled::var_label(tt)
```

### pointmedian (or pm) and guillemetsFR

Both of these functions are here to provide easy access to characters that are not available easily from keyboards.

They can be used directly in inserted chunks of a `Rmarkdown` / `Quarto` document.

`pointmedian` (or `pm`) has a `full` argument to set to FALSE if you only want the specific character.

Same for `guillemetsFR`, but the argument is a `type` argument, which specifies if the call is to open ("in") or to close ("out") the quote. The output include and unbreakable space at the end (for "in") or in the beginning (for "out").

```         
# Example for pointmedian
pointmedian()
pm()

# To keep it short, inside an inserted chunk
Mes camarades étudiant`r pm(full = F)`es de QESS sont des personnes fantastiques.
--> Mes camarades étudiant·es de QESS sont des personnes fantastiques.
```         

```         
# Example for guillemetsFR
guillemetsFR()

# To keep it short, inside an inserted chunk
L'atelier `r guillemetsFR("in")`QESStionnaire`guillemetsFR("out")` a fait une enquête sur la répartition des tâches domestiques et parentales
--> L'atelier « QESStionnaire » a fait une enquête sur la répartition des tâches domestiques et parentales
```


### var_flagged

`var_flagged` was designed to work with flagged variables.

It is still under development.

```         
# Example
sample <- data.frame(variable_x = c("Male", "Female", "Female", NA, NA, "Male", "Female"),
                     variable_x_F = c(1, 1, 1, -1, -2, 1, 1))
flag_meaning <- c("Missing" = -1, "Not applicable" = -2)
var_flagged(x = sample$variable_x, x_flag = sample$variable_x_F,
            flag_meaning = c("Missing" = -1, "Not applicable" = -2))
```

# Comparative Metrics Framework (cmf) in R to evaluate the performance of synthetic data. 
Author : Chan Mun Fai 

**cmf** provides the Ratio of Counts(ROC) function and the Differential Correct Attribution Probability(DCAP). 

>These functions along with other metrics, are used to construct a systematic and comprehensive framework in evaluating the quality of synthetic datasets and different synthesis methods. 

For more details, please refer to [Comparative Metrics Framework in R to evaluate the performance of Synthetic Data](https://github.com/MUNFAI15/DiffPriv/blob/master/comparative%20metrics%20framework.pdf) 

### Install cmf 
The current development version can be installed from source using devtools.

```bash
devtools::install_github("MUNFAI15/cmf")
```

### Usage Examples 
The following script demonstrates how to use the functions in **cmf**. We will use the package **synthpop** to generate synthetic data using the mtcars dataset. 

```bash
library(cmf)
library(synthpop)
df <- mtcars
key_var <- c("cyl", "gear")
target_var <- c("wt", "carb")

syn1 <- syn(df, seed = 1234)
synthpop_df <- syn1$syn
view(synthpop_df)

CAP_original(df, key_var, target_var)
CAP_baseline(df, target_var)
CAP_synthetic(df, synthpop_df, key_var, target_var)

ROC_list(df, synthpop_df)
ROC_indiv(df, synthpop_df, "disp")
ROC_score(df, synthpop_df)
ROC_numeric(df, synthpop_df, "disp", y=2)
```
## References

Taub, J., Elliot, M., Pampaka, M., &amp; Smith, D. (2018). Differential Correct Attribution Probability for Synthetic Data: An Exploration. Privacy in Statistical Databases Lecture Notes in Computer Science, 122-137. doi:10.1007/978-3-319-99771-1_9

Taub, J., Elliot, M., & Raab, G. (2019). Creating the Best Risk-Utility Profile : The Synthetic Data Challenge.

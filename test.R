### transform 

```{r}
p1 =ggplot(heart_df, aes(x = survival_months)) + geom_density()
p2 = ggplot(heart_df, aes(x = age)) + geom_density()
p3 = ggplot(heart_df, aes(x = tumor_size)) + geom_density()
p4 = ggplot(heart_df, aes(x = rne)) + geom_density()
p5 = ggplot(heart_df, aes(x = rnp)) + geom_density()

combined_plot = plot_grid(p1, p2, p3, p4, p5, 
                          ncol = 3,
                          nrow = 2)
combined_plot
```

```{r}
# heart_df_new = 
#   heart_df |> 
#     mutate(rnp = log(rnp, base = 1.5))
#     mutate_at(vars(tumor_size, rne, rnp), sqrt)
# 
# p6 = ggplot(heart_df_new, aes(x = tumor_size)) + geom_density()
# p7 = ggplot(heart_df_new, aes(x = rne)) + geom_density()
# p8 = ggplot(heart_df_new, aes(x = rnp)) + geom_density()
# 
# combined_plot_2 = plot_grid(p6, p7, p8, 
#                            ncol = 3,
#                            nrow = 1)
# combined_plot_2
```




### one-way anova

```{r}
set.seed(123)

heart_df_3 = 
  read_csv("./Project_2_data.csv") |> 
  janitor::clean_names() |> 
  relocate(survival_months, status) |> 
  dplyr::select("status", "age", "tumor_size", "regional_node_examined", "reginol_node_positive")

anova_results = list()

for(i in 2:ncol(heart_df_3)){
  formula_obj <- as.formula(paste(names(heart_df_3)[i], "~ status", sep = ""))
  anova_result <- aov(formula_obj, data = heart_df_3)
  
  p_value = summary(anova_result)[[1]]["Pr(>F)"]["status",]
  
  anova_results[[i - 1]] <- p_value
}

anova_tb = 
  tibble(
    variale = c("age", "tumor_size", "regional_node_examined", "reginol_node_positive"),
    p_value = anova_results
  ) |> 
  unnest(p_value)

anova_tb
```

```{r}
# par(mfrow = c(1,2))
# qqnorm(heart_df_log$age, main = "age")
# qqline(heart_df_log$age, col = 2)
# qqnorm(heart_df_log$tumor_size, main = "tumor_size")
# qqline(heart_df_log$tumor_size, col = 2)
# qqnorm(heart_df_log$regional_node_examined, main = "regional_node_examined")
# qqline(heart_df_log$regional_node_examined, col = 2)
# qqnorm(heart_df_log$reginol_node_positive, main = "reginol_node_positive")
# qqline(heart_df_log$reginol_node_positive, col = 2)
```

<font color = "blue">most continuous data are right skewed, we can use a log function to make it more close to normal distribution </font>
  
  ```{r}
# heart_df_log = 
#   heart_df_log |> 
#     mutate(
#       log_size = log(tumor_size),
#       log_node_ex = log(regional_node_examined),
#       log_node_po = log(reginol_node_positive)
#     )
# 
# qqnorm(heart_df_log$log_node_po, main = "log_node_po")
# qqline(heart_df_log$log_node_po, col = 2)
```

## t-test & char squared

```{r}
x2 = c("race", "marital_status", "t_stage", "n_stage", "x6th_stage", "grade", "a_stage", "estrogen_status", "progesterone_status")

x1 = c("age", "tumor_size", "regional_node_examined", "reginol_node_positive")

library(tableone)
table1 = CreateTableOne(
  vars = c(x1, x2),
  data = heart_df_log,
  factorVars = x2,
  strata = "status",
  addOverall = FALSE)


results1 = print(table1, showAllLevels = FALSE)

knitr::kable(results1[,c(3)], align = 'c',
             caption = 'Table 1')
```

<font color = "blue">`regional_node_examined` is not a very good data，but we can preserve it</font>
  
  ## check the dataset again
  
  ```{r}
heart_df_log = 
  read_csv(".Project_2_data.csv") |> 
  janitor::clean_names() |> 
  relocate(survival_months, status) |> 
  mutate(
    race = factor(race, levels = c("White", "Black", "Other")),
    marital_status = factor(marital_status, levels = c("Married", "Divorced", "Single", "Widowed", "Separated")),
    t_stage = factor(t_stage, levels = c("T1", "T2", "T3", "T4")),
    n_stage = factor(n_stage, levels = c("N1", "N2", "N3")),
    x6th_stage = factor(x6th_stage, levels = c("IIA", "IIIA", "IIIC", "IIB", "IIIB")),
    differentiate = factor(differentiate, levels = c("Poorly differentiated", "Moderately differentiated", "Well differentiated", "Undifferentiated")),
    grade = factor(grade, levels = c("3", "2", "1", "anaplastic; Grade IV")),
    a_stage = factor(a_stage, levels = c("Regional", "Distant")),
    estrogen_status = factor(estrogen_status, levels = c("Positive", "Negative")),
    progesterone_status = factor(progesterone_status, levels = c("Positive", "Negative")),
    status = factor(status, levels = c("Alive", "Dead"))) |> 
  select(-survival_months, -differentiate)

summary(heart_df_log)
str(heart_df_log)
```
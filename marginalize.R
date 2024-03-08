library(dplyr)
set.seed(20240308)
#### Generate Example Data #####
n = 1000
decade = factor(floor(runif(n, min = 40, max = 79)/10)*10)
chd = rbinom(n = n, size = 1L, prob = 0.35)
# inducing some dependencies in Diabetes on CHD
diabetes =
  rbinom(n = n, size = 1L, prob = (0.75 * chd) + 0.25 * (1-chd))
# inducing some dependencies in hemiplegia on Diabetes (and therefore some CHD)
hemiplegia =
  rbinom(n = n, size = 1L, prob = (0.65 * diabetes) + 0.35 * (1-diabetes))

# Create original data set
original_data = tibble(
  decade  = decade,
  chd = chd,
  diabetes = diabetes,
  hemiplegia = hemiplegia)
#
X = model.matrix(object = ~ decade + chd + diabetes + hemiplegia, data = original_data)
beta = c("(Intercept)" = -1.25, "decade50" = 0.01,
         "decade60" = 0.15, "decade70" = 0.2,
         "chd" = 0.32, "diabetes" = 0.48,
         "hemiplegia" = 0.6)

original_data = original_data %>%
  mutate(
    xb = c(X %*% beta),
    p_y = exp(xb)/(1+exp(xb)),
    y = rbinom(n = n, size = 1L, prob = p_y)
  ) %>%
  select(-xb, -p_y)
## - end Generate data


#### Fit Model #####
original_model = glm(y ~ decade + chd + diabetes + hemiplegia, data = original_data,
                     family = binomial())
original_risk_score_coefs = coef(original_model)

#### Get Sufficient Data Matrix for Counting #####
data = model.matrix( ~ -1 + y + decade + chd + diabetes + hemiplegia,
                     data = original_data) %>%
  as_tibble()
model = glm(y ~ decade50 + decade60 + decade70 + chd + diabetes + hemiplegia,
            data = data,
            family = binomial())
risk_score_coefs = coef(model)

stopifnot(all(original_risk_score_coefs == risk_score_coefs))

# Count up the patterns seen
original_summary = original_data %>%
  group_by(across(everything())) %>%
  count() %>%
  ungroup()
# Count up the patterns seen without outcome
summary_no_y = original_data %>%
  group_by(across(-any_of("y"))) %>%
  count() %>%
  ungroup()
# marginals = data %>% colMeans()

# Count up the patterns seen
summary = data %>%
  group_by(across(everything())) %>%
  count() %>%
  ungroup()
# Count up the patterns seen without outcome
summary_no_y = data %>%
  group_by(across(-any_of("y"))) %>%
  count() %>%
  ungroup()
marginals = data %>% colMeans()

# example case to predict
example_prediction_data = tibble(
  decade = factor(50, levels = c(40, 50, 60, 70)),
  chd = 1,
  hemiplegia = 0,
  diabetes = NA_real_
)

# making it a matrix to find for the summary data
example_prediction_X = example_prediction_data %>%
  select(-diabetes)

example_prediction_X = model.matrix(~ -1 + ., data = example_prediction_X) %>%
  as_tibble()

unobserved_example_data = left_join(example_prediction_X, summary_no_y)
unobserved_example_data$raw_p = predict(model,
                                        newdata = unobserved_example_data,
                                        type = "response")
unobserved_example_data = unobserved_example_data %>%
  mutate(marginalized = (n/sum(n)) * raw_p,
         heuristic_1 = 1/dplyr::n() * raw_p,
         heuristic_2 = marginals["diabetes"] * raw_p)
unobserved_example_data
unobserved_example_data %>%
  select(marginalized, heuristic_1, heuristic_2) %>%
  colMeans()

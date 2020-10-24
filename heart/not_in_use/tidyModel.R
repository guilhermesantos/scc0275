# 10-Fold Cross Validation -----------------------------------------------------

run_CV_TM <- 
function(mod_spec, folds, dataset = "test"){

    df = map2_df(.x = folds$splits,.y = folds$id,
        function (split = .x, fold = .y) {
            # Split the data into analysis and assessment tables
            df_analysis <- analysis(split)
            df_assessment <- assessment(split)
            
           
            mod <- mod_spec %>%
                    fit(data = df_analysis)
            
            if(dataset == 'test'){
                table <- 
                  tibble(fold = fold,
                          truth = df_assessment$condition,
                          .pred_positivo = 
                              predict(mod, 
                                      new_data = df_assessment, 
                                      type = "prob")[[".pred_positivo"]],
                          .pred_negativo = 
                              predict(mod, 
                                      new_data = df_assessment, 
                                      type = "prob")[[".pred_negativo"]],
                          .pred_class = 
                              predict(mod, 
                                      new_data = df_assessment) %>% 
                          unlist() %>% 
                          as.character()
                          ) %>%
                          mutate(.pred_class = factor(.pred_class))

            }else{
                table <- 
                    tibble(fold = fold,
                            truth = df_analysis$condition,
                            .pred_positivo = 
                                predict(mod, 
                                        new_data = df_analysis, 
                                        type = "prob")[[".pred_positivo"]],
                            .pred_negativo = 
                                predict(mod, 
                                        new_data = df_analysis, 
                                        type = "prob")[[".pred_negativo"]],
                            .pred_class = 
                                predict(mod, 
                                        new_data = df_analysis) %>% 
                            unlist() %>% 
                            as.character()
                            ) %>%
                            mutate(.pred_class = factor(.pred_class))
            }
    })

    metric = df %>% 
      group_by(fold) %>%
      metrics(truth, .pred_class, .pred_positivo) %>%
      dplyr::filter(.metric %in% c("accuracy", "roc_auc")) %>%
      group_by(.metric) %>%
      summarise(mean = mean(.estimate), sd = sd(.estimate))

    return(list(tab = df, metric = metric))

}

# Split train-test 0.75/0.25
heart_split <- initial_split(tidiedData, strata = condition)
heart_train <- training(heart_split)
heart_test  <- testing(heart_split)

heart_train %>%
  count(condition) %>%
  mutate(prop = n/ sum(n))

heart_test %>%
  count(condition) %>%
  mutate(prop = n/ sum(n))

tidiedData %>%
  count(condition) %>%
  mutate(prop = n/ sum(n))


# Recipe
heart_rec <- tidiedData %>%
  recipe(condition ~.) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_normalize(cont_names) %>%
  step_zv(all_predictors())  %>%
  prep()
juice(heart_rec)

options(yardstick.event_first = FALSE)
folds <- vfold_cv(tidiedData, v = 10)

# Logistic regression-----------------------------------------------------------

lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

lr_wf <- workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(heart_rec) 

lr_rs <- 
  lr_wf %>% 
  fit_resamples(folds, control = control_resamples(save_pred = TRUE))

collect_metrics(lr_rs)

run_CV_TM(mod_spec = lr_wf, folds = folds, dataset = "train")$metric
run_CV_TM(mod_spec = lr_wf, folds = folds, dataset = "test")$metric

# Random forest ----------------------------------------------------------------

rf_mod <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

rf_wf <- workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(heart_rec) 

# rf_rs <- 
#   rf_wf %>% 
#   fit_resamples(folds, control = control_resamples(save_pred = TRUE))

# collect_metrics(rf_rs)

run_CV_TM(mod_spec = rf_wf, folds = folds, dataset = "train")$metric
run_CV_TM(mod_spec = rf_wf, folds = folds, dataset = "test")$metric

# K nearest neighbours----------------------------------------------------------

for (k in 1:10){
  knn_mod <- 
    nearest_neighbor(
      mode = "classification",
      neighbors = 1
    ) %>%
    set_engine("kknn")  

  knn_wf <-  workflow() %>%
    add_model(knn_mod) %>%
    add_recipe(heart_rec) 

  run_CV_TM(mod_spec = knn_wf, folds = folds, dataset = "train")$metric[1, 2:3] 
  run_CV_TM(mod_spec = knn_wf, folds = folds, dataset = "test")$metric[1, 2:3]

}

# knn_rs <- 
#   knn_wf %>% 
#   fit_resamples(folds, control = control_resamples(save_pred = TRUE))
# collect_metrics(knn_rs)

knn_rs %>%
  unnest(.predictions) %>%
  mutate(model = "kknn") %>%
  bind_rows(lr_rs %>%
    unnest(.predictions) %>%
    mutate(model = "lr")) %>%
  bind_rows(rf_rs %>%
    unnest(.predictions) %>%
    mutate(model = "rf")) %>%
  group_by(model) %>%
  roc_curve(condition, .pred_positivo) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(size = 1.5) +
  geom_abline(
    lty = 2, alpha = 0.5,
    color = "gray50",
    size = 1.2
  )


lr_rs %>%
  collect_predictions() %>%
  group_by(id) %>%
  roc_curve(condition, .pred_positivo) %>%
  ggplot(aes(1 - specificity, sensitivity, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_path(show.legend = TRUE, alpha = 0.6, size = 1.2) +
  coord_equal()
lr_fit_rs %>%
  conf_mat_resampled()

knn_mod %>% 
  fit(condition~., data = juice(heart_rec))



lr_plot <- 
  lrP_rs %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())


# Workflow

# lrP_mod <- 
#   logistic_reg(penalty = tune(), mixture = 1) %>% 
#   set_engine("glmnet")

# lda_mod <-
#   discrim_flexible() %>%
#   set_engine("earth")

# lrP_wf <- workflow() %>%
#   add_model(lrP_mod) %>%
#   add_recipe(heart_rec)

# lrP_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

# lrP_grid %>% top_n(-5) # lowest penalty values
# lrP_grid %>% top_n(5) 

# val_set <- validation_split(tidiedData, 
#                             strata = condition, 
#                             prop = 0.75)

# lrP_rs <- 
#   lrP_wf %>% 
#   tune_grid(val_set,
#             grid = lrP_grid,
#             control = control_grid(save_pred = TRUE),
#             metrics = metric_set(roc_auc))


# lrP_rs %>% 
#   show_best("roc_auc", n = 15) %>% 
#   arrange(penalty)




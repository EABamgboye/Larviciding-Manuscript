# ================================
# MaxEnt ROC with Mean Curve + CI
# ================================

library(raster)
library(dismo)
library(caret)
library(ggplot2)
library(dplyr)

# -------------------------------
# 1. Prepare occurrence data
# -------------------------------

occ_wet_mat <- as.matrix(occ_wet)

# Small datasets should use small k
k <- 2
foldsw <- createFolds(1:nrow(occ_wet_mat), k = k)

# Storage objects
roc_list <- list()
auc_valuesw <- numeric(length(foldsw))

# -------------------------------
# 2. Cross-validation loop
# -------------------------------

for(i in seq_along(foldsw)){
  
  train_idx <- setdiff(1:nrow(occ_wet_mat), foldsw[[i]])
  test_idx  <- foldsw[[i]]
  
  train_pts <- occ_wet_mat[train_idx, , drop = FALSE]
  test_pts  <- occ_wet_mat[test_idx, , drop = FALSE]
  
  # Fit MaxEnt
  modelw <- maxent(
    x = wet_stack,
    p = train_pts,
    factors = "landuse"
  )
  
  # Generate background points
  bg_points <- randomPoints(wet_stack, n = 10000)
  
  # Evaluate model
  eval_objw <- evaluate(
    p = test_pts,
    a = bg_points,
    model = modelw,
    x = wet_stack
  )
  
  # Store AUC
  auc_valuesw[i] <- eval_objw@auc
  
  # Store ROC data
  roc_list[[i]] <- data.frame(
    FPR = eval_objw@FPR,
    TPR = eval_objw@TPR,
    Fold = i
  )
}

# Combine ROC curves
roc_df <- bind_rows(roc_list)

# -------------------------------
# 3. Interpolate ROC curves
# -------------------------------

fpr_grid <- seq(0, 1, length.out = 200)

tpr_matrix <- sapply(unique(roc_df$Fold), function(f){
  
  fold_data <- roc_df %>% filter(Fold == f)
  
  approx(
    x = fold_data$FPR,
    y = fold_data$TPR,
    xout = fpr_grid,
    rule = 2
  )$y
})

# -------------------------------
# 4. Compute mean ROC + CI
# -------------------------------

tpr_mean  <- rowMeans(tpr_matrix)

tpr_lower <- apply(tpr_matrix, 1, quantile, 0.025)

tpr_upper <- apply(tpr_matrix, 1, quantile, 0.975)

roc_summary <- data.frame(
  FPR = fpr_grid,
  TPR_mean = tpr_mean,
  TPR_lower = tpr_lower,
  TPR_upper = tpr_upper
)

# -------------------------------
# 5. Plot publication-quality ROC
# -------------------------------

pdf("Agugu bootstraped ROC plot.pdf", width = 12, height = 6)

ggplot() +
  
  geom_ribbon(
    data = roc_summary,
    aes(x = FPR, ymin = TPR_lower, ymax = TPR_upper),
    fill = "steelblue",
    alpha = 0.25
  ) +
  
  geom_line(
    data = roc_summary,
    aes(x = FPR, y = TPR_mean),
    color = "steelblue",
    size = 1.3
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "grey40"
  ) +
  
  labs(
    title = paste0("MaxEnt ROC Curve (Mean AUC = ", round(mean(auc_valuesw),3), ")"),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  
  coord_equal() +
  
  theme_manuscript()

dev.off()







##Use 2nd wet stack


# -------------------------------
# 1. Prepare occurrence data
# -------------------------------

occ_wet_mat <- as.matrix(occ_wet)

# Small datasets should use small k
k <- 2
foldsw <- createFolds(1:nrow(occ_wet_mat), k = k)

# Storage objects
roc_list2 <- list()
auc_valuesw2 <- numeric(length(foldsw))

# -------------------------------
# 2. Cross-validation loop
# -------------------------------

for(i in seq_along(foldsw)){
  
  train_idx <- setdiff(1:nrow(occ_wet_mat), foldsw[[i]])
  test_idx  <- foldsw[[i]]
  
  train_pts <- occ_wet_mat[train_idx, , drop = FALSE]
  test_pts  <- occ_wet_mat[test_idx, , drop = FALSE]
  
  # Fit MaxEnt
  modelw2 <- maxent(
    x = wet_stack2,
    p = train_pts,
    factors = "landuse"
  )
  
  # Generate background points
  bg_points <- randomPoints(wet_stack2, n = 10000)
  
  # Evaluate model
  eval_objw2 <- evaluate(
    p = test_pts,
    a = bg_points,
    model = modelw2,
    x = wet_stack2
  )
  
  # Store AUC
  auc_valuesw2[i] <- eval_objw2@auc
  
  # Store ROC data
  roc_list2[[i]] <- data.frame(
    FPR = eval_objw2@FPR,
    TPR = eval_objw2@TPR,
    Fold = i
  )
}

# Combine ROC curves
roc_df2 <- bind_rows(roc_list2)

# -------------------------------
# 3. Interpolate ROC curves
# -------------------------------

fpr_grid2 <- seq(0, 1, length.out = 200)

tpr_matrix2 <- sapply(unique(roc_df2$Fold), function(f){
  
  fold_data2 <- roc_df2 %>% filter(Fold == f)
  
  approx(
    x = fold_data2$FPR,
    y = fold_data2$TPR,
    xout = fpr_grid2,
    rule = 2
  )$y
})

# -------------------------------
# 4. Compute mean ROC + CI
# -------------------------------

tpr_mean2  <- rowMeans(tpr_matrix2)

tpr_lower2 <- apply(tpr_matrix2, 1, quantile, 0.025)

tpr_upper2 <- apply(tpr_matrix2, 1, quantile, 0.975)

roc_summary2 <- data.frame(
  FPR2 = fpr_grid2,
  TPR_mean2 = tpr_mean2,
  TPR_lower2 = tpr_lower2,
  TPR_upper2 = tpr_upper2
)

# -------------------------------
# 5. Plot publication-quality ROC
# -------------------------------

roc_plot2 <- ggplot() +
  
  geom_ribbon(
    data = roc_summary2,
    aes(x = FPR2, ymin = TPR_lower2, ymax = TPR_upper2),
    fill = "steelblue",
    alpha = 0.25
  ) +
  
  geom_line(
    data = roc_summary2,
    aes(x = FPR2, y = TPR_mean2),
    color = "steelblue",
    size = 1.3
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "grey40"
  ) +
  
  labs(
    title = paste0("MaxEnt ROC Curve (Mean AUC = ", round(mean(auc_valuesw2),3), ")"),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  
  coord_equal() +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(roc_plot2)
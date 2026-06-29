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
# 1) Coordinates matrix in raster CRS
# 1) Coordinates matrix in raster CRS
occ_mat <- coordinates(occ_wet_sp_utm)  # n x 2 matrix (x, y)

occ_chal_mat <- occ_mat

# Small datasets should use small k
k <- 2
foldsc <- createFolds(1:nrow(occ_chal_mat), k = k)

# Storage objects
roc_listc <- list()
auc_valuesc <- numeric(length(foldsc))

# -------------------------------
# 2. Cross-validation loop
# -------------------------------

for(i in seq_along(foldsc)){
  
  train_idx_c <- setdiff(1:nrow(occ_chal_mat), foldsc[[i]])
  test_idx_c  <- foldsc[[i]]
  
  train_pts_c<- occ_chal_mat[train_idx_c, , drop = FALSE]
  test_pts_c  <- occ_chal_mat[test_idx_c, , drop = FALSE]
  
  # Fit MaxEnt
  modelc <- maxent(
    x = cwet_stack_r,
    p = train_pts_c,
    factors = "landuse"
  )
  
  # Generate background points
  bg_pointsc <- randomPoints(cwet_stack_r, n = 10000)
  
  # Evaluate model
  eval_objc <- evaluate(
    p = test_pts_c,
    a = bg_pointsc,
    model = modelc,
    x = cwet_stack_r
  )
  
  # Store AUC
  auc_valuesc[i] <- eval_objc@auc
  
  # Store ROC data
  roc_listc[[i]] <- data.frame(
    FPR = eval_objc@FPR,
    TPR = eval_objc@TPR,
    Fold = i
  )
}

# Combine ROC curves
roc_dfc <- bind_rows(roc_listc)

# -------------------------------
# 3. Interpolate ROC curves
# -------------------------------

fpr_gridc <- seq(0, 1, length.out = 200)

tpr_matrixc <- sapply(unique(roc_dfc$Fold), function(f){
  
  fold_datac <- roc_dfc %>% filter(Fold == f)
  
  approx(
    x = fold_datac$FPR,
    y = fold_datac$TPR,
    xout = fpr_gridc,
    rule = 2
  )$y
})

# -------------------------------
# 4. Compute mean ROC + CI
# -------------------------------

tpr_meanc  <- rowMeans(tpr_matrixc)

tpr_lowerc <- apply(tpr_matrixc, 1, quantile, 0.025)

tpr_upperc <- apply(tpr_matrixc, 1, quantile, 0.975)

roc_summaryc <- data.frame(
  FPR = fpr_gridc,
  TPR_mean = tpr_meanc,
  TPR_lower = tpr_lowerc,
  TPR_upper = tpr_upperc
)

# -------------------------------
# 5. Plot publication-quality ROC
# -------------------------------

pdf("Challenge bootstraped ROC plot.pdf", width = 12, height = 6)

ggplot() +
  
  geom_ribbon(
    data = roc_summaryc,
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
    title = paste0("MaxEnt ROC Curve Challenge(Mean AUC = ", round(mean(auc_valuesc),3), ")"),
    x = "False Positive Rate",
    y = "True Positive Rate"
  ) +
  
  coord_equal() +
  
  theme_manuscript()

dev.off ()



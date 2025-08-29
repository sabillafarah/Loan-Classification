# packages
library(tidyverse)
library(lubridate)
library(caret)
library(randomForest)
library(pROC)
library(recipes)   # optional, pipeline
set.seed(123)
library(readxl)
library(dplyr)

# =====================================================
# 1. Load package & data
# =====================================================
#data asli
Dtasli <- read_excel("C:/SARAH/IDX Data Scientist/Final/loan_data_2007_2014.xlsx")
#View(Dtasli)

Book1 <- read_excel("C:/SARAH/IDX Data Scientist/Final/Book1.xlsx")
#View(Book1)

# =====================================================
# 2. Ambil data random (10 ribu baris)
# =====================================================
set.seed(123)
sample_df <- Book1 %>% sample_n(10000)

# =====================================================
#3. Membinerkan
# =====================================================

#3.1. XE
sample_df <- sample_df %>%
  mutate(XE = case_when(
    XE == "60 months" ~ 0,
    XE == "36 months" ~ 1,
    TRUE ~ NA_real_
  ))

#3.2. XH
sample_df <- sample_df %>%
  mutate(XH = case_when(
    XH == "A" ~ 0,
    XH == "B" ~ 1,
    XH == "C" ~ 2,
    XH == "D" ~ 3,
    XH == "E" ~ 4,
    XH == "F" ~ 5,
    XH == "G" ~ 6,
    XH == "H" ~ 7,
    TRUE ~ NA_real_
  ))

#3.3. XI
sample_df <- sample_df %>%
  mutate(XI = case_when(
    XI == "A1" ~ 0,
    XI == "A2" ~ 1,
    XI == "A3" ~ 2,
    XI == "A4" ~ 3,
    XI == "A5" ~ 4,
    XI == "B1" ~ 5,
    XI == "B2" ~ 6,
    XI == "B3" ~ 7,
    XI == "B4" ~ 8,
    XI == "B5" ~ 9,
    XI == "C1" ~ 10,
    XI == "C2" ~ 11,
    XI == "C3" ~ 12,
    XI == "C4" ~ 13,
    XI == "C5" ~ 14,
    XI == "D1" ~ 15,
    XI == "D2" ~ 16,
    XI == "D3" ~ 17,
    XI == "D4" ~ 18,
    XI == "D5" ~ 19,
    XI == "E1" ~ 20,
    XI == "E2" ~ 21,
    XI == "E3" ~ 22,
    XI == "E4" ~ 23,
    XI == "E5" ~ 24,
    XI == "F1" ~ 25,
    XI == "F2" ~ 26,
    XI == "F3" ~ 27,
    XI == "F4" ~ 28,
    XI == "F5" ~ 29,
    XI == "G1" ~ 30,
    XI == "G2" ~ 31,
    XI == "G3" ~ 32,
    XI == "G4" ~ 33,
    XI == "G5" ~ 34,
    TRUE ~ NA_real_
  ))

#3.4. XJ
sample_df <- sample_df %>%
  mutate(XJ = case_when(
    XJ == "ANY" ~ 0,
    XJ == "MORTGAGE" ~ 1,
    XJ == "NONE" ~ 2,
    XJ == "OTHER" ~ 3,  
    XJ == "OWN" ~ 4,
    XJ == "RENT" ~ 5,
    TRUE ~ NA_real_
  ))

#3.5. XM (bulan)
sample_df <- sample_df %>%
  mutate(XM = case_when(
    XM == "January"   ~ 1,
    XM == "February"  ~ 2,
    XM == "March"     ~ 3,
    XM == "April"     ~ 4,
    XM == "May"       ~ 5,
    XM == "June"      ~ 6,
    XM == "July"      ~ 7,
    XM == "August"    ~ 8,
    XM == "September" ~ 9,
    XM == "October"   ~ 10,
    XM == "November"  ~ 11,
    XM == "December"  ~ 12,
    TRUE ~ NA_real_
  ))

#3.6. XN (status pinjaman)
sample_df <- sample_df %>%
  mutate(XN = case_when(
    XN == "Charged Off"       ~ 0,
    XN == "Current"           ~ 1,
    XN == "Default"           ~ 2,
    XN == "Fully Paid"        ~ 3,
    XN == "In Grace Period"   ~ 4,
    XN == "Late (16-30 days)" ~ 5,
    XN == "Late (31-120 days)"~ 6,
    TRUE ~ NA_real_
  ))

#3.7. XL (Verivication Status)
sample_df <- sample_df %>%
  mutate(XL = case_when(
    XL == "Not Verified"       ~ 0,
    XL == "Source Verified"    ~ 1,
    XL == "Verified"           ~ 2,
    TRUE ~ NA_real_
  ))

# =====================================================
#4 RENAME
# =====================================================

colnames(sample_df)
rename(nama_baru = nama_lama)
sample_df <- sample_df %>% 
  rename(
    ID                        = XA,
    ID_Anggota                = XB,
    Jumlah_Pinjaman           = XC,
    Jumlah_yang_Didanai       = XD,
    Jangka_Waktu              = XE,
    Tingkat_Bunga             = XF,
    Angsuran                  = XG,
    Tingkat_Pinjaman          = XH,
    Sub_Tingkat_Pinjaman      = XI,
    Status_Kepemilikan_Rumah  = XJ,
    Pendapatan_Tahunan        = XK,
    Status_Verifikasi         = XL,
    Tanggal_Pendanaan         = XM,
    Status_Pinjaman           = XN,
    Rasio_DTI                 = XO,
    Riwayat_Tunggakan_2_Tahun = XP,
    Garis_Kredit_Terawal      = XQ,
    Akun_Terbuka              = XR,
    Catatan_Publik            = XS,
    Saldo_Kredit_Bergulir     = XT,
    Tingkat_Pemanfaatan_Kredit_Bergulir = XU,
    Total_Akun_Kredit         = XV,
    Total_Pembayaran          = XW,
    Total_Pembayaran_Investor = XX,
    Bunga_Diterima            = XY
  )


colnames(sample_df)
View(sample_df)

# =====================================================
#4. Modelling
# =====================================================
# =====================================================
######REGRESI
# =====================================================

library(dplyr)    
library(magrittr) 

# 1. Pilih variabel yang dipakai 
df_model <- sample_df %>% 
  select(Status_Pinjaman, Jumlah_Pinjaman, Tingkat_Bunga, 
         Rasio_DTI, Pendapatan_Tahunan, 
         Status_Kepemilikan_Rumah, Status_Verifikasi)

# 2. Buang NA
df_model <- na.omit(df_model)

# 3. Mapping Status_Pinjaman angka → kategori
df_model <- df_model %>%
  mutate(
    Status_Label = case_when(
      Status_Pinjaman == 0 ~ "Fully Paid",
      Status_Pinjaman == 1 ~ "Charged Off",
      Status_Pinjaman == 2 ~ "Default",
      Status_Pinjaman == 3 ~ "Current",
      Status_Pinjaman == 4 ~ "Late 31-120",
      Status_Pinjaman == 5 ~ "Late 16-30",
      Status_Pinjaman == 6 ~ "In Grace Period",
      TRUE ~ "Other"
    ),
    # bikin biner: 1 = Charged Off / Default, 0 = lainnya
    Status_Biner = ifelse(Status_Label %in% c("Charged Off", "Default"), 1, 0)
  )

# Cek distribusi outcome
print(table(df_model$Status_Label))
print(table(df_model$Status_Biner))

# 4. Split Train-Test (80:20)
set.seed(123)
if (length(unique(df_model$Status_Biner)) > 1) {
  train_index <- createDataPartition(df_model$Status_Biner, p = 0.8, list = FALSE)
  
  train_df <- df_model[train_index, ]
  test_df  <- df_model[-train_index, ]
} else {
  stop("❌ Status_Biner hanya punya 1 kelas, logistic regression tidak bisa jalan. Cek kembali mapping Status_Pinjaman.")
}

# 5. Model Regresi Logistik
logit_model <- glm(
  Status_Biner ~ Jumlah_Pinjaman + Tingkat_Bunga +
    Rasio_DTI + Pendapatan_Tahunan +
    Status_Kepemilikan_Rumah + Status_Verifikasi,
  data = train_df,
  family = binomial
)

summary(logit_model)
# 6. Prediksi Probabilitas (Test Data)
test_df$pred_prob <- predict(logit_model, newdata = test_df, type = "response")
test_df$pred_class <- ifelse(test_df$pred_prob > 0.5, 1, 0)

# 7. Evaluasi Model (Confusion Matrix + Akurasi)
library(caret)
cm <- confusionMatrix(
  factor(test_df$pred_class),
  factor(test_df$Status_Biner),
  positive = "1"
)
print(cm)

# 8. Plot ROC Curve
library(pROC)
roc_obj <- roc(test_df$Status_Biner, test_df$pred_prob)

plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve - Logistic Regression")
abline(a = 0, b = 1, lty = 2, col = "red")
cat("AUC =", auc(roc_obj), "\n")

# 9. Plot Distribusi Probabilitas Prediksi
library(ggplot2)

ggplot(test_df, aes(x = factor(Status_Biner), y = pred_prob, fill = factor(Status_Biner))) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Prediksi Probabilitas vs Status Aktual",
    x = "Status Aktual (0 = Lancar, 1 = Gagal Bayar)",
    y = "Probabilitas Prediksi"
  ) +
  theme_minimal()

# 10. Plot Koefisien Model (Coef Plot)
library(broom)

coef_df <- tidy(logit_model, conf.int = TRUE, exponentiate = TRUE)

ggplot(coef_df %>% filter(term != "(Intercept)"),
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "darkgray") +
  coord_flip() +
  labs(
    title = "Coef Plot - Logistic Regression",
    x = "Variabel Prediktor",
    y = "Odds Ratio (exp(β))"
  ) +
  theme_minimal()

# 11. Plot Distribusi Probabilitas
library(ggplot2)
ggplot(test_df, aes(x = factor(Status_Biner), y = pred_prob, fill = factor(Status_Biner))) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Prediksi Probabilitas vs Status Aktual",
    x = "Status Aktual (0 = Lancar, 1 = Gagal Bayar)",
    y = "Probabilitas Prediksi"
  ) +
  theme_minimal()

# 12. Plot Koefisien Model (Coef Plot)
library(broom)
coef_df <- tidy(logit_model, conf.int = TRUE, exponentiate = TRUE)

ggplot(coef_df %>% filter(term != "(Intercept)"),
       aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "darkgray") +
  coord_flip() +
  labs(
    title = "Coef Plot - Logistic Regression",
    x = "Variabel Prediktor",
    y = "Odds Ratio (exp(β))"
  ) +
  theme_minimal()

# 13. Ketepatan Klasifikasi
library(caret)
library(MLmetrics)
library(pROC)

# Semua kelas yang mungkin (dari training)
all_classes <- sort(unique(train_df$Status_Biner))
y_true <- factor(test_df$Status_Biner, levels = all_classes)
pred_prob <- test_df$pred_prob

# Prediksi kelas berdasarkan threshold default 0.5
threshold <- 0.5
y_pred <- ifelse(pred_prob >= threshold, 1, 0)
y_pred <- factor(y_pred, levels = all_classes)

# Classification report per kelas
metrics_list <- lapply(all_classes, function(cls){
  if(sum(y_true==cls) > 0){   # cls ada di test set
    prec <- Precision(ifelse(y_true==cls,1,0), ifelse(y_pred==cls,1,0))
    rec  <- Recall(ifelse(y_true==cls,1,0), ifelse(y_pred==cls,1,0))
    f1   <- F1_Score(ifelse(y_true==cls,1,0), ifelse(y_pred==cls,1,0))
    supp <- sum(y_true==cls)
  } else {                     # cls tidak ada di test set
    prec <- NA; rec <- NA; f1 <- NA; supp <- 0
  }
  return(data.frame(Class=cls, Precision=prec, Recall=rec, F1_Score=f1, Support=supp))
})
report <- do.call(rbind, metrics_list)
cat("=== Classification Report ===\n")
print(report)

# Akurasi keseluruhan
accuracy <- Accuracy(as.numeric(as.character(y_pred)), as.numeric(as.character(y_true)))
cat("\nOverall Accuracy:", round(accuracy,3), "\n")

# ROC & AUC hanya jika ada ≥2 kelas
if(length(all_classes) >= 2 & length(unique(y_true)) >= 2){
  roc_obj <- roc(y_true, pred_prob)
  auc_val <- auc(roc_obj)
  best_threshold <- coords(roc_obj, "best", ret="threshold", transpose = FALSE)
  cat("\nROC-AUC:", round(auc_val,4), "\n")
  cat("Threshold optimal (Youden index):", round(best_threshold,4), "\n")
} else {
  cat("\n⚠️ ROC-AUC hanya bisa dihitung jika ada ≥2 kelas di test set.\n")
}

# Optional: Confusion Matrix sederhana jika ada ≥1 kelas
if(length(unique(y_true)) >= 1){
  cm_table <- table(Prediction=y_pred, Reference=y_true)
  cat("\n=== Confusion Matrix ===\n")
  print(cm_table)
}

# =====================================================
######RANDOM FOREST & KLASIFIKASI (SMOTE)
# =====================================================

library(randomForest)
library(caret)

# 1. Samakan kolom antara train & test
common_cols <- intersect(
  setdiff(names(train_df_balanced), "Status_Pinjaman"), # semua fitur kecuali target
  names(test_df)                                        # fitur yang ada di test
)

# train: hanya fitur common + target
train_df_aligned <- train_df_balanced[, c(common_cols, "Status_Pinjaman")]

# test: hanya fitur common (tanpa target)
test_df_aligned  <- test_df[, common_cols]

# 2. Model Random Forest
set.seed(123)
rf_model_bal <- randomForest(
  Status_Pinjaman ~ ., 
  data = train_df_aligned,
  ntree = 500,
  mtry = sqrt(ncol(train_df_aligned)-1),
  importance = TRUE
)

# 3. Prediksi di test set
rf_pred_bal <- predict(rf_model_bal, newdata = test_df_aligned)

# 4. Akurasi, Precision, Recall, F2-Score
cm_table <- conf_matrix_bal$table
classes  <- colnames(cm_table)

# Precision = TP / (TP+FP)
precision <- diag(cm_table) / rowSums(cm_table)

# Recall = TP / (TP+FN)
recall    <- diag(cm_table) / colSums(cm_table)

# Handle NaN/Inf
precision[is.na(precision) | is.infinite(precision)] <- 0
recall[is.na(recall) | is.infinite(recall)] <- 0

# Buat dataframe metrics
metrics <- data.frame(
  Class = classes,
  Precision = precision,
  Recall = recall
)

# Hitung F2-Score
beta <- 2
metrics$F2 <- (1 + beta^2) * (metrics$Precision * metrics$Recall) /
  ((beta^2 * metrics$Precision) + metrics$Recall)

# Handle NaN/Inf pada F2
metrics$F2[is.na(metrics$F2) | is.infinite(metrics$F2)] <- 0

# Tampilkan per kelas
print(metrics)

accuracy <- mean(rf_pred_bal == test_df$Status_Pinjaman)
cat("Accuracy:", round(accuracy_bal*100, 2), "%\n")
cat("\n Precision :", round(mean(metrics$Precision), 4),
    "\n Recall    :", round(mean(metrics$Recall), 4),
    "\n F2-Score  :", round(mean(metrics$F2), 4), "\n")
accuracy

# ======================================
#5. COMPARISON HISTO KKETEPATAN KLASIFIKASI
# ======================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Hasil evaluasi (isi sesuai dari output kamu)
results <- data.frame(
  Model = c("Logistic Regression", "Random Forest (SMOTE)"),
  Accuracy = c(accuracy, 0.8504), 
  Precision = c(0.546, 0.9232),
  Recall = c(0.545, 0.7802),
  F1 = c(0.541, 0.796)
)

# Ubah ke long format
results_long <- results %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Score")

# Plot barplot
ggplot(results_long, aes(x = Metric, y = Score, fill = Model)) +
  geom_col(position = "dodge", width = 0.6, alpha = 0.9) +
  geom_text(aes(label = percent(Score, accuracy = 0.1)),
            position = position_dodge(width = 0.6), vjust = -0.4, size = 3.5) +
  labs(
    title = "Perbandingan Metrik Evaluasi Model",
    x = "Metrik",
    y = "Skor"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


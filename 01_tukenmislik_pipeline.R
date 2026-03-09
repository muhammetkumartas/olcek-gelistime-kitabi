# ==============================================================================
# ÖLÇEK 1: Tükenmişlik ve Geri Çekilme Ölçeği
# Tam Psikometrik Analiz Pipeline'ı
# ==============================================================================
# Faktörler : SGC (Sosyal Geri Çekilme)  → SGC1–SGC6
#             DGC (Duygusal Geri Çekilme) → DGC1–DGC6
#             DT  (Duygusal Tükenme)      → DT1–DT6
# Başlangıç : 18 madde, 5'li Likert, N=687 ham
# Nihai     : 18 madde, 3 faktör, N=517
# Ters kodlu: DT5, DGC2
# ==============================================================================

# ── 0. PAKETLER ──────────────────────────────────────────────────────────────
pkgs <- c("tidyverse", "psych", "mice", "MVN", "lavaan",
          "semTools", "corrplot", "ggplot2", "openxlsx", "naniar")
invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))

# ── 1. SABITLER ──────────────────────────────────────────────────────────────
set.seed(42)
GECERLI_KOD   <- 1:5
TERS_MADDELER <- c("DT5", "DGC2")
EKSIK_ESIK    <- 0.10
STRAIGHT_SD   <- 0.10
YAS_MIN <- 22; YAS_MAX <- 55
MAHA_P  <- 0.001
YUK_MIN <- 0.32
DELTA_CAPRAZ <- 0.10
N_FAKTOR <- 3

ALT_BOYUT <- list(
  SGC = paste0("SGC", 1:6),
  DGC = paste0("DGC", 1:6),
  DT  = paste0("DT",  1:6)
)
MADDE_COLS <- unname(unlist(ALT_BOYUT))

# Görsel renkler
MAVI  <- "#2C75B6"; KRMZ <- "#C0392B"
YESIL <- "#27AE60"; TUR  <- "#16A085"; MOR <- "#8E44AD"

# ── 2. VERİ OKUMA ────────────────────────────────────────────────────────────
df <- read.csv("data/tukenmislik_ham.csv")
cat(sprintf("\n=== ADIM 1: VERİ OKUMA ===\nHam veri: %d satir x %d sutun\n",
            nrow(df), ncol(df)))
print(head(df, 3))
cat("\nDeğişken tipleri:\n"); print(sapply(df, class))

# ── 3. AYKIRI DEĞER TEMİZLİĞİ ────────────────────────────────────────────────
cat("\n=== ADIM 2: AYKIRI DEĞER TEMİZLİĞİ ===\n")

# 3a. Demografik aralık hataları
yas_hata <- !is.na(df$Yas) & (df$Yas < YAS_MIN | df$Yas > YAS_MAX)
med_hata <- !is.na(df$Medeni_Durum) & !df$Medeni_Durum %in% 1:3
cat(sprintf("Yas aykiri : %d  |  Medeni aykiri: %d\n", sum(yas_hata), sum(med_hata)))
df$Yas[yas_hata]          <- NA
df$Medeni_Durum[med_hata] <- NA

# 3b. Madde aralık hataları
alik_hata_toplam <- 0
for (m in MADDE_COLS) {
  hata <- !is.na(df[[m]]) & !df[[m]] %in% GECERLI_KOD
  if (any(hata)) {
    df[[m]][hata] <- NA
    alik_hata_toplam <- alik_hata_toplam + sum(hata)
    cat(sprintf("  %s: %d aralik hatasi NA yapildi\n", m, sum(hata)))
  }
}
if (alik_hata_toplam == 0) cat("Aralik hatasi: yok\n")

# 3c. Straight-liner tespiti (tüm maddelere aynı yanıt)
satir_sd <- apply(df[, MADDE_COLS], 1, function(x) sd(x, na.rm = TRUE))
sl_idx   <- which(!is.na(satir_sd) & satir_sd < STRAIGHT_SD)
cat(sprintf("Straight-liner (satir SD < %.2f): %d kisi — madde satirlari NA yapildi\n",
            STRAIGHT_SD, length(sl_idx)))
df[sl_idx, MADDE_COLS] <- NA
cat(sprintf("Temizlik sonrasi satir sayisi: %d (degismedi, NA yapildi)\n", nrow(df)))

# ── 4. EKSİK VERİ ANALİZİ ────────────────────────────────────────────────────
cat("\n=== ADIM 3: EKSİK VERİ ANALİZİ ===\n")
eksik_oran <- colMeans(is.na(df[, MADDE_COLS])) * 100
cat("Madde eksik orani (buyukten kucuge):\n")
print(round(sort(eksik_oran, decreasing = TRUE), 2))

# Şekil 1: Madde eksik veri bar grafiği
p1 <- ggplot(data.frame(Madde = names(eksik_oran), Oran = eksik_oran),
             aes(x = reorder(Madde, Oran), y = Oran,
                 fill = case_when(Oran > 10 ~ "Kritik", Oran > 5 ~ "Uyari",
                                  TRUE ~ "Normal"))) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = c(5, 10), linetype = "dashed",
             color = c("orange", KRMZ), linewidth = 0.9) +
  geom_text(aes(label = sprintf("%.1f%%", Oran)), hjust = -0.15, size = 3.2) +
  scale_fill_manual(values = c(Normal = MAVI, Uyari = "orange", Kritik = KRMZ),
                    name = "Durum") +
  coord_flip() + ylim(0, max(eksik_oran) * 1.25) +
  labs(title = "Şekil 1. Madde Düzeyinde Eksik Veri Oranları (%)",
       subtitle = "Turuncu kesik: %5 uyarı | Kırmızı kesik: %10 kritik eşik",
       x = NULL, y = "Eksik Oran (%)") +
  theme_minimal(base_size = 12) + theme(legend.position = "right")
ggsave("figures/sekil01_eksik_veri.png", p1, width = 8, height = 6, dpi = 150)
cat("-> figures/sekil01_eksik_veri.png kaydedildi\n")

# MICE ile çoklu atama
satir_eksik <- rowMeans(is.na(df[, MADDE_COLS]))
df <- df[satir_eksik <= EKSIK_ESIK, ]
cat(sprintf("Satir filtresi (>%%10 eksik): %d satir kaldi\n", nrow(df)))

df_mice <- df[, c("Yas", "Cinsiyet", "Egitim", "Medeni_Durum", MADDE_COLS)]
imp  <- mice(df_mice, m = 5, method = "pmm", seed = 42, printFlag = FALSE)
df_c <- complete(imp, 1)
df[, colnames(df_c)] <- df_c
cat("MICE (m=5, pmm, seed=42) tamamlandi.\n")

# ── 5. TERS KODLAMA ───────────────────────────────────────────────────────────
cat("\n=== ADIM 4: TERS KODLAMA ===\n")
df_tc      <- df
ters_aktif <- intersect(TERS_MADDELER, MADDE_COLS)
for (m in ters_aktif) df_tc[[m]] <- 6 - df_tc[[m]]
cat(sprintf("Ters kodlanan (%d madde): %s\n",
            length(ters_aktif), paste(ters_aktif, collapse = ", ")))
cat("Formul: yeni_deger = 6 - eski_deger (5'li Likert)\n")

# ── 6. TANIMLAYICI İSTATİSTİKLER ──────────────────────────────────────────────
cat("\n=== ADIM 5: TANIMLAYICI İSTATİSTİKLER ===\n")
tanimlayici <- df_tc[, MADDE_COLS] %>%
  summarise(across(everything(), list(
    Ort = ~round(mean(.), 3), SS  = ~round(sd(.), 3),
    Min = ~min(.),            Max = ~max(.),
    Carpiklik = ~round(psych::skew(.), 3),
    Basiklik  = ~round(psych::kurtosi(.), 3)
  ))) %>%
  tidyr::pivot_longer(everything(),
                      names_to  = c("Madde", ".value"),
                      names_sep = "_")
cat("Tanimlayici istatistikler:\n"); print(tanimlayici, n = Inf)
cat(sprintf("\nCarpiklik araligi: [%.3f, %.3f]  (|sk| < 2 kabul)\n",
            min(tanimlayici$Carpiklik), max(tanimlayici$Carpiklik)))
cat(sprintf("Basiklik  araligi: [%.3f, %.3f]  (|ku| < 7 kabul)\n",
            min(tanimlayici$Basiklik), max(tanimlayici$Basiklik)))

# ── 7. NORMALLİK ANALİZİ ─────────────────────────────────────────────────────
cat("\n=== ADIM 6: NORMALLİK ANALİZİ ===\n")
mvn_s <- MVN::mvn(df_tc[, MADDE_COLS], mvn_test = "mardia", univariate_test = "AD")
cat("Mardia Çok Değişkenli Normallik:\n")
print(mvn_s$multivariate_normality)
cat("\nNOT: N > 500'de normallik testleri küçük sapmalara karşı aşırı hassastır.\n")
cat("     |sk| < 2 ve |ku| < 7 pratik kriterleri sağlandığında\n")
cat("     dağılıma daha az duyarlı PAF yöntemi tercih edilir (Fabrigar vd., 1999).\n")

# Histogramlar
png("figures/sekil02_histogramlar.png", width = 1200, height = 900, res = 120)
par(mfrow = c(3, 6), mar = c(3, 2, 2, 1))
for (m in MADDE_COLS) {
  hist(df_tc[[m]], breaks = 5, col = MAVI, border = "white",
       main = m, xlab = "", las = 1, xlim = c(1, 5))
}
dev.off()
cat("-> figures/sekil02_histogramlar.png kaydedildi\n")

# ── 8. MAHALANOBİS UZAKLIĞI ──────────────────────────────────────────────────
cat("\n=== ADIM 7: MAHALANOBİS ÇOK DEĞİŞKENLİ AYKIRI DEĞER ===\n")
maha_data <- df_tc[, MADDE_COLS]
maha_dist <- mahalanobis(maha_data, colMeans(maha_data), cov(maha_data))
maha_p    <- pchisq(maha_dist, df = ncol(maha_data), lower.tail = FALSE)
aykiri_n  <- sum(maha_p < MAHA_P)
cat(sprintf("p < %.3f olan cok degiskenli aykiri: %d / %d\n",
            MAHA_P, aykiri_n, nrow(df_tc)))
if (aykiri_n > 0) {
  df_tc <- df_tc[maha_p >= MAHA_P, ]
  cat(sprintf("Temizlik sonrasi N: %d\n", nrow(df_tc)))
} else cat("Aykiri deger yok — N degismedi.\n")

# Mahalanobis saçılım grafiği
p_maha <- ggplot(data.frame(Index = 1:length(maha_dist), D2 = maha_dist,
                             Aykiri = maha_p < MAHA_P),
                 aes(x = Index, y = D2, color = Aykiri)) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_hline(yintercept = qchisq(1 - MAHA_P, df = length(MADDE_COLS)),
             linetype = "dashed", color = KRMZ, linewidth = 1) +
  scale_color_manual(values = c("FALSE" = MAVI, "TRUE" = KRMZ),
                     labels = c("Normal", "Aykiri"), name = "") +
  labs(title = "Şekil 3. Mahalanobis D² Saçılım Grafiği",
       x = "Katılımcı İndeksi", y = "D² Uzaklığı") +
  theme_minimal(base_size = 12)
ggsave("figures/sekil03_mahalanobis.png", p_maha, width = 8, height = 5, dpi = 150)

# ── 9. KMO VE BARTLETT ───────────────────────────────────────────────────────
cat("\n=== ADIM 8: KMO VE BARTLETT ===\n")
kor_mat <- cor(df_tc[, MADDE_COLS], use = "complete.obs")
kmo     <- psych::KMO(kor_mat)
bart    <- psych::cortest.bartlett(kor_mat, n = nrow(df_tc))
kmo_yorum <- cut(kmo$MSA, c(0,.50,.60,.70,.80,.90,1),
                 c("Kabul Edilemez","Yetersiz","Orta","Iyi","Cok Iyi","Mukemmel"),
                 right = FALSE)
cat(sprintf("KMO Genel MSA : %.3f  [%s]\n", kmo$MSA, kmo_yorum))
cat(sprintf("Bartlett      : χ²(df=%d) = %.2f, p = %.4f\n",
            bart$df, bart$chisq, bart$p.value))
cat("\nMadde bazında MSA:\n"); print(round(sort(kmo$MSAi), 3))
prob_msa <- kmo$MSAi[kmo$MSAi < 0.60]
if (length(prob_msa) > 0) {
  cat("\nPROBLEMLİ maddeler (MSA < .60):\n"); print(round(prob_msa, 3))
} else cat("\nTum maddeler MSA >= .60 — sorun yok.\n")

# Korelasyon ısı haritası
png("figures/sekil04_korelasyon_matrisi.png", width = 900, height = 820, res = 120)
renk_alt <- c(rep(MAVI, 6), rep(TUR, 6), rep(MOR, 6))
corrplot::corrplot(kor_mat, method = "color", type = "lower",
                   tl.col = renk_alt, tl.cex = 0.8, diag = FALSE,
                   col = corrplot::COL2("RdBu", 200),
                   title = "Şekil 4. Korelasyon Matrisi (Alt Üçgen)",
                   mar = c(0, 0, 2, 0))
dev.off()
cat("-> figures/sekil04_korelasyon_matrisi.png kaydedildi\n")

# ── 10. FAKTÖR SAYISI BELIRLEME ───────────────────────────────────────────────
cat("\n=== ADIM 9: FAKTÖR SAYISI BELİRLEME ===\n")
oz    <- eigen(kor_mat)$values
oz_df <- data.frame(Faktor  = seq_along(oz),
                    Ozd     = round(oz, 3),
                    AcikVar = round(oz / sum(oz) * 100, 2)) %>%
  dplyr::mutate(Kumul = cumsum(AcikVar))
cat("Özdeğer tablosu (ilk 8):\n"); print(head(oz_df, 8), row.names = FALSE)
cat(sprintf("Kaiser kriteri (λ ≥ 1): %d faktör\n", sum(oz >= 1)))

p_scree <- ggplot(head(oz_df, 10), aes(x = Faktor, y = Ozd)) +
  geom_line(color = "#333333", linewidth = 1) +
  geom_point(aes(color = Ozd >= 1), size = 5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = KRMZ, linewidth = 0.9) +
  scale_color_manual(values = c("TRUE" = KRMZ, "FALSE" = MAVI),
                     labels = c("λ < 1", "λ ≥ 1"), name = "") +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Şekil 5. Scree Plot — Tükenmişlik Ölçeği",
       x = "Faktör Sırası", y = "Özdeğer (λ)") +
  theme_minimal(base_size = 12) + theme(legend.position = "top")
ggsave("figures/sekil05_scree_plot.png", p_scree, width = 7, height = 5, dpi = 150)
cat("-> figures/sekil05_scree_plot.png kaydedildi\n")

# Paralel Analiz
png("figures/sekil06_paralel_analiz.png", width = 800, height = 600, res = 120)
pa <- psych::fa.parallel(df_tc[, MADDE_COLS], n.iter = 1000, fm = "pa", fa = "fa",
                          main = "Şekil 6. Paralel Analiz", error.bars = TRUE)
dev.off()
cat(sprintf("Paralel Analiz önerisi : %d faktör\n", pa$nfact))

map_s <- psych::vss(df_tc[, MADDE_COLS], n = 8, plot = FALSE)
cat(sprintf("MAP testi önerisi      : %d faktör\n", which.min(map_s$map)))
cat(sprintf("\n>>> KARAR: N_FAKTOR = %d (PA + MAP + teorik yapı)\n\n", N_FAKTOR))

# ── 11. BAŞLANGIÇ AFA ────────────────────────────────────────────────────────
cat("=== ADIM 10: BAŞLANGIÇ AFA (PAF + OBLİMİN) ===\n")
fa_ana <- psych::fa(df_tc[, MADDE_COLS], nfactors = N_FAKTOR,
                    fm = "pa", rotate = "oblimin", warnings = FALSE)
cat("Faktör Yük Matrisi:\n")
print(fa_ana$loadings, digits = 3, cutoff = 0.10)
cat(sprintf("\nUyum: RMSEA=%.3f | TLI=%.3f | BIC=%.1f\n",
            fa_ana$RMSEA[1], fa_ana$TLI, fa_ana$BIC))
cat(sprintf("Phi matrisi (faktörler arası r):\n"))
print(round(fa_ana$Phi, 3))
varyans_pct <- sum(fa_ana$values[1:N_FAKTOR]) / length(MADDE_COLS) * 100
cat(sprintf("Açıklanan varyans: %.1f%%\n", varyans_pct))

# ── 12. MADDE ELİMİNASYONU ────────────────────────────────────────────────────
cat("\n=== ADIM 11: MADDE ELİMİNASYONU ===\n")
yuk_mat  <- unclass(fa_ana$loadings)
kom_vek  <- fa_ana$communality
maks_yuk <- apply(abs(yuk_mat), 1, max)
maks_f   <- apply(abs(yuk_mat), 1, which.max)
capraz_fark <- apply(abs(yuk_mat), 1, function(x) {
  s <- sort(x, decreasing = TRUE); if (length(s) >= 2) s[1] - s[2] else 1
})
capraz_n <- apply(abs(yuk_mat) >= YUK_MIN, 1, sum)

madde_karar <- data.frame(
  Madde       = rownames(yuk_mat),
  Maks_Yuk    = round(maks_yuk, 3),
  Faktor      = paste0("PA", maks_f),
  Komunalite  = round(kom_vek, 3),
  Capraz_Fark = round(capraz_fark, 3),
  stringsAsFactors = FALSE
)
madde_karar$Neden <- ""
madde_karar$Neden[maks_yuk < YUK_MIN] <- "Dusuk yuk (<.32)"
madde_karar$Neden[capraz_n > 1 & capraz_fark < DELTA_CAPRAZ] <- "Capraz yuk (Δ<.10)"
madde_karar$Karar <- ifelse(madde_karar$Neden == "", "Tut ✓", "Ele ✗")

cat("Madde eleme tablosu:\n"); print(madde_karar, row.names = FALSE)

elenecek <- madde_karar$Madde[grepl("Ele", madde_karar$Karar)]
kalacak  <- madde_karar$Madde[grepl("Tut", madde_karar$Karar)]
cat(sprintf("\nElenen  (%d): %s\n", length(elenecek),
            if (length(elenecek) > 0) paste(elenecek, collapse = ", ") else "—"))
cat(sprintf("Kalan   (%d): %s\n", length(kalacak), paste(kalacak, collapse = ", ")))

# Komünalite grafiği
kom_df <- data.frame(Madde = names(kom_vek), h2 = round(kom_vek, 3),
                     Durum = ifelse(kom_vek >= .40, "Yeterli (≥.40)", "Düşük (<.40)"),
                     Karar = madde_karar$Karar)
p_kom <- ggplot(kom_df, aes(x = reorder(Madde, h2), y = h2,
                             fill = Durum,
                             alpha = ifelse(grepl("Tut", Karar), 1.0, 0.35))) +
  geom_col(width = 0.75, color = "white") +
  geom_hline(yintercept = c(.30, .40), linetype = c("solid","dashed"),
             color = c(KRMZ, "orange"), linewidth = 0.9) +
  geom_text(aes(label = sprintf("%.2f", h2)), hjust = -0.15, size = 3.1) +
  scale_fill_manual(values = c("Yeterli (≥.40)" = MAVI, "Düşük (<.40)" = KRMZ), name = "") +
  scale_alpha_identity() + coord_flip() + ylim(0, 1.15) +
  labs(title = "Şekil 7. Madde Komünaliteleri (h²)",
       subtitle = "Şeffaf çubuklar: elenen maddeler | Kırmızı: h² < .30 kritik eşik",
       x = NULL, y = "h²") +
  theme_minimal(base_size = 12)
ggsave("figures/sekil07_komunaliteler.png", p_kom, width = 7, height = 6, dpi = 150)
cat("-> figures/sekil07_komunaliteler.png kaydedildi\n")

# ── 13. RAFİNE AFA ────────────────────────────────────────────────────────────
cat("\n=== ADIM 12: RAFİNE AFA ===\n")
df_rafine <- df_tc[, kalacak]
fa_rafine <- psych::fa(df_rafine, nfactors = N_FAKTOR,
                       fm = "pa", rotate = "oblimin", warnings = FALSE)
cat("Rafine AFA Yük Matrisi:\n")
print(fa_rafine$loadings, digits = 3, cutoff = 0.30)
varyans_rafine <- sum(fa_rafine$values[1:N_FAKTOR]) / ncol(df_rafine) * 100
cat(sprintf("Açıklanan varyans (rafine): %.1f%%\n", varyans_rafine))
cat(sprintf("Uyum: RMSEA=%.3f | TLI=%.3f | BIC=%.1f\n",
            fa_rafine$RMSEA[1], fa_rafine$TLI, fa_rafine$BIC))

# Faktör yük ısı haritası
yuk_raf_df <- as.data.frame(unclass(fa_rafine$loadings)) %>%
  tibble::rownames_to_column("Madde") %>%
  tidyr::pivot_longer(-Madde, names_to = "Faktor", values_to = "Yuk")
p_yuk <- ggplot(yuk_raf_df, aes(x = Faktor, y = reorder(Madde, abs(Yuk)), fill = Yuk)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(abs(Yuk) >= 0.30, sprintf("%.2f", Yuk), "")),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_gradient2(low = "#C0392B", mid = "white", high = "#2C75B6",
                       midpoint = 0, limits = c(-1, 1), name = "Yük") +
  labs(title = "Şekil 8. Rafine AFA Faktör Yük Isı Haritası",
       subtitle = "Gösterilen değerler: |λ| ≥ .30",
       x = "Faktör", y = NULL) +
  theme_minimal(base_size = 11)
ggsave("figures/sekil08_faktor_yuk_isisi.png", p_yuk, width = 6, height = 7, dpi = 150)
cat("-> figures/sekil08_faktor_yuk_isisi.png kaydedildi\n")

# ── 14. GÜVENİRLİK ANALİZİ ───────────────────────────────────────────────────
cat("\n=== ADIM 13: GÜVENİRLİK ANALİZİ ===\n")

omega_al <- function(x) {
  o <- tryCatch(suppressWarnings(suppressMessages(
    psych::omega(x, nfactors = 1, plot = FALSE))), error = function(e) NULL)
  if (is.null(o)) return(NA_real_)
  round(tryCatch(o$omega_t, error = function(e) o$omega.tot), 3)
}

# Alt ölçek güvenirliği
guv_liste <- lapply(names(ALT_BOYUT), function(ab) {
  mds <- intersect(ALT_BOYUT[[ab]], kalacak)
  if (length(mds) < 2) return(NULL)
  a <- psych::alpha(df_rafine[, mds, drop = FALSE], warnings = FALSE)
  data.frame(
    Alt_Olcek  = ab,
    N_Madde    = length(mds),
    Cronbach_a = round(a$total$raw_alpha, 3),
    GA_Alt     = round(a$total$raw_alpha - 1.96 * a$total$ase, 3),
    GA_Ust     = round(a$total$raw_alpha + 1.96 * a$total$ase, 3),
    McDonald_w = omega_al(df_rafine[, mds, drop = FALSE]),
    Yorum      = as.character(cut(a$total$raw_alpha,
                   c(0,.60,.70,.80,.90,1),
                   c("Yetersiz","Sınırda","Kabul","İyi","Mükemmel"),
                   right = FALSE)),
    stringsAsFactors = FALSE
  )
})
guv_df <- dplyr::bind_rows(Filter(Negate(is.null), guv_liste))
cat("Alt ölçek güvenirlik katsayıları:\n"); print(guv_df, row.names = FALSE)

# Tüm ölçek alfası
alpha_full <- psych::alpha(df_rafine, warnings = FALSE)
cat(sprintf("\nTüm Ölçek α = %.3f  [%%.95 GA: %.3f – %.3f]\n",
            alpha_full$total$raw_alpha,
            alpha_full$total$raw_alpha - 1.96 * alpha_full$total$ase,
            alpha_full$total$raw_alpha + 1.96 * alpha_full$total$ase))

# Madde-toplam korelasyonları
mt_kor <- as.data.frame(alpha_full$item.stats) %>%
  tibble::rownames_to_column("Madde") %>%
  dplyr::select(Madde, Ort = mean, SS = sd, r_duzeltilmis = r.drop) %>%
  dplyr::mutate(across(where(is.numeric), ~round(., 3)),
                Kalite = dplyr::case_when(r_duzeltilmis >= .40 ~ "İyi ✓",
                                          r_duzeltilmis >= .30 ~ "Sınırda ⚠",
                                          TRUE ~ "Düşük ✗"))
cat("\nMadde-toplam korelasyonları:\n"); print(mt_kor, row.names = FALSE)

# Alfa sil-ve-bak
alp_del <- as.data.frame(alpha_full$alpha.drop) %>%
  tibble::rownames_to_column("Madde") %>%
  dplyr::select(Madde, a_Silinirse = raw_alpha) %>%
  dplyr::mutate(across(where(is.numeric), ~round(., 3)))
cat("\nMadde silinirse alfa:\n"); print(alp_del, row.names = FALSE)

# Güvenirlik grafiği
p_alpha <- ggplot(guv_df, aes(x = Alt_Olcek, y = Cronbach_a,
                               ymin = GA_Alt, ymax = GA_Ust, color = Alt_Olcek)) +
  geom_point(size = 6) +
  geom_errorbar(width = 0.18, linewidth = 1.3) +
  geom_hline(yintercept = c(.70, .80, .90),
             linetype = "dashed",
             color    = c("#F46D43", "#FDAE61", "#66C2A5"),
             linewidth = 0.8) +
  geom_text(aes(label = sprintf("α=%.3f\nω=%.3f", Cronbach_a, McDonald_w)),
            vjust = -1.4, size = 3.8, fontface = "bold") +
  scale_color_manual(values = c(SGC = MAVI, DGC = TUR, DT = MOR)) +
  ylim(0.5, 1.1) +
  labs(title = "Şekil 9. Alt Ölçek Güvenirlik Katsayıları (%95 GA)",
       x = NULL, y = "Cronbach α") +
  theme_minimal(base_size = 12) + theme(legend.position = "none")
ggsave("figures/sekil09_guvenirlik.png", p_alpha, width = 7, height = 5, dpi = 150)
cat("-> figures/sekil09_guvenirlik.png kaydedildi\n")

# ── 15. DOĞRULAYICI FAKTÖR ANALİZİ ──────────────────────────────────────────
cat("\n=== ADIM 14: DOĞRULAYICI FAKTÖR ANALİZİ (DFA) ===\n")
set.seed(2024)
n_yarim  <- floor(nrow(df_tc) / 2)
idx_afa2 <- sample(nrow(df_tc), n_yarim)
df_dfa   <- df_tc[-idx_afa2, ]
cat(sprintf("DFA örneklem boyutu: %d\n", nrow(df_dfa)))

sgc_kal <- intersect(ALT_BOYUT$SGC, kalacak)
dgc_kal <- intersect(ALT_BOYUT$DGC, kalacak)
dt_kal  <- intersect(ALT_BOYUT$DT,  kalacak)

model_dfa <- sprintf(
  "SGC =~ %s\nDGC =~ %s\nDT  =~ %s",
  paste(sgc_kal, collapse = " + "),
  paste(dgc_kal, collapse = " + "),
  paste(dt_kal,  collapse = " + ")
)
cat("DFA modeli:\n"); cat(model_dfa, "\n\n")

fit_dfa <- lavaan::cfa(model_dfa, data = df_dfa,
                        estimator = "MLR", std.lv = TRUE, missing = "fiml")

uyum_ind <- lavaan::fitMeasures(fit_dfa,
  c("chisq","df","pvalue","chisq.scaled","df.scaled","pvalue.scaled",
    "cfi","tli","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr","nfi"))
cat("DFA Uyum İndeksleri:\n"); print(round(uyum_ind, 3))

# Referans değerler
cat("\nKriter eşikleri:\n")
cat("  CFI/TLI >= .95 (kabul), >= .97 (iyi)\n")
cat("  RMSEA   <  .06 (iyi),   < .08 (kabul)\n")
cat("  SRMR    <  .08 (kabul)\n")

std_coz <- lavaan::standardizedSolution(fit_dfa)
yuk_tab <- std_coz[std_coz$op == "=~",
                   c("lhs", "rhs", "est.std", "se", "z", "pvalue")]
names(yuk_tab) <- c("Faktor", "Madde", "Beta", "SE", "z", "p")
cat("\nStandartlaştırılmış faktör yükleri:\n"); print(yuk_tab, row.names = FALSE)

# DFA path diyagramı (metin tabanlı)
cat("\nModifikasyon İndeksleri (MI > 5):\n")
mi <- lavaan::modindices(fit_dfa, sort. = TRUE)
print(mi[mi$mi > 5, c("lhs","op","rhs","mi","sepc.all")][1:min(10, nrow(mi[mi$mi > 5,])),])

# ── 16. GEÇERLİLİK (AVE, CR, HTMT) ─────────────────────────────────────────
cat("\n=== ADIM 15: GEÇERLİLİK ANALİZİ ===\n")
ave_cr_liste <- lapply(list(SGC = sgc_kal, DGC = dgc_kal, DT = dt_kal),
                       function(mds) {
  std_fn <- std_coz[std_coz$op == "=~" &
                    std_coz$rhs %in% mds, ]
  lambda  <- std_fn$est.std
  ave     <- mean(lambda^2)
  cr      <- sum(lambda)^2 / (sum(lambda)^2 + sum(1 - lambda^2))
  data.frame(AVE = round(ave, 3), CR = round(cr, 3), sqrt_AVE = round(sqrt(ave), 3))
})
ave_cr_df <- dplyr::bind_rows(ave_cr_liste, .id = "Faktor")
cat("Yakınsak geçerlilik (AVE ≥ .50 ✓ | CR ≥ .70 ✓):\n")
print(ave_cr_df, row.names = FALSE)

htmt_res <- semTools::htmt(model_dfa, data = df_dfa)
cat("\nHTMT Matrisi (< .85 kabul | < .90 maksimum):\n"); print(round(htmt_res, 3))

# ── 17. NİHAİ ÖLÇEK VE PUANLAMA ─────────────────────────────────────────────
cat("\n=== ADIM 16: NİHAİ ÖLÇEK VE PUANLAMA ===\n")
df_son         <- df_tc[, c("ID", "Yas", "Cinsiyet", kalacak)]
df_son$Puan_SGC    <- rowMeans(df_son[, sgc_kal], na.rm = TRUE)
df_son$Puan_DGC    <- rowMeans(df_son[, dgc_kal], na.rm = TRUE)
df_son$Puan_DT     <- rowMeans(df_son[, dt_kal],  na.rm = TRUE)
df_son$Puan_Toplam <- rowMeans(df_son[, c("Puan_SGC","Puan_DGC","Puan_DT")])
cat("\nAlt ölçek puan özeti:\n")
print(sapply(df_son[, c("Puan_SGC","Puan_DGC","Puan_DT","Puan_Toplam")],
             function(x) c(Ort = round(mean(x), 3), SS = round(sd(x), 3),
                           Min = round(min(x), 2),  Max = round(max(x), 2))))

# Puan dağılım grafiği
puan_uzun <- df_son %>%
  dplyr::select(ID, Puan_SGC, Puan_DGC, Puan_DT, Puan_Toplam) %>%
  tidyr::pivot_longer(-ID, names_to = "Boyut", values_to = "Puan")
p_dist <- ggplot(puan_uzun, aes(x = Puan, fill = Boyut)) +
  geom_density(alpha = 0.65, color = "white", linewidth = 0.8) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  facet_wrap(~Boyut, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c(Puan_SGC = MAVI, Puan_DGC = TUR,
                                Puan_DT = MOR, Puan_Toplam = YESIL)) +
  labs(title = "Şekil 10. Alt Ölçek Puan Dağılımları",
       subtitle = "Kesik çizgi: ölçek orta noktası (3.0)",
       x = "Puan (1–5)", y = "Yoğunluk") +
  theme_minimal(base_size = 11) + theme(legend.position = "none")
ggsave("figures/sekil10_puan_dagilimi.png", p_dist, width = 8, height = 6, dpi = 150)
cat("-> figures/sekil10_puan_dagilimi.png kaydedildi\n")

# ── 18. EXCEL RAPORU ─────────────────────────────────────────────────────────
cat("\n=== ADIM 17: EXCEL RAPORU ===\n")
wb <- openxlsx::createWorkbook()
ust <- openxlsx::createStyle(fontColour = "white", fgFill = "#1F4E79",
                               textDecoration = "bold", halign = "center")

ws <- function(nm, veri) {
  openxlsx::addWorksheet(wb, nm)
  openxlsx::writeData(wb, nm, veri, headerStyle = ust)
  openxlsx::setColWidths(wb, nm, cols = 1:ncol(veri),
                         widths = "auto")
}
ws("Tanimlayici",  tanimlayici)
ws("Ozdeğerler",   head(oz_df, 10))
ws("KMO_MSA",      data.frame(Madde = names(kmo$MSAi),
                               MSA   = round(kmo$MSAi, 3)))
ws("Madde_Eleme",  madde_karar)
ws("Guvenirlik",   guv_df)
ws("Madde_Toplam", mt_kor)
ws("DFA_Uyum",     data.frame(Indeks = names(uyum_ind),
                               Deger  = round(uyum_ind, 3)))
ws("Gecerlilik",   ave_cr_df)
ws("Puanlar",      df_son[, c("ID", "Puan_SGC", "Puan_DGC",
                               "Puan_DT", "Puan_Toplam")])

openxlsx::saveWorkbook(wb, "output/tukenmislik_analiz_sonuclari.xlsx", overwrite = TRUE)
cat("Excel: output/tukenmislik_analiz_sonuclari.xlsx (9 sekme)\n")
cat("\n============================================================\n")
cat("  Pipeline tamamlandi — Tükenmişlik & Geri Çekilme Ölçeği\n")
cat("============================================================\n")

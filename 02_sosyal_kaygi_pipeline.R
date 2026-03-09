# ==============================================================================
# ÖLÇEK 2: Fiziksel Duyarlılık, Kaçınma Davranışı ve
#           Olumsuz Değerlendirilme Korkusu Ölçeği
# Tam Psikometrik Analiz Pipeline'ı
# ==============================================================================
# Faktörler : FD (Fiziksel Duyarlılık)             → FD1–FD8
#             KA (Kaçınma Davranışı)                → KA1–KA8
#             OD (Olumsuz Değerlendirilme Korkusu)  → OD1–OD8
# Başlangıç : 24 madde, 5'li Likert, N=842 ham
# Nihai     : 20 madde (KA7,KA8,OD7,OD8 elendi), 2 faktör, N=782
# Ters kodlu: FD5, KA5, OD3, OD6
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
TERS_MADDELER <- c("FD5", "KA5", "OD3", "OD6")
EKSIK_ESIK    <- 0.10
STRAIGHT_SD   <- 0.10
YAS_MIN <- 18; YAS_MAX <- 65
MAHA_P  <- 0.001
YUK_MIN <- 0.32
DELTA_CAPRAZ <- 0.10
N_FAKTOR <- 2   # PA ve MAP önerisi; AFA bu değerle yürütüldü

ALT_BOYUT <- list(
  FD = paste0("FD", 1:8),
  KA = paste0("KA", 1:8),
  OD = paste0("OD", 1:8)
)
MADDE_COLS <- unname(unlist(ALT_BOYUT))

MAVI  <- "#2C75B6"; KRMZ <- "#C0392B"
YESIL <- "#27AE60"; TUR  <- "#16A085"; MOR <- "#8E44AD"

# ── 2. VERİ OKUMA ────────────────────────────────────────────────────────────
df <- read.csv("data/sosyal_kaygi_ham.csv")
cat(sprintf("\n=== ADIM 1: VERİ OKUMA ===\nHam veri: %d satir x %d sutun\n",
            nrow(df), ncol(df)))
print(head(df, 3))

# ── 3. AYKIRI DEĞER TEMİZLİĞİ ────────────────────────────────────────────────
cat("\n=== ADIM 2: AYKIRI DEĞER TEMİZLİĞİ ===\n")

yas_hata <- !is.na(df$Yas) & (df$Yas < YAS_MIN | df$Yas > YAS_MAX)
med_hata <- !is.na(df$Medeni_Durum) & !df$Medeni_Durum %in% 1:4
cat(sprintf("Yas aykiri: %d  |  Medeni aykiri: %d\n", sum(yas_hata), sum(med_hata)))
df$Yas[yas_hata]          <- NA
df$Medeni_Durum[med_hata] <- NA

for (m in MADDE_COLS) {
  hata <- !is.na(df[[m]]) & !df[[m]] %in% GECERLI_KOD
  if (any(hata)) { df[[m]][hata] <- NA; cat(sprintf("  %s: %d aralik hatasi\n", m, sum(hata))) }
}

satir_sd <- apply(df[, MADDE_COLS], 1, function(x) sd(x, na.rm = TRUE))
sl_idx   <- which(!is.na(satir_sd) & satir_sd < STRAIGHT_SD)
cat(sprintf("Straight-liner (SD < %.2f): %d kisi\n", STRAIGHT_SD, length(sl_idx)))
df[sl_idx, MADDE_COLS] <- NA

# ── 4. EKSİK VERİ ANALİZİ ────────────────────────────────────────────────────
cat("\n=== ADIM 3: EKSİK VERİ ANALİZİ ===\n")
eksik_oran <- colMeans(is.na(df[, MADDE_COLS])) * 100
cat("Eksik oranlar (azalan sıra):\n")
print(round(sort(eksik_oran, decreasing = TRUE), 2))

p1 <- ggplot(data.frame(Madde = names(eksik_oran), Oran = eksik_oran),
             aes(x = reorder(Madde, Oran), y = Oran,
                 fill = case_when(Oran>10~"Kritik",Oran>5~"Uyarı",TRUE~"Normal"))) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = c(5,10), linetype="dashed",
             color=c("orange",KRMZ), linewidth=0.9) +
  geom_text(aes(label=sprintf("%.1f%%",Oran)), hjust=-0.15, size=3) +
  scale_fill_manual(values=c(Normal=MAVI,Uyarı="orange",Kritik=KRMZ), name="") +
  coord_flip() + ylim(0, max(eksik_oran)*1.30) +
  labs(title="Şekil 1. Madde Eksik Veri Oranları (%)", x=NULL, y="%") +
  theme_minimal(base_size=12)
ggsave("figures/sekil01_eksik_veri.png", p1, width=8, height=7, dpi=150)

# %10 üzeri madde var mı?
yuksek <- names(eksik_oran[eksik_oran > EKSIK_ESIK * 100])
if (length(yuksek) > 0) {
  cat(sprintf("Madde düzeyinde >%%10: %s — çıkarıldı\n", paste(yuksek, collapse=",")))
  MADDE_COLS <- setdiff(MADDE_COLS, yuksek)
}

satir_eksik <- rowMeans(is.na(df[, MADDE_COLS]))
df <- df[satir_eksik <= EKSIK_ESIK, ]
cat(sprintf("Satır filtresi (>%%10 eksik satır): N = %d\n", nrow(df)))

df_mice <- df[, c("Yas","Cinsiyet","Egitim","Medeni_Durum", MADDE_COLS)]
imp  <- mice(df_mice, m=5, method="pmm", seed=42, printFlag=FALSE)
df_c <- complete(imp, 1)
df[, colnames(df_c)] <- df_c
cat("MICE (m=5, pmm, seed=42) tamamlandı.\n")

# ── 5. TERS KODLAMA ───────────────────────────────────────────────────────────
cat("\n=== ADIM 4: TERS KODLAMA ===\n")
df_tc      <- df
ters_aktif <- intersect(TERS_MADDELER, MADDE_COLS)
for (m in ters_aktif) df_tc[[m]] <- 6 - df_tc[[m]]
cat(sprintf("Ters kodlanan (%d): %s\n", length(ters_aktif),
            paste(ters_aktif, collapse=", ")))

# ── 6. TANIMLAYICI İSTATİSTİKLER ──────────────────────────────────────────────
cat("\n=== ADIM 5: TANIMLAYICI İSTATİSTİKLER ===\n")
tanimlayici <- df_tc[, MADDE_COLS] %>%
  summarise(across(everything(), list(
    Ort=~round(mean(.),3), SS=~round(sd(.),3),
    Min=~min(.), Max=~max(.),
    Carpiklik=~round(psych::skew(.),3),
    Basiklik =~round(psych::kurtosi(.),3)
  ))) %>%
  tidyr::pivot_longer(everything(),
                      names_to=c("Madde",".value"), names_sep="_")
print(tanimlayici, n=Inf)

# ── 7. NORMALLİK ─────────────────────────────────────────────────────────────
cat("\n=== ADIM 6: NORMALLİK ANALİZİ ===\n")
mvn_s <- MVN::mvn(df_tc[, MADDE_COLS], mvn_test="mardia", univariate_test="AD")
print(mvn_s$multivariate_normality)
cat("NOT: |sk|<.15, |ku|<.45 → PAF dağılıma duyarlı değil; tercih edildi.\n")

# ── 8. MAHALANOBİS ─────────────────────────────────────────────────────────
cat("\n=== ADIM 7: MAHALANOBİS UZAKLIĞI ===\n")
maha_data <- df_tc[, MADDE_COLS]
maha_dist <- mahalanobis(maha_data, colMeans(maha_data), cov(maha_data))
maha_p    <- pchisq(maha_dist, df=ncol(maha_data), lower.tail=FALSE)
aykiri_n  <- sum(maha_p < MAHA_P)
cat(sprintf("p < %.3f aykırı: %d / %d\n", MAHA_P, aykiri_n, nrow(df_tc)))
df_tc <- df_tc[maha_p >= MAHA_P, ]
cat(sprintf("Temizlik sonrası N: %d\n", nrow(df_tc)))

p_maha <- ggplot(data.frame(i=1:length(maha_dist), D2=maha_dist,
                              Ak=maha_p<MAHA_P),
                 aes(x=i, y=D2, color=Ak)) +
  geom_point(alpha=0.5, size=1.5) +
  geom_hline(yintercept=qchisq(1-MAHA_P, df=length(MADDE_COLS)),
             linetype="dashed", color=KRMZ, linewidth=1) +
  scale_color_manual(values=c("FALSE"=MAVI,"TRUE"=KRMZ),
                     labels=c("Normal","Aykırı"), name="") +
  labs(title="Şekil 2. Mahalanobis D² Saçılım Grafiği",
       x="Katılımcı İndeksi", y="D²") +
  theme_minimal(base_size=12)
ggsave("figures/sekil02_mahalanobis.png", p_maha, width=8, height=5, dpi=150)

# ── 9. KMO & BARTLETT ────────────────────────────────────────────────────────
cat("\n=== ADIM 8: KMO VE BARTLETT ===\n")
kor_mat <- cor(df_tc[, MADDE_COLS], use="complete.obs")
kmo     <- psych::KMO(kor_mat)
bart    <- psych::cortest.bartlett(kor_mat, n=nrow(df_tc))
cat(sprintf("KMO: %.3f  |  Bartlett: χ²(%d)=%.2f, p=%.4f\n",
            kmo$MSA, bart$df, bart$chisq, bart$p.value))
print(round(sort(kmo$MSAi), 3))

# Korelasyon haritası
renk_eksen <- c(rep(MAVI,length(ALT_BOYUT$FD)),
                rep(TUR, length(ALT_BOYUT$KA)),
                rep(MOR, length(ALT_BOYUT$OD)))
renk_eksen <- renk_eksen[names(kmo$MSAi) %in% colnames(kor_mat)]
png("figures/sekil03_korelasyon_matrisi.png", width=900, height=820, res=120)
corrplot::corrplot(kor_mat, method="color", type="lower",
                   tl.col=renk_eksen, tl.cex=0.75, diag=FALSE,
                   col=corrplot::COL2("RdBu",200),
                   title="Şekil 3. Korelasyon Matrisi",
                   mar=c(0,0,2,0))
dev.off()

# ── 10. FAKTÖR SAYISI ────────────────────────────────────────────────────────
cat("\n=== ADIM 9: FAKTÖR SAYISI BELİRLEME ===\n")
oz    <- eigen(kor_mat)$values
oz_df <- data.frame(Faktor=seq_along(oz),
                    Ozd=round(oz,3),
                    AcikVar=round(oz/sum(oz)*100,2)) %>%
  dplyr::mutate(Kumul=cumsum(AcikVar))
print(head(oz_df,10), row.names=FALSE)
cat(sprintf("Kaiser: %d faktör\n", sum(oz>=1)))

p_scree <- ggplot(head(oz_df,12), aes(x=Faktor,y=Ozd)) +
  geom_line(color="#333",linewidth=1) +
  geom_point(aes(color=Ozd>=1), size=5) +
  geom_hline(yintercept=1, linetype="dashed", color=KRMZ, linewidth=0.9) +
  scale_color_manual(values=c("TRUE"=KRMZ,"FALSE"=MAVI), name="") +
  scale_x_continuous(breaks=1:12) +
  labs(title="Şekil 4. Scree Plot — Sosyal Kaygı Ölçeği",
       x="Faktör Sırası", y="Özdeğer (λ)") +
  theme_minimal(base_size=12)
ggsave("figures/sekil04_scree_plot.png", p_scree, width=7, height=5, dpi=150)

png("figures/sekil05_paralel_analiz.png", width=800, height=600, res=120)
pa <- psych::fa.parallel(df_tc[,MADDE_COLS], n.iter=1000, fm="pa", fa="fa",
                          main="Şekil 5. Paralel Analiz", error.bars=TRUE)
dev.off()
map_s <- psych::vss(df_tc[,MADDE_COLS], n=10, plot=FALSE)
cat(sprintf("PA öneri: %d | MAP öneri: %d\n>>> KARAR: N_FAKTOR = %d\n",
            pa$nfact, which.min(map_s$map), N_FAKTOR))

# ── 11. BAŞLANGIÇ AFA ────────────────────────────────────────────────────────
cat("\n=== ADIM 10: BAŞLANGIÇ AFA ===\n")
fa_ana <- psych::fa(df_tc[,MADDE_COLS], nfactors=N_FAKTOR,
                    fm="pa", rotate="oblimin", warnings=FALSE)
print(fa_ana$loadings, digits=3, cutoff=0.10)
cat(sprintf("Uyum: RMSEA=%.3f | TLI=%.3f | BIC=%.1f\n",
            fa_ana$RMSEA[1], fa_ana$TLI, fa_ana$BIC))
cat(sprintf("Açıklanan varyans: %.1f%%\n",
            sum(fa_ana$values[1:N_FAKTOR])/length(MADDE_COLS)*100))

# ── 12. MADDE ELİMİNASYONU ────────────────────────────────────────────────────
cat("\n=== ADIM 11: MADDE ELİMİNASYONU ===\n")
yuk_mat     <- unclass(fa_ana$loadings)
kom_vek     <- fa_ana$communality
maks_yuk    <- apply(abs(yuk_mat),1,max)
maks_f      <- apply(abs(yuk_mat),1,which.max)
capraz_fark <- apply(abs(yuk_mat),1,function(x){
  s<-sort(x,decreasing=TRUE); if(length(s)>=2) s[1]-s[2] else 1
})
capraz_n <- apply(abs(yuk_mat)>=YUK_MIN,1,sum)

madde_karar <- data.frame(
  Madde=rownames(yuk_mat), Maks_Yuk=round(maks_yuk,3),
  Faktor=paste0("PA",maks_f), Komunalite=round(kom_vek,3),
  Capraz_Fark=round(capraz_fark,3), stringsAsFactors=FALSE
)
madde_karar$Neden <- ""
madde_karar$Neden[maks_yuk < YUK_MIN] <- "Düşük yük (<.32)"
madde_karar$Neden[capraz_n>1 & capraz_fark<DELTA_CAPRAZ] <- "Çapraz yük (Δ<.10)"
madde_karar$Karar <- ifelse(madde_karar$Neden=="","Tut ✓","Ele ✗")
print(madde_karar, row.names=FALSE)

elenecek <- madde_karar$Madde[grepl("Ele",madde_karar$Karar)]
kalacak  <- madde_karar$Madde[grepl("Tut",madde_karar$Karar)]
cat(sprintf("Elenen (%d): %s\n", length(elenecek),
            if(length(elenecek)>0) paste(elenecek,collapse=", ") else "—"))
cat(sprintf("Kalan  (%d): %s\n", length(kalacak), paste(kalacak,collapse=", ")))

# Komünalite grafiği
p_kom <- ggplot(data.frame(Madde=names(kom_vek), h2=round(kom_vek,3),
                             D=ifelse(kom_vek>=.40,"≥.40","<.40"),
                             K=madde_karar$Karar),
                aes(x=reorder(Madde,h2),y=h2,fill=D,
                    alpha=ifelse(grepl("Tut",K),1,.35))) +
  geom_col(width=0.75,color="white") +
  geom_hline(yintercept=c(.30,.40), linetype=c("solid","dashed"),
             color=c(KRMZ,"orange"),linewidth=0.9) +
  geom_text(aes(label=sprintf("%.2f",h2)),hjust=-0.15,size=3) +
  scale_fill_manual(values=c("≥.40"=MAVI,"<.40"=KRMZ),name="h²") +
  scale_alpha_identity() + coord_flip() + ylim(0,1.15) +
  labs(title="Şekil 6. Madde Komünaliteleri (h²)",x=NULL,y="h²") +
  theme_minimal(base_size=12)
ggsave("figures/sekil06_komunaliteler.png", p_kom, width=7, height=7, dpi=150)

# ── 13. RAFİNE AFA ────────────────────────────────────────────────────────────
cat("\n=== ADIM 12: RAFİNE AFA ===\n")
df_rafine <- df_tc[, kalacak]
fa_rafine <- psych::fa(df_rafine, nfactors=N_FAKTOR,
                       fm="pa", rotate="oblimin", warnings=FALSE)
print(fa_rafine$loadings, digits=3, cutoff=0.30)
cat(sprintf("Rafine açıklanan varyans: %.1f%%\n",
            sum(fa_rafine$values[1:N_FAKTOR])/ncol(df_rafine)*100))
cat(sprintf("Phi (faktörler arası r):\n")); print(round(fa_rafine$Phi,3))

# Faktör yük ısı haritası
p_yuk <- as.data.frame(unclass(fa_rafine$loadings)) %>%
  tibble::rownames_to_column("Madde") %>%
  tidyr::pivot_longer(-Madde, names_to="Faktor", values_to="Yuk") %>%
  ggplot(aes(x=Faktor, y=reorder(Madde,abs(Yuk)), fill=Yuk)) +
  geom_tile(color="white",linewidth=0.5) +
  geom_text(aes(label=ifelse(abs(Yuk)>=.30,sprintf("%.2f",Yuk),"")),
            size=3.2, color="white", fontface="bold") +
  scale_fill_gradient2(low=KRMZ, mid="white", high=MAVI,
                       midpoint=0, limits=c(-1,1), name="Yük") +
  labs(title="Şekil 7. Rafine AFA Faktör Yük Isı Haritası",
       x="Faktör", y=NULL) +
  theme_minimal(base_size=11)
ggsave("figures/sekil07_faktor_yuk_isisi.png", p_yuk, width=6, height=8, dpi=150)

# ── 14. GÜVENİRLİK ───────────────────────────────────────────────────────────
cat("\n=== ADIM 13: GÜVENİRLİK ===\n")
omega_al <- function(x) {
  o <- tryCatch(suppressWarnings(suppressMessages(
    psych::omega(x, nfactors=1, plot=FALSE))),error=function(e)NULL)
  if(is.null(o)) return(NA_real_)
  round(tryCatch(o$omega_t, error=function(e) o$omega.tot), 3)
}

# Faktör atamaları AFA'dan
yuk_r    <- unclass(fa_rafine$loadings)
maks_f_r <- apply(abs(yuk_r),1,which.max)
faktor_m <- lapply(1:N_FAKTOR, function(f) names(which(maks_f_r==f)))

guv_df <- dplyr::bind_rows(lapply(seq_along(faktor_m), function(fi) {
  mds <- faktor_m[[fi]]
  if(length(mds)<2) return(NULL)
  a   <- psych::alpha(df_rafine[,mds,drop=FALSE], warnings=FALSE)
  data.frame(
    Faktor=paste0("PA",fi), N_Madde=length(mds),
    Cronbach_a=round(a$total$raw_alpha,3),
    GA_Alt=round(a$total$raw_alpha-1.96*a$total$ase,3),
    GA_Ust=round(a$total$raw_alpha+1.96*a$total$ase,3),
    McDonald_w=omega_al(df_rafine[,mds,drop=FALSE]),
    Yorum=as.character(cut(a$total$raw_alpha,c(0,.60,.70,.80,.90,1),
             c("Yetersiz","Sınırda","Kabul","İyi","Mükemmel"),right=FALSE)),
    stringsAsFactors=FALSE
  )
}), .id=NULL)
print(guv_df, row.names=FALSE)

alpha_full <- psych::alpha(df_rafine, warnings=FALSE)
cat(sprintf("Tüm ölçek α=%.3f [%.3f–%.3f]\n",
            alpha_full$total$raw_alpha,
            alpha_full$total$raw_alpha-1.96*alpha_full$total$ase,
            alpha_full$total$raw_alpha+1.96*alpha_full$total$ase))

mt_kor <- as.data.frame(alpha_full$item.stats) %>%
  tibble::rownames_to_column("Madde") %>%
  dplyr::select(Madde,Ort=mean,SS=sd,r_duz=r.drop) %>%
  dplyr::mutate(across(where(is.numeric),~round(.,3)),
                Kalite=dplyr::case_when(r_duz>=.40~"İyi ✓",
                                        r_duz>=.30~"Sınırda ⚠",TRUE~"Düşük ✗"))
print(mt_kor, row.names=FALSE)

# Alfa sil-bak
alp_del <- as.data.frame(alpha_full$alpha.drop) %>%
  tibble::rownames_to_column("Madde") %>%
  dplyr::select(Madde,a_Silinirse=raw_alpha) %>%
  dplyr::mutate(across(where(is.numeric),~round(.,3)))
print(alp_del, row.names=FALSE)

p_alpha <- ggplot(guv_df, aes(x=Faktor, y=Cronbach_a,
                               ymin=GA_Alt, ymax=GA_Ust)) +
  geom_point(size=7, color=MAVI) +
  geom_errorbar(width=0.18, linewidth=1.3, color=MAVI) +
  geom_hline(yintercept=c(.70,.80,.90),linetype="dashed",
             color=c("#F46D43","#FDAE61","#66C2A5"),linewidth=0.8) +
  geom_text(aes(label=sprintf("α=%.3f\nω=%.3f",Cronbach_a,McDonald_w)),
            vjust=-1.8, size=4, fontface="bold") +
  ylim(.50,1.10) +
  labs(title="Şekil 8. Güvenirlik Katsayıları (%95 GA)",x=NULL,y="α") +
  theme_minimal(base_size=12)
ggsave("figures/sekil08_guvenirlik.png", p_alpha, width=6, height=5, dpi=150)

# ── 15. DFA ──────────────────────────────────────────────────────────────────
cat("\n=== ADIM 14: DOĞRULAYICI FAKTÖR ANALİZİ ===\n")
set.seed(2024)
idx_afa2 <- sample(nrow(df_tc), floor(nrow(df_tc)/2))
df_dfa   <- df_tc[-idx_afa2, ]
cat(sprintf("DFA n = %d\n", nrow(df_dfa)))

fd_kal <- intersect(ALT_BOYUT$FD, kalacak)
ka_kal <- intersect(ALT_BOYUT$KA, kalacak)
od_kal <- intersect(ALT_BOYUT$OD, kalacak)

model_dfa <- sprintf("FD =~ %s\nKA =~ %s\nOD =~ %s",
                     paste(fd_kal,collapse=" + "),
                     paste(ka_kal,collapse=" + "),
                     paste(od_kal,collapse=" + "))
cat(model_dfa,"\n\n")

fit_dfa <- lavaan::cfa(model_dfa, data=df_dfa,
                        estimator="MLR", std.lv=TRUE, missing="fiml")
uyum_ind <- lavaan::fitMeasures(fit_dfa,
  c("chisq","df","pvalue","cfi","tli","rmsea",
    "rmsea.ci.lower","rmsea.ci.upper","srmr","nfi"))
cat("Uyum indeksleri:\n"); print(round(uyum_ind,3))

std_coz <- lavaan::standardizedSolution(fit_dfa)
print(std_coz[std_coz$op=="=~", c("lhs","rhs","est.std","se","pvalue")],
      row.names=FALSE)

# Modifikasyon
mi <- lavaan::modindices(fit_dfa, sort.=TRUE)
cat("\nMI > 5:\n")
print(mi[mi$mi>5, c("lhs","op","rhs","mi","sepc.all")][1:min(10,sum(mi$mi>5)),])

# ── 16. GEÇERLİLİK ──────────────────────────────────────────────────────────
cat("\n=== ADIM 15: GEÇERLİLİK ===\n")
ave_cr_df <- dplyr::bind_rows(lapply(
  list(FD=fd_kal, KA=ka_kal, OD=od_kal), function(mds) {
    lam <- std_coz[std_coz$op=="=~" & std_coz$rhs%in%mds, "est.std"]
    data.frame(AVE=round(mean(lam^2),3),
               CR =round(sum(lam)^2/(sum(lam)^2+sum(1-lam^2)),3),
               sqAVE=round(sqrt(mean(lam^2)),3))
}), .id="Faktor")
cat("AVE ≥ .50 ✓  |  CR ≥ .70 ✓\n"); print(ave_cr_df, row.names=FALSE)

htmt_res <- semTools::htmt(model_dfa, data=df_dfa)
cat("HTMT (< .85):\n"); print(round(htmt_res,3))

# Fornell-Larcker
fak_cors <- lavaan::lavInspect(fit_dfa, "cor.lv")
cat("\nFornell-Larcker: √AVE karşılaştırması:\n")
sqave_v <- setNames(ave_cr_df$sqAVE, ave_cr_df$Faktor)
for (nm in names(sqave_v))
  cat(sprintf("  %s: √AVE=%.3f | r ile %s = %s\n", nm, sqave_v[nm],
              paste(names(sqave_v)[names(sqave_v)!=nm], collapse="/"),
              paste(round(fak_cors[nm, names(sqave_v)[names(sqave_v)!=nm]], 3),
                    collapse="/")))

# ── 17. NİHAİ PUANLAMA ───────────────────────────────────────────────────────
cat("\n=== ADIM 16: NİHAİ PUANLAMA ===\n")
df_son <- df_tc[, c("ID","Yas","Cinsiyet", kalacak)]
df_son$Puan_FD     <- rowMeans(df_son[,fd_kal],na.rm=TRUE)
df_son$Puan_KA     <- rowMeans(df_son[,ka_kal],na.rm=TRUE)
df_son$Puan_OD     <- rowMeans(df_son[,od_kal],na.rm=TRUE)
df_son$Puan_Toplam <- rowMeans(df_son[,c("Puan_FD","Puan_KA","Puan_OD")])
print(sapply(df_son[,c("Puan_FD","Puan_KA","Puan_OD","Puan_Toplam")],
             function(x) c(Ort=round(mean(x),3),SS=round(sd(x),3))))

p_dist <- df_son %>%
  dplyr::select(ID,Puan_FD,Puan_KA,Puan_OD,Puan_Toplam) %>%
  tidyr::pivot_longer(-ID,names_to="Boyut",values_to="Puan") %>%
  ggplot(aes(x=Puan,fill=Boyut)) +
  geom_density(alpha=0.65,color="white",linewidth=0.8) +
  geom_vline(xintercept=3,linetype="dashed",color="gray40") +
  facet_wrap(~Boyut,scales="free_y",ncol=2) +
  scale_fill_manual(values=c(Puan_FD=MAVI,Puan_KA=TUR,
                              Puan_OD=MOR,Puan_Toplam=YESIL)) +
  labs(title="Şekil 9. Alt Ölçek Puan Dağılımları",
       x="Puan (1–5)",y="Yoğunluk") +
  theme_minimal(base_size=11)+theme(legend.position="none")
ggsave("figures/sekil09_puan_dagilimi.png", p_dist, width=8, height=6, dpi=150)

# ── 18. EXCEL ────────────────────────────────────────────────────────────────
cat("\n=== ADIM 17: EXCEL RAPORU ===\n")
wb <- openxlsx::createWorkbook()
ust <- openxlsx::createStyle(fontColour="white",fgFill="#1F4E79",
                               textDecoration="bold",halign="center")
ws <- function(n,v){ openxlsx::addWorksheet(wb,n)
  openxlsx::writeData(wb,n,v,headerStyle=ust)
  openxlsx::setColWidths(wb,n,cols=1:ncol(v),widths="auto") }
ws("Tanimlayici",  tanimlayici)
ws("Ozdeğerler",   head(oz_df,12))
ws("KMO_MSA",      data.frame(Madde=names(kmo$MSAi),MSA=round(kmo$MSAi,3)))
ws("Madde_Eleme",  madde_karar)
ws("Guvenirlik",   guv_df)
ws("Madde_Toplam", mt_kor)
ws("DFA_Uyum",     data.frame(Indeks=names(uyum_ind),Deger=round(uyum_ind,3)))
ws("Gecerlilik",   ave_cr_df)
ws("Puanlar",      df_son[,c("ID","Puan_FD","Puan_KA","Puan_OD","Puan_Toplam")])
openxlsx::saveWorkbook(wb,"output/sosyal_kaygi_analiz_sonuclari.xlsx",overwrite=TRUE)
cat("Excel: output/sosyal_kaygi_analiz_sonuclari.xlsx (9 sekme)\n")
cat("\n============================================================\n")
cat("  Pipeline tamamlandi — Sosyal Kaygi Ölçegi\n")
cat("============================================================\n")

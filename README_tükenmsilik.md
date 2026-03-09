# Ölçek 1: Tükenmişlik ve Geri Çekilme Ölçeği

## Genel Bilgi

| Özellik | Değer |
|---------|-------|
| **Ölçek Adı** | Tükenmişlik ve Duygusal Geri Çekilme Ölçeği |
| **Madde Sayısı (Başlangıç)** | 18 |
| **Madde Sayısı (Nihai)** | 18 |
| **Faktör Sayısı** | 3 |
| **Ölçek Tipi** | 5'li Likert (1=Hiçbir Zaman … 5=Her Zaman) |
| **Ham N** | 687 |
| **Temiz N** | 517 |

---

## Faktör Yapısı

| Faktör | Kısaltma | Maddeler | Ters Kodlanan |
|--------|----------|----------|--------------|
| Sosyal Geri Çekilme | SGC | SGC1, SGC2, SGC3, SGC4, SGC5, SGC6 | — |
| Duygusal Geri Çekilme | DGC | DGC1, DGC2, DGC3, DGC4, DGC5, DGC6 | DGC2 |
| Duygusal Tükenme | DT | DT1, DT2, DT3, DT4, DT5, DT6 | DT5 |

---

## Veri Seti: `tukenmislik_ham.csv`

### Değişkenler

| Sütun | Tip | Açıklama | Değerler |
|-------|-----|----------|---------|
| ID | int | Katılımcı kimliği | 1–687 |
| Yas | int | Yaş | 22–55 |
| Cinsiyet | int | Cinsiyet | 1=Kadın, 2=Erkek |
| Egitim | int | Eğitim düzeyi | 1=İlk, 2=Lise, 3=Lisans, 4=LÜ |
| Medeni_Durum | int | Medeni durum | 1=Bekar, 2=Evli, 3=Diğer |
| SGC1–SGC6 | int/NA | Sosyal Geri Çekilme | 1–5 (NA mümkün) |
| DGC1–DGC6 | int/NA | Duygusal Geri Çekilme | 1–5 (NA mümkün) |
| DT1–DT6 | int/NA | Duygusal Tükenme | 1–5 (NA mümkün) |

### Veri Kirliliği (kasıtlı — gerçekçi senaryo)
- ~%4 madde düzeyinde eksik veri (NA)
- 32 straight-liner katılımcı (SD < 0.10 → maddeleri NA yapılır)
- Demografik aralık hataları (pipeline'da temizlenir)

---

## Pipeline Özeti: `01_tukenmislik_pipeline.R`

### Kritik Bulgular
- **KMO** = .935 → Mükemmel
- **Bartlett** χ²(153) = ~4500, p < .001
- **Paralel Analiz** → 3 faktör önerisi
- **Açıklanan Varyans** = %62.7 (rafine AFA)
- **Cronbach α**: SGC=.917, DGC=.910, DT=.898
- **DFA Uyum**: CFI≈1.00, RMSEA≈.000, SRMR≈.030
- **AVE**: SGC=.657, DGC=.642, DT=.571 (tümü ≥ .50)
- **HTMT**: Maksimum = .499 (< .85 ✓)

### Üretilen Grafikler
| Dosya | İçerik |
|-------|--------|
| sekil01_eksik_veri.png | Madde eksik veri oranları bar grafiği |
| sekil02_histogramlar.png | 18 madde histogram matrisi |
| sekil03_mahalanobis.png | D² saçılım grafiği |
| sekil04_korelasyon_matrisi.png | Corrplot ısı haritası |
| sekil05_scree_plot.png | Scree plot |
| sekil06_paralel_analiz.png | PA gerçek vs. simüle özdeğerler |
| sekil07_komunaliteler.png | Madde komünaliteleri çubuk grafiği |
| sekil08_faktor_yuk_isisi.png | Faktör yük ısı haritası |
| sekil09_guvenirlik.png | Alt ölçek α/ω + %95 GA |
| sekil10_puan_dagilimi.png | Alt ölçek puan yoğunluk grafiği |

### Excel Sekmeleri: `tukenmislik_analiz_sonuclari.xlsx`
`Tanimlayici · Ozdeğerler · KMO_MSA · Madde_Eleme · Guvenirlik · Madde_Toplam · DFA_Uyum · Gecerlilik · Puanlar`

---

## Puanlama Rehberi

```r
# Ters kodlama (önce yapılmalı)
df$DT5  <- 6 - df$DT5
df$DGC2 <- 6 - df$DGC2

# Alt ölçek puanları (madde ortalaması)
df$Puan_SGC <- rowMeans(df[, paste0("SGC", 1:6)], na.rm = TRUE)
df$Puan_DGC <- rowMeans(df[, paste0("DGC", 1:6)], na.rm = TRUE)
df$Puan_DT  <- rowMeans(df[, paste0("DT",  1:6)], na.rm = TRUE)

# Toplam puan
df$Puan_Toplam <- rowMeans(df[, c("Puan_SGC","Puan_DGC","Puan_DT")])
```

> Puan aralığı: 1–5. Yüksek puan = ilgili boyutun yüksek düzeyi.

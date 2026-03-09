# Ölçek 3: Psikolojik Dayanıklılık Ölçeği (PD / MO / SD)

## Genel Bilgi

| Özellik | Değer |
|---------|-------|
| **Ölçek Adı** | Psikolojik Dayanıklılık Ölçeği |
| **Madde Sayısı (Başlangıç)** | 21 |
| **Faktör Sayısı** | 3 |
| **Ölçek Tipi** | 5'li Likert (1=Hiç Katılmıyorum … 5=Tamamen Katılıyorum) |
| **Ham N** | 624 |
| **Ters Kodlanan** | MO3, MO6, SD7 |

---

## Faktör Yapısı

| Faktör | Kısaltma | Maddeler | Ters Kodlanan |
|--------|----------|----------|--------------|
| Pozitif Duygu Düzenleme | PD | PD1–PD7 | — |
| Motivasyon & Öz-Yeterlik | MO | MO1–MO7 | MO3, MO6 |
| Sosyal Destek Algısı | SD | SD1–SD7 | SD7 |

---

## Veri Seti: `psikolojik_dayaniklilik_ham.csv`

| Sütun | Tip | Açıklama | Değerler |
|-------|-----|----------|---------|
| ID | int | Katılımcı kimliği | 1–624 |
| Yas | int | Yaş | 18–60 |
| Cinsiyet | int | Cinsiyet | 1=Kadın, 2=Erkek |
| Egitim | int | Eğitim düzeyi | 1–4 |
| Medeni_Durum | int | Medeni durum | 1–4 |
| Meslek | int | Meslek grubu | 1=Öğrenci, 2=Memur, 3=Özel, 4=Serbest, 5=Diğer |
| PD1–PD7 | int/NA | Pozitif Duygu Düzenleme | 1–5 |
| MO1–MO7 | int/NA | Motivasyon & Öz-Yeterlik | 1–5 |
| SD1–SD7 | int/NA | Sosyal Destek Algısı | 1–5 |

### Veri Kirliliği
- Tüm maddeler: ~%5 eksik
- 18 straight-liner katılımcı
- Demografik aralık hataları

---

## Pipeline Özeti: `03_psikolojik_dayaniklilik_pipeline.R`

Bu ölçek diğer ikisinden farklı olarak **meslek grupları karşılaştırması** grafiği de içermektedir.

### Üretilen Grafikler
| Dosya | İçerik |
|-------|--------|
| sekil01_eksik_veri.png | Madde eksik oranları |
| sekil02_profil.png | Madde profil grafiği (Ort ± 1 SS) |
| sekil03_histogramlar.png | 21 madde histogram matrisi |
| sekil04_mahalanobis.png | D² saçılım grafiği |
| sekil05_korelasyon_matrisi.png | Corrplot — alt boyutlar renkli |
| sekil06_scree_plot.png | Scree plot |
| sekil07_paralel_analiz.png | Paralel analiz |
| sekil08_komunaliteler.png | h² çubuk grafiği |
| sekil09_faktor_yuk_isisi.png | Faktör yük ısı haritası |
| sekil10_guvenirlik.png | Güvenirlik katsayıları |
| sekil11_puan_dagilimi.png | Puan yoğunluk grafikleri |
| sekil12_meslek_karsilastirma.png | Meslek gruplarına göre alt ölçek ortalamaları |

### Excel Sekmeleri
`Tanimlayici · Ozdeğerler · KMO_MSA · Madde_Eleme · Guvenirlik · Madde_Toplam · Alfa_Sil · DFA_Uyum · Gecerlilik · Puanlar`

---

## Puanlama Rehberi

```r
# Ters kodlama
for (m in c("MO3","MO6","SD7")) df[[m]] <- 6 - df[[m]]

# Alt ölçek puanları
df$Puan_PD <- rowMeans(df[, paste0("PD", 1:7)], na.rm = TRUE)
df$Puan_MO <- rowMeans(df[, paste0("MO", 1:7)], na.rm = TRUE)
df$Puan_SD <- rowMeans(df[, paste0("SD", 1:7)], na.rm = TRUE)
df$Puan_Toplam <- rowMeans(df[, c("Puan_PD","Puan_MO","Puan_SD")])
```

> **Yorum:** Yüksek puan = yüksek psikolojik dayanıklılık.  
> PD ≈ 3.40, MO ≈ 3.25, SD ≈ 2.98 örneklem ortalamaları beklenmektedir.

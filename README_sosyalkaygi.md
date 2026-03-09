# Ölçek 2: Sosyal Kaygı Ölçeği (FD / KA / OD)

## Genel Bilgi

| Özellik | Değer |
|---------|-------|
| **Ölçek Adı** | Fiziksel Duyarlılık, Kaçınma Davranışı ve Olumsuz Değerlendirilme Korkusu Ölçeği |
| **Madde Sayısı (Başlangıç)** | 24 |
| **Madde Sayısı (Nihai)** | 20 (KA7, KA8, OD7, OD8 elendi) |
| **Faktör Sayısı** | 2 (PA1: FD+OD, PA2: KA) |
| **Ölçek Tipi** | 5'li Likert (1=Kesinlikle Katılmıyorum … 5=Kesinlikle Katılıyorum) |
| **Ham N** | 842 |
| **Temiz N** | 782 |

---

## Faktör Yapısı

| Faktör | Kısaltma | Maddeler | Ters Kodlanan |
|--------|----------|----------|--------------|
| Fiziksel Duyarlılık | FD | FD1–FD8 | FD5 |
| Kaçınma Davranışı | KA | KA1–KA6 (KA7-8 elendi) | KA5 |
| Olumsuz Değ. Korkusu | OD | OD1–OD6 (OD7-8 elendi) | OD3, OD6 |

> **Not:** OD maddeleri AFA'da FD ile aynı faktörde (PA1) toplanmıştır.  
> Teorik olarak bağımsız 3 alt boyut varsayılmış, ancak veri 2-faktörlü yapıyı desteklemiştir.

---

## Veri Seti: `sosyal_kaygi_ham.csv`

| Sütun | Tip | Açıklama | Değerler |
|-------|-----|----------|---------|
| ID | int | Katılımcı kimliği | 1–842 |
| Yas | int | Yaş | 18–65 |
| Cinsiyet | int | Cinsiyet | 1=Kadın, 2=Erkek |
| Egitim | int | Eğitim düzeyi | 1–4 |
| Medeni_Durum | int | Medeni durum | 1–4 |
| FD1–FD8 | int/NA | Fiziksel Duyarlılık | 1–5 |
| KA1–KA8 | int/NA | Kaçınma Davranışı | 1–5 |
| OD1–OD8 | int/NA | Ol. Değ. Korkusu | 1–5 |

### Veri Kirliliği
- FD maddeleri: ~%6 eksik | KA ve OD maddeleri: ~%7 eksik
- 29 straight-liner katılımcı
- Demografik aralık hataları

---

## Pipeline Özeti: `02_sosyal_kaygi_pipeline.R`

### Kritik Bulgular
- **KMO** = .947 → Mükemmel
- **Bartlett** χ²(276) = 8786.16, p < .001
- **Paralel Analiz** → 3 faktör | **MAP** → 3 faktör | **Uygulanan** → 2 faktör
- **Elenen maddeler**: KA7 (h²=.06), KA8 (h²=.05), OD7 (h²=.02), OD8 (h²=.02)
- **Rafine açıklanan varyans** = %49.5
- **Cronbach α**: Fiziksel Duyarlılık=.900, Kaçınma=.920, Tüm=.906
- **OD3, OD4, OD6**: Madde-toplam r sınırda (.348–.371)

### Üretilen Grafikler
| Dosya | İçerik |
|-------|--------|
| sekil01_eksik_veri.png | Madde eksik oranları |
| sekil02_mahalanobis.png | D² saçılım grafiği |
| sekil03_korelasyon_matrisi.png | Corrplot — alt boyutlar renkli |
| sekil04_scree_plot.png | Scree plot |
| sekil05_paralel_analiz.png | Paralel analiz |
| sekil06_komunaliteler.png | h² çubuk grafiği |
| sekil07_faktor_yuk_isisi.png | Faktör yük ısı haritası (20 madde) |
| sekil08_guvenirlik.png | Güvenirlik katsayıları |
| sekil09_puan_dagilimi.png | Puan yoğunluk grafikleri |

---

## Puanlama Rehberi

```r
# Ters kodlama
for (m in c("FD5","KA5","OD3","OD6")) df[[m]] <- 6 - df[[m]]

# Puanlama (elenen maddeler çıkarılmış haliyle)
df$Puan_FD <- rowMeans(df[, paste0("FD", 1:8)], na.rm = TRUE)
df$Puan_KA <- rowMeans(df[, paste0("KA", 1:6)], na.rm = TRUE)   # KA7,KA8 yok
df$Puan_OD <- rowMeans(df[, paste0("OD", 1:6)], na.rm = TRUE)   # OD7,OD8 yok
df$Puan_Toplam <- rowMeans(df[, c("Puan_FD","Puan_KA","Puan_OD")])
```

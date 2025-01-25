# 1. UČitavanje i prilagodba podataka

# Učitavanje potrebnih biblioteka
library(tidyverse)

# Učitavanje podataka
data_path <- "Podaci/student_data.csv"  # Putanja do datoteke - promjenit putanju
studentData <- read_csv(data_path)

# --- Pregled osnovnih informacija ---

# 1. Provjera strukture podataka
cat("\n# Struktura podataka (prije obrade):\n")
str(studentData)  # Pregled vrsta varijabli

# 2. Prikaz prvih nekoliko redova za bolji uvid
cat("\n# Prvih nekoliko redaka podataka:\n")
head(studentData)

# 3. Provjera nedostajućih vrijednosti
missing_values <- sum(is.na(studentData))
if (missing_values == 0) {
  cat("\nNema nedostajućih vrijednosti! 😊\n")
} else {
  cat("\nPostoje", missing_values, "nedostajuće vrijednosti. Potrebno ih je obraditi.\n")
}

# --- Pretvaranje varijabli u odgovarajuće tipove ---

# Funkcija za pretvaranje određenih kolona u faktore
convert_to_factor <- function(data, cols) {
  for (col in cols) {
    data[[col]] <- as.factor(data[[col]])
  }
  return(data)
}

# Pretvaranje u nominalne faktore (nesortirane kategorije)
nominal_factors <- c(
  "school", "sex", "address", "famsize", "Pstatus", "schoolsup", "famsup",
  "paid_mat", "paid_por", "activities", "nursery", "higher", "internet", "romantic",
  "Mjob", "Fjob", "reason", "guardian"
)
studentData <- convert_to_factor(studentData, nominal_factors)
#ispis za provjeru nominalnih i binarnih faktora
str(studentData)

# Pretvaranje u ordinalne faktore (sortirane kategorije)
ordinal_factors <- list(
  famrel = 1:5,
  freetime = 1:5,
  goout = 1:5,
  Dalc = 1:5,
  Walc = 1:5,
  health = 1:5,
  traveltime = c(1, 2, 3, 4),
  studytime = c(1, 2, 3, 4),
  failures_mat = c(0, 1, 2, 3, 4),
  failures_por = c(0, 1, 2, 3, 4),
  Medu = c(0, 1, 2, 3, 4),
  Fedu = c(0, 1, 2, 3, 4)
)

for (col in names(ordinal_factors)) {
  levels <- ordinal_factors[[col]]
  if (col %in% c("Medu", "Fedu")) {
    labels <- c("None", "Primary (4th grade)", "5th-9th grade", "Secondary education", "Higher education")
    studentData[[col]] <- factor(studentData[[col]], levels = levels, labels = labels, ordered = TRUE)
  } else {
    studentData[[col]] <- factor(studentData[[col]], levels = levels, ordered = TRUE)
  }
}

# --- Pregled podataka nakon prilagodbe ---

# Pregled strukture nakon obrade
cat("\n# Struktura podataka (nakon obrade):\n")
str(studentData)

# Vizualni pregled (samo za razvojni proces)
view(studentData)

# Provjera svih faktora u studentData i ispis razina
for (col in names(studentData)) {
  if (is.factor(studentData[[col]])) {
    cat("\nVarijabla:", col, "\n")
    cat("Razine:", levels(studentData[[col]]), "\n")
  }
}


# 2. Exploratory Data Analysis (EDA): Upoznavanje s podacima

# --- Generalno upoznavanje s podacima ---

# 1. Osnovni statistički pregled
cat("\n# Osnovni pregled statistika:\n")
summary(studentData)

# --- Vizualizacija podataka ---

# 1. Distribucija studenata po školama i spolu
library(ggplot2)
ggplot(studentData, aes(x = school, fill = sex)) +
  geom_bar(position = "dodge", color = "black") +
  labs(
    title = "Distribucija studenata po školama",
    x = "Škola",
    y = "Broj učenika"
  ) +
  scale_fill_manual(values = c("skyblue", "pink")) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.5
  ) +
  theme_minimal()

# 2. Histogram dobi učenika po školama
ggplot(studentData, aes(x = age, fill = school)) + 
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "stack") + 
  labs(
    title = "Distribucija dobi učenika po školama",
    x = "Dob",
    y = "Broj učenika"
  ) + 
  scale_fill_manual(values = c("lightblue", "lightcoral")) + 
  theme_minimal()

# 3. Histogram dobi učenika po spolu
ggplot(studentData, aes(x = age, fill = sex)) + 
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, position = "stack") + 
  labs(
    title = "Distribucija dobi učenika po spolu",
    x = "Dob",
    y = "Broj učenika"
  ) + 
  scale_fill_manual(values = c("skyblue", "pink")) + 
  theme_minimal()

# 4. Distribucija mjesta stanovanja (Ruralno/Urbano)
ggplot(studentData, aes(x = address, fill = address)) + 
  geom_bar(color = "black") + 
  labs(
    title = "Distribucija mjesta stanovanja",
    x = "Mjesto stanovanja",
    y = "Broj učenika"
  ) + 
  scale_fill_manual(values = c("lightgreen", "lightcoral")) + 
  geom_text(
    stat = "count",
    aes(label = ..count..),
    position = position_dodge(width = 0.8),
    vjust = -0.5
  ) + 
  theme_minimal()

# 5. Histogram konačnih ocjena iz matematike 3. srednje (G3_mat)
ggplot(studentData, aes(x = G3_mat)) + 
  geom_histogram(binwidth = 1, fill = "purple", color = "black", alpha = 0.7) + 
  labs(
    title = "Distribucija konačnih ocjena iz matematike",
    x = "Konačna ocjena iz matematike",
    y = "Broj učenika"
  ) + 
  theme_minimal()

# 6. Histogram konačnih ocjena iz portugalskog jezika 3. srednje (G3_por)
ggplot(studentData, aes(x = G3_por)) + 
  geom_histogram(binwidth = 1, fill = "orange", color = "black", alpha = 0.7) + 
  labs(
    title = "Distribucija konačnih ocjena iz portugalskog jezika",
    x = "Konačna ocjena iz portugalskog jezika",
    y = "Broj učenika"
  ) + 
  theme_minimal()

# --- Analiza korelacija ---

# 1. Izračun matrice korelacije za numeričke varijable (Pearsonov koeficijent korelacije)
cor_matrix <- cor(studentData[, sapply(studentData, is.numeric)], use = "complete.obs")

# 2. Ispis matrice korelacije
cat("\nMatrica koeficijenata korelacije:\n")
print(cor_matrix)

# 3. Vizualizacija matrice korelacije
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)
corrplot(
  cor_matrix, 
  method = "circle", 
  type = "full", 
  order = "hclust", 
  addCoef.col = "black", 
  tl.col = "black", 
  col = colorRampPalette(c("red", "white", "blue"))(200), 
  tl.cex = 0.8, 
  number.cex = 0.7
)

# 4. Ispis varijabli s visokom korelacijom
high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
if (nrow(high_cor) > 0) {
  cat("\nParovi varijabli s visokom korelacijom (apsolutna vrijednost > 0.7):\n")
  for (i in 1:nrow(high_cor)) {
    var1 <- rownames(cor_matrix)[high_cor[i, 1]]
    var2 <- colnames(cor_matrix)[high_cor[i, 2]]
    correlation <- cor_matrix[high_cor[i, 1], high_cor[i, 2]]
    cat(sprintf("Varijabla '%s' ima visoku korelaciju (%.2f) s varijablom '%s'.\n", var1, correlation, var2))
  }
} else {
  cat("\nNema parova varijabli s visokom korelacijom (apsolutna vrijednost > 0.7).\n")
}



# 3. Istraživačka pitanja i analize

# 1. pitanje: Jesu li prosječne konačne ocjene iz matematike različite između spolova?
# Napomena: Konkretno gledamo ocjene iz matematike za 3. srednje. Ako je potrebno, možemo uključiti i preostale razrede.

# 1. Pretpostavke za t-test:
# a) Nezavisnost uzoraka: 
# - Pretpostavljamo da rezultati jednog učenika iz jedne grupe ne utječu na rezultate iz druge grupe.
# b) Nasumičnost uzorka:
# - Pretpostavljamo da su uzorci nasumično odabrani.
# c) Normalnost distribucije:
# - Ocjene za svaku grupu trebaju biti normalno distribuirane. Testiramo normalnost distribucije pomoću Shapiro-Wilk testa.
# d) Homogenost varijanci: - testiramo ili s f-testom ili sa Leveneov test
# - Varijance između spolova trebaju biti iste. Ako nisu, koristit ćemo Welchov t-test koji ne zahtijeva ovu pretpostavku.

# 2. Opisna statistika i grafički prikaz:

# Grupiranje po spolu i računanje osnovnih statistika
studentData %>%
  group_by(sex) %>%
  summarise(
    mean_G3_mat = mean(G3_mat, na.rm = TRUE),  # Srednja vrijednost ocjena
    median_G3_mat = median(G3_mat, na.rm = TRUE),  # Medijan ocjena
    sd_G3_mat = sd(G3_mat, na.rm = TRUE),  # Standardna devijacija ocjena
    count = n()  # Broj učenika
  )

# Histogram za spol i ocjene
ggplot(studentData, aes(x = G3_mat, fill = sex)) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black") +
  labs(title = "Distribucija konačnih ocjena iz matematike po spolu",
       x = "Konačna ocjena iz matematike",
       y = "Broj učenika") +
  scale_fill_manual(values = c("pink", "skyblue")) +
  theme_minimal() +
  facet_wrap(~ sex, ncol = 2)  # Dva odvojena grafa, jedan za djevojčice, drugi za dječake

# Box plot za spol i ocjene
ggplot(studentData, aes(x = sex, y = G3_mat, fill = sex)) +
  geom_boxplot() +
  labs(title = "Razlike u konačnim ocjenama iz matematike po spolu",
       x = "Spol",
       y = "Konačna ocjena iz matematike") +
  scale_fill_manual(values = c("pink", "skyblue")) +
  theme_minimal()

# 3. Testiranje normalnosti:
female_data <- studentData$G3_mat[studentData$sex == "F"]
male_data <- studentData$G3_mat[studentData$sex == "M"]

# Grafički prikaz normalnosti - QQ plotovi:

# QQ plot za djevojke
qqnorm(female_data, main = "QQ Plot za djevojke")
qqline(female_data, col = "red")

# QQ plot za dječake
qqnorm(male_data, main = "QQ Plot za dječake")
qqline(male_data, col = "red")

# Test normalnosti (Shapiro-Wilk test) za oba spola
# H0: Populacija je normalno distribuirana
# H1: Populacija nije normalno distribuirana
shapiro_female <- shapiro.test(female_data)
shapiro_male <- shapiro.test(male_data)

# Rezultati normalnosti
print(shapiro_female)
print(shapiro_male)

# Rezultat: S obzirom na vrlo male p-vrijednosti (manje od 0.05), odbacujemo H0 i zaključujemo da distribucije ocjena nisu normalne za oba spola.

# !! Komentari za asistenta:
# 1. **Uklanjanje outliera - Ima li smisla ukloniti outlieri?**
# 2. **T-test i normalnost - Je li dovoljno da distribucija bude približno normalna za primjenu t-testa?** Iako W statistika iz Shapiro-Wilk testa je relativno visoka (oko 0.9+), distribucije se očigledno ne ponašaju u potpunosti normalno, pa je pitanje je li dovoljno za korištenje t-testa.
# 3. **Alternativa za nenormalnu distribuciju**: Ako distribucija nije normalna, koristimo **Mann-Whitney U test** (neparametrijski test), koji ne zahtijeva normalnost distribucije.Jel to okej?

# 4. Leveneov test za homogenost varijanci:
# Testiramo pretpostavku homogenosti varijanci pomoću Leveneovog testa.
# !! Napomena: Leveneov test je otporan na nenormalnost distribucije, pa je to bolja opcija od F-testa kada distribucije nisu savršeno normalne.
# !! Pitat asistenta: Jel to okej i jel ima smisla? Ili je okej koristiti i f-test iako je on jako osjetljiv na normalnost? , imamo veliki n > 30

if (!require(car)) install.packages("car")
library(car)

# Leveneov test za varijance među spolovima
levene_test_result <- leveneTest(G3_mat ~ sex, data = studentData)

# Rezultat Leveneovog testa
print(levene_test_result)

# Rezultat: p-vrijednost je 0.6711, što znači da ne možemo odbaciti H0 i pretpostavljamo da varijance između spolova nisu različite.
# Ako je p-vrijednost veća od 0.05, pretpostavljamo da varijance između spolova nisu značajno različite.

# !! Komentari za asistenta:
# 1. **Leveneov test vs F-test**: Leveneov test je prikladniji u ovom slučaju jer je otporan na nenormalnost distribucije.
# 2. **F-test za homogenost varijanci**: Iako možemo koristiti F-test, on zahtijeva normalnost distribucije, pa bi u našem slučaju bio manje primjeren, jer smo već uočili odstupanja od normalnosti u QQ plotovima.

# 5. Statistički test:
# Ako su normalnost i homogenost zadovoljeni -> koristit ćemo t-test.
# Ako nisu zadovoljeni -> koristit ćemo Mann-Whitney U test.
t_test_result <- t.test(G3_mat ~ sex, data = studentData, var.equal = TRUE, alternative = "less") # mozda je bolje bez less cisto zbog Izbjegavanja stereotipa
print(t_test_result)
# Odbacujemo H0 u korist h1, tj. djecaci imaju vecu srednju ocjenu iz matematike od djevojaka

# S obzirom da distribucije nisu normalne, koristimo Mann-Whitney U test za usporedbu srednjih ocjena između spolova.
# Mann-Whitney U test
wilcox_test_result <- wilcox.test(G3_mat ~ sex, data = studentData)
print(wilcox_test_result)

# Rezultat: p-vrijednost je manja od 0.05, što znači da možemo odbaciti H0 i zaključiti da postoji statistički značajna razlika u distribuciji ocjena između dječaka i djevojčica.

# !! Komentari za asistenta:
# 1. **Srednje vrijednosti - mogu li zaključiti da su srednje vrijednosti različite?** Iako smo utvrdili da postoji statistički značajna razlika u distribuciji ocjena, to ne znači nužno da su srednje vrijednosti različite.
# 2. **Bootstrap metoda**: Možemo razmisliti o korištenju bootstrap metode za dodatnu provjeru.


# Analiza 4. pitanja: Postoji li razlika u broju izostanka iz nastave?
cat("Postoji li razlika u broju izostanak iz nastave?")
# Ispisujemo broj izostanak iz matematike i portugalskog jezika
cat("\nIzostanci iz matematike:\n")
print(studentData$absences_mat)

cat("\nIzostanci iz portugalskog jezika:\n")
print(studentData$absences_por)

# Racunamo koliko je ukupno izostanaka i od sad na dalje gledamo ukupne izostanke (matematika + portugalski)
# Osim ako nije drugačije navedeno
studentData$total_absences <- studentData$absences_mat + studentData$absences_por
# Ispisujemo
cat("\nUkupni izostanci:\n")
print(studentData$total_absences)

# Histogram izostanaka
ggplot(studentData, aes(x = total_absences)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(
    title = "Distribucija izostanaka",
    x = "Broj izostanaka",
    y = "Broj učenika"
  ) +
  theme_minimal()

# Sažetak statistike za izostanke
summary(studentData$total_absences)
# Boxplot izostanaka
ggplot(studentData, aes(y = total_absences)) +
  geom_boxplot(fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(
    title = "Boxplot izostanaka",
    x = "",
    y = "Broj izostanaka"
  ) +
  theme_minimal()

# IQR metoda za identifikaciju outliera
Q1 <- quantile(studentData$total_absences, 0.25)
Q3 <- quantile(studentData$total_absences, 0.75)
IQR <- Q3 - Q1

# Definicija outliera
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filtriranje outliera
cleaned_data <- studentData[studentData$total_absences >= lower_bound & studentData$total_absences <= upper_bound, ]

# Provjera rezultata nakon uklanjanja outliera
summary(cleaned_data$total_absences)
# Boxplot izostanaka
ggplot(cleaned_data, aes(y = total_absences)) +
  geom_boxplot(fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(
    title = "Boxplot izostanaka",
    x = "",
    y = "Broj izostanaka"
  ) +
  theme_minimal()


# Sad radimo analizu između izostanaka i mjesta stanovanja
# H0: Nema razlike u iznosu izostanaka između učenika iz urbanih i ruralnih područja.
# H1: Učenici iz ruralnih područja imaju veće izostanke od učenika iz urbanih područja.

# Shapiro-Wilk normality test za urbane i ruralne izostanke
cat("\nShapiro-Wilk test za normalnost (urbano):\n")
shapiro.test(studentData$total_absences[studentData$address == "U"])
cat("\nShapiro-Wilk test za normalnost (ruralno):\n")
shapiro.test(studentData$total_absences[studentData$address == "R"])

# Wilcoxon rank sum test (Mann-Whitney U test)
# Zato što podaci nisu normalno distribuirani koristimo ovaj neparametarski test koji omogućuje usporedbu dviju nezavisnih skupina na temelju njihovih rangova, a ne na pretpostavci o normalnosti
wilcox_result <- wilcox.test(total_absences ~ address, data = studentData)

# Prikaz rezultata testa
print(wilcox_result)

# P-vrijednost > 0.05 i zato ne možemo odbaciti nultu hipotezu
# Zaključujemo da nema razlike u iznosu izostanaka između učenika iz ruralnih i urbanih područja

# Vizualizacija izostanaka po mjestu stanovanja
# Box plot za ukupne izostanke po mjestu stanovanja
ggplot(studentData, aes(x = address, y = total_absences, fill = address)) + 
  geom_boxplot(color = "black", alpha = 0.7) + 
  labs(
    title = "Distribucija ukupnih izostanaka po mjestu stanovanja",
    x = "Mjesto stanovanja",
    y = "Ukupni izostanci"
  ) + 
  scale_fill_manual(values = c("lightgreen", "lightcoral")) + 
  theme_minimal()

# Testiramo postoji li razlika u broju izostanaka i konzumacije alkohola tijekom tjedna
# H0: Ne postoji razlika u distribuciji zmeđu različitih grupa konzumacije alkohola tjedno.
# H1: Postoji razlika u distribuciji zmeđu različitih grupa konzumacije alkohola tjedno

# Provjera normalnosti za izostanke
shapiro_test_absences <- shapiro.test(studentData$total_absences)

# Ispis rezultata
print(shapiro_test_absences)

# Provjera normalnosti za Dalc (konzumacija alkohola)
shapiro_test_dalc <- shapiro.test(studentData$Dalc)

# Ispis rezultata
print(shapiro_test_dalc)


# Ako p-vrijednost < 0.05, podaci nisu normalni i možemo koristiti Kruskal-Wallis test


# Kruskal-Wallis test za izostanke u odnosu na konzumaciju alkohola tjedno
kruskal_test_alcohol <- kruskal.test(total_absences ~ Dalc, data = studentData)

# Ispis rezultata testa
print(kruskal_test_alcohol)

# Budući da je p-vrijednost < 0.05 odbacujemo nultu hipotezu 
# Možemo zaključiti da da postoji statistički značajna razlika u izostancima između grupa
# Ovaj test ne daje izravno odgovor na to je li viša konzumacija alkohola povezana s većim brojem izostanaka,
# ali daje pokazatelj da razlike u izostancima postoje među grupama

# Post-hoc analiza
# Koristi se za parne usporedbe između svih kombinacija grupa, nakon što je Kruskal-Wallis pokazao postojanje razlika
# Pairwise Wilcoxon rank sum test za izostanke po konzumaciji alkohola tjedno
pairwise_wilcox_alcohol <- pairwise.wilcox.test(studentData$total_absences, studentData$Dalc, 
                                                p.adjust.method = "BH")

# Ispis rezultata
print(pairwise_wilcox_alcohol)

# Postoji razlika između grupa 1 i 3
# Odbacujemo H0 koja kaze da nema razlike između tih dviju grupa
# "Na temelju pairwise Wilcoxon testa, 
# otkriveno je da postoji statistički značajna razlika u broju izostanaka između učenika 
# koji rijetko konzumiraju alkohol (grupa 1) i onih koji češće konzumiraju alkohol (grupa 3). 
# Učenici u grupi 3 (češća konzumacija alkohola) imaju više izostanaka u usporedbi s 
# učenicima u grupi 1 (rijetka konzumacija alkohola), što može sugerirati da 
# konzumacija alkohola utječe na povećanje broja izostanaka."


# Srednje vrijednosti izostanaka po grupama
group_means <- tapply(studentData$total_absences, studentData$Dalc, mean)
# Ispis srednjih vrijednosti
print(group_means)

# Pretpostavljamo da su grupe 1 (rijetko) i 3 (češće)
mean_group_1 <- group_means[1]
mean_group_3 <- group_means[3]

# Izračunaj procentualnu razliku između grupe 1 i grupe 3
percentage_diff <- ((mean_group_3 - mean_group_1) / mean_group_1) * 100
cat("Procentualna razlika između grupe 1 i grupe 3:", percentage_diff, "%\n")

# Boxplot za izostanke prema konzumaciji alkohola tjedno
ggplot(studentData, aes(x = factor(Dalc), y = total_absences, fill = factor(Dalc))) +
  geom_boxplot() +
  labs(
    title = "Izostanci u odnosu na konzumaciju alkohola tjedno",
    x = "Konzumacija alkohola tjedno",
    y = "Izostanci"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# Pretvori Dalc u numeričku varijablu
studentData$Dalc <- as.numeric(as.character(studentData$Dalc))

# Korelacija između konzumacije alkohola i izostanaka (Spearman)
cor_alcohol_absences <- cor(studentData$Dalc, studentData$total_absences, method = "spearman")
# Ispis rezultata
cat("Spearman korelacija između konzumacije alkohola i izostanaka: ", cor_alcohol_absences, "\n")

# Procentualna razlika ukazuje na značajnu razliku između grupa (1 i 3)
# u prosječnim brojevima izostanaka, dok Spearmanova korelacija pokazuje da ta razlika 
# nije dovoljno jaka ili dosljedna kroz cijeli skup podataka

# Ukratko, postoji statistički značajna razlika u izostancima među grupama, 
# a povećanje konzumacije alkohola može biti povezano s višim izostancima, 
# ali ta povezanost nije jako jaka kad gledamo cijeli skup podataka







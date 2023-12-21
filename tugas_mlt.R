library(haven)
library(dplyr)
library(MASS)
library(car)

#LOAD DATASET======================================================
fkrtl <- read_dta("C:/Users/asus/OneDrive - UGM 365/Data Sampel BPJS Kes 2015-2020/Data Reguler/2019202003_fkrtl.dta") # nolint
peserta <- read_dta("C:/Users/asus/OneDrive - UGM 365/Data Sampel BPJS Kes 2015-2020/Data Reguler/2015202001_kepesertaan.dta") # nolint

# DATA PRE-PROCESSING==============================================
## Subset for only cancer patient and year 2020
fkrtl <- fkrtl[fkrtl$FKL03 >= "2020-01-01", ]
target <- c("C000",	"C001",	"C002",	"C003",	"C004"
    , "C005",	"C006",	"C008",	"C009",	"C01",	"C020",	"C021", "D097"
    , "C022",	"C023",	"C024",	"C028",	"C029",	"C030",	"C031",	"D099"
    , "C039",	"C040",	"C041",	"C048",	"C049",	"C050",	"C051",	"C052"
    , "C058",	"C059",	"C060",	"C061",	"C062",	"C068",	"C069",	"C07"
    , "C080",	"C081",	"C088",	"C089",	"C090",	"C091",	"C098",	"C099"
    , "C100",	"C101",	"C102",	"C103",	"C104",	"C108",	"C109",	"C110"
    , "C111",	"C112",	"C113",	"C118",	"C119",	"C12",	"C130",	"C131"
    , "C132",	"C138",	"C139",	"C140",	"C141",	"C142",	"C148",	"C150"
    , "C151",	"C152",	"C153",	"C154",	"C155",	"C158",	"C159",	"C160"
    , "C161",	"C162",	"C163",	"C164",	"C165",	"C166",	"C168",	"C169"
    , "C170",	"C171",	"C172",	"C173",	"C178",	"C179",	"C180",	"C181"
    , "C182",	"C183",	"C184",	"C185",	"C186",	"C187",	"C188",	"C189"
    , "C19",	"C20",	"C210",	"C211",	"C212",	"C218",	"C220",	"C221"
    , "C222",	"C223",	"C224",	"C227",	"C229",	"C23",	"C240",	"C241"
    , "C248",	"C249",	"C250",	"C251",	"C252",	"C253",	"C254",	"C257"
    , "C258",	"C259",	"C260",	"C261",	"C268",	"C269",	"C300",	"C301"
    , "C310",	"C311",	"C312",	"C313",	"C318",	"C319",	"C320",	"C321"
    , "C322",	"C323",	"C328",	"C329",	"C33",	"C340",	"C341",	"C342"
    , "C343",	"C348",	"C349",	"C37",	"C380",	"C381",	"C382",	"C383"
    , "C384",	"C388",	"C390",	"C398",	"C399",	"C400",	"C401",	"C402"
    , "C403",	"C408",	"C409",	"C410",	"C411",	"C412",	"C413",	"C414"
    , "C418",	"C419",	"C430",	"C431",	"C432",	"C433",	"C434",	"C435"
    , "C436",	"C437",	"C438",	"C439",	"C440",	"C441",	"C442",	"C443"
    , "C444",	"C445",	"C446",	"C447",	"C448",	"C449",	"C470",	"C471"
    , "C472",	"C473",	"C474",	"C475",	"C476",	"C478",	"C479",	"C480"
    , "C481",	"C482",	"C488",	"C490",	"C491",	"C492",	"C493",	"C494"
    , "C495",	"C496",	"C498",	"C499",	"C500",	"C501",	"C502",	"C503"
    , "C504",	"C505",	"C506",	"C508",	"C509",	"C510",	"C511",	"C512"
    , "C518",	"C519",	"C52",	"C530",	"C531",	"C538",	"C539",	"C540"
    , "C541",	"C542",	"C543",	"C548",	"C549",	"C55",	"C56",	"C570"
    , "C571",	"C572",	"C573",	"C574",	"C577",	"C578",	"C579",	"C58"
    , "C600",	"C601",	"C602",	"C608",	"C609",	"C61",	"C620",	"C621"
    , "C629",	"C630",	"C631",	"C632",	"C637",	"C638",	"C639",	"C64"
    , "C65",	"C66",	"C670",	"C671",	"C672",	"C673",	"C674",	"C675"
    , "C676",	"C677",	"C678",	"C679",	"C680",	"C681",	"C688",	"C689"
    , "C690",	"C691",	"C692",	"C693",	"C694",	"C695",	"C696",	"C698"
    , "C699",	"C700",	"C701",	"C709",	"C710",	"C711",	"C712",	"C713"
    , "C714",	"C715",	"C716",	"C717",	"C718",	"C719",	"C720",	"C721"
    , "C722",	"C723",	"C724",	"C725",	"C728",	"C729",	"C73",	"C740"
    , "C741",	"C749",	"C750",	"C751",	"C752",	"C753",	"C754",	"C755"
    , "C758",	"C759",	"C760",	"C761",	"C762",	"C763",	"C764",	"C765"
    , "C767",	"C768",	"C770",	"C771",	"C772",	"C773",	"C774",	"C775"
    , "C778",	"C779",	"C780",	"C781",	"C782",	"C783",	"C784",	"C785"
    , "C786",	"C787",	"C788",	"C790",	"C791",	"C792",	"C793",	"C794"
    , "C795",	"C796",	"C797",	"C798",	"C80",	"C900",	"C901",	"C902"
    , "C97",	"D000",	"D001",	"D002",	"D010",	"D011",	"D012",	"D013"
    , "D014",	"D015",	"D017",	"D019",	"D020",	"D021",	"D022",	"D023"
    , "D024",	"D030",	"D031",	"D032",	"D033",	"D034",	"D035",	"D036"
    , "D037",	"D038",	"D039",	"D040",	"D041",	"D042",	"D043",	"D044"
    , "D045",	"D046",	"D047",	"D048",	"D049",	"D050",	"D051",	"D057"
    , "D059",	"D060",	"D061",	"D067",	"D069",	"D070",	"D071",	"D072"
    , "D073",	"D074",	"D075",	"D076",	"D090",	"D091",	"D092",	"D093")
fkrtl <- fkrtl[fkrtl$FKL18 %in% target, ]

## drop duplicated values
sum(duplicated(fkrtl))
fkrtl <- fkrtl %>%
  distinct(.keep_all = TRUE)

## Feature engineering (creating variables)
### creating lama_kunjungan variable
fkrtl$lama_kunjungan <- fkrtl$FKL04 - fkrtl$FKL03 + 1

### creating age variable
current_year <- 2020
peserta$usia <- current_year - as.numeric(format(
  as.Date(peserta$PSTV03, format = "%d/%m/%Y"), "%Y"))

### creating jumlah rujukan variable
fkrtl <- fkrtl %>%
  group_by(FKP02) %>%
  mutate(jumlah_rujukan = n())
fkrtl$jumlah_rujukan[is.na(fkrtl$jumlah_rujukan)] <- 0

### create response variable
fkrtl <- fkrtl %>%
  group_by(PSTV01) %>%
  mutate(Frekuensi_klaim = n())

## Discretization Numerical Variable
### age discretization (<20 = 1, 20-40 = 2, >40 = 3)
peserta$usia <- cut(peserta$usia,
                   breaks=c(-1, 19, 39, 120),
                   labels=c(1, 2, 3))

## Re-groping variable
### re-grouping status pernikahan  (1 = tidak kawin, 2 = kawin)
peserta$PSTV06[peserta$PSTV06 == 3] <- 1
peserta$PSTV06 <- factor(peserta$PSTV06, levels = c(1, 2, NaN))

### re-grouping Provinsi Tempat tinggal
peserta <- peserta %>%
  mutate(PSTV09 = case_when(
    PSTV09 %in% c(31, 32, 33, 34, 35, 36) ~ "1",
    PSTV09 %in% c(13, 14, 16, 18, 51, 52) ~ "2",
    PSTV09 %in% c(11, 12, 15, 17, 19, 21, 61, 71, 72, 73, 74, 75, 76) ~ "3",
    PSTV09 %in% c(62, 63, 64, 65) ~ "4",
    PSTV09 %in% c(53, 81, 82, 91, 94) ~ "5"))

### re-grouping FKL09: Tipe FKRTL
fkrtl$FKL09 <- cut(fkrtl$FKL09, #(RS Kelas	1, RS Swasta	2, RS TNI Polri	3, RS khusus	4, lain-lain 5) # nolint: line_length_linter.
                   breaks = c(0, 4, 8, 12, 24, 27),
                   labels = c(1, 2, 3, 4, 5))

### re-grouping FKL07: kepemilikian fkrtl
fkrtl <- fkrtl %>% #swasta = 1, non-swasta = 2
  mutate(FKL07 = case_when(
    FKL07 %in% c(9) ~ "1",
    FKL07 %in% c(1, 2, 3, 4, 5, 6, 7, 8) ~ "2"))

### re-grouping FKL12: segmentasi peserta
fkrtl$FKL12 <- cut(fkrtl$FKL12, #(Bukan pekerja = 1, c(PBI APBN, PBI APBD, PBPU, PPU) = 2) # nolint: line_length_linter.
                   breaks = c(0, 1, 5),
                   labels = c(1, 2))


## Dropping variables
fkrtl <- subset(fkrtl, select = -c(FKP02, FKL02, FKL03,
  FKL04, PSTV02, PSTV15, FKL41, FKL42, FKL43, FKL44, FKL45,
  FKL46, FKL47, FKL48, FKL40, FKL39, FKL38, FKL37, FKL36,
  FKL35, FKL34, FKL33, FKL32, FKL30, FKL29, FKL28, FKL27,
  FKL26, FKL25, FKL22, FKL21, FKL20, FKL19, FKL19A, FKL18,
  FKL18A, FKL17, FKL17A, FKL16A, FKL16, FKL15A, FKL15, FKL13,
  FKL12, FKL11, FKL10, FKL06, FKL05, FKL31))
peserta <- subset(peserta, select = -c(PSTV02, PSTV03, PSTV06, # nolint: line_length_linter.
  PSTV07, PSTV08, PSTV10, PSTV11, PSTV13, PSTV14, PSTV15, PSTV16,
  PSTV17, PSTV18))

## data merging
df <- merge(peserta, fkrtl, by = "PSTV01", all.x = TRUE)
head(df)

## drop duplicated values
df <- df %>%
  distinct(.keep_all = TRUE)
nrow(df)

# Data preprocessing II========================================
## Changing df columns name
colnames(df) <- c("No_peserta", "Hub_keluarga", "JK", "Prov_domisili",
    "Jenis_faskes", "Usia", "Kepemilikan_FKRTL", "Jenis_FKRTL",
    "Tipe_FKRTL", "Status_kepulangan", "Tingkat_keparahan",
    "Lama_kunjungan", "Jumlah_rujukan", "Frekuensi_klaim")
## Checking missing value in dataset
colSums(is.na(df))
## Imputing missing value
df$Jumlah_rujukan <- replace(df$Jumlah_rujukan, is.na(df$Jumlah_rujukan), 0)
df$Lama_kunjungan <- replace(df$Lama_kunjungan, is.na(df$Lama_kunjungan), 0)
df$Frekuensi_klaim <- replace(df$Frekuensi_klaim, is.na(df$Frekuensi_klaim), 0)
## Checking missing value in dataset
colSums(is.na(df))

# EXPORTING DATASET===========================================
write.csv(df, "Data Hasil Preprocessing.CSV")

# DATA VISUALIZATION==========================================
## PSTV05: jenis kelamin
bar <- barplot(table(df$JK), col = "skyblue")
text(bar, table(df$JK) - 500, labels = table(df$JK))
## fasilitas kesehatan
bar <- barplot(table(df$Jenis_faskes), col = "orange")
text(bar, table(df$Jenis_faskes) - 500, labels = table(df$Jenis_faskes))
## klaim
bar <- barplot(table(df$Frekuensi_klaim), col = "purple", main = "Klaim",)
text(bar, table(df$Frekuensi_klaim) + 100, labels = table(df$Frekuensi_klaim))
## Usia
bar <- barplot(table(df$Usia), col = "skyblue")
text(bar, table(df$Usia) - 500, labels = table(df$Usia))

# MODELLING =================================================
## Model 1: Full Model
pois <- glm(Frekuensi_klaim ~ factor(Hub_keluarga) + JK + 
        factor(Usia) + Lama_kunjungan + Jumlah_rujukan,
    family = poisson(link = "log"), data = df)
summary(pois)
cat("Deviance dari model 1 yang signifikan: ",
  pois$deviance / pois$df.residual)
cat("AIC dari model 1 yang signifikan: ", pois$aic)

## Model 2: Interaksi hub_keluarga*Usia
pois2 <- glm(Frekuensi_klaim ~ factor(Hub_keluarga)*factor(Usia) + JK + 
       Lama_kunjungan + Jumlah_rujukan,
    family = poisson(link = "log"), data = df)
summary(pois2)
cat("Deviance dari model 2 yang signifikan: ",
  pois2$deviance / pois2$df.residual)
cat("AIC dari model 2 yang signifikan: ", pois2$aic)

## Model 3: Interaksi Lama_kunjungan*Jumlah_rujukan
pois3 <- glm(Frekuensi_klaim ~ factor(Hub_keluarga) + JK + 
        factor(Usia) + Lama_kunjungan*Jumlah_rujukan,
    family = poisson(link = "log"), data = df)
summary(pois3)
cat("Deviance dari model 3 yang signifikan: ",
  pois3$deviance / pois3$df.residual)
cat("AIC dari model 3 yang signifikan: ", pois3$aic)

## Model 4: Interaksi JK*Usia
pois4 <- glm(Frekuensi_klaim ~ factor(Hub_keluarga) + JK*factor(Usia) 
            + Lama_kunjungan + Jumlah_rujukan,
    family = poisson(link = "log"), data = df)
summary(pois4)
cat("Deviance dari model 4 yang signifikan: ",
  pois4$deviance / pois4$df.residual)
cat("AIC dari model 4 yang signifikan: ", pois4$aic)

## Model 5: Interaksi JK*Usia
pois5 <- glm(Frekuensi_klaim ~ factor(Hub_keluarga) + JK*factor(Usia) 
            + Lama_kunjungan*Jumlah_rujukan,
    family = poisson(link = "log"), data = df)
summary(pois5)
cat("Deviance dari model 5 yang signifikan: ",
  pois5$deviance / pois5$df.residual)
cat("AIC dari model 5 yang signifikan: ", pois5$aic)

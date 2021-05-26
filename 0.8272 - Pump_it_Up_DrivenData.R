
# -------------------------------------------------------------------- #
# Competition: "Pump it Up: Data Mining the Water Table - Driven Data
# Date: "16/02/2021"
# Ranking: 42
# Score: 0.8272
# -------------------------------------------------------------------- #



suppressPackageStartupMessages({ 
  library(data.table)   # Clean and manipulate data (manejo de dataframes)
  library(dplyr)        # Clean and manipulate data - Tidyverse
  library(ggplot2)      # Graphics engine
  library(inspectdf)    # EDA (Exploratory data analysis)
  library(DataExplorer) # EDA (Exploratory data analysis)
  library(tictoc)       # Measure existing times
  library (ranger)      # Random forest
  library(forcats)      # Manipulate categorical variables - Tidyverse
  library(stringr)      # Manipulate strings - Tidyverse
  library(stringi)      # Manipulate strings - Fast
  library(lubridate)    # Manipulate dates - Tidyverse
})



# Load train and test datasets from the competition
labIn <- fread("./data/01_labels.csv", nThread = 3)
trainIn <- fread("./data/02_trainset.csv", data.table = FALSE) 
testIn <- fread("./data/03_testset.csv", data.table = FALSE)
trainIn$status_group <- as.factor(labIn$status_group)



# VARIABLES #
# Select categorical variables to include
cat_df <- trainIn %>% select(where(is.character)) 
res_levels <- as.data.frame(apply(cat_df, 2, function(x) length(unique(Filter(is.character, x)))))
names(res_levels)[1] <- c('numlevels')
res_levels$vars <- rownames(res_levels)
rownames(res_levels) <- NULL
res_levels %>%  arrange(desc(-numlevels))
var_mod <- res_levels$vars
var_cat <- setdiff(var_mod, c('quantity_group', 'recorded_by', 'payment_type','subvillage', 'wpt_name', 'waterpoint_type_group', 'extraction_type_group'))

# Select numeric variables
var_num <- trainIn %>% select(where(is.numeric), status_group) %>% names()

# Select binary variables
var_log <- trainIn %>% select(where(is.logical)) %>% names()

var_mod <- c(var_num, var_cat, var_log)
train_numcatlog <- trainIn [ ,var_mod]
train_numcatlog <- cbind(train_numcatlog,trainIn$date_recorded)
names(train_numcatlog)[names(train_numcatlog) == "trainIn$date_recorded"] <- "date_recorded"


# Factorize categorical variables
for ( i in 1:ncol(train_numcatlog)) {
  col_tmp <- train_numcatlog[,i]
  if(class(col_tmp) == "character"){
    train_numcatlog[ ,i] <- as.factor(train_numcatlog[,i])
  } else {next} 
}



# --------------------------------------------------- #
# ---------------- F. ENGINEERING ------------------- #
# --------------------------------------------------- #

# ---- WARD - POP
wardpop <- rbind( trainIn[ ,c('ward', 'population') ], testIn[ , c('ward', 'population')])
wardpop_tmp<- wardpop$ward %>%
  str_to_lower() %>%
  str_replace_all(" ", "") %>%
  str_replace_all("'", "")
wardpop_dt <- data.table(
  ward_tmp = wardpop_tmp,
  population = wardpop$population
wardpop_dt[ ,fe_sum := sum(population), by = ward_tmp]
wardpop_dt[ , fe_freq := .N, by = ward_tmp ]
wardpop_dt [, fe_ratio := fe_sum/fe_freq, ward_tmp]
wardpop_gd <- unique(wardpop_dt[ , .(ward_tmp, fe_ratio)])

  #-- Train:
  wardtrain_tmp <- trainIn$ward %>% 
    str_to_lower() %>%
    str_replace_all(" ", "") %>%
    str_replace_all("'", "")
  
  wardtrain_end <- data.table( ward_end = wardtrain_tmp )
  wardtrain_gd <- merge( wardtrain_end, wardpop_gd, by.x = c('ward_end'), by.y = ('ward_tmp'), sort=FALSE )
  train_numcatlog$fe_wardratio <- wardtrain_gd$fe_ratio
  #-- Test:
  wardtest_tmp <- testIn$ward %>% 
    str_to_lower() %>%
    str_replace_all(" ", "") %>%
    str_replace_all("'", "")
  
  wardtest_end <- data.table( ward_end = wardtest_tmp )
  wardtest_gd <- merge( wardtest_end, wardpop_gd, by.x = c('ward_end'), by.y = ('ward_tmp'), all.x = TRUE, sort=FALSE )
  testIn$fe_wardratio <- wardtest_gd$fe_ratio
  
  
# ---- DISTANCIA
  train_numcatlog$fe_dist <- sqrt ( (train_numcatlog$longitude)^2 + (train_numcatlog$latitude)^2 )
  testIn$fe_dist <- sqrt ( (testIn$longitude)^2 + (testIn$latitude)^2 )
  
  
# ---- FUNDER
  funder_tot <- c(as.vector(train_numcatlog$funder), testIn$funder)
  funder_cln <- funder_tot %>%
    str_to_lower() %>%
    str_replace_all(" ", "") %>%
    str_replace_all("'", "") 
  funder_dt <- data.table(funder_tmp = funder_cln)
  funder_dt[ , fe_funder := .N, by = funder_tmp ]
  funder_gd <- unique(funder_dt)
  setorder(funder_gd, -fe_funder)
  #-- Train:
  funder_tr <- data.table (fund_tr = funder_cln [1:nrow(train_numcatlog)])
  fundertr_gd <- merge ( funder_tr, funder_gd, by.x = c('fund_tr'), by.y = c('funder_tmp'), sort = FALSE)
  train_numcatlog$fe_funder = fundertr_gd$fe_funder
  # --Test:
  funder_tst <- data.table (fund_tr = funder_cln [ (nrow(train_numcatlog) + 1) : length(funder_cln) ] )
  fundertst_gd <- merge ( funder_tst, funder_gd, by.x = c('fund_tr'), by.y = c('funder_tmp'), sort = FALSE)
  testIn$fe_funder = fundertst_gd$fe_funder
  
  
# ---- INSTALLER
  installer_tot <- c(as.vector(train_numcatlog$installer), testIn$installer)
  installer_cl <- installer_tot %>% 
    str_to_lower() %>%            # minusculas
    str_replace_all(" ", "") %>%  # espacios
    str_replace_all("'", "") %>%  # acentos
    str_replace_all("/", "") %>%  # Barra /
    str_replace_all("-", "") 
  
  installer_gd <- fct_lump_min(as.factor(installer_cl), min = 3)
  # --- Train
  installer_tr <- data.table(lump_installer= installer_gd[ 1:nrow(train_numcatlog)])
  train_numcatlog$lump_installer <- installer_tr$lump_installer 
  train_numcatlog$installer<- NULL 
    # --- Test
  installer_test <- data.table(lump_installer = installer_gd[ (nrow(train_numcatlog)+1):length(installer_gd)])
  testIn$lump_installer <- installer_test$lump_installer
  
  
# ---- DAY
  train_numcatlog$fe_day <- as.numeric(as.Date("2021-01-01") - as.Date(train_numcatlog$date_recorded))
  testIn$fe_day<- as.numeric(as.Date("2021-01-01") - as.Date(testIn$date_recorded))
  
  
# ---- MONTH
  train_numcatlog$fe_month <- month(ymd(train_numcatlog$date_recorded))
  testIn$fe_month <- month(ymd(testIn$date_recorded))
  

# ---- SEASON
  #-- Train
  train_numcatlog$date_recorded<- as.Date(train_numcatlog$date_recorded,format="%Y/%m/%d")
  train_numcatlog$mes <- as.numeric(format(train_numcatlog$date_recorded,'%m'))
  Estacion <- train_numcatlog$mes
  Estacion <- if_else((Estacion<= 5)|(Estacion>= 11), 'Humeda','Seca')
  train_numcatlog = cbind(train_numcatlog,Estacion)
  train_numcatlog$Estacion<- as.factor(train_numcatlog$Estacion)
  train_numcatlog$mes<- NULL # Eliminamos la variable mes creada para calcular estacion
  
  #-- Test
  testIn$date_recorded<- as.Date(testIn$date_recorded,format="%Y/%m/%d")
  testIn$mes <- as.numeric(format(testIn$date_recorded,'%m'))
  Estacion <- testIn$mes
  Estacion <- if_else((Estacion<= 5)|(Estacion>= 11), 'Humeda','Seca')
  testIn = cbind(testIn,Estacion)
  testIn$Estacion<- as.factor(testIn$Estacion)
  

# ---- PREMIT
  # -- Train
  permit <- (train_numcatlog$permit)
  permit <- if_else(is.na(permit), TRUE, train_numcatlog$permit)
  train_numcatlog <- cbind(train_numcatlog, permit)
  train_numcatlog <- train_numcatlog[,-32]
  
  # --Test
  permit <- (testIn$permit)
  permit <- if_else(is.na(permit), TRUE, testIn$permit)
  testIn <- cbind(testIn, permit)
  testIn <- testIn[,-23]

    
# ---- PUBLIC MEETING
  # -- Train
  public_meeting <- (train_numcatlog$public_meeting)
  public_meeting <- if_else(is.na(public_meeting), TRUE, train_numcatlog$public_meeting)
  train_numcatlog <- cbind(train_numcatlog, public_meeting)
  train_numcatlog <- train_numcatlog[,-31]
  
  # --Test
  public_meeting <- (testIn$public_meeting)
  public_meeting <- if_else(is.na(public_meeting), TRUE, testIn$public_meeting)
  testIn <- cbind(testIn, public_meeting)
  testIn <- testIn[,-19]
  
  
# ---- INTERACTIONS
  train_numcatlog$public_meeting <- as.factor(train_numcatlog$public_meeting)
  testIn$public_meeting <- as.factor(testIn$public_meeting)
  ```{r}
  # --------------- QUALITY_GROUP ---------------- #
  testIn$quality_group <- as.factor(testIn$quality_group)
  train_numcatlog$int_qg<- train_numcatlog$quality_group : train_numcatlog$public_meeting
  testIn$int_qg <- testIn$quality_group : testIn$public_meeting
  train_numcatlog$quality_group <- NULL
  testIn$quality_group <- NULL
  
  # ---------------- SOURCE_CLASS ---------------- #
  testIn$source_class <- as.factor(testIn$source_class)
  train_numcatlog$int_sc <- train_numcatlog$source_class : train_numcatlog$public_meeting
  testIn$int_sc <- testIn$source_class : testIn$public_meeting
  train_numcatlog$source_class <- NULL
  testIn$source_class <- NULL
  
  # ---------------- GPS_HEIGHT ----------------- #
  train_numcatlog$gps_height <- as.factor(train_numcatlog$gps_height)
  testIn$gps_height <- as.factor(testIn$gps_height)
  train_numcatlog$int_gps <- train_numcatlog$gps_height : train_numcatlog$public_meeting
  testIn$int_gps <- testIn$gps_height : testIn$public_meeting
  train_numcatlog$gps_height <- NULL
  testIn$gps_height <- NULL
  
  # ------------- MANAGEMENT_GROUP --------------- #
  testIn$management_group <- as.factor(testIn$management_group)
  train_numcatlog$int_mg <- train_numcatlog$management_group : train_numcatlog$public_meeting
  testIn$int_mg <- testIn$management_group : testIn$public_meeting
  train_numcatlog$management_group <- NULL
  testIn$management_group <- NULL
  
  
  
  
  
# --------------------------------------------------- #
# -------------------- MODEL ------------------------ #
# --------------------------------------------------- #
  mymodel_impor <- ranger(
    status_group ~., 
    importance = 'impurity',
    num.trees = 1000 , 
    mtry = 7 , 
    data=train_numcatlog
  )
  
  mymodel_impor$prediction.error
  
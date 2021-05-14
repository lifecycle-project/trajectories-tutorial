################################################################################
## Project: lc-mlm-tutorial
## Script purpose: Upload data
## Date: 21sdt April 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(dsUpload)
library(MolgenisArmadillo)
library(tableone)
library(haven)
library(stringr)
library(tidyverse)

setwd("/Users/timcadman/OneDrive - University of Bristol/repos/lc-mlm-tutorial")
################################################################################
# 1. Load stata files  
################################################################################
data <- list(
	mlm_alspac = read_dta("./data/ALSPAC_simulatedData.dta"),
	mlm_bcg = read_dta("./data/BCG_simulatedData.dta"),
	mlm_bib = read_dta("./data/BiB_simulatedData.dta"),
	mlm_chs = read_dta("./data/CHS_simulatedData.dta"),
	mlm_probit = read_dta("./data/PROBIT_simulatedData.dta")
	)

################################################################################
# 2. Tidy up data, set classes etc  
################################################################################
data <- data %>%
map(function(x){

x %>%
mutate(
	sex = as.factor(girl), 
	ethnicity = as.factor(ethnicity),
	pat_soc = as.factor(father_socialclass), 
	mat_ed = as.factor(mother_education)
	) %>%
select(-girl, -father_socialclass, -mother_education) %>%
as.data.frame()

})


################################################################################
# 2. Log in to test server
################################################################################
mlm_login_data <- data.frame(
  server = "https://armadillo.test.molgenis.org", 
  storage = "https://armadillo-minio.test.molgenis.org", 
  driver = "ArmadilloDriver")

du.login(mlm_login_data)

################################################################################
# 3. Create buckets to use in the analysis  
################################################################################
buckets <- c("mlmalspac", "mlmbcg", "mlmbib", "mlmchs", "mlmprobit")

buckets %>% map(armadillo.create_project)

################################################################################
# 4. Upload data  
################################################################################
names(data) <- buckets

data %>%
imap(
	~armadillo.upload_table(
		project = .y,
		folder = "tutorial", 
		table = .x, 
		name = "data")
	)
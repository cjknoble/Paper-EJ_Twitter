library(dplyr)
library(tidyr)
library(sf)
library(tibble)
library(psych)
library(car)
library(spdep)
library(sp)
library(rgdal)
library(spatialreg)

###############################################################################
################################ Analysis #####################################
###############################################################################

# Import shapefiles - Tweet Counts with Selected (1k) Words Only 
gb_tract <- readOGR(".../Shapefiles/Tract_HazResACS_Phase5_v4.shp", GDAL1_integer64_policy=T, verbose=T)

# Calculate environmental justice awareness and plot density
gb_tract$ej_awareness <- RankNorm(gb_tract$EJTweetCou / gb_tract$Area_Tract)
plot(stats::density(gb_tract$ej_awareness))

# Correct MNDWI by dividing value by 1 million (should be on scale -1 to 1)
gb_tract$MNDWI_Mean <- gb_tract$MNDWI_Mean / 1000000

### LINEARITY
variables_to_plot <- c("Prp_Blck_A", "Prp_Hspnc_", "Prp_Blw_Hg", "Prp_Ttl_W_", "Medn_Incom",
                       "Cntm_Sit_1", "FldZne_Per", "PM25", "Trnst_Sts1", "UrbanArea_", 
                       "Prp_Hshlds", "NDVI_Grd_C", "MSAVI_Mean", "VARI_Mean", 
                       "NDMI_Mean_", "MNDWI_Mean", "AWEI_Mean", "Lndflls_Ds", 
                       "BrwnFlds_D", "Prks_Dst_N", "Grnd_Wtr_1", "AFV_Stats1")

for (var in variables_to_plot) {
  scatter.smooth(gb_tract[[var]], gb_tract$ej_awareness, lpars = list(col = "red"))
}

### MODELS
# Define formulas and calculate AIC
formula_list <- list(
  ej_awareness ~ NDVI_Grd_C,
  ej_awareness ~ MSAVI_Mean,
  ej_awareness ~ VARI_Mean,
  ej_awareness ~ NDMI_Mean_,
  ej_awareness ~ MNDWI_Mean + I(MNDWI_Mean^2),
  ej_awareness ~ AWEI_Mean + I(AWEI_Mean^2)
)

for (f in formula_list) {
  print(summary(lm(f, data = gb_tract)))
  print(AIC(lm(f, data = gb_tract)))
}

### INDEPENDENCE
df_data <- gb_tract %>%
  select(Prp_Blck_A, Prp_Hspnc_, Prp_Hshlds, Prp_Blw_Hg, Prp_Ttl_W_, Medn_Incom, 
         Cntm_Sit_1, FldZne_Per, PM25, Trnst_Sts1, UrbanArea_, MNDWI_Mean, 
         NDMI_Mean_, Lndflls_Ds, BrwnFlds_D, Prks_Dst_N, Grnd_Wtr_1, AFV_Stats1) %>%
  data.frame()

cor_matrix <- cor(df_data)
View(cor_matrix)
pairs.panels(cor_matrix)

### FINAL FORMULA
final_formula <- ej_awareness ~ 
  Prp_Blck_A + 
  Prp_Hspnc_ + 
  Prp_Hshlds + I(Prp_Hshlds^2) + 
  Prp_Blw_Hg + 
  Prp_Ttl_W_ + I(Prp_Ttl_W_^2) + 
  Medn_Incom + 
  Cntm_Sit_1 + 
  FldZne_Per +  
  PM25 + 
  NDMI_Mean_ + 
  Trnst_Sts1 + 
  UrbanArea_ + 
  MNDWI_Mean + 
  AFV_Stats1

# Fit and summarize the linear model
summary(lm(final_formula, data = gb_tract))

# Spatial analysis
gb_adj_listw <- nb2listw(poly2nb(gb_tract), style="W", zero.policy = TRUE)
plot.listw(gb_adj_listw, coordinates(gb_tract))

# Test for global spatial autocorrelation
lm.morantest(lm(gb_tract$ej_awareness ~ 1), gb_adj_listw)
moran.plot(gb_tract$ej_awareness, listw = gb_adj_listw, xlab = 'Area Weighted INT Tweet Count', ylab = 'Area Weighted INT Spatially Lagged Tweet Count')

# LM tests for spatial autocorrelation
lm.LMtests(lm(final_formula, data = gb_tract), gb_adj_listw, test="all")

# Fit and summarize spatial error model
summary(lmoi_sar_error_tract <- errorsarlm(final_formula, data = gb_tract, listw = gb_adj_listw))
AIC(lmoi_sar_error_tract)


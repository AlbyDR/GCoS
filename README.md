# Green Cooling Services
Simulating and mapping Green Cooling Services in urban environments

This page aims to share R codes to simulate Evapotranspirative Cooling Service and Radiative Cooling Service based on urban ET and skin surface temperature using the Soil-Canopy-Observation of Photosynthesis and Energy fluxes (SCOPE). A case study using open-access data for different European cities is used throughout the repository. SCOPE code is originally in MATLAB, but it will be run through the R package rSCOPE (see more at https://github.com/AlbyDR/rSCOPE).

### **The codes are divided into:**

**1- Model inputs collection and preprocessing (R)**

      1.1 Download meteorological station data (DWD - German Climate Data Center)
      
      1.2 Interpolation of the meteorological data

      1.3 LAI derived Remote Sensing Data (Copernicus)

      1.4 Vegetation height and vegetation fraction (Berlin Environmental Atlas)

      1.5 Eddy Covariance data cleaning and footprints for validation (TUB UCO)
 
   
   
**2- Modelling (Matlab through R)**

      2.1 SCOPE input parameters preparation (pixel timeseries)  

      2.2 Run the SCOPE model

      2.3 Getting the prediction
      
      2.4 Simulated climate change scenarios (Sensitivity analysis)
     
   
   
   
**3- Results and Model accuracy assessment (R)**

         3.1 Correct the prediction to urban environments and map ET and Greening Cooling Services
      
         3.2 Model accuracy assessment with EC data (if available)

         3.3 Generate nice plots, maps and figures
         
         
#### References:
Rocha, A. D., Vulova, S., Meier, F., Förster, M., & Kleinschmit, B. (2022). Mapping evapotranspirative and radiative cooling services in an urban environment. SSRN Electronic Journal, 85. https://doi.org/10.2139/ssrn.4089553.

Duarte Rocha, A., Vulova, S., van der Tol, C., Förster, M., and Kleinschmit, B.: Modelling hourly evapotranspiration in urban environments with SCOPE using open remote sensing and meteorological data, Hydrol. Earth Syst. Sci., 26, 1111–1129, https://doi.org/10.5194/hess-26-1111-2022, 2022.

#### Package repository
The codes and R package for the input pre-processing, modelling and mapping are available in GitHub https://github.com/AlbyDR/URBAN_ET and https://github.com/AlbyDR/rSCOPE

Duarte Rocha, A.: AlbyDR/rSCOPE: rSCOPE v1.0 (Evapotranspiration), Zenodo [code], https://doi.org/10.5281/zenodo.6204580, 2022.

#### Data repository
Duarte Rocha, A. (2022). Berlin Evapotranspiration and Cooling Services. https://doi.org/10.14279/depositonce-15870

### Methodology Framework
The flowchart shows the two-stage modelling processing to derive urban ET and greening cooling service index from open-access data inputs.

<img src="https://user-images.githubusercontent.com/40297927/179981190-b0a6445c-e067-40cd-8e4c-78d7d809bad7.png" width=70% height=70%>

###### *Fig. Flowchart of the two-stage modelling approach to derive urban ET from open-access data inputs.*

### Output products:

  - Urban ET [mm] for different aggregation periods (from hourly to annual) that can be divided by soil and canopy

<img src="https://user-images.githubusercontent.com/40297927/180026610-1efa895b-5f1a-44f5-9101-bafdc57d0ca3.png" width=80% height=80%>

###### *Fig. Map of annual ET for Berlin in 2020 (a), zoom-in for the surroundings of the two EC towers, the built-up area TUCC (b) and the residential area ROTH (c), and an urban forest close to residential areas. The distribution of daily modelled ET in the year 2020 at the three locations (e), the red line (built-up area), the black (residential area) and the green (urban forest). The daily ET values from the two towers were extracted (average) using footprints, while the forest values were extracted for the specific forest polygon. Water bodies are not considered in the model and are represented in white.*
</figure>

  - Greening cooling service (GCoS) and the sub-indices Evapotranspirative Cooling Service (ECoS) and Radiative Cooling Service (RCoS)

<img src="https://user-images.githubusercontent.com/40297927/179998177-d884881d-7e04-4420-bc66-aa5e3dd819f8.png" width=80% height=80% />

###### *Fig. Greening cooling service index for the hottest day in 2020 (8th of August) - Berlin (a). The two sub-indices: Evapotranspirative Cooling Service (b) and Radiative Cooling Service. GCoS for six locations (1 km2) for which different surface characteristics (see below LC/LU – Copernicus, Urban Atlas - 2018).*



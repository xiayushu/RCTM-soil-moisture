# RCTM-soil-moisture-v1
This is a soil moisture estimation model developed within the Rangeland Carbon Tracking and Management (RCTM) tool:  https://www.woodwellclimate.org/project/carbon-monitoring-in-rangelands/. The detailed methodology and modeling results are described in the paper published in 2022 at PeerJ "Machine learning based estimation of field-scale daily, high resolution, multi-depth soil moisture for the Western and Midwestern United States" by Xia. Y, Watts, J., Machmuller, M., and Sanderman, J. You can find the paper here: doi: 10.7717/peerj.14275.

We screened soil moisture records (2002-2019) from the U.S. national datasets (SCAN and USCRN, see the figure below for spatial distribution) and joined those with soil, climate, topographic, and remote sensing covariates to build a machine-learning based model for soil moisture estimates from different depths. The algorithm has been validated and then applied at a daily time-step with a spatial resolution of 30m at the field scale. Codes can be modified to change the default spatial/ temporal coverage and resolution. The algorithm was also tested against different land use types including grassland, cropland, forest, pasture, and shrubs. 

Table S2 within our 2022 paper details the use of each script. Our scripts are written in R, Google Colab, and Google Earth Engine. If you have any questions, please feel free to contact Yushu Xia at yxia@woodwellclimate.org.

![Figure 1](https://user-images.githubusercontent.com/113474190/193369473-9baa32d8-a850-471d-948f-a6c0c2aee6c7.png)

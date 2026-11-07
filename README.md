# HJA_snowmelt
R workflow for snowmelt modelling in the H.J. Andrews Experimental Forest, integrating station data preprocessing, common observation periods, potential direct solar radiation, and model calibration across multiple stations using snow water equivalent observations. NOTE: Notations in the sctript are written in german!

#bachelor thesis - University of Bonn

R code for the H. J. Andrews Forest workflow of my bachelor's thesis.

References:

ANDERSON, E. (2006): Snow Accumulation and Ablation Model – SNOW-17. [Online-Publikation]. [Zugriff am: 06.04.2026]. Verfügbar unter: https://www.weather.gov/media/owp/oh/hrl/docs/22snow17.pdf

ANDREWS FOREST PROGRAM (o.J.): Provisional Data Portal. [Website]. [Zugriff am: 08.04.2026]. Verfügbar unter: https://andrewsforest.oregonstate.edu/data/streaming/provisional-data-portal

BEVEN, K. & BINLEY, A. (1992): The future of distributed models: Model calibration and uncertainty prediction. In: Hydrological Processes 6, 3. S. 279-298.

EFSTRATIADIS, A. & KOUTSOYIANNIS, D. (2010): One decade of multi-objective calibration approaches in hydrological modelling: a review. In: Hydrological Sciences Journal – Journal des Sciences Hydrologiques 55,

S. 58-78.
HARPOLD, A. A., KAPLAN, M. L., KLOS, P. Z., LINK, T., MCNAMARA, J. P., RAJAGOPAL, S., SCHUMER, R. & STEELE, C. M. (2017): Rain or snow: hydrologic processes, observations, prediction, and research needs. In: Hydrology and Earth System Sciences 21. S. 1-22.

HAUGHTON, N., ABRAMOWITZ, G., PITMAN, A. J., OR, D., BEST, M. J., JOHNSON, H. R., BALSAMO, G., BOONE, A., CUNTZ, M., DECHARME, B., DIRMEYER, P. A., DONG, J., EK, M., GUO, Z., HAVERD, V., VAN DEN HURK, B. J. J., NEARING, G. S., PAK, B., SANTANELLO JR., J. A., STEVENS, L. E. & VUICHARD, N. (2016): The Plumbing of Land Surface Models: Is Poor Performance a Result of Methodology or Data Quality? In: Journal of Hydrometeorology 17, 6. S. 1705–1720.

HOCK, R. (1999): A distributed temperature-index ice- and snowmelt model including potential direct solar radiation. In: Journal of Glaciology 45, 149. S. 101–111.

HÄRER, S., BERNHARDT, M. & SCHULZ, K. (2016): PRACTISE – Photo Rectification And ClassificaTIon SoftwarE (V.2.1). In: Geoscientific Model Development 9, 1. S. 307-321.

ISMAIL, M. F., BOGACKI, W., DISSE, M., SCHÄFER, M. & KIRSCHBAUER, L. (2023): Estimating degreeday factors of snow based on energy flux components. In: The Cryosphere 17. S. 211–231.

IQBAL, M. (1983): An Introduction to Solar Radiation. (Academic Press) Toronto.

JENÍČEK, M., BEITLEROVÁ, H., HASA, M., KUČEROVÁ, D., PEVNÁ, H. & PODZIMEK, S. (2012): Modelling snow accumulation and snowmelt runoff – present approaches and results. In: AUC Geographica 47, 2. S. 15–24.

KASTEN, F. & YOUNG, A. T. (1989): Revised optical air mass tables and approximation formula. In: Applied Optics 28, 22. S. 4735-4738.

KNOBEN, W. J. M., FREER, J. E. & WOODS, R. A. (2019): Technical note: Inherent benchmark or not? Comparing Nash–Sutcliffe and Kling–Gupta efficiency scores. In: Hydrology and Earth System Sciences 23. S. 4323-4331.

LIU, Y., FERNÁNDEZ-ORTEGA, J., MUDARRA, M. & HARTMANN, A. (2022): Pitfalls and a feasible solution for using KGE as an informal likelihood function in MCMC methods: DREAM(ZS) as an example. In: Hydrology and Earth System Sciences 26. S. 5341-5355.

MCKEE, A. & BIERLMAIER, F. (1987): H.J. Andrews Experimental Forest, Oregon. In: Greenland, D. (Hrsg.): The climates of the Long-Term Ecological Research sites. Occas. Pap. 44. Boulder, CO.

MOTEVALLI, M., ZADBAR, A., ELYASI, E. & JALAAL, M. (2015): Using Monte-Carlo approach for analysis of quantitative and qualitative operation of reservoirs system with regard to the inflow uncertainty. In: Journal of African Earth Sciences 105. S. 1–16.

OUGAHI, J. H. & ROWAN, J. S. (2024): Combining Hydrological Models and Remote Sensing to Characterize Snowpack Dynamics in High Mountains. In: Remote Sensing 16, 264. DOI: 10.3390/rs16020264.

SETIYOWATI, Y. A., HARISUSENO, D. & SAJALI, M. A. (2025): Comparison of correlation, PBIAS and RSR between monthly, daily, and hourly GPM rainfall data. In: Jurnal Penelitian Pendidikan IPA 11, 6. S. 615–624.

SHIELDS, M. D. & ZHANG, J. (2016): The generalization of Latin hypercube sampling. In: Reliability Engineering and System Safety 148. S. 96–108.

STRANG, G. (2023): Calculus. [Online-Publikation]. [Zugriff am: 08.04.2026]. Verfügbar unter: https://ocw.mit.edu/courses/res-18-001-calculus-fall-2023/

SZOKOLAY, S. V. (2007): Solar Geometry. [Online-Publikation]. [Zugriff am: 06.04.2023]. Verfügbar unter: https://www.plea-arch.org/wp-content/uploads/PLEA-NOTE-1-SOLAR-GEOMETRY.pdf

THIEMIG, V., ROJAS, R., ZAMBRANO-BIGIARINI, M. & DE ROO, A. (2013): Hydrological evaluation of satellite-based rainfall estimates over the Volta and Baro-Akobo Basin. In: Journal of Hydrology 499. S. 324-338.

U. S. DEPARTMENT OF AGRICULTURE (Hrsg.) (o.J.): H. J. Andrews Experimental Forest. [Website] [Zugriff am: 06.04.2026] Verfügbar unter: https://research.fs.usda.gov/pnw/forestsandranges/locations/hjandrews

YILMAZ, K. K., GUPTA, H. V. & WAGENER, T. (2008): A process-based diagnostic approach to model evaluation: Application to the NWS distributed hydrologic model. In: Water Resources Research 44. DOI: 10.1029/2007WR006716.

ZHANG, F., ZHANG, H., HAGEN, S. C., YE, M., WANG, D., GUI, D., ZENG, C., TIAN, L. & LIU, J. (2015): Snow cover and runoff modelling in a high mountain catchment with scarce data: effects of temperature and precipitation parameters. In: Hydrological Processes 29. S. 52-65.

ZHANG, T., STACKHOUSE, P. W., MACPHERSON, B. & MIKOVITZ, J. C. (2021): A solar azimuth formula that renders circumstantial treatment unnecessary without compromising mathematical rigor: Mathematical setup, application and extension of a formula based on the subsolar point and atan2 function. In: Renewable Energy 172. S. 1333-1340.
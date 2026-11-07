#================================================================================================================================================================================================
###H.J. ANDREWS EXPERIMENTAL FOREST - VORWORT###
#================================================================================================================================================================================================
##DIESER CODE IST IM RAHMEN EINER BACHELORARBEIT AN DER UNIVERSITÄT BONN IM FACHBEREICH GEOGRAPHIE ENTSTANDEN##

##DIESER IST ALS FORTSETZUNG DER ARBEIT VON LUTZ KLEIN, DOKTORAND DER AG HYDROLOGIE, ZU VERSTEHEN!##

##BENÖTIGTE PACKAGES SOWIE DATEN KÖNNEN IN DER README-DATEI DES GITHUB-REPOSITORIES AUFGEFUNDEN WERDEN##

##INHALT:##
#ABSCHNITT A: STATIONSDATEN EINLESEN, STANDARDISIEREN UND GEMEINSAME ZEITFENSTER ABLEITEN
#ABSCHNITT B: POTENZIELLE DIREKTSTRAHLUNG
#ABSCHNITT C: MEHRSTATIONSKALIBRIERUNG

##ALLE PFADE MÜSSEN MANUELL ANGEPASST WERDEN!##

##KI-NUTZUNG: DIE ERSTELLUNG DES CODES WURDE DURCH DIE TOOLS GITHUB COPILOT, CODEX UND CHATGPT UNTERSTÜTZT!##

##VERWENDETE UND WEITERFÜHRENDE LITERATUR UND QUELLEN SIND IN DER README-DATEI DES GITHUB-REPOSITORIES AUFGEFÜHRT!## 

###WICHTIG: DIE TECHNISCHEN VORAUSSETZUNGEN AN DIESES SKRIPT SIND BEI KOMPLETTEN DURCHLAUF HOCH!##
##DESWEGEN EMPFOHLEN: 32 GB ARBEITSSPEICHER UND SOLIDE CPU##
##IST DIES NICHT GEGEBEN: EMPFEHLENSWERT DIE BERECHNUNGS-ZEITRÄUME ZU MINIMIEREN!##

#================================================================================================================================================================================================
###ABSCHNITT A: STATIONSDATEN EINLESEN, STANDARDISIEREN UND GEMEINSAME ZEITFENSTER ABLEITEN###
#================================================================================================================================================================================================
#================================================================================================================================================================================================
###SCHRITT A.0: PFADE ZU DEN HJA-STATIONSDATEN FESTLEGEN###
#================================================================================================================================================================================================
##ANMERKUNG: DIE KLIMADATEN WERDEN ALS .xlsx-FILES IN EINEM GEMEINSAMEN ORDNER ERWARTET!##
##DIE EXCEL-FILES SOLLTEN FOLGENDEN NAMEN HABEN: "HJA_ALL_TEMP.xlsx", "HJA_ALL_SWE.xlsx" UND "HJA_ALL_PRECIP.xlsx"##
##INNERHALB DER EXCEL-FILES WERDEN DIE DATEN IN EINZELNEN SHEETS PRO STATION ABGELEGT, MIT DEN SPALTEN "Site", "Date", "Lon", "Lat", "Elevation" UND DER VARIABLENSPALTEN##

#A.0.1 Pfad zu Klimadaten (Temperatur, SWE, Niederschlag)
HJA_climate_dir <- "PFAD/ZU/KLIMA/DATEN/ORDNER" #PFAD MUSS ANGEPASST WERDEN!

#A.0.2 Pfade zu den Excel-Dateien der Temperatur-, SWE- und Niederschlagsdaten
HJA_temp_xlsx <- file.path(HJA_climate_dir, "HJA_ALL_TEMP.xlsx")
HJA_swe_xlsx  <- file.path(HJA_climate_dir, "HJA_ALL_SWE.xlsx")
HJA_prec_xlsx <- file.path(HJA_climate_dir, "HJA_ALL_PRECIP.xlsx")

#A.0.3 Schalter zur Vereinheitlichung der Stationsnamen
#Suffixe wie "_1" oder "_2" werden entfernt, damit identische Stationen korrekt zusammengeführt werden können
HJA_drop_site_suffix <- TRUE

#================================================================================================================================================================================================
###SCHRITT A.1: HILFSFUNKTIONEN ZUM EINLESEN UND STANDARDISIEREN DER HJA-STATIONSDATEN###
#================================================================================================================================================================================================

#A.1.1 Datumsangaben robust in Date-Objekte umwandeln
HJA_parse_date <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x))         return(as.Date(x, origin = "1899-12-30"))
  
  x_chr <- as.character(x)
  
  parsed_date <- suppressWarnings(dmy(x_chr))
  
  if (all(is.na(parsed_date))) {
    parsed_date <- suppressWarnings(ymd(x_chr))}
  
  parsed_date}

#A.1.2 Stationsnamen vereinheitlichen
HJA_normalize_site_name <- function(site_name, drop_suffix = TRUE) {
  site_name <- as.character(site_name)
  
  if (drop_suffix) {
    site_name <- str_replace(site_name, "_\\d+$", "")}
  
  site_name}

#A.1.3 Stationsmetadaten aus einem Dataframe extrahieren
HJA_extract_station_metadata <- function(station_df) {
  
  station_df %>%
    mutate(Site = HJA_normalize_site_name(Site, drop_suffix = HJA_drop_site_suffix)) %>%
    group_by(Site) %>%
    summarise(
      Lon       = dplyr::first(Lon),
      Lat       = dplyr::first(Lat),
      Elevation = dplyr::first(Elevation),
      .groups   = "drop")}

#A.1.4 Alle Sheets aus HJA-Excel-Datei einlesen
HJA_read_all_sheets <- function(xlsx_path) {
  
  HJA_sheet_names <- excel_sheets(xlsx_path)
  stopifnot(length(HJA_sheet_names) > 0)
  
  purrr::imap_dfr(HJA_sheet_names, ~{
    
    current_df <- read_excel(xlsx_path, sheet = .x)
    
    required_columns <- c("Site", "Date", "Lon", "Lat", "Elevation")
    missing_columns  <- setdiff(required_columns, names(current_df))
    
    if (length(missing_columns) > 0) {
      stop(
        "Fehlende Spalten in ",
        basename(xlsx_path),
        " / Sheet ", .x,": ", paste(missing_columns, collapse = ", "))}
    
    current_df})}

#A.1.5 Verfügbaren Zeitraum einer Variable pro Station bestimmen
HJA_get_available_range <- function(df, value_column, station_names) {
  
  df %>%
    filter(
      Site %in% station_names,
      !is.na(.data[[value_column]]),
      !is.na(Date)) %>%
    group_by(Site) %>%
    summarise(
      start = min(Date),
      end   = max(Date),
      n     = n_distinct(Date),
      .groups = "drop")}

#================================================================================================================================================================================================
###SCHRITT A.2: ROHDATEN EINLESEN UND METADATEN DER STATIONEN ABLEITEN###
#================================================================================================================================================================================================

#A.2.1 Rohdaten aus den drei Excel-Dateien einlesen
HJA_prec_raw <- HJA_read_all_sheets(HJA_prec_xlsx)
HJA_swe_raw  <- HJA_read_all_sheets(HJA_swe_xlsx)
HJA_temp_raw <- HJA_read_all_sheets(HJA_temp_xlsx)

#A.2.2 Stationsnamen und Datumsangaben vereinheitlichen
HJA_prec_raw <- HJA_prec_raw %>%
  mutate(
    Site = HJA_normalize_site_name(Site, drop_suffix = HJA_drop_site_suffix),
    Date = HJA_parse_date(Date))

HJA_swe_raw <- HJA_swe_raw %>%
  mutate(
    Site = HJA_normalize_site_name(Site, drop_suffix = HJA_drop_site_suffix),
    Date = HJA_parse_date(Date))

HJA_temp_raw <- HJA_temp_raw %>%
  mutate(
    Site = HJA_normalize_site_name(Site, drop_suffix = HJA_drop_site_suffix),
    Date = HJA_parse_date(Date))

#A.2.3 Stationsmetadaten aus den einzelnen Datensätzen extrahieren
HJA_meta_prec <- HJA_extract_station_metadata(HJA_prec_raw)
HJA_meta_swe  <- HJA_extract_station_metadata(HJA_swe_raw)
HJA_meta_temp <- HJA_extract_station_metadata(HJA_temp_raw)

#A.2.4 Vollständige Metadatentabelle aller Stationen erzeugen
HJA_all_meta <- bind_rows(HJA_meta_prec, HJA_meta_swe, HJA_meta_temp) %>%
  group_by(Site) %>%
  summarise(
    Lon       = Lon[which.max(!is.na(Lon))][1],
    Lat       = Lat[which.max(!is.na(Lat))][1],
    Elevation = Elevation[which.max(!is.na(Elevation))][1],
    .groups   = "drop")

#================================================================================================================================================================================================
###SCHRITT A.3: TEMPERATUR-, SWE- UND NIEDERSCHLAGSDATEN STANDARDISIEREN###
#================================================================================================================================================================================================

#A.3.1 Tägliche Niederschlagsdaten standardisieren
HJA_prec_daily <- HJA_prec_raw %>%
  filter(!is.na(Date)) %>%
  transmute(Site, Date, P = `P[mm]`) %>%
  arrange(Site, Date)

#A.3.2 Tägliche SWE-Daten standardisieren
HJA_swe_daily <- HJA_swe_raw %>%
  filter(!is.na(Date)) %>%
  transmute(Site, Date, SWE = SWE) %>%
  arrange(Site, Date)

#A.3.3 Tägliche Temperaturdaten standardisieren
HJA_temp_daily <- HJA_temp_raw %>%
  filter(!is.na(Date)) %>%
  transmute(Site, Date, T = Temperature) %>%
  arrange(Site, Date)

#================================================================================================================================================================================================
###SCHRITT A.4: GEMEINSAME ZEITFENSTER DER STATIONEN BESTIMMEN###
#================================================================================================================================================================================================

#A.4.1 Stationen identifizieren, die in allen drei Datensätzen vorkommen
HJA_common_stations <- Reduce(intersect, list(
  unique(HJA_swe_daily$Site),
  unique(HJA_temp_daily$Site),
  unique(HJA_prec_daily$Site)))

#A.4.2 Verfügbare Zeiträume für SWE, Temperatur und Niederschlag je Station bestimmen
HJA_range_swe <- HJA_get_available_range(
  df = HJA_swe_daily,
  value_column = "SWE",
  station_names = HJA_common_stations) %>%
  rename(start_swe = start, end_swe = end)

HJA_range_temp <- HJA_get_available_range(
  df = HJA_temp_daily,
  value_column = "T",
  station_names = HJA_common_stations) %>%
  rename(start_temp = start, end_temp = end)

HJA_range_prec <- HJA_get_available_range(
  df = HJA_prec_daily,
  value_column = "P",
  station_names = HJA_common_stations) %>%
  rename(start_prec = start, end_prec = end)

#A.4.3 Gemeinsames Zeitfenster je Station als Schnittmenge aller drei Variablen bestimmen
HJA_station_range <- HJA_range_swe %>%
  left_join(HJA_range_temp, by = "Site") %>%
  left_join(HJA_range_prec, by = "Site") %>%
  mutate(
    start_common = pmax(start_swe, start_temp, start_prec, na.rm = TRUE),
    end_common   = pmin(end_swe, end_temp, end_prec, na.rm = TRUE)) %>%
  select(Site, start_common, end_common)

#A.4.4 Stationen ohne gemeinsamen Zeitraum ausschließen
HJA_station_range_valid <- HJA_station_range %>%
  filter(
    !is.na(start_common),
    !is.na(end_common),
    start_common <= end_common)

HJA_dropped_stations <- setdiff(HJA_station_range$Site, HJA_station_range_valid$Site)

print(HJA_station_range_valid)

#A.4.5 Globales Kalibrierfenster bestimmen
#ANMERKUNG: DIESER ZEITRAUM ENTSPRICHT DER SCHNITTMENGE ALLER GÜLTIGEN STATIONEN##
HJA_sim_start <- max(HJA_station_range_valid$start_common, na.rm = TRUE)
HJA_sim_end   <- min(HJA_station_range_valid$end_common, na.rm = TRUE)

#================================================================================================================================================================================================
###SCHRITT A.5: STATIONSSPEZIFISCHE DAILY-DATAFRAMES FÜR DIE WEITERE MODELLIERUNG ERSTELLEN###
#================================================================================================================================================================================================

#A.5.1 Für jede gültige Station einen zusammengeführten Daily-Dataframe erzeugen
HJA_station_daily_df <- tibble(Site = HJA_station_range_valid$Site) %>%
  left_join(HJA_station_range_valid, by = "Site") %>%
  mutate(
    data = purrr::pmap(
      list(Site, start_common, end_common),
      \(current_site, current_start, current_end) {
        HJA_swe_daily %>%
          filter(Site == current_site) %>%
          left_join(
            HJA_temp_daily %>% filter(Site == current_site),
            by = c("Site", "Date")) %>%
          left_join(
            HJA_prec_daily %>% filter(Site == current_site),
            by = c("Site", "Date")) %>%
          left_join(
            HJA_all_meta %>% filter(Site == current_site),
            by = "Site") %>%
          filter(Date >= current_start, Date <= current_end) %>%
          arrange(Date)}))

#A.5.2 Kontrolle der Zeilenzahlen und Zeitfenster je Station
HJA_station_daily_df %>%
  mutate(n = purrr::map_int(data, nrow)) %>%
  select(Site, n, start_common, end_common) %>%
  print(n = Inf)

#================================================================================================================================================================================================
###ABSCHNITT B: POTENZIELLE DIREKTSTRAHLUNG###
#================================================================================================================================================================================================
#================================================================================================================================================================================================
###SCHRITT B.1: VORBEREITUNG###
#================================================================================================================================================================================================

#B.1.1 Pfad zum HJA-DEM festlegen
HJA_Ipot_dem_path <- "PFAD/ZU/DEM/HJA_DEM_1m.tif" #PFAD MUSS ANGEPASST WERDEN!
HJA_Ipot_dem_path <- "C:/Users/theos/Desktop/ABGABE_BACHELORARBEIT/Daten/H. J. Andrews Experimental Forest/DEM/HJA_DEM_1M.tif"

HJA_Ipot_dem_1m <- rast(HJA_Ipot_dem_path)

#B.1.2 DEM für die spätere Strahlungsberechnung auf 10 m aggregieren
HJA_Ipot_dem <- aggregate(HJA_Ipot_dem_1m, fact = 10, fun = mean, na.rm = TRUE)

#B.1.3 Pufferradius der stationsspezifischen AOIs in Metern festlegen
HJA_Ipot_buf_m <- 150

#B.1.4 Untersuchungszeitraum auf Tagesbasis festlegen
HJA_Ipot_date_start <- HJA_sim_start
HJA_Ipot_date_end   <- HJA_sim_end
HJA_Ipot_dates_seq  <- seq(HJA_Ipot_date_start, HJA_Ipot_date_end, by = "day")

#B.1.5 Stündliche Zeitschritte festlegen
HJA_Ipot_hours_seq <- 0:23

#B.1.6 Solarkonstante und atmosphärischen Transmissionsparameter festlegen
HJA_Ipot_solar_constant     <- 1368
HJA_Ipot_psi_alpha_default  <- 0.75

#B.1.7 Zeitzone festlegen
HJA_Ipot_tz <- "America/Los_Angeles"

#B.1.8 Hilfsfunktion zur Umrechnung von Grad in Radiant nach STRANG (2023)
HJA_Ipot_deg2rad <- function(x) x * pi / 180

#================================================================================================================================================================================================
###SCHRITT B.2: STATIONSKOORDINATEN AUS DER XLSX AUSLESEN UND STATIONSSPEZIFISCHE BUFFER ERSTELLEN###
#================================================================================================================================================================================================

#B.2.1 Sheet-Namen der SWE-Datei einlesen (Die Wahl der SWE-Datei ist willkürlich, da die Stationskoordinaten in allen drei Excel-Dateien identisch sind)
HJA_Ipot_station_sheets <- excel_sheets(HJA_swe_xlsx)

#B.2.2 Nur jene Stationen behalten, die im zusammengeführten Daily-Dataframe enthalten sind
HJA_Ipot_use_sites <- HJA_station_daily_df$Site
HJA_Ipot_station_sheets <- HJA_Ipot_station_sheets[HJA_Ipot_station_sheets %in% HJA_Ipot_use_sites]


#B.2.3 Hilfsfunktion zum Auslesen von geographischer Länge und Breite aus einem Sheet
HJA_Ipot_get_station_latlon <- function(sheet_name) {
  
  HJA_Ipot_sheet_df <- readxl::read_xlsx(HJA_swe_xlsx, sheet = sheet_name)
  
  HJA_Ipot_colnames_lower <- tolower(names(HJA_Ipot_sheet_df))
  
  HJA_Ipot_lon_col <- names(HJA_Ipot_sheet_df)[match(TRUE, HJA_Ipot_colnames_lower %in% "lon")]
  HJA_Ipot_lat_col <- names(HJA_Ipot_sheet_df)[match(TRUE, HJA_Ipot_colnames_lower %in% "lat")]
  
  HJA_Ipot_lon <- as.numeric(HJA_Ipot_sheet_df[[HJA_Ipot_lon_col]][1])
  HJA_Ipot_lat <- as.numeric(HJA_Ipot_sheet_df[[HJA_Ipot_lat_col]][1])
  
  data.frame(
    Site = sheet_name,
    lon  = HJA_Ipot_lon,
    lat  = HJA_Ipot_lat)}

#B.2.4 Tabelle mit den Stationskoordinaten erzeugen
##ANMERKUNG: ES KOMMT ZU "warnings()" DIE IGNORIERT WERDEN KÖNNEN##
HJA_Ipot_sites_ll <- do.call(
  rbind,
  lapply(HJA_Ipot_station_sheets, HJA_Ipot_get_station_latlon))

#B.2.5 Stationspunkte zunächst in WGS 84 als sf-Objekte anlegen
HJA_Ipot_sites_sf_ll <- st_as_sf(
  HJA_Ipot_sites_ll,
  coords = c("lon", "lat"),
  crs = 4326)

#B.2.6 Stationspunkte in das CRS des aggregierten DEMs transformieren
HJA_Ipot_sites_sf_utm <- st_transform(HJA_Ipot_sites_sf_ll, crs(HJA_Ipot_dem))

#B.2.7 Stationsspezifische Buffer im DEM-Koordinatensystem erzeugen
HJA_Ipot_sites_buf_utm <- st_buffer(HJA_Ipot_sites_sf_utm, dist = HJA_Ipot_buf_m)

#================================================================================================================================================================================================
###SCHRITT B.3: FUNKTIONEN ZUR SONNENGEOMETRIE UND LUFTMASSE###
#================================================================================================================================================================================================
##ANMERKUNG: BERECHNUNG AUF BASIS SONNENGEOMETRISCHER NÄHERUNGS-FUNKTIONEN NACH IQBAL (1983) und SZOKOLAY (2007)##

#B.3.1 Solare Deklination und Equation of Time berechnen (in Abhängigkeit von Tag des Jahres und Tagwinkel gamma)
HJA_Ipot_compute_solar_declination_eot <- function(day_of_year) {
  
  day_angle_gamma <- 2 * pi * (day_of_year - 1) / 365
  
  solar_declination_rad <- 0.006918 -
    0.399912 * cos(day_angle_gamma) + 0.070257 * sin(day_angle_gamma) -
    0.006758 * cos(2 * day_angle_gamma) + 0.000907 * sin(2 * day_angle_gamma) -
    0.002697 * cos(3 * day_angle_gamma) + 0.00148  * sin(3 * day_angle_gamma)
  
  equation_of_time_min <- 229.18 * (
    0.000075 +
      0.001868 * cos(day_angle_gamma) -
      0.032077 * sin(day_angle_gamma) -
      0.014615 * cos(2 * day_angle_gamma) -
      0.040849 * sin(2 * day_angle_gamma))
  
  list(
    solar_declination_rad = solar_declination_rad,
    equation_of_time_min  = equation_of_time_min)}

#3.2 Zeitzonen-Offset relativ zu UTC für ein gegebenes Datum bestimmen
HJA_Ipot_get_tz_offset_hours <- function(date, tz = HJA_Ipot_tz) {
  
  reference_time <- as.POSIXct(paste(date, "12:00:00"), tz = tz)
  as.integer(as.numeric(format(reference_time, "%z")) / 100)}

#B.3.3 Sonnenhöhe und solaren Azimut berechnen
#Azimut-Konvention: 0 = Norden, pi/2 = Osten, pi = Süden, 3pi/2 = Westen
#Stundenwinkel: 0 = Sonnenhöchststand, positiv am Nachmittag, negativ am Vormittag
HJA_Ipot_compute_solar_position <- function(date, hour, latitude_deg, longitude_deg, tz_offset_hours) {
  
  day_of_year <- lubridate::yday(date)
  
  solar_geometry <- HJA_Ipot_compute_solar_declination_eot(day_of_year)
  solar_declination_rad <- solar_geometry$solar_declination_rad
  equation_of_time_min  <- solar_geometry$equation_of_time_min
  
  latitude_rad <- HJA_Ipot_deg2rad(latitude_deg)
  
  #Lokaler Standardmeridian der Zeitzone
  local_standard_time_meridian <- 15 * tz_offset_hours
  
  #Umrechnung von lokaler Uhrzeit in True Solar Time
  time_minutes <- hour * 60
  time_correction_min <- equation_of_time_min + 4 * (longitude_deg - local_standard_time_meridian)
  true_solar_time_min <- time_minutes + time_correction_min
  
  #Stundenwinkel in Radiant
  hour_angle_rad <- HJA_Ipot_deg2rad((true_solar_time_min / 4) - 180)
  
  #Sonnenhöhe in Radiant
  sin_solar_altitude <- sin(latitude_rad) * sin(solar_declination_rad) +
    cos(latitude_rad) * cos(solar_declination_rad) * cos(hour_angle_rad)
  
  solar_altitude_rad <- asin(pmin(pmax(sin_solar_altitude, -1), 1))
  
  #Solarer Azimut in Radiant
  cos_solar_altitude <- cos(solar_altitude_rad)
  
  if (cos_solar_altitude < 1e-6) {
    
    solar_azimuth_rad <- NA_real_} else {
    
    sin_solar_azimuth <- -cos(solar_declination_rad) * sin(hour_angle_rad) / cos_solar_altitude
    cos_solar_azimuth <- (sin(solar_declination_rad) - sin(latitude_rad) * sin(solar_altitude_rad)) /
      (cos(latitude_rad) * cos_solar_altitude)
    
    solar_azimuth_raw <- atan2(sin_solar_azimuth, cos_solar_azimuth)
    solar_azimuth_rad <- ifelse(solar_azimuth_raw < 0, solar_azimuth_raw + 2 * pi, solar_azimuth_raw)}
  
  list(
    solar_altitude_rad = solar_altitude_rad,
    solar_azimuth_rad  = solar_azimuth_rad)}

#B.3.4 Luftmasse nach Kasten & Young (1989) berechnen
HJA_Ipot_compute_air_mass_kasten_young <- function(solar_altitude_rad) {
  
  solar_altitude_deg <- solar_altitude_rad * 180 / pi
  solar_zenith_deg   <- 90 - solar_altitude_deg
  
  air_mass <- 1 / (
    cos(HJA_Ipot_deg2rad(solar_zenith_deg)) +
      0.50572 * (96.07995 - solar_zenith_deg)^(-1.6364))
  
  air_mass}

#================================================================================================================================================================================================
###SCHRITT B.4: FUNKTION ZUR BERECHNUNG DER TÄGLICHEN POTENZIELLEN DIREKTSTRAHLUNG JE STATION###
#================================================================================================================================================================================================

#B.4.1 Tägliche potenzielle Direktstrahlung für eine Station berechnen
HJA_Ipot_compute_daily_for_site <- function(
    current_date,
    HJA_Ipot_dem_site,
    HJA_Ipot_cos_slope,
    HJA_Ipot_sin_slope,
    HJA_Ipot_cos_aspect,
    HJA_Ipot_sin_aspect,
    HJA_Ipot_latitude_deg,
    HJA_Ipot_longitude_deg,
    HJA_Ipot_site_point_vect,
    HJA_Ipot_site_buffer_vect,
    HJA_Ipot_hours_seq = HJA_Ipot_hours_seq,
    HJA_Ipot_solar_constant = HJA_Ipot_solar_constant,
    HJA_Ipot_psi_alpha = HJA_Ipot_psi_alpha_default,
    HJA_Ipot_tz = HJA_Ipot_tz) {
  
  #Tag des Jahres und Exzentrizitätskorrektur der Erde-Sonne-Distanz
  HJA_Ipot_day_of_year <- lubridate::yday(current_date)
  HJA_Ipot_E0 <- 1 + 0.033 * cos(2 * pi * HJA_Ipot_day_of_year / 365)
  
  #Zeitzonen-Offset für den jeweiligen Tag bestimmen
  HJA_Ipot_tz_offset_hours <- HJA_Ipot_get_tz_offset_hours(
    date = current_date,
    tz   = HJA_Ipot_tz)
  
  #Tagesraster der potenziellen Direktstrahlung initialisieren
  HJA_Ipot_day_raster <- rast(HJA_Ipot_dem_site)
  values(HJA_Ipot_day_raster) <- 0
  
  #Stündliche Integration über alle Stunden des Tages
  for (current_hour in HJA_Ipot_hours_seq) {
    
    HJA_Ipot_solar_position <- HJA_Ipot_compute_solar_position(
      date            = current_date,
      hour            = current_hour,
      latitude_deg    = HJA_Ipot_latitude_deg,
      longitude_deg   = HJA_Ipot_longitude_deg,
      tz_offset_hours = HJA_Ipot_tz_offset_hours)
    
    HJA_Ipot_solar_altitude_rad <- HJA_Ipot_solar_position$solar_altitude_rad
    HJA_Ipot_solar_azimuth_rad  <- HJA_Ipot_solar_position$solar_azimuth_rad
    
    #Nachtstunden oder ungültige Sonnenpositionen überspringen
    if (!is.finite(HJA_Ipot_solar_altitude_rad) || HJA_Ipot_solar_altitude_rad <= 0 || !is.finite(HJA_Ipot_solar_azimuth_rad)) next
    
    HJA_Ipot_sin_altitude <- sin(HJA_Ipot_solar_altitude_rad)
    HJA_Ipot_cos_altitude <- cos(HJA_Ipot_solar_altitude_rad)
    HJA_Ipot_cos_azimuth  <- cos(HJA_Ipot_solar_azimuth_rad)
    HJA_Ipot_sin_azimuth  <- sin(HJA_Ipot_solar_azimuth_rad)
    
    HJA_Ipot_cos_azimuth_aspect <- HJA_Ipot_cos_azimuth * HJA_Ipot_cos_aspect + HJA_Ipot_sin_azimuth * HJA_Ipot_sin_aspect
    
    #Einfallswinkel auf geneigter Fläche
    HJA_Ipot_cosi <- HJA_Ipot_sin_altitude * HJA_Ipot_cos_slope + HJA_Ipot_cos_altitude * HJA_Ipot_sin_slope * HJA_Ipot_cos_azimuth_aspect
    HJA_Ipot_cosi <- clamp(HJA_Ipot_cosi, lower = 0, upper = 1, values = TRUE)
    
    #Luftmasse und atmosphärische Transmissivität
    HJA_Ipot_air_mass <- HJA_Ipot_compute_air_mass_kasten_young(HJA_Ipot_solar_altitude_rad)
    HJA_Ipot_tau <- HJA_Ipot_psi_alpha ^ HJA_Ipot_air_mass
    
    #Potenzielle Direktstrahlung in W/m² und Umrechnung in MJ/m² pro Stunde
    HJA_Ipot_Wm2 <- (HJA_Ipot_solar_constant * HJA_Ipot_E0 * HJA_Ipot_tau) * HJA_Ipot_cosi
    HJA_Ipot_MJm2_h <- HJA_Ipot_Wm2 * (3600 / 1e6)
    
    #Stündlichen Beitrag zum Tagesraster addieren
    HJA_Ipot_day_raster <- HJA_Ipot_day_raster + HJA_Ipot_MJm2_h}
  
  #Mittelwert der täglichen potenziellen Direktstrahlung im Stationsbuffer bestimmen
  HJA_Ipot_buffer_mean <- terra::global(
    mask(HJA_Ipot_day_raster, HJA_Ipot_site_buffer_vect),
    "mean",
    na.rm = TRUE)[1, 1]
  
  HJA_Ipot_point_value <- terra::extract(HJA_Ipot_day_raster, HJA_Ipot_site_point_vect)[1, 2]
  
  list(
    Date              = current_date,
    HJA_Ipot_buf_mean = as.numeric(HJA_Ipot_buffer_mean),
    HJA_Ipot_point    = as.numeric(HJA_Ipot_point_value))}

#================================================================================================================================================================================================
###SCHRITT B.5: LOOP ÜBER STATIONEN: DEM CLIP + PRECOMPUTE SLOPE/ASPECT + DAILY TABLE SPEICHERN###
#================================================================================================================================================================================================

#B.5.1 Leeren Dataframe für die täglichen Ergebnisse aller Stationen initialisieren
HJA_Ipot_station_daily <- data.frame()

#B.5.2 Loop über alle Stationen mit gültigen Daten
##ANMERKUNG: DEN GANZEN ZUSAMMENHÄNGENDEN LOOP AUSFÜHREN##
for (i in seq_len(nrow(HJA_Ipot_sites_ll))) {
  
  HJA_Ipot_site_name <- HJA_Ipot_sites_ll$Site[i]
  
  #Stationspunkt und Buffer als sf-Objekte und terra-Vektoren anlegen
  HJA_Ipot_site_point_sf <- HJA_Ipot_sites_sf_utm[i, ]
  HJA_Ipot_site_buf_sf   <- HJA_Ipot_sites_buf_utm[i, ]
  
  HJA_Ipot_site_point_vect <- vect(HJA_Ipot_site_point_sf)
  HJA_Ipot_site_buf_vect   <- vect(HJA_Ipot_site_buf_sf)
  
  #DEM mask auf Buffer
  HJA_Ipot_dem_site <- crop(HJA_Ipot_dem, HJA_Ipot_site_buf_vect) |> mask(HJA_Ipot_site_buf_vect)
  
  #slope/aspect einmalig berechnen
  HJA_Ipot_slope_deg  <- terrain(HJA_Ipot_dem_site, v = "slope",  unit = "degrees")
  HJA_Ipot_aspect_deg <- terrain(HJA_Ipot_dem_site, v = "aspect", unit = "degrees")
  
  #Umrechnung von Grad in Bogenmaß
  HJA_Ipot_slope  <- HJA_Ipot_slope_deg  * (pi/180)
  HJA_Ipot_aspect <- HJA_Ipot_aspect_deg * (pi/180)
  
  #Vorberechnung von Sinus und Kosinus von Slope und Aspect für die spätere Strahlungsberechnung
  HJA_Ipot_cos_slope <- cos(HJA_Ipot_slope)
  HJA_Ipot_sin_slope <- sin(HJA_Ipot_slope)
  HJA_Ipot_cos_aspect <- cos(HJA_Ipot_aspect)
  HJA_Ipot_sin_aspect <- sin(HJA_Ipot_aspect)
  
  #Latitude und Longitude der Station extrahieren
  HJA_Ipot_latitude_deg  <- HJA_Ipot_sites_ll$lat[i]
  HJA_Ipot_longitude_deg <- HJA_Ipot_sites_ll$lon[i]
  
  #Daily loop (pro Station)
  HJA_Ipot_tmp <- lapply(HJA_Ipot_dates_seq, function(current_date) {
    HJA_Ipot_compute_daily_for_site(
      current_date             = current_date,
      HJA_Ipot_dem_site        = HJA_Ipot_dem_site,
      HJA_Ipot_cos_slope       = HJA_Ipot_cos_slope,
      HJA_Ipot_sin_slope       = HJA_Ipot_sin_slope,
      HJA_Ipot_cos_aspect      = HJA_Ipot_cos_aspect,
      HJA_Ipot_sin_aspect      = HJA_Ipot_sin_aspect,
      HJA_Ipot_latitude_deg    = HJA_Ipot_latitude_deg,
      HJA_Ipot_longitude_deg   = HJA_Ipot_longitude_deg,
      HJA_Ipot_site_point_vect = HJA_Ipot_site_point_vect,
      HJA_Ipot_site_buffer_vect= HJA_Ipot_site_buf_vect,
      HJA_Ipot_hours_seq       = HJA_Ipot_hours_seq,
      HJA_Ipot_solar_constant  = HJA_Ipot_solar_constant,
      HJA_Ipot_psi_alpha       = HJA_Ipot_psi_alpha_default,
      HJA_Ipot_tz              = HJA_Ipot_tz)})
  
  HJA_Ipot_tmp_df <- dplyr::bind_rows(HJA_Ipot_tmp) %>%
    mutate(
      Date = as.Date(Date),
      Site = HJA_Ipot_site_name)
  
  HJA_Ipot_station_daily <- dplyr::bind_rows(HJA_Ipot_station_daily, HJA_Ipot_tmp_df)}


#B.5.3 Endgültigen Dataframe mit den täglichen potenziellen Direktstrahlungswerten
HJA_Ipot_station_daily <- HJA_Ipot_station_daily %>%
  mutate(Date = as.Date(Date)) %>%
  select(Site, Date, HJA_Ipot_buf_mean, HJA_Ipot_point) %>%
  arrange(Site, Date)

print(head(HJA_Ipot_station_daily))
summary(HJA_Ipot_station_daily$HJA_Ipot_buf_mean)

#================================================================================================================================================================================================
###SCHRITT B.6: VISUALISIERUNG DER STATIONSSPEZIFISCHEN ZEITREIHEN DER POTENZIELLEN DIREKTSTRAHLUNG###
#================================================================================================================================================================================================

#B.6.1 Plot-Dataframe auf Basis der Buffer-Mittelwerte vorbereiten
HJA_Ipot_plot_df <- HJA_Ipot_station_daily %>%
  dplyr::select(Site, Date, HJA_Ipot_buf_mean) %>%
  dplyr::mutate(
    Site = factor(Site, levels = c("CENMET", "PRIMET", "UPLMET")))

#B.6.2 Gemeinsames y-Limit für alle Plots bestimmen
HJA_Ipot_ymax <- max(HJA_Ipot_plot_df$HJA_Ipot_buf_mean, na.rm = TRUE)

#B.6.3 Zeitreihenplot der potenziellen Direktstrahlung für alle Stationen
ggplot(HJA_Ipot_plot_df, aes(x = Date, y = HJA_Ipot_buf_mean, color = Site)) +
  geom_line(linewidth = 1.0, na.rm = TRUE) +
  scale_color_manual(
    values = c(
      "CENMET" = "blue",
      "PRIMET" = "red",
      "UPLMET" = "black"),
    name = NULL) +
  scale_y_continuous(
    limits = c(0, HJA_Ipot_ymax),
    breaks = seq(0, ceiling(HJA_Ipot_ymax / 5) * 5, by = 5)) +
  labs(
    x = "Datum",
    y = "Potenzielle Direktstrahlung (MJ m⁻² d⁻¹)") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 13),
    legend.text  = element_text(size = 13))

#================================================================================================================================================================================================
###SCHRITT 7: BASIS-QC DER STATIONSBASIERTEN ZEITREIHEN DER POTENZIELLEN DIREKTSTRAHLUNG###
#================================================================================================================================================================================================

#B.7.1 Datumsformat sicherstellen
HJA_Ipot_station_daily <- HJA_Ipot_station_daily %>%
  mutate(Date = as.Date(Date))

#B.7.2 Struktur und Missing Values prüfen
HJA_Ipot_basic_qc <- HJA_Ipot_station_daily %>%
  summarise(
    n_rows = n(),
    n_sites = n_distinct(Site),
    start_date = min(Date, na.rm = TRUE),
    end_date = max(Date, na.rm = TRUE),
    na_buf = sum(!is.finite(HJA_Ipot_buf_mean)),
    na_point = sum(!is.finite(HJA_Ipot_point)))

print(HJA_Ipot_basic_qc)

#B.7.3 Wertebereich pro Station bestimmen
HJA_Ipot_range_by_site <- HJA_Ipot_station_daily %>%
  group_by(Site) %>%
  summarise(
    min_buf_MJ = min(HJA_Ipot_buf_mean, na.rm = TRUE),
    max_buf_MJ = max(HJA_Ipot_buf_mean, na.rm = TRUE),
    min_point_MJ = min(HJA_Ipot_point, na.rm = TRUE),
    max_point_MJ = max(HJA_Ipot_point, na.rm = TRUE),
    .groups = "drop")

print(HJA_Ipot_range_by_site)

#================================================================================================================================================================================================
###SCHRITT B.8: STRUKTURELLER PLAUSIBILITÄTSCHECK DER STATIONSBASIERTEN ZEITREIHEN DER POTENZIELLEN DIREKTSTRAHLUNG###
#================================================================================================================================================================================================

#8.1 Zeitinformationen ergänzen
##ANMERKUNG: DIE SEASON_SIMPLE-KATEGORIEN SIND NUR EINE GROBE UNTERTEILUNG##
HJA_Ipot_station_check <- HJA_Ipot_station_daily %>%
  mutate(
    month = lubridate::month(Date),
    season_simple = case_when(
      month %in% c(12, 1, 2) ~ "winter",
      month %in% c(3, 4, 5)  ~ "spring",
      month %in% c(6, 7, 8)  ~ "summer",
      TRUE                   ~ "autumn"))

#B.8.2 Stationsübersicht mit Mittelwerten, Streuung und Spannweite erstellen
HJA_Ipot_station_summary <- HJA_Ipot_station_check %>%
  group_by(Site) %>%
  summarise(
    n_days = n(),
    HJA_Ipot_buf_mean_MJ  = mean(HJA_Ipot_buf_mean, na.rm = TRUE),
    HJA_Ipot_buf_sd_MJ    = sd(HJA_Ipot_buf_mean, na.rm = TRUE),
    HJA_Ipot_buf_min_MJ   = min(HJA_Ipot_buf_mean, na.rm = TRUE),
    HJA_Ipot_buf_max_MJ   = max(HJA_Ipot_buf_mean, na.rm = TRUE),
    HJA_Ipot_point_mean_MJ = mean(HJA_Ipot_point, na.rm = TRUE),
    HJA_Ipot_point_sd_MJ   = sd(HJA_Ipot_point, na.rm = TRUE),
    HJA_Ipot_point_min_MJ  = min(HJA_Ipot_point, na.rm = TRUE),
    HJA_Ipot_point_max_MJ  = max(HJA_Ipot_point, na.rm = TRUE),
    .groups = "drop")

print(HJA_Ipot_station_summary)

#B.8.3 Konsistenz zwischen Buffer-Mittel und Punktwert prüfen
HJA_Ipot_point_buffer_check <- HJA_Ipot_station_check %>%
  group_by(Site) %>%
  summarise(
    cor_point_buffer = cor(HJA_Ipot_buf_mean, HJA_Ipot_point, use = "complete.obs"),
    mean_abs_diff_MJ = mean(abs(HJA_Ipot_buf_mean - HJA_Ipot_point), na.rm = TRUE),
    mean_signed_diff_MJ = mean(HJA_Ipot_point - HJA_Ipot_buf_mean, na.rm = TRUE),
    .groups = "drop")

print(HJA_Ipot_point_buffer_check)

#B.8.4 Saisonale Mittelwerte pro Station bestimmen
HJA_Ipot_seasonal_summary <- HJA_Ipot_station_check %>%
  group_by(Site, season_simple) %>%
  summarise(
    HJA_Ipot_buf_mean_MJ   = mean(HJA_Ipot_buf_mean, na.rm = TRUE),
    HJA_Ipot_point_mean_MJ = mean(HJA_Ipot_point, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(Site, factor(season_simple, levels = c("winter", "spring", "summer", "autumn")))

print(HJA_Ipot_seasonal_summary)

#B.8.5 Winter-Sommer-Kontrast je Station bestimmen
HJA_Ipot_winter_summer_contrast <- HJA_Ipot_seasonal_summary %>%
  select(Site, season_simple, HJA_Ipot_buf_mean_MJ, HJA_Ipot_point_mean_MJ) %>%
  tidyr::pivot_wider(
    names_from = season_simple,
    values_from = c(HJA_Ipot_buf_mean_MJ, HJA_Ipot_point_mean_MJ)) %>%
  mutate(
    buf_winter_to_summer_ratio = HJA_Ipot_buf_mean_MJ_winter / HJA_Ipot_buf_mean_MJ_summer,
    point_winter_to_summer_ratio = HJA_Ipot_point_mean_MJ_winter / HJA_Ipot_point_mean_MJ_summer)

print(HJA_Ipot_winter_summer_contrast)

#================================================================================================================================================================================================
###SCHRITT B.9: Sensitivitätscheck für psi_alpha###
#================================================================================================================================================================================================

#B.9.1 Zu testende psi_alpha-Werte und repräsentative Tagesstichprobe festlegen
HJA_Ipot_psi_alpha_grid <- c(0.75, 0.65, 0.85)

#B.9.2 Hilfsfunktion zur Einteilung der Jahreszeiten in einfache Kategorien
HJA_Ipot_get_season_simple <- function(date) {
  month_value <- lubridate::month(date)
  
  if (month_value %in% c(12, 1, 2)) return("winter")
  if (month_value %in% c(3, 4, 5))  return("spring")
  if (month_value %in% c(6, 7, 8))  return("summer")
  return("autumn")}

#B.9.3 Repräsentative Tagesstichprobe für die Sensitivitätsanalyse erstellen.3 
HJA_Ipot_sensitivity_dates_tbl <- data.frame(
  Date = HJA_Ipot_dates_seq,
  season_simple = vapply(HJA_Ipot_dates_seq, HJA_Ipot_get_season_simple, character(1)))

#B.9.4 Pro Saison eine zufällige Stichprobe von Tagen für die Sensitivitätsanalyse ziehen
HJA_Ipot_n_dates_per_season <- 30

#B.9.5 Stichprobe der Tage für die Sensitivitätsanalyse erstellen
HJA_Ipot_sensitivity_dates <- HJA_Ipot_sensitivity_dates_tbl %>%
  group_by(season_simple) %>%
  group_modify(~{
    n_take <- min(HJA_Ipot_n_dates_per_season, nrow(.x))
    .x[sample(seq_len(nrow(.x)), n_take), , drop = FALSE]}) %>%
  ungroup() %>%
  arrange(Date) %>%
  pull(Date)

#B.9.6 Hilfsfunktion: stationsbasierte I_pot-Zeitreihen für einen psi_alpha-Wert berechnen
HJA_Ipot_compute_psi_alpha_sensitivity <- function(dates_seq, psi_alpha_value) {
  
  HJA_Ipot_sensitivity_all <- data.frame()
  
  for (i in seq_len(nrow(HJA_Ipot_sites_ll))) {
    
    HJA_Ipot_site_name <- HJA_Ipot_sites_ll$Site[i]
    cat("\npsi_alpha =", psi_alpha_value, "| Station:", HJA_Ipot_site_name, "\n")
    
    HJA_Ipot_site_point_sf <- HJA_Ipot_sites_sf_utm[i, ]
    HJA_Ipot_site_buf_sf   <- HJA_Ipot_sites_buf_utm[i, ]
    
    HJA_Ipot_site_point_vect <- vect(HJA_Ipot_site_point_sf)
    HJA_Ipot_site_buf_vect   <- vect(HJA_Ipot_site_buf_sf)
    
    HJA_Ipot_dem_site <- crop(HJA_Ipot_dem, HJA_Ipot_site_buf_vect) |> mask(HJA_Ipot_site_buf_vect)
    
    HJA_Ipot_slope_deg  <- terrain(HJA_Ipot_dem_site, v = "slope",  unit = "degrees")
    HJA_Ipot_aspect_deg <- terrain(HJA_Ipot_dem_site, v = "aspect", unit = "degrees")
    
    HJA_Ipot_slope_rad  <- HJA_Ipot_slope_deg  * (pi / 180)
    HJA_Ipot_aspect_rad <- HJA_Ipot_aspect_deg * (pi / 180)
    
    HJA_Ipot_cos_slope  <- cos(HJA_Ipot_slope_rad)
    HJA_Ipot_sin_slope  <- sin(HJA_Ipot_slope_rad)
    HJA_Ipot_cos_aspect <- cos(HJA_Ipot_aspect_rad)
    HJA_Ipot_sin_aspect <- sin(HJA_Ipot_aspect_rad)
    
    HJA_Ipot_latitude_deg  <- HJA_Ipot_sites_ll$lat[i]
    HJA_Ipot_longitude_deg <- HJA_Ipot_sites_ll$lon[i]
    
    HJA_Ipot_site_results <- lapply(dates_seq, function(current_date) {
      
      HJA_Ipot_daily_result <- HJA_Ipot_compute_daily_for_site(
        current_date             = current_date,
        HJA_Ipot_dem_site        = HJA_Ipot_dem_site,
        HJA_Ipot_cos_slope       = HJA_Ipot_cos_slope,
        HJA_Ipot_sin_slope       = HJA_Ipot_sin_slope,
        HJA_Ipot_cos_aspect      = HJA_Ipot_cos_aspect,
        HJA_Ipot_sin_aspect      = HJA_Ipot_sin_aspect,
        HJA_Ipot_latitude_deg    = HJA_Ipot_latitude_deg,
        HJA_Ipot_longitude_deg   = HJA_Ipot_longitude_deg,
        HJA_Ipot_site_point_vect = HJA_Ipot_site_point_vect,
        HJA_Ipot_site_buffer_vect = HJA_Ipot_site_buf_vect,
        HJA_Ipot_hours_seq       = HJA_Ipot_hours_seq,
        HJA_Ipot_solar_constant  = HJA_Ipot_solar_constant,
        HJA_Ipot_psi_alpha       = psi_alpha_value,
        HJA_Ipot_tz              = HJA_Ipot_tz)
      
      data.frame(
        Site = HJA_Ipot_site_name,
        Date = as.Date(HJA_Ipot_daily_result$Date),
        psi_alpha = psi_alpha_value,
        HJA_Ipot_buf_mean = HJA_Ipot_daily_result$HJA_Ipot_buf_mean,
        HJA_Ipot_point    = HJA_Ipot_daily_result$HJA_Ipot_point)})
    
    HJA_Ipot_sensitivity_all <- rbind(
      HJA_Ipot_sensitivity_all,
      do.call(rbind, HJA_Ipot_site_results))}
  
  HJA_Ipot_sensitivity_all %>%
    mutate(
      season_simple = vapply(Date, HJA_Ipot_get_season_simple, character(1))) %>%
    arrange(psi_alpha, Site, Date)}

#B.9.3 psi_alpha-Grid durchrechnen
HJA_Ipot_psi_alpha_sensitivity_results <- vector("list", length(HJA_Ipot_psi_alpha_grid))

for (psi_alpha_index in seq_along(HJA_Ipot_psi_alpha_grid)) {
  
  current_psi_alpha <- HJA_Ipot_psi_alpha_grid[psi_alpha_index]
  
  HJA_Ipot_psi_alpha_sensitivity_results[[psi_alpha_index]] <- HJA_Ipot_compute_psi_alpha_sensitivity(
    dates_seq = HJA_Ipot_sensitivity_dates,
    psi_alpha_value = current_psi_alpha)}

HJA_Ipot_psi_alpha_sensitivity <- bind_rows(HJA_Ipot_psi_alpha_sensitivity_results)

#B.9.4 Mittelwerte der potenziellen Direktstrahlung pro psi_alpha und Jahreszeit
HJA_Ipot_psi_alpha_scaling_summary <- HJA_Ipot_psi_alpha_sensitivity %>%
  group_by(psi_alpha, season_simple) %>%
  summarise(
    HJA_Ipot_buf_mean_MJ   = mean(HJA_Ipot_buf_mean, na.rm = TRUE),
    HJA_Ipot_point_mean_MJ = mean(HJA_Ipot_point, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(psi_alpha, factor(season_simple, levels = c("winter", "spring", "summer", "autumn")))

print(HJA_Ipot_psi_alpha_scaling_summary)

#B.9.5 Stationsweise Mittelwerte der potenziellen Direktstrahlung pro psi_alpha und Jahreszeit
HJA_Ipot_psi_alpha_station_summary <- HJA_Ipot_psi_alpha_sensitivity %>%
  group_by(psi_alpha, Site, season_simple) %>%
  summarise(
    HJA_Ipot_buf_mean_MJ   = mean(HJA_Ipot_buf_mean, na.rm = TRUE),
    HJA_Ipot_point_mean_MJ = mean(HJA_Ipot_point, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(psi_alpha, Site, factor(season_simple, levels = c("winter", "spring", "summer", "autumn")))

print(HJA_Ipot_psi_alpha_station_summary)

#B.9.6 Relative Änderung gegenüber psi_alpha = 0.75
HJA_Ipot_psi_alpha_reference <- HJA_Ipot_psi_alpha_sensitivity %>%
  filter(psi_alpha == 0.75) %>%
  select(
    Site,
    Date,
    HJA_Ipot_buf_reference   = HJA_Ipot_buf_mean,
    HJA_Ipot_point_reference = HJA_Ipot_point)

HJA_Ipot_psi_alpha_relative_change <- HJA_Ipot_psi_alpha_sensitivity %>%
  left_join(HJA_Ipot_psi_alpha_reference, by = c("Site", "Date")) %>%
  mutate(
    HJA_Ipot_buf_ratio_to_reference   = HJA_Ipot_buf_mean / HJA_Ipot_buf_reference,
    HJA_Ipot_point_ratio_to_reference = HJA_Ipot_point / HJA_Ipot_point_reference) %>%
  group_by(psi_alpha, Site, season_simple) %>%
  summarise(
    HJA_Ipot_buf_ratio_to_reference   = mean(HJA_Ipot_buf_ratio_to_reference, na.rm = TRUE),
    HJA_Ipot_point_ratio_to_reference = mean(HJA_Ipot_point_ratio_to_reference, na.rm = TRUE),
    .groups = "drop") %>%
  arrange(psi_alpha, Site, factor(season_simple, levels = c("winter", "spring", "summer", "autumn")))

print(HJA_Ipot_psi_alpha_relative_change)

#B.9.7 Plot-Dataframe vorbereiten
HJA_Ipot_psi_alpha_plot_df <- HJA_Ipot_psi_alpha_sensitivity %>%
  dplyr::select(Site, Date, psi_alpha, HJA_Ipot_buf_mean) %>%
  dplyr::mutate(
    Site = factor(Site, levels = c("CENMET", "PRIMET", "UPLMET")),
    psi_alpha = factor(psi_alpha, levels = c(0.75, 0.65, 0.85)))

HJA_Ipot_psi_alpha_ymax <- ceiling(max(HJA_Ipot_psi_alpha_plot_df$HJA_Ipot_buf_mean, na.rm = TRUE) / 5) * 5

#B.9.8 Plot der Sensitivität der potenziellen Direktstrahlung gegenüber psi_alpha
ggplot(HJA_Ipot_psi_alpha_plot_df, aes(x = Date, y = HJA_Ipot_buf_mean, color = psi_alpha)) +
  geom_line(linewidth = 1.5, na.rm = TRUE) +
  facet_wrap(~ Site, ncol = 1, scales = "fixed") +
  scale_color_manual(
    values = c(
      "0.75" = "black",
      "0.65" = "blue",
      "0.85" = "red"),
    labels = c(
      "0.75" = expression(psi[alpha] == 0.75),
      "0.65" = expression(psi[alpha] == 0.65),
      "0.85" = expression(psi[alpha] == 0.85)),
    name = NULL) +
  scale_y_continuous(
    limits = c(0, HJA_Ipot_psi_alpha_ymax),
    breaks = seq(0, HJA_Ipot_psi_alpha_ymax, by = 10)) +
  labs(
    x = "Datum",
    y = "Potenzielle Direktstrahlung (MJ m⁻² d⁻¹)") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.x  = element_text(size = 13),
    axis.text.y  = element_text(size = 13),
    legend.text  = element_text(size = 13))

#9.9 Kompakte stationsweise Gesamtauswertung über alle Jahreszeiten
HJA_Ipot_psi_alpha_relative_change_compact <- HJA_Ipot_psi_alpha_sensitivity %>%
  left_join(HJA_Ipot_psi_alpha_reference, by = c("Site", "Date")) %>%
  mutate(
    HJA_Ipot_buf_ratio_to_reference = HJA_Ipot_buf_mean / HJA_Ipot_buf_reference
  ) %>%
  group_by(psi_alpha, Site) %>%
  summarise(
    rel_change_mean = mean(HJA_Ipot_buf_ratio_to_reference, na.rm = TRUE),
    rel_change_pct  = (rel_change_mean - 1) * 100,
    .groups = "drop"
  ) %>%
  arrange(psi_alpha, Site)

print(HJA_Ipot_psi_alpha_relative_change_compact)

#================================================================================================================================================================================================
###ABSCHNITT C: MEHRSTATIOSNKALIBRIERUNG###
#================================================================================================================================================================================================
#================================================================================================================================================================================================
###SCHRITT C.1: VORBEREITUNGEN###
#================================================================================================================================================================================================

#C.1.1 Reproduzierbarkeit sicherstellen
set.seed(1)

#C.1.2 Kalibrierfenster festlegen
HJA_calibration_start <- HJA_sim_start
HJA_calibration_end   <- HJA_sim_end

#C.1.3 In die Kalibrierung einbezogene Stationen festlegen
HJA_calibration_sites <- HJA_station_daily_df$Site

#C.1.4 Fixwerte für die Niederschlagspartitionierung festlegen
HJA_calibration_T_snow_fixed <- -1
HJA_calibration_T_rain_fixed <-  2

#C.1.5 LHS- und Monte-Carlo-Einstellungen festlegen
HJA_calibration_N_lhs <- 5000
HJA_calibration_seed  <- 1

#C.1.6 Parametergrenzen des Screening-Ranges festlegen (Mit stationsspezifischen Multiplikatoren für kP)
HJA_calibration_parameter_mins <- c(
  kP_global      = 0.6,
  kP_mult_CENMET = 0.6,
  kP_mult_UPLMET = 0.6, 
  Tcrit          = -1.5,
  Cdd            = 0.5,
  kR             = 0.00)

HJA_calibration_parameter_maxs <- c(
  kP_global      = 1.6,
  kP_mult_CENMET = 1.6,
  kP_mult_UPLMET = 1.6,
  Tcrit          = 2.5,
  Cdd            = 8.0,
  kR             = 2.50)

#C.1.7 Penalty-Settings für einen fairen Multi-Station-Fit festlegen
##ANMERKUNG: PENALTY-TERM MIT GEWICHTUNG DER KGE-UNTERGRENZEN FÜR JEDE STATION, UM EIN AUSREIßERISCHES ANPASSEN AN EINZELNE STATIONEN ZU VERMEIDEN##
HJA_calibration_kge_floor       <- 0.50
HJA_calibration_penalty_weight  <- 5.0

#================================================================================================================================================================================================
###SCHRITT C.2: Finalen Kalibrier-Dataframe aus SWE, Klima und I_pot aufbauen###
#================================================================================================================================================================================================

#C.2.1 Stationsspezifische SWE-, Temperatur- und Niederschlagsdaten für das Kalibrierfenster vorbereiten
HJA_calibration_station_input <- HJA_station_daily_df %>%
  select(Site, data) %>%
  tidyr::unnest(cols = c(data), names_repair = "minimal") %>%
  { names(.) <- make.unique(names(.)); . } %>%
  transmute(
    Site       = as.character(Site),
    Date       = as.Date(Date),
    SWE_obs_mm = as.numeric(SWE),
    T_c        = as.numeric(T),
    P_mm       = as.numeric(P),
    Elev_m     = as.numeric(Elevation)) %>%
  filter(Date >= HJA_calibration_start, Date <= HJA_calibration_end)

#C.2.2 Stationsspezifische Zeitreihe der potenziellen Direktstrahlung für das Kalibrierfenster vorbereiten
HJA_calibration_Ipot_input <- HJA_Ipot_station_daily %>%
  transmute(
    Site            = as.character(Site),
    Date            = as.Date(Date),
    I_pot_MJm2_day  = as.numeric(HJA_Ipot_buf_mean)) %>%
  filter(Date >= HJA_calibration_start, Date <= HJA_calibration_end)

#C.2.3 Finalen Kalibrier-Dataframe aus Stationsdaten und potenzieller Direktstrahlung zusammenführen
HJA_calibration_df <- HJA_calibration_station_input %>%
  left_join(HJA_calibration_Ipot_input, by = c("Site", "Date")) %>%
  arrange(Site, Date)

#C.2.4 Complete-Case-Filter anwenden
HJA_calibration_df <- HJA_calibration_df %>%
  filter(
    is.finite(SWE_obs_mm),
    is.finite(T_c),
    is.finite(P_mm),
    is.finite(I_pot_MJm2_day))

#================================================================================================================================================================================================
###SCHRITT 3: MODELLFUNKTIONEN FÜR DIE MULTI-STATION-SWE-SIMULATION DEFINIEREN###
#================================================================================================================================================================================================

#C.3.1 lineare Niederschlagspartitionierung nach HARPOLD et al. (2017)
HJA_calibration_partition_precip <- function(T_mean_C, P_total_mm, T_snow, T_rain) {
  
  snow_fraction <- ifelse(
    is.na(T_mean_C) | is.na(P_total_mm),
    NA_real_,
    ifelse(
      T_mean_C <= T_snow, 1,
      ifelse(
        T_mean_C >= T_rain, 0,
        (T_rain - T_mean_C) / (T_rain - T_snow))))
  
  snow_mm <- P_total_mm * snow_fraction
  rain_mm <- P_total_mm - snow_mm
  
  list(
    snow_fraction = snow_fraction,
    snow_mm       = snow_mm,
    rain_mm       = rain_mm)}

##AMMERKUNG: IN ANLEHNUNG AN HOCK (1999), ANDERSON (2006), HARPOLD et al. (2017) und BEVEN & BINLEY (1992)##

#E.3.2 Hauptfunktion: Modellierung des SWE auf Basis der Kalibrier-Inputs und der empirischen Parameter
HJA_calibration_run_swe_model <- function(
    df,
    params,
    T_snow = HJA_calibration_T_snow_fixed,
    T_rain = HJA_calibration_T_rain_fixed) {
  
  kP_global      <- as.numeric(params["kP_global"])
  kP_mult_CENMET <- as.numeric(params["kP_mult_CENMET"])
  kP_mult_UPLMET <- as.numeric(params["kP_mult_UPLMET"])
  Tcrit          <- as.numeric(params["Tcrit"])
  Cdd            <- as.numeric(params["Cdd"])
  kR             <- as.numeric(params["kR"])
  
  HJA_calibration_site_list <- split(df, df$Site)
  
  HJA_calibration_simulation_list <- lapply(names(HJA_calibration_site_list), function(current_site) {
    
    simulation_df <- HJA_calibration_site_list[[current_site]]
    simulation_df <- simulation_df[order(simulation_df$Date), ]
    
    simulation_df$snow_fraction     <- NA_real_
    simulation_df$snow_mm           <- NA_real_
    simulation_df$rain_mm           <- NA_real_
    simulation_df$snow_input_mm     <- NA_real_
    simulation_df$melt_potential_mm <- NA_real_
    simulation_df$melt_actual_mm    <- NA_real_
    simulation_df$SWE_sim_mm        <- NA_real_
    
    current_site_kP <- if (current_site == "CENMET") {
      kP_global * kP_mult_CENMET } else if (current_site == "UPLMET") {
      kP_global * kP_mult_UPLMET} else {
      kP_global}
    
    SWE_previous <- ifelse(
      is.finite(simulation_df$SWE_obs_mm[1]),
      simulation_df$SWE_obs_mm[1],
      0)
    
    simulation_df$SWE_sim_mm[1] <- SWE_previous
    
    if (nrow(simulation_df) >= 2) {
      for (day_index in 2:nrow(simulation_df)) {
        
        current_temperature <- simulation_df$T_c[day_index]
        current_precip      <- simulation_df$P_mm[day_index]
        current_radiation   <- simulation_df$I_pot_MJm2_day[day_index]
        
        #Niederschlagspartitionierung
        partition_result <- HJA_calibration_partition_precip(
          T_mean_C = current_temperature,
          P_total_mm = current_precip,
          T_snow = T_snow,
          T_rain = T_rain)
        
        current_snow_mm       <- partition_result$snow_mm
        current_rain_mm       <- partition_result$rain_mm
        current_snow_fraction <- partition_result$snow_fraction
        
        #Schneeinput mit skalierendem Niederschlagsfaktor
        current_snow_input <- ifelse(
          is.na(current_snow_mm),
          NA_real_,
          current_site_kP * current_snow_mm)
        
        #Potenzielle Schmelze: Temperaturindex + Strahlung
        current_melt_potential <- ifelse(
          is.na(current_temperature) | is.na(current_radiation),
          NA_real_,
          pmax(0, Cdd * (current_temperature - Tcrit) + kR * current_radiation))
        
        #Speicher vor Schmelze
        SWE_before_melt <- ifelse(
          is.na(current_snow_input),
          SWE_previous,
          SWE_previous + current_snow_input)
        
        #Tatsächliche Schmelze kann Speicher nicht überschreiten
        current_melt_actual <- ifelse(
          is.na(current_melt_potential) | is.na(SWE_before_melt),
          NA_real_,
          min(SWE_before_melt, current_melt_potential))
        
        #Aktualisierter Schneespeicher
        current_SWE <- ifelse(
          is.na(SWE_before_melt) | is.na(current_melt_actual),
          NA_real_,
          pmax(0, SWE_before_melt - current_melt_actual))
        
        # Ergebnisse speichern
        simulation_df$snow_fraction[day_index]     <- current_snow_fraction
        simulation_df$snow_mm[day_index]           <- current_snow_mm
        simulation_df$rain_mm[day_index]           <- current_rain_mm
        simulation_df$snow_input_mm[day_index]     <- current_snow_input
        simulation_df$melt_potential_mm[day_index] <- current_melt_potential
        simulation_df$melt_actual_mm[day_index]    <- current_melt_actual
        simulation_df$SWE_sim_mm[day_index]        <- current_SWE
        
        SWE_previous <- ifelse(is.na(current_SWE), SWE_previous, current_SWE)}}
    
    simulation_df})
  
  do.call(rbind, HJA_calibration_simulation_list)}

#================================================================================================================================================================================================
###SCHRITT C.4: GÜTEMASSE UND SCORE-FUNKTIONEN FÜR DIE KALIBRIERUNG DEFINIEREN###
#================================================================================================================================================================================================
#ANMERKUNG: nach KNOBEN et al. (2019), SETIYOWATI et al. (2025) und HAUGHTON et al. (2016)#

#C.4.1 RMSE (Root Mean Square Error)
HJA_calibration_rmse <- function(obs, sim) {
  valid_values <- is.finite(obs) & is.finite(sim)
  if (sum(valid_values) == 0) return(NA_real_)
  sqrt(mean((sim[valid_values] - obs[valid_values])^2))}

#C.4.2 MBE (Mean Bias Error)
HJA_calibration_mbe <- function(obs, sim) {
  valid_values <- is.finite(obs) & is.finite(sim)
  if (sum(valid_values) == 0) return(NA_real_)
  mean(sim[valid_values] - obs[valid_values])}

#C.4.3 Pearson-Korrelationskoeffizient r
HJA_calibration_pearson_r <- function(obs, sim) {
  valid_values <- is.finite(obs) & is.finite(sim)
  if (sum(valid_values) < 2) return(NA_real_)
  stats::cor(obs[valid_values], sim[valid_values], method = "pearson")}

#C.4.4 KGE (Kling-Gupta Efficiency)
HJA_calibration_kge <- function(obs, sim) {
  valid_values <- is.finite(obs) & is.finite(sim)
  if (sum(valid_values) == 0) return(NA_real_)
  
  sim_valid <- sim[valid_values]
  obs_valid <- obs[valid_values]
  
  r     <- suppressWarnings(cor(sim_valid, obs_valid))
  alpha <- sd(sim_valid) / sd(obs_valid)
  beta  <- mean(sim_valid) / mean(obs_valid)
  
  1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)}

#C.4.5 Score-Funktion für einen Multi-Station-Run
##ANMERKUNG: IN ANLEHNUNG AN EFSTRATIADIS & KOUTSOYIANNIS (2010), LIU et al. (2022) und THIEMIG et al. (2013)##
HJA_calibration_score_run <- function(
    simulation_df,
    kge_floor = HJA_calibration_kge_floor,
    penalty_weight = HJA_calibration_penalty_weight) {
  
  HJA_calibration_by_station <- simulation_df %>%
    dplyr::group_by(Site) %>%
    dplyr::summarise(
      n_obs = sum(is.finite(SWE_obs_mm) & is.finite(SWE_sim_mm)),
      RMSE  = HJA_calibration_rmse(obs = SWE_obs_mm, sim = SWE_sim_mm),
      MBE   = HJA_calibration_mbe(obs = SWE_obs_mm, sim = SWE_sim_mm),
      r     = HJA_calibration_pearson_r(obs = SWE_obs_mm, sim = SWE_sim_mm),
      KGE   = HJA_calibration_kge(obs = SWE_obs_mm, sim = SWE_sim_mm),
      .groups = "drop")
  
  HJA_calibration_KGE_mean <- mean(HJA_calibration_by_station$KGE, na.rm = TRUE)
  HJA_calibration_KGE_min  <- min(HJA_calibration_by_station$KGE, na.rm = TRUE)
  HJA_calibration_penalty  <- penalty_weight * pmax(0, kge_floor - HJA_calibration_KGE_min)
  
  # main_score klein = gut
  HJA_calibration_main_score <- (1 - HJA_calibration_KGE_mean) + HJA_calibration_penalty
  
  list(
    main_score = HJA_calibration_main_score,
    KGE_mean   = HJA_calibration_KGE_mean,
    KGE_min    = HJA_calibration_KGE_min,
    penalty    = HJA_calibration_penalty,
    by_station = HJA_calibration_by_station)}

#================================================================================================================================================================================================
###SCHRITT C.5: KALIBRIERUNG (LHS/MONTE-CARLO): PARAMETER-SAMPLE AUFBAUEN###
#================================================================================================================================================================================================

#C.5.1 LHS-ähnliches Parameter-Sampling definieren (in Anlehnung an SHIELDS & ZHANG 2016)
HJA_calibration_generate_lhs_sample <- function(N, parameter_mins, parameter_maxs) {
  parameter_names <- names(parameter_mins)
  
  lhs_unit_sample <- sapply(parameter_names, function(parameter_name) {
    unit_values <- (seq_len(N) - runif(N)) / N
    sample(unit_values, size = N, replace = FALSE)})
  
  lhs_sample_df <- as.data.frame(lhs_unit_sample)
  
  for (parameter_name in parameter_names) {
    lhs_sample_df[[parameter_name]] <- parameter_mins[parameter_name] +
      lhs_sample_df[[parameter_name]] * (parameter_maxs[parameter_name] - parameter_mins[parameter_name])}
  
  lhs_sample_df}

#C.5.2 LHS-Sample ziehen
set.seed(HJA_calibration_seed)

HJA_calibration_parameter_sample <- HJA_calibration_generate_lhs_sample(
  N = HJA_calibration_N_lhs,
  parameter_mins = HJA_calibration_parameter_mins,
  parameter_maxs = HJA_calibration_parameter_maxs)

#C.5.3 Run-ID ergänzen und Reihenfolge festlegen
HJA_calibration_parameter_sample <- HJA_calibration_parameter_sample %>%
  mutate(run_id = row_number()) %>%
  select(
    run_id,
    kP_global,
    kP_mult_CENMET,
    kP_mult_UPLMET,
    Tcrit,
    Cdd,
    kR)

#================================================================================================================================================================================================
###SCHRITT C.6: KALIBRIERUNGSLOOP ÜBER ALLE LHS-LÄUFE###
#================================================================================================================================================================================================

#C.6.1 Hauptscore definieren
HJA_calibration_main_score <- "main_score"

#C.6.2 Leere Ergebnisliste anlegen
HJA_calibration_results <- vector("list", length = nrow(HJA_calibration_parameter_sample))

#C.6.3 Fortschrittsbalken initialisieren
HJA_calibration_progress <- txtProgressBar(
  min = 0,
  max = nrow(HJA_calibration_parameter_sample),
  style = 3)

#C.6.4 Loop über alle LHS-Läufe
for (run_index in seq_len(nrow(HJA_calibration_parameter_sample))) {
  setTxtProgressBar(HJA_calibration_progress, run_index)
  
  current_parameter_row <- HJA_calibration_parameter_sample[run_index, ]
  
  current_parameter_vector <- list(
    kP_global      = current_parameter_row$kP_global,
    kP_mult_CENMET = current_parameter_row$kP_mult_CENMET,
    kP_mult_UPLMET = current_parameter_row$kP_mult_UPLMET,
    Tcrit          = current_parameter_row$Tcrit,
    Cdd            = current_parameter_row$Cdd,
    kR             = current_parameter_row$kR)
  
  current_model_run <- HJA_calibration_run_swe_model(
    df = HJA_calibration_df,
    params = current_parameter_vector,
    T_snow = HJA_calibration_T_snow_fixed,
    T_rain = HJA_calibration_T_rain_fixed)
  
  current_scores <- HJA_calibration_score_run(current_model_run)
  
  HJA_calibration_results[[run_index]] <- data.frame(
    run_id          = current_parameter_row$run_id,
    kP_global       = current_parameter_row$kP_global,
    kP_mult_CENMET  = current_parameter_row$kP_mult_CENMET,
    kP_mult_UPLMET  = current_parameter_row$kP_mult_UPLMET,
    Tcrit           = current_parameter_row$Tcrit,
    Cdd             = current_parameter_row$Cdd,
    kR              = current_parameter_row$kR,
    main_score      = current_scores$main_score,
    KGE_mean        = current_scores$KGE_mean,
    KGE_min         = current_scores$KGE_min,
    penalty         = current_scores$penalty)}

close(HJA_calibration_progress)

#C.6.5 Ergebnistabelle zusammenführen und nach Hauptscore sortieren
HJA_calibration_results_df <- dplyr::bind_rows(HJA_calibration_results) %>%
  arrange(main_score)

#C.6.6 Quick check
print(head(HJA_calibration_results_df, 10))
summary(HJA_calibration_results_df)

#================================================================================================================================================================================================
###SCHRITT C.7: BESTEN LAUF EXTRAHIEREN UND ERNEUT DURCHRECHNEN###
#================================================================================================================================================================================================

#C.7.1 Besten Parametersatz auswählen
HJA_calibration_best_run_summary <- HJA_calibration_results_df %>%
  slice(1)

print(HJA_calibration_best_run_summary)

#C.7.2 Parametervektor für den besten Lauf aufbauen
HJA_calibration_best_params <- list(
  kP_global      = HJA_calibration_best_run_summary$kP_global,
  kP_mult_CENMET = HJA_calibration_best_run_summary$kP_mult_CENMET,
  kP_mult_UPLMET = HJA_calibration_best_run_summary$kP_mult_UPLMET,
  Tcrit          = HJA_calibration_best_run_summary$Tcrit,
  Cdd            = HJA_calibration_best_run_summary$Cdd,
  kR             = HJA_calibration_best_run_summary$kR)

print(HJA_calibration_best_params)

#C.7.3 Best-Run-Zeitreihe erneut simulieren
HJA_calibration_best_run_df <- HJA_calibration_run_swe_model(
  df = HJA_calibration_df,
  params = HJA_calibration_best_params,
  T_snow = HJA_calibration_T_snow_fixed,
  T_rain = HJA_calibration_T_rain_fixed)

#C.7.4 Begleitende Scores des besten Laufs prüfen
HJA_calibration_best_run_scores <- HJA_calibration_score_run(
  HJA_calibration_best_run_df)

print(HJA_calibration_best_run_scores)

#C.7.5 Stationsscores des besten Laufs separat bereitstellen
HJA_calibration_best_station_scores <- HJA_calibration_best_run_scores$by_station %>%
  arrange(KGE)

print(HJA_calibration_best_station_scores)

#================================================================================================================================================================================================
###SCHRITT C.8: GGPlot - 3 PANELS (PRIMET/CENMET/UPLMET) OBS vs SIM###
#================================================================================================================================================================================================

#C.8.1 KGE pro Station für die Panel-Labels berechnen
HJA_calibration_kge_panel_table <- HJA_calibration_best_run_df %>%
  group_by(Site) %>%
  summarise(
    KGE = HJA_calibration_kge(obs = SWE_obs_mm, sim = SWE_sim_mm),
    .groups = "drop") %>%
  mutate(
    panel = paste0(Site, " | KGE: ", round(KGE, 2)))

#C.8.2 Long-Format für ggplot vorbereiten
HJA_calibration_best_run_plot_df <- HJA_calibration_best_run_df %>%
  select(Site, Date, SWE_obs_mm, SWE_sim_mm) %>%
  pivot_longer(
    cols = c(SWE_obs_mm, SWE_sim_mm),
    names_to = "series",
    values_to = "SWE_mm") %>%
  mutate(
    series = recode(
      series,
      SWE_obs_mm = "Observed",
      SWE_sim_mm = "Simulated")) %>%
  left_join(
    HJA_calibration_kge_panel_table %>% select(Site, panel),
    by = "Site")

#C.8.3 Gemeinsames y-Limit bestimmen
HJA_calibration_SWE_ymax <- max(
  HJA_calibration_best_run_plot_df$SWE_mm,
  na.rm = TRUE)

#C.8.4 Plot: 3 Panels untereinander
ggplot(HJA_calibration_best_run_plot_df, aes(x = Date, y = SWE_mm, color = series)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  facet_wrap(~ panel, ncol = 1, scales = "fixed") +
  scale_color_manual(
    values = c(
      "Observed"  = "blue",
      "Simulated" = "red"),
    name = NULL) +
  scale_y_continuous(
    limits = c(0, HJA_calibration_SWE_ymax)) +
  labs(
    x = "Datum",
    y = "SWE (mm)") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11, lineheight = 0.9),
    strip.background = element_rect(fill = "grey85", colour = "black"),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.text  = element_text(size = 12))

#================================================================================================================================================================================================
###SCHRITT C.9: ROBUSTHEIT DER KALIBRIERUNG - TOP-ENSEMBLE (ÄQUIFINALITÄT)###
#================================================================================================================================================================================================

#C.9.1 Top-Ensemble definieren: beste 30 Läufe
HJA_calibration_top_n <- min(30, nrow(HJA_calibration_results_df))

HJA_calibration_top_runs <- HJA_calibration_results_df %>%
  slice(seq_len(HJA_calibration_top_n))

#C.9.2 Hilfsfunktion für 5-, 50- und 95-%-Quantile
HJA_calibration_quantile_summary <- function(x) {
  as.numeric(stats::quantile(x, probs = c(0.05, 0.50, 0.95), na.rm = TRUE))}

#C.9.3 Parameter-Quantile des Top-Ensembles
HJA_calibration_top_parameter_quantiles <- data.frame(
  top_n = HJA_calibration_top_n,
  
  kP_global_q05 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kP_global)[1],
  kP_global_q50 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kP_global)[2],
  kP_global_q95 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kP_global)[3],
  
  kP_mult_CENMET_q05 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kP_mult_CENMET)[1],
  kP_mult_CENMET_q50 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kP_mult_CENMET)[2],
  kP_mult_CENMET_q95 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kP_mult_CENMET)[3],
  
  kP_mult_UPLMET_q05 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kP_mult_UPLMET)[1],
  kP_mult_UPLMET_q50 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kP_mult_UPLMET)[2],
  kP_mult_UPLMET_q95 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kP_mult_UPLMET)[3],
  
  Tcrit_q05 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$Tcrit)[1],
  Tcrit_q50 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$Tcrit)[2],
  Tcrit_q95 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$Tcrit)[3],
  
  Cdd_q05 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$Cdd)[1],
  Cdd_q50 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$Cdd)[2],
  Cdd_q95 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$Cdd)[3],
  
  kR_q05 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kR)[1],
  kR_q50 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kR)[2],
  kR_q95 = HJA_calibration_quantile_summary(HJA_calibration_top_runs$kR)[3])

print(HJA_calibration_top_parameter_quantiles)

#C.9.4 Spannweite der Gütemaße innerhalb des Top-Ensembles
HJA_calibration_top_score_ranges <- data.frame(
  top_n = nrow(HJA_calibration_top_runs),
  
  main_score_min = min(HJA_calibration_top_runs$main_score, na.rm = TRUE),
  main_score_max = max(HJA_calibration_top_runs$main_score, na.rm = TRUE),
  
  KGE_mean_min = min(HJA_calibration_top_runs$KGE_mean, na.rm = TRUE),
  KGE_mean_max = max(HJA_calibration_top_runs$KGE_mean, na.rm = TRUE),
  
  KGE_min_min = min(HJA_calibration_top_runs$KGE_min, na.rm = TRUE),
  KGE_min_max = max(HJA_calibration_top_runs$KGE_min, na.rm = TRUE),
  
  penalty_min = min(HJA_calibration_top_runs$penalty, na.rm = TRUE),
  penalty_max = max(HJA_calibration_top_runs$penalty, na.rm = TRUE))

print(HJA_calibration_top_score_ranges)

#C.9.5 Spannweite der Parameter innerhalb des Top-Ensembles
HJA_calibration_top_parameter_ranges <- data.frame(
  top_n = nrow(HJA_calibration_top_runs),
  
  kP_global_min = min(HJA_calibration_top_runs$kP_global, na.rm = TRUE),
  kP_global_max = max(HJA_calibration_top_runs$kP_global, na.rm = TRUE),
  
  kP_mult_CENMET_min = min(HJA_calibration_top_runs$kP_mult_CENMET, na.rm = TRUE),
  kP_mult_CENMET_max = max(HJA_calibration_top_runs$kP_mult_CENMET, na.rm = TRUE),
  
  kP_mult_UPLMET_min = min(HJA_calibration_top_runs$kP_mult_UPLMET, na.rm = TRUE),
  kP_mult_UPLMET_max = max(HJA_calibration_top_runs$kP_mult_UPLMET, na.rm = TRUE),
  
  Tcrit_min = min(HJA_calibration_top_runs$Tcrit, na.rm = TRUE),
  Tcrit_max = max(HJA_calibration_top_runs$Tcrit, na.rm = TRUE),
  
  Cdd_min = min(HJA_calibration_top_runs$Cdd, na.rm = TRUE),
  Cdd_max = max(HJA_calibration_top_runs$Cdd, na.rm = TRUE),
  
  kR_min = min(HJA_calibration_top_runs$kR, na.rm = TRUE),
  kR_max = max(HJA_calibration_top_runs$kR, na.rm = TRUE))

print(HJA_calibration_top_parameter_ranges)

#================================================================================================================================================================================================
###SCHRITT C.10: EVENT-FENSTER FÜR ABLATION UND AKKUMULATION###
#================================================================================================================================================================================================
##ANMERKUNG: PROZESSNAHE PLAUSIBILISIERUNG IN ANLEHNUNG AN DIE GRUNDIDEEN VON YILMAZ et al. (2008)##

#C.10.1 Parameter für die Event-Fenster festlegen
HJA_calibration_event_window_days  <- 14
HJA_calibration_event_min_gap_days <- 30

#C.10.2 Datenbasis aus Beobachtung und Best-Run-Simulation vorbereiten
HJA_calibration_event_base_df <- HJA_calibration_best_run_df %>%
  transmute(
    Site,
    date       = as.Date(Date),
    swe_obs_mm = as.numeric(SWE_obs_mm),
    swe_sim_mm = as.numeric(SWE_sim_mm)) %>%
  arrange(Site, date)

#C.10.3 Hilfsfunktion: Fenster mit Mindestabstand auswählen
HJA_calibration_select_window_with_gap <- function(candidate_windows, chosen_windows, min_gap_days) {
  
  if (nrow(candidate_windows) == 0) return(chosen_windows[0, , drop = FALSE])
  if (nrow(chosen_windows) == 0)    return(candidate_windows[1, , drop = FALSE])
  
  for (window_index in seq_len(nrow(candidate_windows))) {
    current_window <- candidate_windows[window_index, ]
    
    if (all(abs(as.numeric(difftime(current_window$win_start,
                                    chosen_windows$win_start,
                                    units = "days"))) >= min_gap_days)) {
      return(current_window)}}
  
  candidate_windows[1, , drop = FALSE]}

#C.10.4 Hilfsfunktion: Event-Fenster je Station nur auf Basis der beobachteten SWE-Änderung bestimmen
HJA_calibration_get_event_windows_one_site_obs_only <- function(df_site,
                                                                site_name,
                                                                window_days = 14,
                                                                min_gap_days = 30) {
  
  rolling_window_table <- df_site %>%
    arrange(date) %>%
    mutate(
      obs_end   = dplyr::lead(swe_obs_mm, window_days - 1),
      sim_end   = dplyr::lead(swe_sim_mm, window_days - 1),
      win_end   = dplyr::lead(date,       window_days - 1),
      delta_obs = obs_end - swe_obs_mm,
      delta_sim = sim_end - swe_sim_mm) %>%
    transmute(
      win_start = date,
      win_end   = win_end,
      obs_start = swe_obs_mm,
      obs_end   = obs_end,
      sim_start = swe_sim_mm,
      sim_end   = sim_end,
      delta_obs = delta_obs,
      delta_sim = delta_sim) %>%
    filter(
      !is.na(win_end),
      is.finite(obs_start), is.finite(obs_end),
      is.finite(sim_start), is.finite(sim_end),
      is.finite(delta_obs), is.finite(delta_sim))
  
  accumulation_candidates <- rolling_window_table %>%
    filter(delta_obs > 0) %>%
    arrange(desc(delta_obs))
  
  ablation_candidates <- rolling_window_table %>%
    filter(delta_obs < 0) %>%
    arrange(delta_obs)
  
  accumulation_window <- HJA_calibration_select_window_with_gap(
    accumulation_candidates,
    rolling_window_table[0, , drop = FALSE],
    min_gap_days)
  
  ablation_window <- HJA_calibration_select_window_with_gap(
    ablation_candidates,
    accumulation_window,
    min_gap_days)
  
  bind_rows(
    accumulation_window %>% mutate(event_type = "Akkumulation"),
    ablation_window %>% mutate(event_type = "Ablation")) %>%
    select(
      event_type, win_start, win_end,
      delta_obs, delta_sim, obs_start, obs_end, sim_start, sim_end)}

#10.5 Event-Fenster für alle Stationen bestimmen
HJA_calibration_event_windows <- HJA_calibration_event_base_df %>%
  group_by(Site) %>%
  group_modify(~ HJA_calibration_get_event_windows_one_site_obs_only(
    df_site      = .x,
    site_name    = .y$Site[[1]],
    window_days  = HJA_calibration_event_window_days,
    min_gap_days = HJA_calibration_event_min_gap_days)) %>%
  ungroup()

print(HJA_calibration_event_windows)

#C.10.6 Plot-Dataframe für die Event-Fenster vorbereiten
HJA_calibration_event_plot_df <- HJA_calibration_event_base_df %>%
  inner_join(
    HJA_calibration_event_windows,
    by = "Site",
    relationship = "many-to-many") %>%
  filter(date >= win_start, date <= win_end) %>%
  mutate(
    Site = factor(Site, levels = c("CENMET", "PRIMET", "UPLMET")),
    event_type = factor(event_type, levels = c("Ablation", "Akkumulation")),
    panel_lab = paste0(
      Site, "\n",
      format(win_start, "%Y-%m-%d"), " - ", format(win_end, "%Y-%m-%d"))) %>%
  select(Site, event_type, panel_lab, date, swe_obs_mm, swe_sim_mm)

#C.10.7 Kombinierten Plot vorbereiten und darstellen
HJA_calibration_event_plot_df_combined <- dplyr::bind_rows(
  HJA_calibration_event_plot_df %>%
    filter(event_type == "Ablation") %>%
    dplyr::mutate(
      panel_order = dplyr::case_when(
        Site == "CENMET" ~ 1,
        Site == "PRIMET" ~ 2,
        Site == "UPLMET" ~ 3)),
  HJA_calibration_event_plot_df %>%
    filter(event_type == "Akkumulation") %>%
    dplyr::mutate(
      panel_order = dplyr::case_when(
        Site == "CENMET" ~ 4,
        Site == "PRIMET" ~ 5,
        Site == "UPLMET" ~ 6)))

HJA_calibration_event_panel_levels <- HJA_calibration_event_plot_df_combined %>%
  dplyr::distinct(panel_lab, panel_order) %>%
  dplyr::arrange(panel_order) %>%
  dplyr::pull(panel_lab)

HJA_calibration_event_plot_df_combined <- HJA_calibration_event_plot_df_combined %>%
  dplyr::mutate(
    panel_lab = factor(panel_lab, levels = HJA_calibration_event_panel_levels))

#C.10.8 Plot: 6 Panels (Ablation/Akkumulation pro Station)
ggplot(HJA_calibration_event_plot_df_combined, aes(x = date, y = swe_obs_mm, color = "Observed")) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_line(aes(y = swe_sim_mm, color = "Simulated"), linewidth = 0.8, na.rm = TRUE) +
  facet_wrap(~ panel_lab, ncol = 3, scales = "free") +
  scale_color_manual(
    values = c(
      "Observed"  = "blue",
      "Simulated" = "red"),
    name = NULL) +
  scale_x_date(
    date_breaks = "5 days",
    date_labels = "%d. %b") +
  labs(
    x = "Datum",
    y = "SWE (mm)") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11, lineheight = 0.9),
    strip.background = element_rect(fill = "grey85", colour = "black"),
    legend.position = "bottom",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.text  = element_text(size = 12))

#================================================================================================================================================================================================
###SCHRITT C.11: VERGLEICH DER BESTEN MODELLVARIANTEN###
#================================================================================================================================================================================================

#C.11.1 Anzahl der zu vergleichenden Top-Varianten festlegen
HJA_calibration_compare_top_n <- 30

#C.11.2 Beste Modellvarianten aus der Ergebnistabelle auswählen
HJA_calibration_top_model_variants <- HJA_calibration_results_df %>%
  slice(seq_len(min(HJA_calibration_compare_top_n, nrow(HJA_calibration_results_df))))

#C.11.3 Kompakte Vergleichstabelle der besten Modellvarianten erstellen
HJA_calibration_top_model_table <- HJA_calibration_top_model_variants %>%
  dplyr::transmute(
    rank = seq_len(n()),
    run_id = run_id,
    main_score = round(main_score, 4),
    KGE_mean   = round(KGE_mean, 4),
    KGE_min    = round(KGE_min, 4),
    penalty    = round(penalty, 4),
    kP_global      = round(kP_global, 4),
    kP_mult_CENMET = round(kP_mult_CENMET, 4),
    kP_mult_UPLMET = round(kP_mult_UPLMET, 4),
    Tcrit = round(Tcrit, 4),
    Cdd   = round(Cdd, 4),
    kR    = round(kR, 4) )

print(HJA_calibration_top_model_table)

#C.11.4 Spannweite innerhalb der besten Modellvarianten zusammenfassen
HJA_calibration_top_model_ranges <- data.frame(
  top_n = nrow(HJA_calibration_top_model_variants),
  
  main_score_min = min(HJA_calibration_top_model_variants$main_score, na.rm = TRUE),
  main_score_max = max(HJA_calibration_top_model_variants$main_score, na.rm = TRUE),
  
  KGE_mean_min = min(HJA_calibration_top_model_variants$KGE_mean, na.rm = TRUE),
  KGE_mean_max = max(HJA_calibration_top_model_variants$KGE_mean, na.rm = TRUE),
  
  KGE_min_min = min(HJA_calibration_top_model_variants$KGE_min, na.rm = TRUE),
  KGE_min_max = max(HJA_calibration_top_model_variants$KGE_min, na.rm = TRUE),
  
  penalty_min = min(HJA_calibration_top_model_variants$penalty, na.rm = TRUE),
  penalty_max = max(HJA_calibration_top_model_variants$penalty, na.rm = TRUE),
  
  kP_global_min = min(HJA_calibration_top_model_variants$kP_global, na.rm = TRUE),
  kP_global_max = max(HJA_calibration_top_model_variants$kP_global, na.rm = TRUE),
  
  kP_mult_CENMET_min = min(HJA_calibration_top_model_variants$kP_mult_CENMET, na.rm = TRUE),
  kP_mult_CENMET_max = max(HJA_calibration_top_model_variants$kP_mult_CENMET, na.rm = TRUE),
  
  kP_mult_UPLMET_min = min(HJA_calibration_top_model_variants$kP_mult_UPLMET, na.rm = TRUE),
  kP_mult_UPLMET_max = max(HJA_calibration_top_model_variants$kP_mult_UPLMET, na.rm = TRUE),
  
  Tcrit_min = min(HJA_calibration_top_model_variants$Tcrit, na.rm = TRUE),
  Tcrit_max = max(HJA_calibration_top_model_variants$Tcrit, na.rm = TRUE),
  
  Cdd_min = min(HJA_calibration_top_model_variants$Cdd, na.rm = TRUE),
  Cdd_max = max(HJA_calibration_top_model_variants$Cdd, na.rm = TRUE),
  
  kR_min = min(HJA_calibration_top_model_variants$kR, na.rm = TRUE),
  kR_max = max(HJA_calibration_top_model_variants$kR, na.rm = TRUE))

print(HJA_calibration_top_model_ranges)  

#================================================================================================================================================================================================
###SCHRITT 12: TEST DES STRAHLUNGSTERMS (kR = 0)###
#================================================================================================================================================================================================

#C.12.1 Parametersatz ohne Strahlungsterm definieren
HJA_calibration_best_params_without_radiation <- HJA_calibration_best_params
HJA_calibration_best_params_without_radiation$kR <- 0

#C.12.2 Best-Run ohne Strahlungsterm simulieren
HJA_calibration_run_without_radiation <- HJA_calibration_run_swe_model(
  df = HJA_calibration_df,
  params = HJA_calibration_best_params_without_radiation,
  T_snow = HJA_calibration_T_snow_fixed,
  T_rain = HJA_calibration_T_rain_fixed)

#C.12.3 Stationsspezifische Gütemaße: Best-Run vs. kR = 0
HJA_calibration_station_comparison_without_radiation <- HJA_calibration_best_run_df %>%
  group_by(Site) %>%
  summarise(
    KGE_best_run  = HJA_calibration_kge(obs = SWE_obs_mm, sim = SWE_sim_mm),
    RMSE_best_run = HJA_calibration_rmse(obs = SWE_obs_mm, sim = SWE_sim_mm),
    MBE_best_run  = HJA_calibration_mbe(obs = SWE_obs_mm, sim = SWE_sim_mm),
    r_best_run    = HJA_calibration_pearson_r(obs = SWE_obs_mm, sim = SWE_sim_mm),
    .groups = "drop") %>%
  left_join(
    HJA_calibration_run_without_radiation %>%
      group_by(Site) %>%
      summarise(
        KGE_kR_0  = HJA_calibration_kge(obs = SWE_obs_mm, sim = SWE_sim_mm),
        RMSE_kR_0 = HJA_calibration_rmse(obs = SWE_obs_mm, sim = SWE_sim_mm),
        MBE_kR_0  = HJA_calibration_mbe(obs = SWE_obs_mm, sim = SWE_sim_mm),
        r_kR_0    = HJA_calibration_pearson_r(obs = SWE_obs_mm, sim = SWE_sim_mm),
        .groups = "drop"),
    by = "Site") %>%
  mutate(
    d_KGE  = KGE_kR_0  - KGE_best_run,
    d_RMSE = RMSE_kR_0 - RMSE_best_run,
    d_MBE  = MBE_kR_0  - MBE_best_run,
    d_r    = r_kR_0    - r_best_run) %>%
  arrange(Site)

print(HJA_calibration_station_comparison_without_radiation)

#C.12.4 Plot-Dataframe für den direkten Vergleich beider Simulationen vorbereiten
HJA_calibration_radiation_plot_df <- HJA_calibration_best_run_df %>%
  select(Site, Date, SWE_sim_mm) %>%
  rename(SWE_best_run_mm = SWE_sim_mm) %>%
  left_join(
    HJA_calibration_run_without_radiation %>%
      select(Site, Date, SWE_sim_mm) %>%
      rename(SWE_kR_0_mm = SWE_sim_mm),
    by = c("Site", "Date")) %>%
  pivot_longer(
    cols = c(SWE_best_run_mm, SWE_kR_0_mm),
    names_to = "series",
    values_to = "SWE_mm") %>%
  mutate(
    series = recode(
      series,
      SWE_best_run_mm = "Best-Run",
      SWE_kR_0_mm     = "Best-Run kR = 0"),
    Site = factor(Site, levels = c("CENMET", "PRIMET", "UPLMET")))

#C.12.5 Plot: Best-Run vs. Best-Run ohne Strahlungsterm
ggplot(HJA_calibration_radiation_plot_df, aes(x = Date, y = SWE_mm, color = series)) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  facet_wrap(~ Site, ncol = 1, scales = "fixed") +
  scale_color_manual(
    values = c(
      "Best-Run"        = "red",
      "Best-Run kR = 0" = "black"),
    name = NULL) +
  labs(
    x = "Datum",
    y = "SWE (mm)") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "grey85", colour = "black"),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.text.x  = element_text(size = 12),
    axis.text.y  = element_text(size = 12),
    legend.text  = element_text(size = 12))
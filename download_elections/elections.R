# setwd("~/Documents/Projects/vox_military_jop")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("tidyr", "dplyr", "purrr")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# muniSpain and infoelectoral (Github)
if(!all("infoelectoral" %in% rownames(installed.packages()))){
  if(!"devtools" %in% rownames(installed.packages())){install.packages("devtools")}
  library(devtools)
  install_github("rOpenSpain/infoelectoral")
}
# Load
lapply(c(pkg, "infoelectoral"), library, character.only = TRUE)
# -------------------------

## VOX vote in 2019

# Abril 2019
# download
e19_04r = mesas(tipo_eleccion = "congreso", anno = 2019, mes = "04")
# transform
e19_04 = e19_04r %>%
  filter(siglas == "VOX") %>%
  filter(municipio != "CERA") %>%
  mutate(muni_code = paste0(codigo_provincia,codigo_municipio)) %>%
  filter(muni_code != "99999") %>%
  rename(distrito = codigo_distrito, seccion = codigo_seccion) %>%
  group_by(muni_code, distrito, seccion) %>%
  summarise(
    censo = sum(censo_ine),
    validos = sum(votos_candidaturas, votos_blancos),
    VOX = sum(votos)) %>%
  mutate(VOX = VOX/validos) %>%
  mutate(CUSEC = paste0(
    sprintf("%05d", as.integer(muni_code)),
    sprintf("%02d", as.integer(distrito)),
    sprintf("%03d", as.integer(seccion))),
  part = validos / censo) %>%
  filter(seccion != 0) %>%
  rename(VOX_a19 = VOX, part_a19 = part, censo_a19 = censo) %>%
  select(CUSEC, VOX_a19, part_a19, censo_a19)
# save
# write.csv(e19_04, "download_elections/output/VOX_2019_04.csv", row.names = FALSE)

## Noviembre 2019
# download
e19_11r = mesas(tipo_eleccion = "congreso", anno = 2019, mes = "11")
# transform
e19_11 = e19_11r %>%
  filter(siglas == "VOX") %>%
  filter(municipio != "CERA") %>%
  mutate(muni_code = paste0(codigo_provincia,codigo_municipio)) %>%
  filter(muni_code != "99999") %>%
  rename(distrito = codigo_distrito, seccion = codigo_seccion) %>%
  group_by(muni_code, distrito, seccion) %>%
  summarise(
    censo = sum(censo_ine),
    validos = sum(votos_candidaturas, votos_blancos),
    VOX = sum(votos)) %>%
  mutate(VOX = VOX/validos) %>%
  mutate(CUSEC = paste0(
    sprintf("%05d", as.integer(muni_code)),
    sprintf("%02d", as.integer(distrito)),
    sprintf("%03d", as.integer(seccion))),
  part = validos / censo) %>%
  filter(seccion != 0) %>%
  rename(VOX_n19 = VOX, part_n19 = part, censo_n19 = censo) %>%
  select(CUSEC, VOX_n19, part_n19, censo_n19)
# save
# write.csv(e19_11, "download_elections/output/VOX_2019_11.csv", row.names = FALSE)

### ---------------------
### TRACKING EXTREME-RIGHT AND PP SINCE THE EARLY 1970s

# Alternative names
ERight_alt = c("SE", "FN", "MFE", "FE-JONS", "FE-I", "M.CAT.E", "FA", "UFM-MFE",
  "CUN", "PAEC", "FE (I)", "FE (A)", "MCE", "FEA", "FEI", "AUN",
  "ES2000", "FE", "FEI-FE 2000", "LF", "PE2000", "LA FALANGE",
  "PLAT. ES.", "PLA-ESP2000", "DN", "FE de las JONS", "ESPAÑA 2000",
  "AES", "AuN", "D.N.", "E-2000", "F.A.", "F.E. de las J.O.N.S.",
  "FRENTE", "CTC", "A.u.N.", "C.T.C.", "FE DE LAS JONS", "F.E.",
  "AN", "M.F.E.", "F.E", "PxC", "FE de las J")
# c("FALANGE ESPANOLA DE LAS JONS", "ASOCIACION POLITICA FUERZA NUEVA",
#   "FALANGE ASTURIANA", "FALANGE ESPANOLA INDEPENDIENTE", "MOVIMIENTO CATOLICO ESPANOL",
#   "MOVIMIENTO FALANGISTA DE ESPANA", "SOLIDARIDAD ESPANOLA",
#   "UNIDAD FALANG.MONTANESA-MOVIM.FALANG.DE ESPANA", "COALICION DE UNIDAD NACIONAL",
#   "PARTIDO ESPANOL CRISTIANO", "FALANGE ESPANOLA DE LAS J.O.N.S.",
#   "FALANGE ESPANOLA AUTENTICA", "ALIANZA POR LA UNIDAD NACIONAL",
#   "PLATAFORMA ESPANA 2000", "LA FALANGE", "FALANGE ESPANOLA INDEPENDIENTE-FALANGE 2000",
#   "DEMOCRACIA NACIONAL", "ESPANA 2000", "FALANGE AUTENTICA", "ALTERNATIVA ESPANOLA",
#   "COMUNION TRADICIONALISTA CARLISTA", "ESPANA-2000", "FALANGE ESPANOLA DE LA JONS",
#   "ALIANZA NACIONAL", "FRENTE ESPANOL", "PLATAFORMA PER CATALUNYA")
PP_alt = c("PP", "PP-FORO", "PP-PAR", "UPN-PP", "PP-EU", "P.P.", "P.P-E.U.",
  "PP-UPM", "AP-PDP-PDL-", "AP-PDP", "AP-PDP-UV", "AP-PDP-PAR", "UPN-AP-PDP",
  "AP-PDP-PL", "AP-PDP-PL-C", "AP-PL-UPN", "PP-C DE G", "NA+", "UPN-PP")

# Election dates
elec_dates = data.frame(
  yr = c(1982, 1986, 1989, 1993, 1996,
  2000, 2004, 2008, 2011, 2015, 2016, 2019, 2019),
  mth = c("10", "06", "10", "06", "03",
  "03", "03", "03", "11", "12", "06", "04", "11"))


# Function to transform
elec_clean = function(rawdf, label){

  df0 = rawdf %>%
    mutate(siglas = ifelse(siglas %in% PP_alt, "PP", siglas)) %>%
    mutate(siglas = ifelse(siglas %in% ERight_alt, "ERight", siglas)) %>%
    filter(siglas %in% c("PP", "ERight") & municipio != "CERA") %>%
    mutate(muni_code = paste0(codigo_provincia,codigo_municipio)) %>%
    filter(muni_code != "99999") %>%
    filter(codigo_seccion != "9999") %>%
    rename(censo = censo_ine, distrito = codigo_distrito, seccion = codigo_seccion) %>%
    mutate(seccion = gsub("[A-Z]", "", seccion))

  df_censo = df0 %>%
    select(muni_code, distrito, seccion, codigo_mesa,
      censo, votos_candidaturas, votos_blancos, votos_nulos) %>%
    unique() %>%
    group_by(muni_code, distrito, seccion) %>%
    summarize(censo = sum(censo),
      votos = sum(votos_candidaturas, votos_blancos, votos_nulos)) %>%
    mutate(part = votos/censo) %>%
    mutate(part = ifelse(part == "Inf", NA, part)) %>%
    select(c(-censo, -votos))

  df = df0 %>%
    group_by(muni_code, distrito, seccion, siglas) %>%
    summarise(
      validos = sum(votos_candidaturas, votos_blancos),
      votos = sum(votos)) %>%
    mutate(share = votos / validos) %>%
    # mutate(part = validos / censo) %>%
    select(c(-votos, -validos)) %>%
    pivot_wider(names_from = siglas, values_from = share) %>%
    left_join(df_censo) %>%
    mutate(
      muni_code = sprintf("%05d", as.integer(muni_code)),
      distrito = sprintf("%02d", as.integer(distrito)),
      seccion = sprintf("%03d", as.integer(seccion)))

  names(df)[names(df) %in% c("PP", "ERight", "part")] = paste(
    names(df)[names(df) %in% c("PP", "ERight", "part")], label, sep = "_")

  return(df)

}

## Download

# Empty list
df_list = vector("list", nrow(elec_dates))

# Fill it up
for(i in 1:length(df_list)){
  l = paste(elec_dates$yr[i], elec_dates$mth[i], sep = "_")
  r = mesas(tipo_eleccion = "congreso", anno = elec_dates$yr[i], mes = elec_dates$mth[i])
  df_list[[i]] = elec_clean(rawdf = r, label = l)
}

# Merge all data frame
df_PP_ER = df_list %>%
  reduce(full_join, by = c("muni_code", "distrito", "seccion")) %>%
  mutate(CUSEC = paste0(
    sprintf("%05d", as.integer(muni_code)),
    sprintf("%02d", as.integer(distrito)),
    sprintf("%03d", as.integer(seccion)))) %>%
  select(c(-muni_code, -distrito, -seccion))
# write.csv(df_PP_ER, "download_elections/output/all_elec_PP_ER.csv", row.names = FALSE)


# ------------------------------------------
### JOIN THE WHOLE THING
data = merge(e19_04, e19_11, all = TRUE)
data = merge(data, df_PP_ER, all.x = TRUE)
### AND SAVE
write.csv(data, "download_elections/output/elec_data.csv", row.names = FALSE)

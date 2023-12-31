# CODEBOOK

### Individual-level dataset (`input_data/barometer.csv`)

- `ESTU`      : Study number (CIS)
- `survey`    : Survey date (character)
- `date`      : Survey date (date class)
- `CCAA`      : Region (CCAA, Autonomous Community)
- `MUN`       : Municipality (only for larger municipalities)
- `partyvote` : Vote intention
- `votevox`   : Vote intention to VOX (binary)
- `votepp`    : Vote intention to PP (binary)
- `mili`      : Military occupation
- `CNO11`     : Occupation code
- `leftright` : Ideological left-right selfplacement
- `female`    : Female
- `age`       : Age
- `estudios`  : Education level
- `sitlab_rec`: Occupation status (character)
- `working`   : Employed (binary)
- `unemployed`: Unemployed (binary)
- `civilstat` : Civil status
- `religion`  : Religious status
- `religious` : Religious (binary)

---

### Local-level dataset (`create_dataset/output/dataset.csv`)

**Note**: `slm_datajoin/output/shp.rds` includes this same dataset in a shapefile at the census section-level

- `CUSEC`                 : Census section ID
- `muni_code`             : Municipality INE code (see [this](https://github.com/franvillamil/muniSpain))
- `distrito`              : District ID
- `VOX_a19`               : Vote share VOX, April 2019 elections
- `part_a19`              : Turnout, April 2019 elections
- `censo_a19`             : Electoral census, April 2019 elections
- `VOX_n19`               : Vote share VOX, November 2019 elections
- `part_n19`              : Turnout, November 2019 elections
- `censo_n19`             : Electoral census, November 2019 elections
- `seccion`               : Electoral section
- `ERight_1982_10`        : Vote share to extreme right parties, 1982 elections
- `PP_1982_10`            : PP share, 1982 elections
- `part_1982_10`          : Turnout, 1982 elections
- `PP_1986_06`            : Vote share to extreme right parties, 1986 elections
- `ERight_1986_06`        : PP share, 1986 elections
- `part_1986_06`          : Turnout, 1986 elections
- `ERight_1989_10`        : Vote share to extreme right parties, 1989 elections
- `PP_1989_10`            : PP share, 1989 elections
- `part_1989_10`          : Turnout, 1989 elections
- `PP_1993_06`            : Vote share to extreme right parties, 1993 elections
- `ERight_1993_06`        : PP share, 1993 elections
- `part_1993_06`          : Turnout, 1993 elections
- `PP_1996_03`            : Vote share to extreme right parties, 1996 elections
- `ERight_1996_03`        : PP share, 1996 elections
- `part_1996_03`          : Turnout, 1996 elections
- `ERight_2000_03`        : Vote share to extreme right parties, 2000 elections
- `PP_2000_03`            : PP share, 2000 elections
- `part_2000_03`          : Turnout, 2000 elections
- `ERight_2004_03`        : Vote share to extreme right parties, 2004 elections
- `PP_2004_03`            : PP share, 2004 elections
- `part_2004_03`          : Turnout, 2004 elections
- `ERight_2008_03`        : Vote share to extreme right parties, 2008 elections
- `PP_2008_03`            : PP share, 2008 elections
- `part_2008_03`          : Turnout, 2008 elections
- `PP_2011_11`            : Vote share to extreme right parties, 2011 elections
- `ERight_2011_11`        : PP share, 2011 elections
- `part_2011_11`          : Turnout, 2011 elections
- `PP_2015_12`            : Vote share to extreme right parties, 2015 elections
- `ERight_2015_12`        : PP share, 2015 elections
- `part_2015_12`          : Turnout, 2015 elections
- `PP_2016_06`            : Vote share to extreme right parties, 2016 elections
- `ERight_2016_06`        : PP share, 2016 elections
- `part_2016_06`          : Turnout, 2016 elections
- `PP_2019_04`            : Vote share to extreme right parties, 2019 elections
- `ERight_2019_04`        : PP share, 2019 elections
- `part_2019_04`          : Turnout, 2019 elections
- `PP_2019_11`            : Vote share to extreme right parties, 2019 elections
- `ERight_2019_11`        : PP share, 2019 elections
- `part_2019_11`          : Turnout, 2019 elections
- `renta_h_2017`          : Mean household income, 2017
- `pop_2017`              : Population, 2017
- `mean_hhold_size_2017`  : Mean household size, 2017
- `pop_over65_2017`       : Population over 65, 2017
- `army`                  : Army facility
- `army_noHQ`             : Army facility (excluding HQ)
- `army_nb`               : Army facility in neighboring section
- `army_nb10`             : Army facility within 10km
- `army_nb5`              : Army facility within 5km
- `army_nb2`              : Army facility within 2km
- `army_noHQ_nb`          : Army facility in neighboring section (excl. HQ)
- `army_noHQ_nb10`        : Army facility within 10km (excl. HQ)
- `army_noHQ_nb5`         : Army facility within 5km (excl. HQ)
- `army_noHQ_nb2`         : Army facility within 2km (excl. HQ)
- `min_dist_army`         : Minimum distance to army facility
- `inv_log_dist`          : Inverse logged distance to army facility
- `prov`                  : Province
- `region_militar_franc`  : Section in capital of military region during Francoist regime
- `region_militar_democ`  : Section in capital of military region during democracy
- `muni_pop_2017`         : Municipality-level population in 2017
- `muni_num_secs`         : Number of sections in municipality
- `muni_pop_per_sec`      : Municipality-level population by section
- `cities50k`             : Section in city over 50,000 inhabitants
- `cuarteles1920`         : Military facility in 1920
- `cuarteles1920_totalcap`: Personnel in military facilities in 1920
- `pop_chg_3011`          : Population change between 1930 and 2011 at the municipality-level
- `izq1936`               : Leftist support in 1936 elections
- `dcha1936`              : Rightist support in 1936 elections
- `pop_2017_l`            : Logged population, 2017
- `muni_pop_2017_l`       : Logged municipality-level population, 2017
- `renta_h_2017_l`        : Logged mean household income, 2017

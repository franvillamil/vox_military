.PHONY: all clean clean_analyses help taskflow
.DELETE_ON_ERROR:

## > all (default) : excludes spatial analyses
all: descriptives survey analyses_lm extra
## > really_all	 : including sp models (computing-intensive)
really_all: all analyses_sp
## Rest of stuff
data: create_dataset/output/dataset.Rout
descriptives: descriptives/desc.Rout
analyses_lm: lm/lm.Rout lm_diff/lm_diff.Rout
analyses_sp: slm/slm_invd.Rout slm/slm_nb.Rout slm_tables/slm_tables.Rout
survey: survey_analyses/s_analyses.Rout
extra: cuarteles1920/output/c1920_CUSEC.csv c1920_models/c1920_models.Rout cses/cses.Rout politbarometer/pb.Rout

## > clean	 : remove all output files
clean:
	rm -rfv */output
	rm -fv */*.Rout

# Self-documentation (^## lines)
help:
	@sed -n 's/^##//p' Makefile

## > taskflow	 : create dependencies graph
taskflow:
	R CMD BATCH taskflow/create_dependency_graph.R
	rm create_dependency_graph.Rout
	dot -Grankdir=LR -Tpdf taskflow/dependency_list.txt -o taskflow/workflow.pdf
	sips -s format jpeg taskflow/workflow.pdf --out taskflow/workflow.jpeg

# ------------------------
# Creating dataset

download_elections/output/elec_data.csv: download_elections/elections.R
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

distance_matrix/output/min_dist.csv: distance_matrix/distance.R spatial_overlay/output/cuarteles_CUSEC.csv download_shp/shp_secciones_2019/SECC_CE_20190101.shp
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

create_dataset/output/dataset.csv: create_dataset/dataset.R download_elections/output/elec_data.csv input_data/secc_censal_renta.csv input_data/secc_censal_indic_demograficos.csv spatial_lags/output/cuarteles_lags.csv distance_matrix/output/min_dist.csv cuarteles1920/output/c1920_CUSEC.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

spatial_lags/output/cuarteles_lags.csv: spatial_lags/spatial.R download_shp/shp_secciones_2019/SECC_CE_20190101.shp spatial_overlay/output/cuarteles_CUSEC.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

spatial_overlay/output/cuarteles_CUSEC.csv: spatial_overlay/overlay.R input_data/cuarteles.csv download_shp/shp_secciones_2019/SECC_CE_20190101.shp
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

cuarteles1920/output/c1920_CUSEC.csv: cuarteles1920/overlay.R input_data/cuarteles1920.csv download_shp/shp_secciones_2019/SECC_CE_20190101.shp
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

# ------------------------
# Analyses

descriptives/desc.Rout: descriptives/desc.R func/raincloud_func.R download_shp/shp_provincias/gadm36_ESP_2.shp create_dataset/output/dataset.csv download_shp/shp_secciones_2019/SECC_CE_20190101.shp input_data/cuarteles.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out
	find ./descriptives/output -name "*map*" -exec pdfcrop {} {} \;

lm/lm.Rout: lm/lm.R create_dataset/output/dataset.csv func/my_stargazer.R
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

lm_diff/lm_diff.Rout: lm_diff/lm_diff.R create_dataset/output/dataset.csv func/my_stargazer.R
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

slm_datajoin/output/shp.rds: slm_datajoin/join.R download_shp/shp_secciones_2019/SECC_CE_20190101.shp create_dataset/output/dataset.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

slm/slm_invd.Rout: slm/slm_invd.R slm_datajoin/output/shp.rds
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

slm/slm_nb.Rout: slm/slm_nb.R slm_datajoin/output/shp.rds
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out
	pdfcrop slm/output/pred_dy_nb.pdf slm/output/pred_dy_nb.pdf

slm_tables/slm_tables.Rout: slm_tables/slm_tables.R func/my_stargazer.R func/lambda_row.R slm/slm_invd.Rout slm/slm_nb.Rout
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

survey_analyses/s_analyses.Rout: survey_analyses/s_analyses.R input_data/barometers_full.csv func/my_stargazer.R func/se.R func/raincloud_func.R
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

c1920_models/c1920_models.Rout: c1920_models/c1920_models.R create_dataset/output/dataset.csv
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

cses/cses.Rout: cses/cses.R
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

politbarometer/pb.Rout: politbarometer/pb.R input_data/ZA2391_v13-0-0.dta.zip
	mkdir -p $(@D)
	Rscript --no-save --verbose $< 2>&1 | tee $<out

mkdir -p slm/output
Rscript --no-save --verbose slm/slm_invd.R 2>&1 | tee slm/slm_invd.Rout
mkdir -p slm/output
Rscript --no-save --verbose slm/slm_nb.R 2>&1 | tee slm/slm_nb.Rout
pdfcrop slm/output/pred_dy_nb.pdf slm/output/pred_dy_nb.pdf
mkdir -p slm_tables/output
Rscript --no-save --verbose slm_tables/slm_tables.R 2>&1 | tee slm_tables/slm_tables.Rout

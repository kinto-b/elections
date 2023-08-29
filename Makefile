.PHONY: all clean

# Leave load out of the all target since the data should not change
all: results plots predictions report

clean:
	rm -rf data outputs

load:
	Rscript load.R

results:
	Rscript fit.R

plots: results
	Rscript plot.R

predictions: results
	Rscript predict.R

report: plots predictions
	Rscript -e "rmarkdown::render('README.qmd')"


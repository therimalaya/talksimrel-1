all:
	Rscript -e "rmarkdown::render('Classification.Rmd', output_format = 'all')"
all:
	Rscript -e "rmarkdown::render('index.Rmd', output_format = 'all')"

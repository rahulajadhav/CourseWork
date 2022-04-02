x <- read.csv2("companies.csv", header = TRUE, sep = ",")
x
barplot(x$job.posting,
        names.arg=x$site,
        ylim = c(0,300),
        main ="JOBS OPENINGS DISTRIBUTION")

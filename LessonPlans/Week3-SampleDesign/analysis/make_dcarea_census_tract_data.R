library(xtable)

setwd("/Users/bader/work/Teaching/Courses/ResearchPracticum/LessonPlans/Week3-SampleDesign/analysis")


data.dir <- "/Users/bader/work/Data/Census/DCMetro/DCAndBorderingCounties/"
dcarea.file <- paste0(data.dir,"R10835953_SL140.txt")

lines <- readLines(dcarea.file)
dcarea.labels <- as.vector(strsplit(lines[1],'\t')[[1]])
dcarea.labels <- gsub("\"","",dcarea.labels)
dcarea.names <- strsplit(lines[2],'\t')[[1]]
tmp <- tempfile()
writeLines(lines[-2:-1],tmp)
dcarea <- read.delim(tmp,header=FALSE,col.names=dcarea.names)
names(dcarea.labels) <- dcarea.names

vars <- matrix(dcarea.labels,ncol=1)
row.names(vars) <- dcarea.names
colnames(vars) <- c("Label")
xvars <- xtable(vars)
align(xvars) <- ("rp{5in}")
print.xtable(xvars,
             tabular.environment="longtable",
             floating=FALSE, size="small",
             file="dcareaCensusVariableNamesTable.tex"
             )
write.csv(dcarea,file="dcarea_census_tract_data.csv")

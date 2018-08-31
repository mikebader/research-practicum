library(xtable)

setwd("/Users/bader/work/Teaching/Courses/ResearchPracticum/LessonPlans/Week3-SampleDesign/analysis")


data.dir <- "/Users/bader/work/Data/"
dc.data.dir <- paste0(data.dir,"Census/DCMetro/DCAndBorderingCounties/")
dcarea.file <- paste0(dc.data.dir,"R10835953_SL140.txt")

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



yrs <- c("70","80","90","00","10")
races <- c("nhwht","nhblk","hisp","asian")
keep.vars <- unlist(lapply(races,paste0,yrs))
pcn <- read.csv("/Users/bader/work/otherwriting/2016-2017/KCRWMaps/data/tabular/pcn02.csv")
#pcn[,keep.vars] <- sapply(keep.vars,function(x){round(pcn[[x]]/50,0)})
#keep.vars <- c("trtid10str",keep.vars)
pcn$Geo_FIPS <- pcn$trtid10str

ltdb <- read.csv("/Users/bader/work/otherwriting/2016-2017/KCRWMaps/data/tabular/ltdb.csv")
ltdb$Geo_FIPS <- sprintf("%011.0f",ltdb$fips10)
ltdb.dc <- ltdb[ltdb$Geo_FIPS%in%dcarea$Geo_FIPS,]
ltdb.dc <- merge(ltdb.dc,pcn)

pop.and.pov <- c("pop.","npov.")
keep.vars <- c(keep.vars,unlist(lapply(pop.and.pov,paste0,c(0:4))))
ltdb.dc[,keep.vars] <- sapply(keep.vars,function(x){round(ltdb.dc[[x]]/50,0)})
write.csv(ltdb.dc,"/Users/bader/work/Teaching/Courses/ResearchPracticum/LessonPlans/Week4-DCArea/analysis/dcarea_ltdb_data.csv")



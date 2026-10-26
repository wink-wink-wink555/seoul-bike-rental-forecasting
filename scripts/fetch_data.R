# Download the fixed official snapshot. Normal runs use the bundled local files.
raw_dir<-path_for("data","raw");dir.create(raw_dir,recursive=TRUE,showWarnings=FALSE)
csv_path<-file.path(raw_dir,"SeoulBikeData.csv")
zip_path<-file.path(raw_dir,"seoul_bike_sharing_demand.zip")
if(!file.exists(csv_path)) {
 if(!file.exists(zip_path))utils::download.file("https://archive.ics.uci.edu/static/public/560/seoul+bike+sharing+demand.zip",zip_path,mode="wb")
 utils::unzip(zip_path,files="SeoulBikeData.csv",exdir=raw_dir)
}
stopifnot(tolower(unname(tools::md5sum(csv_path)))=="bf632307dcb419f67df589faaa0318e1")
message("Verified local official UCI snapshot.")

#####################################
# Creating Kirby21 Database
#####################################
rm(list=ls())
library(R.utils)
library(oro.nifti)

dl_stub = "http://www.nitrc.org/frs/downloadlink.php/"
ids = 1:42
nitrc_ids = 2200+ids
dl_link = paste0(dl_stub, nitrc_ids)
# outdir = path.expand("~/Dropbox/Packages/kirby21")
outdir = path.expand("~/kirby21")
if (!file.exists(outdir)){
	dir.create(outdir)
}
outdirs = file.path(outdir, paste0("KKI2009-", 
	sprintf("%02.0f", ids)))
destfiles = paste0(outdirs, ".tar.bz2")

i = 13
for (i in seq_along(destfiles)){
	outfile = destfiles[i]
	outdir = outdirs[i]
	kki_id = basename(outdir)
	if (!file.exists(outfile)){
		download.file(dl_link[i], destfile = outfile, method="wget")
	}
	keepers = c("FLAIR", "MPRAGE", "T2w")

	fnames = paste0(kki_id, "-", keepers)
	fnames = c(outer(fnames, c(".nii", ".par"), paste0))
	niis = file.path(outdir, grep("nii$", fnames, value=TRUE))
	gzipped_niis = paste0(niis, ".gz")
	if ( !all(file.exists(gzipped_niis)) ) {
		if ( !all(file.exists(file.path(outdir, fnames))) ) {
			untar(outfile, files = fnames,
				exdir = outdir, 
				compressed = "bzip2", verbose=TRUE)
		}
		res = mapply(function(nii, gnii){
			print(nii)
			gzip(nii, destname = gnii, compression = 9)
			}, niis, gzipped_niis)
	}
	if ( all(file.exists(niis)) ) {
		res = mapply(function(nii, gnii){
			print(nii)
			gzip(nii, destname = gnii, compression = 9)
			}, niis, gzipped_niis)
	}	
	print(i)
}


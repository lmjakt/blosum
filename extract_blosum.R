## This is a copy of a file found in the parent directory. It has been
## modified and as such has been copied. It should probably have a different
## name to reflect this.

## This extracts blosum matrices from the blast source code tables
## This makes lots of assumptions about the nature of the source
## code that may not be true in future versions of blast.

## to find the files containing the matrices:
## (assuming that the source code to blast+ is in ~/applications
## list.files("~/applications/blast+/", full.names=TRUE, recursive=TRUE, pattern="blosum")

extract.blosum  <- function(fn){
    lines  <- readLines(fn)
    beg.i  <- grep("^static const TNCBIScore", lines)[1] + 1
    end.i  <- grep("};", lines)[1] - 1
    data  <- paste(lines[beg.i:end.i], collapse="")
    comments   <- c()
    while(grepl("/\\*.+?\\*/", data)){
        comments  <- c(comments, sub(".+?/\\*(.+?)\\*/.+", "\\1", data))
        data  <- sub("(.+?)/\\*(.+?)\\*/(.+)", "\\1 \\3", data)
    }
    ## the first comments line is a column header we can get rid of that
    comments  <- comments[-1]
    ## the rest of the numbers are scores filled by row. We can probably do:
    scores  <- as.integer( strsplit( data, "," )[[1]] )
    score.m  <- matrix(scores, nrow=length(comments), ncol=length(comments), byrow=TRUE )
    rownames(score.m)  <- comments
    colnames(score.m)  <- comments
    score.m
}

blosum.f <- list.files("~/applications/blast+", full.names=TRUE, recursive=TRUE, pattern="blosum")
names(blosum.f) <- sub(".*?/sm_(blosum[0-9]+)\\.c", "\\1", blosum.f)

blosum <- lapply(blosum.f, extract.blosum)

## write to file:
for(nm in names(blosum))
    write.table(blosum[[nm]], file=paste0(nm, ".tsv"), sep="\t", quote=FALSE)


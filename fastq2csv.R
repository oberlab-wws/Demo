split_utf8_to_int <- function(x){
    purrr::map(x,function(x){utf8ToInt(x)-33})
}
cbf <- function(x,pos){
    cname <- c("id","nucleotide","p","score")
    lx <- length(x)
    pseq <- seq(from=(pos-1),length.out=lx)
    dplyr::mutate(tibble::as_tibble(split(x,cname[(pseq %% 4) + 1 ])),seq_id=seq(from = as.integer((pos-1) /4),length.out=dplyr::n())+1) %>% dplyr::select(-p) %>% tidyr::separate(col=id,into=c("read_grp","sample_id"),sep=" ") %>%
        dplyr::mutate(nucleotide = stringr::str_split(nucleotide,pattern=""),
                      score=split_utf8_to_int(score),
                      seq_pos=map(nucleotide,seq_along)
                      ) %>% tidyr::unnest(cols=c(nucleotide,score,seq_pos))
}

read_tidy_fastq <- function(file,max_reads=100){
    n_lines <- max_reads*4
    fastqc <- readr::read_lines(file,
                                n_max=n_lines)
    return(cbf(fastqc,1))
}

##     readr::DataFrameCallback$new(cbf))
## fastqc <- mutate(fastqc,is_gc=nucleotide %in% c("G","C")) %>% group_by(seq_id) %>% mutate(rel_score=score-score[1]) %>% ungroup()

## read_qc <- group_by(fastqc,seq_id) %>% summarise(mean_score=log(mean(exp(score))),
##                                                  min_score=min(score),
##                                                  max_score=max(score),
##                                                  gcp=sum(nucleotide %in% c("G","C"))/length(nucleotide),
##                                                  L=length(nucleotide)
##                                                  )
## qc_df <- filter(fastqc,nucleotide!="N") %>% group_by(seq_pos,is_gc) %>%
##     summarise(mean_score = log(mean(exp(score))),
##               sd_score=log(sd(exp(score))),
##               mean_rel_score=mean(rel_score),
##               ct=n())
## filter(qc_df) %>% ggplot(aes(x=seq_pos,y=mean_rel_score,col=is_gc))+geom_point(position="dodge")

## fastqcm <- as.data.frame(matrix(fastqc,
##     nseq / 4,
##     4,
##     byrow = TRUE,
##     dimnames = list(NULL, c("id", "seq", "p", "score"))
##     )[, -3],
##     stringsAsFactors = FALSE
##     )

## scorevec <- lapply(fastqcm$score,utf8ToInt)
## letvec <- lapply(fastqcm$seq,

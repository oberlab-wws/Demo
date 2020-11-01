# dplyr lives in the tidyverse
library(tidyverse)
library(tidylog)

variants <- read_csv("../demo/data/combined_tidy_vcf.csv")

select(variants, sample_id, REF, ALT, DP)
select(variants, -CHROM)

unique(variants$CHROM)

variants_drop_chrom <- select(variants, -CHROM)

# select() selects columns and filter() filters rows

filter(variants, sample_id == "SRR2584863")
variants[variants$sample_id == "SRR2584863", ]

filter(variants, sample_id == "SRR2584863" & INDEL == TRUE)
filter(variants, QUAL > 100 | DP > 10)

# Position between 1e6 and 2e6
# and (DP > 10
# or QUAl > 50)

filter(variants, (POS >= 1e6 & POS <= 2e6) &
         (DP > 10 | QUAL > 50) )

filter(variants, REF %in% c("T", "C", "A"))

# is.na() checks if data is missing
filter(variants, !is.na(IDV))

variants_new <- select(variants, -c(REF, ALT))
filter(variants_new, !is.na(IDV))

# The pipe operator looks like this %>% 

variants %>% 
  select(-c(REF,ALT)) %>% 
  filter(!is.na(IDV)) %>% 
  head()

# mutate() creates new columns based on old ones

variants %>% 
  mutate(qual100 = QUAL > 100) %>%
  select(sample_id, qual100)

variants %>% mutate(PROB = 1 - 10^-(QUAL / 10)) %>%
  select(sample_id, QUAL, PROB) %>% arrange(desc(QUAL))

# nchar() 
nchar("zach")

# create a new column called indel_size that has the size
# (positive or negative) of indels

variants %>% 
  mutate(indel_size = nchar(ALT) - nchar(REF)) %>%
  select(sample_id, INDEL, REF, ALT, indel_size) %>% 
  filter(indel_size < 0)

variants <- variants %>% 
  mutate(indel_size = nchar(ALT) - nchar(REF))

variants <- variants %>%
  mutate(mutation_type = case_when(
    indel_size > 0 ~ "insertion",
    indel_size < 0 ~ "deletion",
    indel_size == 0 ~ "point"
  ))

variants %>% filter(is.na(mutation_type))

variants %>% group_by(mutation_type) %>%
  summarize(
    mean_size = mean(indel_size),
    median_size = median(indel_size)
  )
  
variants %>% group_by(mutation_type) %>%
  summarize(
    max_size = max(abs(indel_size))
  )

variants %>% group_by(mutation_type) %>%
  summarize(
    counts = n()
  )

variants %>%
  count(mutation_type)

variants %>%
  count(sample_id)

variants %>% group_by(sample_id, mutation_type) %>%
  summarize(
    count = n()
  )

variants_wide <- variants %>%
  count(sample_id, mutation_type) %>% 
  spread(mutation_type, n)
variants_wide

variants_wide %>% 
  gather(mutation_type, n)


### PLOTTING IN GGPLOT ###

# ggplot2 lives in the tidyverse

# ggplot(data = <YOUR DATA>, 
#      mapping = aes()) + <GEOM>() + ...

ggplot(data = variants, mapping = aes(x = QUAL)) +
  geom_density()

pl <- ggplot(data = variants, mapping = aes(x = POS,
                                      y = QUAL)) + 
  geom_point()




ggplot(data = variants, mapping = aes(x = POS,
                                      y = QUAL,
                                      color = DP)) +
  geom_point(alpha = 0.5)


pl + geom_point(aes(x = POS, y = DP ), 
                color = "red") + labs(y = "Quality (black) / Depth (red)")

# Plot position vs read depth (DP) and 
# adjust transparency

ggplot(data = variants, mapping = aes(x = POS,
                                      y = MQ), alpha = 0.3) +
  geom_point(aes(color = sample_id)) +
  facet_grid(.~sample_id) + 
  scale_y_log10()


ggplot(data = variants, mapping = aes(x = QUAL)) +
  geom_density(aes(fill = sample_id), alpha = 0.2)

ggplot(data = variants, mapping = aes(x = QUAL)) +
  geom_density(color = "red") + 
  geom_histogram(aes(y = ..density..))

variants %>% filter(mutation_type != "point") %>% 
  mutate(abs_size = abs(indel_size)) %>%
  ggplot(mapping = aes(y = log(abs_size), 
                      x = mutation_type,
                      fill = mutation_type)) +
  geom_violin() + 
  scale_y_log10() + 
  geom_jitter(alpha = 0.3)


### Altmetrics

altmetrics <- read_tsv("../demo/data/altmetrics/counts_raw.txt", guess_max = 10000)

spaltmetrics %>% ggplot(mapping = aes(x = authorsCount,
                                    y = pdfDownloadsCount))+
  geom_point() +
  scale_x_log10() + 
  scale_y_log10()

altmetrics <- read.table("../demo/data/altmetrics/counts_norm.txt", header = T)

altmetrics %>% group_by(journal, year) %>%
  summarise(mean = mean(authorsCount, na.rm = T)) %>% 
  ggplot(mapping = aes(x = year,
                       y = mean,
                       color = journal)) + 
  geom_line() + 
  #facet_wrap(~journal) + 
  scale_y_log10()

altmetrics %>% ggplot(mapping = aes(x = authorsCount,
                                    y = wosCountThru2011)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10() + 
  geom_smooth(method = "lm")

altmetrics %>% filter(journal == "pgen") %>% 
  mutate(novel = str_detect(tolower(title), "landscape")) %>%
  ggplot(mapping = aes(x = wosCountThru2011, 
                       fill = novel)) +
  geom_density(alpha = 0.2) + 
  scale_x_log10()

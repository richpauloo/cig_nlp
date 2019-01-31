library(dplyr)
library(RefManageR) # for formatting bibtex
library(xlsx)

fp <- "C:/Users/rpauloo/Desktop/2019 CItation/"

b1 <- ReadBib(paste0(fp, "2015 bibtex_export v2.bib")) %>% as.data.frame()
b2 <- ReadBib(paste0(fp, "2016 bibtex_export v2.bib")) %>% as.data.frame()
b3 <- ReadBib(paste0(fp, "2017 bibtex_export v2.bib")) %>% as.data.frame()
b4 <- ReadBib(paste0(fp, "2018 bibtex_export v2.bib")) %>% as.data.frame()

b <- bind_rows(b1, b2, b3, b4)

# count journals to determine popular and not so popular ones
b_n <- count(b, journal)

b <- left_join(b, b_n, by = "journal")

# arrange and reorganize columns for easy viewing
b <- b %>% arrange(desc(n)) %>% select(bibtype:title, year:address, journal, n)
  
# write to data frame for manual cleaning
write.xlsx(b, "bib_df.xlsx", row.names = FALSE)


# papers published per year
b %>% mutate(year = as.numeric(year)) %>% 
  filter(n > 10, year >= 1990) %>% 
  count(year) %>% 
  ggplot(aes(year, nn)) +
  geom_line() +
  labs(x = "Year", y = "Count") +
  theme_minimal()

# papers per year per journal
b %>% mutate(year = as.numeric(year)) %>% 
  filter(n > 10, year >= 1990) %>% 
  count(journal, year) %>% 
  ggplot(aes(year, nn, fill = journal)) +
  geom_col() + 
  scale_fill_viridis_d() +
  labs(x = "Year", y = "Count") +
  theme_minimal()


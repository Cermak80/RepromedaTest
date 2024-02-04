# Zpracování praktické části testu pomocí software R
# Bc. Jakub Čermák 

# Vyčištění prostředí
rm(list = ls())


#Načtení potřebných knihoven
library(tidyverse)
library(readr)
library(fixest)
library(modelsummary)
library(ggplot2)
library(officer)
library(flextable)
# Načtení dat

transfery <- read_csv("transfery.csv", col_types = cols(vek_mother = col_double(), 
                                                        vek_embryo = col_double(), clinical_gravidity = col_factor(levels = c("1", 
                                                                                                                              "0"))))

# Úkol A
#Tvorba tabulky A pomocí funkcí z balíku dplyr
TabulkaA <- transfery %>% mutate(Vek_Matka = case_when(vek_mother <= 29 ~ "do 29",
                                                       vek_mother >= 30 & vek_mother < 35 ~ "30-34",
                                                       vek_mother >= 35 & vek_mother < 40 ~ "35-39",
                                                       vek_mother >= 40 ~ "40 a výše",)) %>% 
  group_by(Vek_Matka) %>%   summarise(Uspesnost = (sum(clinical_gravidity == 1,na.rm = TRUE) * 100) / n())
# Následující část slouží pro přidání informace o celkové úspěšnosti
x <- sum(transfery$clinical_gravidity == 1,na.rm = TRUE) * 100
y <- as.numeric(sum(!is.na(transfery$clinical_gravidity)))
z <- x/y
celkova_uspesnost <- data.frame(Vek_Matka = "Celkově",Uspesnost = z)
TabulkaA <- TabulkaA %>% rbind(celkova_uspesnost) %>% drop_na()

# Úkol B
# Pro určení statistické významnosti využiji lineární regresi
transferym1 <- transfery %>% mutate(clinical_gravidity = as.numeric(as.character(clinical_gravidity)))
m1 <- feols(clinical_gravidity ~ vek_mother, data = transferym1, vcov ="HC1") 
modelsummary(m1,
             estimate  = c("{estimate} ({std.error}){stars}"),
             statistic = NULL,
             fmt = 3,
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_omit = "R2 Adj|IC|Lik")

# Z výsledků můžeme vidět, že na hladině významnosti 5 % je věk matky statisticky významný se záporným koeficientem,
# Tzn. podle očekávání, stáří ženy má negativní vliv (je potřeba tyto výsledky brát s rezervou, pro přesvědčivější výsledky by byla potřeba větší analýza, kde bychom mohli kontrolovat pro některé další faktory, které mohou mít na úspěšnost vliv)

# Úkol C 
# Nejprve vyfiltrujeme data
 
transferyC <- transfery %>%
  mutate(f_donor = ifelse(is.na(f_donor), 0, f_donor)) %>% filter(f_donor != 1)

TabulkaC <- transferyC %>% mutate(Vek_embryo = case_when(vek_embryo <= 29 ~ "do 29",
                                                         vek_embryo >= 30 & vek_embryo < 35 ~ "30-34",
                                                         vek_embryo >= 35 & vek_embryo < 40 ~ "35-39",
                                                         vek_embryo >= 40 ~ "40 a výše",)) %>%
group_by(Vek_embryo) %>%   summarise(Uspesnost = (sum(clinical_gravidity == 1,na.rm = TRUE) * 100) / n()) %>% drop_na()
x <- sum(transferyC$clinical_gravidity == 1,na.rm = TRUE) * 100
y <- as.numeric(sum(!is.na(transferyC$clinical_gravidity)))
z <- x/y
celkova_uspesnost <- data.frame(Vek_embryo = "Celkově",Uspesnost = z)
TabulkaC <- TabulkaC %>% rbind(celkova_uspesnost)

# Stejně jako v předchozím případě využijeme lineární regresi
transferym2 <- transferyC %>% mutate(clinical_gravidity = as.numeric(as.character(clinical_gravidity)))
m2 <- feols(clinical_gravidity ~ vek_embryo, data = transferym2, vcov ="HC1") 
modelsummary(m2,
             estimate  = c("{estimate} ({std.error}){stars}"),
             statistic = NULL,
             fmt = 3,
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_omit = "R2 Adj|IC|Lik")
# Stejně jako u věku matky, na hladině 5 % zamítáme hypotézu a statistickém nevýznamu a můžeme pozorovat záporný vliv délky stáří embrya na úspěch


# Úkol D
TabulkaD <- transfery %>%
  mutate(genetic_method = ifelse(is.na(genetic_method), "bez genetické metody (prázdná hodnota)", genetic_method)) %>% 
  mutate(genetic_method = case_when(
    genetic_method == "PGT-A" ~ "PGT-A",
    genetic_method == "PGT-SR" ~ "PGT-SR",
    genetic_method == "Karyomapping" ~ "Karyomapping",
    genetic_method == "OneGene" ~ "OneGene",
    genetic_method == "bez genetické metody (prázdná hodnota)" ~ "bez genetické metody (prázdná hodnota)",
    TRUE ~ "ostatní")) %>% group_by(genetic_method) %>% summarise(Pocet_pouziti=n())

# Úkol E
# Stejně jako v předchozích případech jsem se rozhodl využít lineární regresi
m3 <- feols(clinical_gravidity ~ sex, data = transferym1, vcov ="HC1") 
# Získání klasických p-hodnot
modelsummary(m3,
             estimate  = c("{estimate} ({std.error}){stars}"),
             statistic = NULL,
             fmt = 3,
             stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
             gof_omit = "R2 Adj|IC|Lik",
             )

# Jak můžeme vidět, tak ani jeden z koeficientů pro muže a ženy není na hladině 5 % statisticky významný

# Úkol E
#Graf k úkolu A
TabulkaA %>% mutate(Vek_Matka = factor(Vek_Matka, levels = c("do 29", "30-34", "35-39", "40 a výše", "Celkově")))%>%  ggplot( aes(x = Vek_Matka, y = Uspesnost)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Úspěšnost podle věku matky",
       x = "Věk matky",
       y = "Úspěšnost (%)") +
  theme_minimal()

# Uložení do souboru PNG
ggsave("GrafUkolA.png",plot = GrafA, width = 8, height = 6, units = "in")

#Graf k úkolu C
TabulkaC %>% mutate(Vek_embryo = factor(Vek_embryo, levels = c("do 29", "30-34", "35-39", "40 a výše", "Celkově")))%>%  ggplot( aes(x = Vek_embryo, y = Uspesnost)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Úspěšnost podle věku embrya",
       x = "Věk embrya",
       y = "Úspěšnost (%)") +
  theme_minimal()

# Uložení do souboru PNG
ggsave("GrafUkolC.png", width = 8, height = 6, units = "in")


# Graf k úkolu D
TabulkaD %>% mutate(genetic_method = factor(genetic_method , levels = c("Karyomapping", "OneGene", "PGT-A", "PGT-SR", "bez genetické metody (prázdná hodnota)","ostatní")))%>%  ggplot( aes(x = genetic_method, y = Pocet_pouziti)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Počet použití jednotlivých metod",
       x = "Název metody",
       y = "Počet použití") +
  theme_minimal()

# Uložení do souboru PNG
ggsave("GrafUkolD.png", width = 8, height = 6, units = "in")



# Úkol číslo 2 
# Vytvoříme funkci, do které budeme moci zadat naše parametry a tato funkce vytvoří námi zadaný dokument
# Rozhodně by se dalo pohrát se vzhledem tabulky, pokud by šlo o oficiální dokument
Create_word <- function(argument1,argument2,argument3){ 
  if(!(is.character(argument1) && is.character(argument2) && is.character(argument3)))
    return("Bad inputs!")
  else
  {
    word_doc <- read_docx()
    
    styl_nadpisu <- fp_text(bold=T,
                            font.size = 24,
                            )
    nadpis <- ftext("Výsledný protokol genetického vyšetření",styl_nadpisu) %>% fpar()
    Tabulka <- data.frame(
      Údaje = c("Jméno a příjmení:","Rodné číslo:","Datum odběru:"),
      Vyplnit = c(argument1,argument2,argument3)
      
    )
    
    set_flextable_defaults(
      font.size = 12, font.family = "Open Sans",
      font.color = "#333333",
      table.layout = "fixed",
      border.color = "gray",
      padding.top = 3, padding.bottom = 3,
      padding.left = 4, padding.right = 4,)
    
    flex_table <- flextable(Tabulka)
    set_table_properties(flex_table, layout = "autofit",width = 1)
    
    
    
    
    
    
    word_doc <- word_doc %>% body_add(nadpis,style="centered") %>% body_add_par("") %>%
      body_add_flextable(flex_table)
    
    
    print(word_doc,"Vysledny_protokol.docx")
  }
}

Create_word("František Suchý","223424/6756","22.2.2024")



  

##################################################################
#Github Token :: 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
##################################################################

library(glue)
library(rvest)
library(polite)

## Lista de UFs necessárias para adicionarmos à URL do site da OLX
ufs <- c("ac", "al", "ap", "am", "ba", "ce", "df", "es", 
         "go", "ma", "mt", "ms", "mg", "pa", "pb", "pr", 
         "pe", "pi", "rj", "rn", "rs","ro", "rr", "sc", 
         "sp", "se", "to")

olx <- data.frame()

for (k in 1:length(ufs)) {
  
  for (i in 1:100) {
    
    if(i == 1){
      html <- read_html(glue("https://{ufs[k]}.olx.com.br/imoveis"))
    } else{
      html <- read_html(glue("https://{ufs[k]}.olx.com.br/imoveis?o={i}"))
    }
    
    titulo <- html_nodes(html, xpath = paste0('/html/body/div[2]/div[2]/div[2]/div/div[2]/div[2]/div/div[5]/div/div/div/div[11]/div/div/div/ul/li/div/a/div/div[2]/div[1]/div[1]/div[1]/div/h2')) %>% html_text2()
    info_bas <- html_nodes(html, xpath = paste0('/html/body/div[2]/div[2]/div[2]/div/div[2]/div[2]/div/div[5]/div/div/div/div[11]/div/div/div/ul/li/div/a/div/div[2]/div[1]/div[2]/div/div/div[1]/div/div')) %>% html_text2()
    preco <- html_nodes(html, xpath = paste0('/html/body/div[2]/div[2]/div[2]/div/div[2]/div[2]/div/div[5]/div/div/div/div[11]/div/div/div/ul/li/div/a/div/div[2]/div[1]/div[1]/div[2]/div/div/div/span')) %>% html_text2()
    local <- html_nodes(html, xpath = paste0('/html/body/div[2]/div[2]/div[2]/div/div[2]/div[2]/div/div[5]/div/div/div/div[11]/div/div/div/ul/li/div/a/div/div[2]/div[2]/div[3]/div[1]/div/div/span')) %>% html_text2()
    img <- html_nodes(html, xpath = paste0('/html/body/div[2]/div[2]/div[2]/div/div[2]/div[2]/div/div[5]/div/div/div/div[11]/div/div/div/ul/li/div/a/div/div[1]/div[1]/div/img')) %>% html_attr("src")
    url <- html_nodes(html, xpath = paste0('/html/body/div[2]/div[2]/div[2]/div/div[2]/div[2]/div/div[5]/div/div/div/div[11]/div/div/div/ul/li/div/a')) %>% html_attr('href')
    uf <- ufs[k]
    
    if((isFALSE((length(titulo) == length(preco)) == ((length(info_bas) == length(local)) == (length(img) == length(url)))) == TRUE) | (length(titulo) == 0)){
      
      print(glue("URL não existente da página {i} da uf {ufs[k]}. Pulando para a próxima página: {i+1}"))
      next
      
    } else{
      
      olx_new <- data.frame(titulo = titulo, infos = info_bas, preco = preco, local = local, img = img, url = url, uf = uf)
      olx <- rbind(olx_new, olx)
      
      print(i)
    }
  }
  
  print(glue("{str_to_upper(ufs[k])} concluído!"))
  
}

olx %>% 
  mutate("modelo" = str_extract(string = olx2$infos, pattern = "À venda|Para alugar"),
                "quartos" = str_extract(string = olx2$infos, pattern = "\\d{1,2} quarto.?|5 ou mais quartos") %>% str_extract("\\d{1,2}") %>% as.numeric(),
                "metros2" = str_extract(string = olx2$infos, pattern = "\\d{1,5}m²") %>% str_extract("\\d{1,5}") %>% as.numeric(),
                "vagas" = str_extract(string = olx2$infos, pattern = "\\d{1,2} vagas") %>% str_extract("\\d{1,2}") %>% as.numeric(),
                "preco" = str_replace_all(string = olx2$preco, "\\.", "") %>% str_remove("R\\$ ") %>% as.numeric(),
                "condominio" = str_extract(string = olx2$infos, pattern = "Condomínio\\: R\\$ \\d{1,5}\\.?\\d{1,}|Condomínio\\: R\\$ \\d{1,5}") %>% str_replace_all("\\.", "") %>% str_extract("\\d{1,}") %>% as.numeric()) %>%
  select(!c("infos")) -> olx_final

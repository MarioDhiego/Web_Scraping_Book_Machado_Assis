# ==============================================================================
# PROJETO: WEB SCRAPING (WININET) E TEXT MINING - MACHADO DE ASSIS (WIKISOURCE)
# Baseado na arquitetura de Sillas Gonzaga (pacote literaturaBR)
# ==============================================================================

library(tidyverse)
library(rvest)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(lexiconPT)
library(scales)

theme_set(theme_minimal(base_size = 11))

# ==============================================================================
# ETAPA 0: CONFIGURAÇÃO DE REDE E LEITURA HTML NATIVA DO WINDOWS (WININET)
# ==============================================================================
proxy_sistema <- tryCatch(curl::ie_get_proxy_for_url("https://pt.wikisource.org"),
                          error = function(e) NA)
if (!is.na(proxy_sistema) && proxy_sistema != "") {
  Sys.setenv(https_proxy = proxy_sistema, http_proxy = proxy_sistema)
}

ler_html_wininet <- function(url) {
  arquivo_temp <- tempfile(fileext = ".html")
  
  status <- tryCatch(
    suppressWarnings(
      download.file(url, destfile = arquivo_temp, method = "wininet", quiet = TRUE)
    ),
    error = function(e) return(1)
  )
  
  if (status != 0 || !file.exists(arquivo_temp)) return(NULL)
  
  pagina <- tryCatch(read_html(arquivo_temp, encoding = "UTF-8"), error = function(e) NULL)
  unlink(arquivo_temp)
  return(pagina)
}

# ==============================================================================
# ETAPA 1: FUNÇÕES DE WEB SCRAPING DO WIKISOURCE
# ==============================================================================
extrair_texto_pagina <- function(url_pagina) {
  Sys.sleep(0.25) # Pausa ética
  
  pagina <- ler_html_wininet(url_pagina)
  if (is.null(pagina)) {
    return(tibble(url = url_pagina, paragraph_number = NA_integer_, text = NA_character_))
  }
  
  paragrafos <- pagina %>%
    html_elements("#mw-content-text .mw-parser-output p") %>%
    html_text2() %>%
    str_remove_all("\\[\\d+\\]") %>%
    str_squish()
  
  paragrafos <- paragrafos[paragrafos != "" & nchar(paragrafos) > 1]
  
  if (length(paragrafos) == 0) {
    return(tibble(url = url_pagina, paragraph_number = NA_integer_, text = NA_character_))
  }
  
  tibble(
    url = url_pagina,
    paragraph_number = seq_along(paragrafos),
    text = paragrafos
  )
}

extrair_livro_wikisource <- function(url_livro) {
  message("Iniciando raspagem: ", url_livro)
  html_principal <- ler_html_wininet(url_livro)
  
  if (is.null(html_principal)) {
    warning("Não foi possível acessar: ", url_livro)
    return(NULL)
  }
  
  titulo_livro <- html_principal %>%
    html_element("#firstHeading") %>%
    html_text2()
  
  links_elementos <- html_principal %>%
    html_elements("#mw-content-text a")
  
  hrefs <- html_attr(links_elementos, "href")
  nomes_cap <- html_text2(links_elementos)
  
  hrefs_completos <- ifelse(
    !is.na(hrefs) & str_starts(hrefs, "/wiki/"),
    paste0("https://pt.wikisource.org", hrefs),
    hrefs
  )
  
  # Decodifica URLs para comparar corretamente títulos com acentos (ex: Memórias Póstumas)
  prefixo_decodificado <- URLdecode(paste0(url_livro, "/"))
  hrefs_decodificados <- URLdecode(hrefs_completos)
  
  filtro <- !is.na(hrefs_decodificados) &
    str_starts(hrefs_decodificados, fixed(prefixo_decodificado)) &
    !str_detect(hrefs_decodificados, "#|\\?|Índice|Capa|Ficha")
  
  if (sum(filtro) > 0) {
    df_urls <- tibble(
      book_name = titulo_livro,
      chapter_name = nomes_cap[filtro],
      url = hrefs_completos[filtro]
    ) %>%
      filter(chapter_name != "", !str_detect(chapter_name, "^(Anterior|Próximo|Índice)$")) %>%
      distinct(url, .keep_all = TRUE)
    
    message(" -> Encontrados ", nrow(df_urls), " capítulos. Baixando textos...")
    
    df_textos <- map_dfr(df_urls$url, extrair_texto_pagina)
    df_final <- left_join(df_urls, df_textos, by = "url")
    
  } else {
    message(" -> Obra em página única. Extraindo texto integral...")
    df_textos <- extrair_texto_pagina(url_livro)
    df_final <- df_textos %>%
      mutate(book_name = titulo_livro, chapter_name = "Texto Único") %>%
      select(book_name, chapter_name, url, paragraph_number, text)
  }
  
  return(df_final)
}

# ==============================================================================
# ETAPA 2: EXECUÇÃO DA RASPAGEM (OBRAS COMPLETAS DE MACHADO DE ASSIS)
# ==============================================================================
# Novo arquivo de cache para baixar as obras completas corrigidas
arquivo_cache <- "machado_wikisource_completo.rds"

urls_machado <- c(
  "https://pt.wikisource.org/wiki/Dom_Casmurro",
  "https://pt.wikisource.org/wiki/Mem%C3%B3rias_P%C3%B3stumas_de_Br%C3%A1s_Cubas",
  "https://pt.wikisource.org/wiki/Quincas_Borba",
  "https://pt.wikisource.org/wiki/O_Alienista",
  "https://pt.wikisource.org/wiki/Helena",
  "https://pt.wikisource.org/wiki/Memorial_de_Aires",
  "https://pt.wikisource.org/wiki/Esa%C3%BA_e_Jac%C3%B3",
  "https://pt.wikisource.org/wiki/Pap%C3%A9is_Avulsos"
)

if (file.exists(arquivo_cache)) {
  message("Carregando dados locais de '", arquivo_cache, "'...")
  df_livros_bruto <- readRDS(arquivo_cache)
} else {
  df_livros_bruto <- map_dfr(urls_machado, extrair_livro_wikisource)
  saveRDS(df_livros_bruto, arquivo_cache)
  message("Raspagem concluída e salva em '", arquivo_cache, "'!")
}

# ==============================================================================
# ETAPA 3: LIMPEZA, AJUSTES E MANIPULAÇÃO DOS DADOS
# ==============================================================================
df_livros <- df_livros_bruto %>%
  filter(!is.na(text)) %>%
  filter(!str_detect(text, "^(Índice|Anterior|Próximo|Wikisource|Obra editada)")) %>%
  mutate(
    author = "Machado de Assis",
    book_short = case_when(
      str_detect(book_name, "Dom Casmurro") ~ "Dom Casmurro",
      str_detect(book_name, "Brás Cubas") ~ "Memórias Póstumas",
      str_detect(book_name, "Quincas Borba") ~ "Quincas Borba",
      str_detect(book_name, "Alienista") ~ "O Alienista",
      str_detect(book_name, "Helena") ~ "Helena",
      str_detect(book_name, "Memorial de Aires") ~ "Memorial de Aires",
      str_detect(book_name, "Esaú") ~ "Esaú e Jacó",
      str_detect(book_name, "Papéis Avulsos") ~ "Papéis Avulsos",
      TRUE ~ book_name
    ),
    # Correção da case-sensitivity em "A mão e a luva" caso você volte a usá-la
    fase = ifelse(
      str_to_lower(book_short) %in% c("helena", "a mão e a luva", "iaiá garcia"),
      "Fase Romântica",
      "Fase Realista"
    )
  )

# Tabela Resumo Estrutural
tabela_resumo <- df_livros %>%
  group_by(Livro = book_short, Fase = fase) %>%
  summarise(
    Capitulos = n_distinct(chapter_name),
    Paragrafos = n(),
    Media_Paragrafos_Cap = round(Paragrafos / Capitulos, 1),
    .groups = "drop"
  ) %>%
  arrange(Fase, desc(Paragrafos))

print("=== RESUMO ESTRUTURAL DOS LIVROS ===")
print(tabela_resumo)

# ==============================================================================
# ETAPA 4: CONSTRUÇÃO DO CORPUS E MATRIZ DOCUMENTO-TERMO (QUANTEDA)
# ==============================================================================
df_corpus_agregado <- df_livros %>%
  group_by(book_short) %>%
  summarise(text = paste(text, collapse = " "), .groups = "drop")

meu_corpus <- corpus(df_corpus_agregado$text, 
                     docnames = df_corpus_agregado$book_short)

sw_extra <- c("é", "era", "disse", "ia", "ser", "ter", "havia", "ainda", "assim", "tudo", "nada", "outro", "outra")

tokens_limpos <- tokens(
  meu_corpus,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
) %>%
  tokens_tolower() %>%
  tokens_remove(c(stopwords("portuguese"), sw_extra))

matriz_dfm <- dfm(tokens_limpos)

# ==============================================================================
# ETAPA 5: COMPLEXIDADE LÉXICA E LEGIBILIDADE
# ==============================================================================
div_lexica <- textstat_lexdiv(matriz_dfm, measure = c("TTR", "CTTR"))
legibilidade <- textstat_readability(meu_corpus, measure = "Flesch.Kincaid")

tabela_complexidade <- div_lexica %>%
  left_join(legibilidade, by = "document") %>%
  rename(
    Livro = document,
    Diversidade_TTR = TTR,
    Diversidade_Corrigida_CTTR = CTTR,
    Complexidade_Flesch_Kincaid = Flesch.Kincaid
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(Diversidade_Corrigida_CTTR))

print("=== MÉTRICAS DE COMPLEXIDADE LÉXICA E LEGIBILIDADE ===")
print(tabela_complexidade)

# ==============================================================================
# ETAPA 6: OCORRÊNCIA DE PALAVRAS ESPECÍFICAS E DISPERSÃO (X-RAY)
# ==============================================================================
top_palavras <- df_livros %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% c(stopwords("portuguese"), sw_extra), 
         !str_detect(word, "^[0-9]+$"),
         nchar(word) > 2) %>%
  count(book_short, word, sort = TRUE) %>%
  group_by(book_short) %>%
  slice_max(n, n = 15, with_ties = FALSE) %>%
  ungroup()

g_top <- ggplot(top_palavras, 
                aes(x = reorder_within(word, n, book_short), 
                    y = n, 
                    fill = book_short)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ book_short, scales = "free", ncol = 4) +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top 15 Palavras Mais Frequentes por Obra de Machado de Assis",
    subtitle = "Excluindo stopwords e verbos auxiliares comuns",
    x = NULL, y = "Frequência Absoluta"
  ) +
  theme(strip.text = element_text(face = "bold"))
print(g_top)

palavras_alvo <- c("olhos", "amor", "morte", "fogo", 
                   "alma", "razão", "loucura", "senhora")

freq_especificas <- df_livros %>%
  unnest_tokens(word, text) %>%
  group_by(book_short) %>%
  mutate(total_palavras_livro = n()) %>%
  filter(word %in% palavras_alvo) %>%
  count(book_short, word, total_palavras_livro) %>%
  mutate(freq_por_10mil = round((n / total_palavras_livro) * 10000, 2)) %>%
  ungroup() %>%
  arrange(word, desc(freq_por_10mil))

print("=== OCORRÊNCIA DE PALAVRAS ESPECÍFICAS (A CADA 10 MIL PALAVRAS) ===")
print(freq_especificas %>% select(book_short, word, n, freq_por_10mil), n = 30)

g_heatmap <- ggplot(freq_especificas, aes(x = word, y = book_short, fill = freq_por_10mil)) +
  geom_tile(color = "white") +
  geom_text(aes(label = freq_por_10mil), color = "black", size = 3.2) +
  scale_fill_gradient(low = "#f7fbff", high = "#2171b5") +
  labs(
    title = "Intensidade Temática: Ocorrência Relativa por Obra",
    subtitle = "Frequência normalizada a cada 10.000 palavras",
    x = "Palavra-Chave", y = NULL, fill = "Freq / 10k"
  )
print(g_heatmap)

kwic_busca <- kwic(tokens(meu_corpus),
                   pattern = c("olhos", "alma", "amor", "dinheiro", "senhora"),
                   case_insensitive = TRUE)

if (nrow(kwic_busca) > 0) {
  g_xray <- textplot_xray(kwic_busca, scale = "relative") +
    aes(color = keyword) +
    scale_color_brewer(palette = "Dark2", guide = "none") +
    labs(
      title = "Dispersão Léxica ao Longo das Obras (Raio-X)",
      x = "Índice Relativo de Tokens (Início -> Fim da Obra)"
    )
  print(g_xray)
}

# ==============================================================================
# ETAPA 7: ANÁLISE DE SENTIMENTO AO LONGO DA NARRATIVA (LEXICONPT)
# ==============================================================================
data("oplexicon_v3.0")

# Deduplica o dicionário para evitar multiplicação de palavras (many-to-many)
lexico_unico <- oplexicon_v3.0 %>%
  distinct(term, .keep_all = TRUE)

df_tokens_sent <- df_livros %>%
  unnest_tokens(term, text) %>%
  inner_join(lexico_unico, by = "term")

ordem_capitulos <- df_livros %>%
  distinct(book_short, chapter_name) %>%
  group_by(book_short) %>%
  mutate(
    num_cap = row_number(),
    posicao_relativa = num_cap / max(num_cap)
  ) %>%
  ungroup()

df_sentimento <- df_tokens_sent %>%
  group_by(book_short, chapter_name) %>%
  summarise(
    polaridade_soma = sum(polarity, na.rm = TRUE),
    polaridade_media = mean(polarity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(ordem_capitulos, by = c("book_short", "chapter_name")) %>%
  arrange(book_short, posicao_relativa)

tabela_sentimentos <- df_sentimento %>%
  group_by(Livro = book_short) %>%
  summarise(
    Media_Polaridade_Cap = round(mean(polaridade_soma), 2),
    Desvio_Padrao_Cap = round(sd(polaridade_soma), 2),
    Cap_Mais_Positivo = chapter_name[which.max(polaridade_soma)],
    Cap_Mais_Negativo = chapter_name[which.min(polaridade_soma)],
    .groups = "drop"
  ) %>%
  arrange(desc(Media_Polaridade_Cap))

print("=== ESTATÍSTICAS DE SENTIMENTO POR OBRA ===")
print(tabela_sentimentos)

# Filtra apenas livros com 3 ou mais capítulos para plotar arcos emocionais e volatilidade
df_sentimento_valido <- df_sentimento %>%
  group_by(book_short) %>%
  filter(n() >= 3) %>%
  ungroup()

# Gráfico 1: Arco Emocional ao longo da obra
g_sent1 <- ggplot(df_sentimento_valido, aes(x = posicao_relativa, 
                                            y = polaridade_soma, 
                                            color = book_short)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_line(alpha = 0.35) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.0) +
  facet_wrap(~ book_short, scales = "free_y", ncol = 4) +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Arco Emocional das Obras de Machado de Assis",
    subtitle = "Saldo de polaridade por capítulo (OpLexicon v3.0) com curva de tendência",
    x = "Posição Relativa no Livro",
    y = "Saldo de Sentimento"
  ) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))
print(g_sent1)

# Gráfico 2: Volatilidade Emocional (Desvio Padrão entre capítulos)
g_sent2 <- df_sentimento_valido %>%
  group_by(book_short) %>%
  summarise(desvio = sd(polaridade_soma, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(desvio)) %>%
  mutate(book_short = fct_reorder(book_short, desvio)) %>%
  ggplot(aes(x = book_short, y = desvio, fill = book_short)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Volatilidade Emocional por Obra",
    subtitle = "Desvio padrão da polaridade entre os capítulos",
    x = NULL,
    y = "Desvio Padrão do Sentimento"
  )
print(g_sent2)
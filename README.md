# 📚 Curso Básico de Mineração de Texto em R — Machado de Assis

[![Quarto Website](https://img.shields.io/badge/Quarto-Website-blue?logo=quarto)](https://mariodhiego.github.io/Web_Scraping_Book_Machado_Assis/)
[![R Version](https://img.shields.io/badge/R-4.5.5-276DC3?logo=r)](https://www.r-project.org/)
[![Acessibilidade](https://img.shields.io/badge/Design-Okabe--Ito%20Colorblind--Safe-0072B2)](#)
[![Licença](https://img.shields.io/badge/Licença-CC%20BY--NC--SA%204.0-lightgrey)](#)

Material didático interativo e relatório técnico de pós-graduação sobre **Processamento de Linguagem Natural (NLP)**, **Web Scraping** e **Estatística Multivariada** aplicados às obras completas de **Machado de Assis** (1876–1908).

🌐 **Acesse o site interativo:** [https://mariodhiego.github.io/Web_Scraping_Book_Machado_Assis/](https://mariodhiego.github.io/Web_Scraping_Book_Machado_Assis/)  
📄 **Apostila Técnica Completa (PDF):** Disponível para download direto no menu do site (`resumo-curso-text-mining.pdf`).

---

## 📊 Estrutura do Curso

1. **Dataset & Contexto Histórico:** Biografia, geografia literária interativa (`leaflet`) e cronologia das 7 obras integrais raspadas do Wikisource (827 capítulos, 8.915 parágrafos e +428 mil tokens).
2. **Web Scraping & Pipeline:** Raspagem ética na árvore DOM (`rvest` + motor nativo `wininet`), contorno de proxy corporativo, higienização via Regex (`stringr`) e estruturação `tidy` / `quanteda`.
3. **Métricas Estatísticas:** Formulação matemática de $TTR$, $CTTR$, legibilidade *Flesch-Kincaid*, $TF\text{-}IDF$, polaridade *OpLexicon v3.0*, Lei de Zipf, *Keyness* ($\chi^2$), Análise de Correspondência (SVD / Critério $\beta$), LDA e Teoria dos Grafos.
4. **Análise Descritiva:** Volumetria, tabela interativa por capítulo (`DT`), assinatura lexical ($TF\text{-}IDF$), dispersão narrativa (*X-Ray*) e bigramas.
5. **Análise de Sentimentos:** Arcos emocionais longitudinais suavizados via LOESS, volatilidade dramática ($\sigma_S$), teste de Kruskal-Wallis e nuvem comparativa de polaridade.
6. **Análise Inferencial:** Ajuste log-log da Lei de Zipf, teste de hipótese entre Romantismo e Realismo, clusterização hierárquica (`ward.D2`), mapa perceptual de correspondência, tópicos latentes (LDA) e rede semântica de Louvain.
7. **Referências (ABNT NBR 6023):** Fontes primárias, crítica literária, artigos seminais e bibliotecas **R**.

---

## ⚙️ Reprodutibilidade Local

```bash
# Clonar o repositório
git clone [https://github.com/MarioDhiego/Web_Scraping_Book_Machado_Assis.git](https://github.com/MarioDhiego/Web_Scraping_Book_Machado_Assis.git)

# Compilar todas as páginas HTML e a Apostila em PDF (Typst)
quarto render

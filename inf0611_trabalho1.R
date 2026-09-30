######################################################################
# INF-0611 Recuperação de Informação                                 #
#                                                                    #
# Trabalho 1 - Recuperação de Texto                                  #
######################################################################
# Nome COMPLETO dos integrantes do grupo:                            #
#   - Anderson Luis Bento Soares                                     #
#   - Eduardo Bouhid                                                 #
#   - Mateus Coelho                                                  #
#                                                                    #
######################################################################

######################################################################
# Configurações Preliminares                                         #
######################################################################

# Carregando as bibliotecas
library(tokenizers)
library(dplyr)
library(udpipe)
library(tidytext)
library(tidyverse)


# Carregando os arquivos auxiliares
source("./ranking_metrics.R", encoding = "UTF-8")
source("./trabalho1_base.R", encoding = "UTF-8")

# Configure aqui o diretório onde se encontram os arquivos do trabalho
# setwd("")


######################################################################
#
# Questão 1
#
######################################################################

# Lendo os documentos (artigos da revista TIME)
# sem processamento de texto (não mude essa linha)
docs <- process_data("time.txt", "XX-Text [[:alnum:]]", "Article_0", 
                     convertcase = TRUE, remove_stopwords = FALSE)
# Visualizando os documentos (apenas para debuging)
# head(docs)

# Lendo uma lista de consultas (não mude essa linha)
queries <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                        "Query_0", convertcase = TRUE, 
                        remove_stopwords = FALSE)
# Visualizando as consultas (apenas para debuging)
# head(queries)
# Exemplo de acesso aos tokens de uma consulta
q1 <- queries[queries$doc_id == "Query_01",]; q1

# Lendo uma lista de vetores de ground_truth
ground_truths <- read.csv("relevance.csv", header = TRUE)

# Visualizando os ground_truths (apenas para debuging)
head(ground_truths)
# Exemplo de acesso vetor de ground_truth da consulta 1:
ground_truths[1,]
# Exemplo de impressão dos ids dos documentos relevantes da consulta 1:
# Visualizando o ranking (apenas para debuging)
names(ground_truths)[ground_truths[1,]==1]
# names(ground_truths)

# Computando a matriz de termo-documento

term_freq <- document_term_frequencies(x = docs, document = "doc_id", term = "word")

# Computando as estatísticas da coleção e convertendo em data.frame
# k = 1.2 e b = 0.75 são o valor "default". @Anderson, fica à vontade
# pra mexer dps
docs_stats <- as.data.frame(document_term_frequencies_statistics(x = term_freq, k = 1.2, b = 0.75))
# Visualizando as estatísticas da coleção (apenas para debuging)
# head(docs_stats)

######################################################################
#
# Questão 2
#
######################################################################


# query: Elemento da lista de consultas, use a segunda coluna desse 
#        objeto para o cálculo do ranking
# ground_truth: Linha do data.frame de ground_truths referente a query
# stats: data.frame contendo as estatísticas da base
# stat_name: Nome da estatística de interesse, como ela está escrita 
#            no data.frame stats
# top: Tamanho do ranking a ser usado nos cálculos de precisão 
#      e revocação
# text: Título adicional do gráfico gerado, deve ser usado para 
#       identificar a questão e a consulta
computa_resultados <- function(query, ground_truth, stats, stat_name, 
                               top, text) {
  # Criando ranking (função do arquivo base)
  # Dica: você pode acessar a segunda coluna da query a partir de $word ou [["word"]]
  ranking <- get_ranking_by_stats(stat_name, stats, query$word)
  # Visualizando o ranking (apenas para debuging)
  # head(ranking, n = 5)
  
  # Ids dos documentos na ordem do ranking. O doc_id é um fator
  # ordenado; convertemos para character para indexar o ground_truth
  # pelo nome da coluna (ex.: "Article_0308"), e não pelo código
  # interno do fator, que pode não bater com a ordem das colunas
  # do relevance.csv (ex.: se algum documento sumir após o
  # processamento na Questão 3)
  ranking_ids <- as.character(ranking$doc_id)

  # Calculando a precisão
  # Dica: para calcular a precisão, revocação e utilizar a função plot_prec_e_rev,
  # utilize a coluna doc_id do ranking gerado (você pode acessar com $doc_id)
  p <- precision(ground_truth, ranking_ids, top)

  # Calculando a revocação
  r <- recall(ground_truth, ranking_ids, top)

  # Imprimindo os valores de precisão e revocação
  cat(paste("Consulta: ", query[1,1], "\nPrecisão: ", p, 
            "\tRevocação: ", r, "\n"))
  
  # Gerando o plot Precisão + Revocação (função do arquivo base)
  # print() garante que o gráfico apareça também quando o arquivo
  # é executado via source()
  print(plot_prec_e_rev(ranking_ids, ground_truth, top, text))
}

# Definindo a consulta 1 
# Dicas para as variáveis consulta1 e n_consulta1:
# Para a variável consulta1, você deve acessar os tokens de uma consulta, conforme
# o exemplo da linha 52 e 53.
# Para a variável n_consulta1, você deve informar o número da consulta. Por exemplo,
# se usar a Query_01 como consulta, n_consulta1 deve receber o valor 1.
# Consulta 1 escolhida: Query_06 (9 documentos relevantes)
# "ceremonial suicides committed by some buddhist monks in south
#  viet nam and what they are seeking to gain by such acts ."
# @Matheus: usar as mesmas consultas (6 e 33) na Questão 3
consulta1 <- queries[queries$doc_id == "Query_06",]
n_consulta1 <- 6

## Exemplo de uso da função computa_resultados:
# computa_resultados(consulta1, ground_truths[n_consulta1, ], 
#                    docs_stats, "nome da statistica", 
#                    top = 20, "titulo")

# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1, ground_truths[n_consulta1, ],
                   docs_stats, "tf_idf",
                   top = 20, "- Q2 - Consulta 6 - tf-idf")

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1, ground_truths[n_consulta1, ],
                   docs_stats, "bm25",
                   top = 20, "- Q2 - Consulta 6 - bm25")


# Definindo a consulta 2 
# Consulta 2 escolhida: Query_033 (18 documentos relevantes)
# "president de gaulle's policy on british entry into the
#  common market ."
consulta2 <- queries[queries$doc_id == "Query_033",]
n_consulta2 <- 33

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2, ground_truths[n_consulta2, ],
                   docs_stats, "tf_idf",
                   top = 20, "- Q2 - Consulta 33 - tf-idf")

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2, ground_truths[n_consulta2, ],
                   docs_stats, "bm25",
                   top = 20, "- Q2 - Consulta 33 - bm25")


######################################################################
#
# Questão 2 - Escreva sua análise abaixo
#
######################################################################
# (b)(i) Consultas escolhidas: Query_06 (9 relevantes) e Query_033
# (18 relevantes). P@k e R@k são a precisão e a revocação nos k
# primeiros do ranking; AP@20 é a precisão média em k = 20.
#
#   Consulta  Modelo   P@5   P@10  P@20  R@10  R@20  AP@20
#   6         tf-idf   0.60  0.40  0.45  0.44  1.00  0.51
#   6         bm25     0.80  0.80  0.45  0.89  1.00  0.92
#   33        tf-idf   1.00  0.90  0.55  0.50  0.61  0.58
#   33        bm25     0.80  0.90  0.70  0.50  0.78  0.64
#
# O bm25 teve o melhor resultado nas duas consultas.
#
# Na consulta 6, precisão e revocação em k = 20 empatam (0.45 e 1.0), pois os dois modelos trazem os 9 relevantes entre os 20 primeiros. 
# A diferença está na ordem: o bm25 coloca 8 dos 9 relevantes nas 9 primeiras posições (P@10 = 0.80, R@10 = 0.89), 
# enquanto o tf-idf espalha os relevantes até a posição 20 (P@10 = 0.40, R@10 = 0.44). 
# Isso aparece nos gráficos: a curva de revocação do bm25 chega a 1.0 em k = 11, e a do tf-idf só em k = 20. A precisão média (AP@20), que
# leva em conta a posição dos relevantes, é 0.92 no bm25 contra 0.51 no tf-idf.
#
# Na consulta 33, o tf-idf começa melhor (P@5 = 1.0 contra 0.8) e os dois empatam em k = 10 (P@10 = 0.90). Entre as posições 11 e 20, o
# bm25 encontra mais 5 relevantes e o tf-idf só mais 2: em k = 20 o bm25 recupera 14 dos 18 relevantes contra 11 do tf-idf (P@20 = 0.70
# contra 0.55, R@20 = 0.78 contra 0.61). O AP@20 também é maior no bm25 (0.64 contra 0.58).
#
# A diferença vem da forma como cada modelo pesa a frequência do termo. No tf-idf do udpipe, o tf é a frequência dividida pelo
# tamanho do documento e cresce linearmente. Isso favorece documentos curtos e documentos que repetem muito um único termo da consulta.
# No bm25, a frequência satura (k = 1.2), ou seja, cada repetição do mesmo termo soma cada vez menos ao peso, e o tamanho do documento
# pesa menos (b = 0.75, relativo ao tamanho médio). O bm25 favorece documentos que contêm vários termos diferentes da consulta.
#
# Os rankings mostram os dois efeitos. Na consulta 6, as posições 1 e 3 do tf-idf são documentos irrelevantes curtos, com 203 e 167
# tokens, cujo termo de maior peso é "viet". Já os 9 relevantes têm em média 1043 tokens (a média da coleção é 585), e o bm25 sobe
# relevantes longos, como um de 2500 tokens na posição 3. Na consulta 33, dos 6 documentos que só o tf-idf coloca no top-20, 5
# têm a maior parte do peso vinda do termo "de" e só 1 é relevante, dos 6 que só o bm25 coloca no top-20, 4 são relevantes e contêm
# mais termos distintos da consulta (5 a 9, contra 4 ou 5).

######################################################################
#
# Questão 3
#
######################################################################
# Na função process_data está apenas a função para remoção de 
# stopwords está implementada. Sinta-se a vontade para testar 
# outras técnicas de processamento de texto vista em aula.

# Lendo os documentos (artigos da revista TIME) 
# com processamento de texto
docs_proc <- process_data("time.txt", "XX-Text [[:alnum:]]",  
                          "Article_0", convertcase = TRUE, 
                          remove_stopwords = TRUE)
# Visualizando os documentos (apenas para debuging)
# head(docs_proc)


# Lendo uma lista de consultas
queries_proc <- process_data("queries.txt", "XX-Find [[:alnum:]]", 
                             "Query_0", convertcase = TRUE, 
                             remove_stopwords = TRUE)
# Visualizando as consultas (apenas para debuging)
# head(queries_proc)

# Computando a matriz de termo-documento
term_freq_proc <- document_term_frequencies(x = docs_proc, document = "doc_id", term = "word")

# Computando as estatísticas da coleção e convertendo em data.frame
# Mesmos k e b da Questão 1, para que a única diferença seja a
# remoção de stopwords
docs_stats_proc <- as.data.frame(document_term_frequencies_statistics(x = term_freq_proc, k = 1.2, b = 0.75))


# Definindo a consulta 1
# Mesmas consultas da Questão 2 (Query_06 e Query_033)
consulta1_proc <- queries_proc[queries_proc$doc_id == "Query_06",]
n_consulta1_proc <- 6
# Resultados para a consulta 1 e tf_idf
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ],
                   docs_stats_proc, "tf_idf",
                   top = 20, "- Q3 - Consulta 6 - tf-idf (sem stopwords)")

# Resultados para a consulta 1 e bm25
computa_resultados(consulta1_proc, ground_truths[n_consulta1_proc, ],
                   docs_stats_proc, "bm25",
                   top = 20, "- Q3 - Consulta 6 - bm25 (sem stopwords)")


# Definindo a consulta 2
consulta2_proc <- queries_proc[queries_proc$doc_id == "Query_033",]
n_consulta2_proc <- 33

# Resultados para a consulta 2 e tf_idf
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ],
                   docs_stats_proc, "tf_idf",
                   top = 20, "- Q3 - Consulta 33 - tf-idf (sem stopwords)")

# Resultados para a consulta 2 e bm25
computa_resultados(consulta2_proc, ground_truths[n_consulta2_proc, ],
                   docs_stats_proc, "bm25",
                   top = 20, "- Q3 - Consulta 33 - bm25 (sem stopwords)")


# (a)(ii) Média das precisões médias em k = 20 sobre todas as
# consultas de avaliação, para tf-idf e bm25, com e sem stopwords.
# A i-ésima linha do relevance.csv corresponde à consulta "Query_0<i>".

# Ids dos documentos ordenados por uma estatística para uma consulta.
# Se o ranking tiver menos de k documentos (poucos documentos contêm
# os termos da consulta), completamos com os demais documentos da
# coleção, que não pontuaram e ficam, portanto, no fim do ranking.
# Isso evita índices NA ao acessar o ground_truth nas posições 1:k.
ranking_completo <- function(query, stats, stat_name) {
  if (nrow(query) == 0) return(names(ground_truths))
  ids <- as.character(get_ranking_by_stats(stat_name, stats, query$word)$doc_id)
  c(ids, setdiff(names(ground_truths), ids))
}

# MAP@k de um modelo (estatística + coleção processada ou não)
map_modelo <- function(queries, stats, stat_name, k = 20) {
  pares <- lapply(seq_len(nrow(ground_truths)), function(i) {
    query <- queries[queries$doc_id == paste0("Query_0", i), ]
    list(ground_truths[i, ], ranking_completo(query, stats, stat_name))
  })
  map(pares, k)
}

resultados_map <- data.frame(
  modelo = c("tf-idf", "bm25", "tf-idf", "bm25"),
  stopwords = c("com", "com", "sem", "sem"),
  map_20 = c(map_modelo(queries, docs_stats, "tf_idf"),
             map_modelo(queries, docs_stats, "bm25"),
             map_modelo(queries_proc, docs_stats_proc, "tf_idf"),
             map_modelo(queries_proc, docs_stats_proc, "bm25"))
)
print(resultados_map)

######################################################################
#
# Questão 3 - Escreva sua análise abaixo
#
######################################################################
# (a)(i) Mesmas consultas da Questão 2 (Query_06 e Query_033), antes
# e depois da remoção de stopwords (k = 20 e mesmos k e b do bm25).
#
#   Consulta  Modelo  Stopwords   P@5   P@10  P@20  R@10  R@20  AP@20
#   6         tf-idf  mantidas    0.60  0.40  0.45  0.44  1.00  0.51
#   6         tf-idf  removidas   0.60  0.50  0.45  0.56  1.00  0.52
#   6         bm25    mantidas    0.80  0.80  0.45  0.89  1.00  0.92
#   6         bm25    removidas   1.00  0.90  0.45  1.00  1.00  0.95
#   33        tf-idf  mantidas    1.00  0.90  0.55  0.50  0.61  0.58
#   33        tf-idf  removidas   1.00  0.90  0.55  0.50  0.61  0.57
#   33        bm25    mantidas    0.80  0.90  0.70  0.50  0.78  0.64
#   33        bm25    removidas   0.80  0.90  0.70  0.50  0.78  0.67
#
# Em k = 20, a remoção de stopwords não mudou a precisão nem a revocação de nenhum dos dois modelos:
# os mesmos relevantes continuam entre os 20 primeiros (na consulta 6, todos os 9; na 33, 11 no tf-idf e 14 no bm25).
# O impacto aparece na ordem dentro do top-20, visível nos k menores e no AP@20.
#
# Na consulta 6 (removidas: "by", "some", "in", "and", "what", "they", "are", "to", "such"), os dois modelos melhoram.
# O bm25 passa a colocar 9 relevantes nas 10 primeiras posições (P@10 de 0.80 para 0.90, R@10 de 0.89 para 1.00, AP@20 de 0.92 para 0.95).
# O tf-idf ganha um relevante na posição 10 (P@10 de 0.40 para 0.50, R@10 de 0.44 para 0.56), mas os dois documentos irrelevantes curtos
# do topo (Article_0154 e Article_0419) continuam nas posições 1 e 3, pois o peso deles vem do termo "viet", que não é stopword.
#
# Na consulta 33 (removidas: "on", "into", "the"), precisão e revocação ficam iguais em todos os k mostrados, e só o AP@20 muda:
# sobe no bm25 (0.64 para 0.67) e cai levemente no tf-idf (0.58 para 0.57).
# O termo "de", que dominava o tf-idf nessa consulta (ver Questão 2), não está na lista de stopwords do tidytext e continua na consulta,
# por isso o tf-idf praticamente não muda.
#
# O efeito é pequeno porque as stopwords mais comuns já têm idf quase nulo: "the" tem idf 0, "in" e "to" 0.002, "and" 0.007.
# Elas quase não somam pontos a nenhum documento. As que pesam são as menos frequentes, como "such" (idf 1.35), "what" (0.98) e "some" (0.79).
# Essas adicionam ruído, pontuando documentos que não tratam do tema. Além disso, a remoção muda o tamanho dos documentos (de 585 para 288 tokens em média),
# o que altera o tf normalizado do tf-idf e a normalização por tamanho do bm25, e reordena documentos mesmo sem mudar quais estão no top-20.
#
# (a)(ii) Média das precisões médias (MAP) em k = 20 nas 59 consultas do relevance.csv.
# A precisão média de cada consulta divide pelo total de relevantes (até 20), então um relevante fora do ranking conta como perdido.
#
#   Modelo  Stopwords   MAP@20
#   tf-idf  mantidas    0.477
#   tf-idf  removidas   0.488
#   bm25    mantidas    0.571
#   bm25    removidas   0.588
#
# O melhor método foi o bm25 com remoção de stopwords (MAP@20 = 0.588). A escolha do modelo pesa muito mais que o pré-processamento.
# Trocar tf-idf por bm25 aumenta o MAP em cerca de 0.10 nos dois cenários.
# Consulta a consulta, o bm25 tem AP@20 maior que o tf-idf em 39 das 59 consultas com stopwords (perde em 12, empata em 8)
# e em 41 sem stopwords (perde em 8, empata em 10).
# A explicação é a da Questão 2: a saturação da frequência e a normalização pelo tamanho médio favorecem documentos
# que contêm vários termos da consulta, em vez de documentos curtos que repetem um único termo.
#
# A remoção de stopwords dá um ganho menor, mas consistente nos dois modelos: +0.011 no tf-idf e +0.017 no bm25.
# No bm25, o AP@20 melhora em 24 consultas, piora em 11 e fica igual em 24.
# No tf-idf, melhora em 25, piora em 14 e fica igual em 20.
# Como as stopwords mais frequentes já têm idf quase zero, o ganho vem de tirar as de idf mais alto, que trazem ruído,
# e de medir o tamanho dos documentos só pelas palavras com conteúdo.


######################################################################
#
# Extra
#
# # Comando para salvar todos os plots gerados e que estão abertos no 
# Rstudio no momemto da execução. Esse comando pode ajudar a comparar 
# os gráfico lado a lado.
# 
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics",
#                              full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", 
#                               full.names = TRUE)
# file.copy(from=plots.png.paths, to="~/Desktop/")
######################################################################

































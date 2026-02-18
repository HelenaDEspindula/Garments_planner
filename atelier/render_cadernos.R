# ============================================
# RENDER CADERNOS DO ATELIÊ
# ============================================

library(googlesheets4)
library(dplyr)
library(rmarkdown)
library(withr)

# --------------------------------------------
# 1. AUTENTICAÇÃO GOOGLE
# --------------------------------------------
gs4_auth(cache = TRUE)

# --------------------------------------------
# 2. CONFIGURAÇÃO
# --------------------------------------------

# Use apenas o ID da planilha
link_planilha <- "170PFaca90a_0HLy41tXjx4uc6wYBblbLTLNnwH8vwfw"

# --------------------------------------------
# 3. EXECUTA DENTRO DA PASTA atelier
# --------------------------------------------

with_dir("atelier", {
  
  cat("Lendo planilha...\n")
  
  projetos <- read_sheet(link_planilha, sheet = "projetos")
  materiais <- read_sheet(link_planilha, sheet = "materiais_projeto")
  
  # Padronização preventiva
  projetos <- projetos |>
    mutate(
      status = tolower(trimws(status)),
      etapa  = tolower(trimws(etapa))
    )
  
  gerar_caderno <- function(status_filtro, titulo_pdf, nome_arquivo) {
    
    cat("Gerando:", nome_arquivo, "\n")
    
    projetos_filtrados <- projetos |>
      filter(status == status_filtro)
    
    # Ordenação inteligente
    if(status_filtro == "ativo") {
      projetos_filtrados <- projetos_filtrados |>
        arrange(data_inicio)
    }
    
    if(status_filtro == "concluido") {
      projetos_filtrados <- projetos_filtrados |>
        arrange(data_fim)
    }
    
    if(status_filtro == "ideia") {
      projetos_filtrados <- projetos_filtrados |>
        arrange(nome)
    }
    
    rmarkdown::render(
      "modelo_caderno.Rmd",
      output_file = nome_arquivo,
      output_dir = "_pdf",
      params = list(
        status_filtro = status_filtro,
        titulo = titulo_pdf,
        projetos = projetos_filtrados,
        materiais = materiais
      ),
      envir = new.env()
    )
  }
  
  # --------------------------------------------
  # 4. GERAR OS 3 LIVROS
  # --------------------------------------------
  
  gerar_caderno(
    status_filtro = "ideia",
    titulo_pdf = "Caderno de Ideias 2026",
    nome_arquivo = "Caderno_Ideias_2026.pdf"
  )
  
  gerar_caderno(
    status_filtro = "ativo",
    titulo_pdf = "Caderno de Projetos Ativos 2026",
    nome_arquivo = "Caderno_Ativos_2026.pdf"
  )
  
  gerar_caderno(
    status_filtro = "concluido",
    titulo_pdf = "Arquivo de Projetos Concluídos 2026",
    nome_arquivo = "Caderno_Concluidos_2026.pdf"
  )
  
  cat("Concluído.\n")
})

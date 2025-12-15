# Caderno de Projetos de Roupas (RMarkdown -> PDF)

Este projeto gera um PDF pronto para imprimir, com:
- Pagina de medidas (com data)
- Secao de projetos com campos para preencher e caixas para swatches de tecido

## Como compilar
1. Abra `caderno-projetos-roupas.Rmd` no RStudio.
2. Knit -> Knit to PDF

## Requisitos
- R + RStudio
- rmarkdown + knitr
- LaTeX instalado (recomendado: TinyTeX)

Para instalar TinyTeX:
```r
install.packages("tinytex")
tinytex::install_tinytex()
```

## Como separar
- Entre projetos: use uma linha `----`
- Entre categorias (se voce criar mais capitulos): use `\categorybreak` antes de um novo `# Categoria`

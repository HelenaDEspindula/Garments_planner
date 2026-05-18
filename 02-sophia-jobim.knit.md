# Curso por Correspondência — Sophia Jobim

**Referência:** JOBIM, Sophia. *O sistema de corte e costura de Sophia Jobim: os anos de ouro de Mme Carvalho no Liceu Império: 1932-1954*. São Paulo: Portal de Livros Abertos da USP, 2024. Disponível em: https://www.livrosabertos.abcd.usp.br/portaldelivrosUSP/catalog/view/1327/1210/4651

Este capítulo documenta o estudo do método de modelagem de Sophia Jobim, transcrevendo as instruções originais e implementando-as em código R parametrizado.

---

## Blusa (Corpo Simples)

**Página de referência:** ~111 do PDF original

### Medidas Utilizadas

O livro utiliza como exemplo as medidas do **manequim 48**, que são as mais aproximadas do corpo médio feminino.

\begin{table}

\caption{(\#tab:medidas-blusa)Medidas do manequim 48 e parâmetros calculados}
\centering
\begin{tabular}[t]{l|r}
\hline
Descrição & Valor\_cm\\
\hline
Circunferência do busto & 90\\
\hline
Comprimento frente & 44\\
\hline
Comprimento costas & 41\\
\hline
Aumento (folga) & 6\\
\hline
Largura do molde (busto+folga)/2 & 48\\
\hline
Altura do molde & 44\\
\hline
\end{tabular}
\end{table}

Instruções Originais (transcrição)

    Para facilitar as explicações, tomarei como exemplo as medidas do manequim 48, que são as mais aproximadas do corpo médio feminino. Essas medidas são, para a blusa: 90 cm para a circunferência do busto; 44 cm para o comprimento da blusa, na frente, e 41 cm para o comprimento nas costas.

    Para que a blusa não fique justa como um colete, torna-se necessário dar-lhe um aumento de 6 cm na circunferência do busto, resultando 96. Como vamos fazer apenas a metade do molde, isto é, metade da frente e metade das costas, dividiremos por 2 a medida total 96 cm. Que resulta 48 cm.


Passo 1: Retângulo Base ABCD

Instrução original:

    Corta-se um retângulo de papel ABCD em que a largura AB é a circunferência do busto aumentada de 6 cm e o resultado dividido por 2; a altura AD, do retângulo, é o comprimento da blusa, tirado pela frente. Para o nosso exemplo, o retângulo terá 48 cm de largura e 44 cm de altura.


\begin{center}\includegraphics[width=1\linewidth]{images/cache/blusa_passo1_dc87bbf6df5a98a4ff2a4a83cedc5896} \end{center}



Passo 2: Deslocamento para Costas (Reta EF)

Instrução original:

    A partir dos pontos A e D, passa-se para a direita 3 cm e traça-se a reta EF.

Implementação em R:



































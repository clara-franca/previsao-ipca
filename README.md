# Análise Comparativa de Modelos Estatísticos para Previsão do IPCA

Este repositório contém os códigos, dados e relatório final do Trabalho de Conclusão de Curso (TCC) apresentado por Ana Clara Barbosa de França ao Departamento de Estatística da Universidade de Brasília (UnB).

---

## Objetivo

O objetivo deste trabalho foi comparar diferentes abordagens estatísticas para prever o Índice Nacional de Preços ao Consumidor Amplo (IPCA), utilizando desde modelos clássicos de séries temporais até redes neurais artificiais.

---

## Modelos Utilizados

- **SARIMA**  
- **SARIMAX** (com covariáveis exógenas)  
- **Redes Neurais Artificiais (RNA)**  
- **Redes Neurais Autorregressivas (NNAR)**  
- **Regularização LASSO** para seleção de variáveis

---

## Dados

- IPCA mensal (jan/2011 a mai/2025)  
- 123 variáveis socioeconômicas, oriundas de:
  - IBGE, BCB, IPEA, FGV, ANBIMA, Conab, Abras, CNI, Eletrobras, entre outras
- Os dados foram processados e armazenados em `dados.RData`

---

## Validação

- Validação cruzada com janela deslizante
- Avaliação com métricas: **MAE** e **RMSE**
- Comparações com dados reais de outubro/2024 a maio/2025
- Previsões até dezembro de 2027

---

## Principais Resultados

- Modelos que utilizam **variáveis exógenas** (SARIMAX, NNAR) superaram os modelos univariados
- Projeções médias:
  - **2025**: ≈ 5%
  - **2026 e 2027**: ligeiramente acima de 4%
- **Convergência com expectativas do mercado (Relatório Focus)**



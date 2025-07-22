# 📊 Análise Comparativa de Modelos Estatísticos para Previsão do IPCA

Este repositório contém os códigos, dados e relatório final do Trabalho de Conclusão de Curso (TCC) apresentado por Ana Clara Barbosa de França ao Departamento de Estatística da Universidade de Brasília (UnB), com foco na previsão do IPCA utilizando modelos estatísticos avançados.

---

## 🧠 Objetivo

Comparar a performance de diferentes modelos estatísticos — incluindo modelos de séries temporais clássicos e redes neurais — para a previsão do IPCA (Índice Nacional de Preços ao Consumidor Amplo), com base em dados reais e em múltiplos horizontes de previsão.

---

## ⚙️ Modelos Aplicados

- **SARIMA**  
- **SARIMAX** (com covariáveis exógenas selecionadas via LASSO)  
- **Redes Neurais Artificiais (RNA)**  
- **Redes Neurais Autorregressivas (NNAR)**

---

## 📁 Dados Utilizados

- IPCA mensal (jan/2011 a set/2024)  
- 123 variáveis socioeconômicas, oriundas de:
  - IBGE, BCB, IPEA, FGV, ANBIMA, Conab, Abras, CNI, Eletrobras, entre outras

| Arquivo `.RData`                         | Descrição                                                                 |
|------------------------------------------|---------------------------------------------------------------------------|
| `covariates_Train_ts.RData`              | Covariáveis socioeconômicas utilizadas no treino dos modelos              |
| `covariates_Test_ts.RData`               | Projeções das covariáveis para previsão fora da amostra (out/2024–daz/2027) |
| `dependent_variables_ts.RData`           | Série de variação mensal do IPCA                                         |
| `responses_series_level.RData`           | Série com o IPCA em nível absoluto (índice base 100 = dez/1993)          |

---

## 🔍 Metodologia

- **Seleção de variáveis** com LASSO
- **Validação cruzada com janela deslizante**
- Avaliação dos modelos com métricas:
  - MAE (Mean Absolute Error)
  - RMSE (Root Mean Squared Error)
- **Previsões fora da amostra** comparadas com valores reais (out/2024 a mai/2025)
- **Projeções de longo prazo** até dezembro de 2027

---

## 📈 Resultados

- **Modelos com variáveis exógenas (SARIMAX, NNAR)** apresentaram melhor desempenho
- **SARIMA (modelo univariado)** teve menor acurácia
- Projeções médias do IPCA:
  - **2025**: ~5%
  - **2026–2027**: levemente acima de 4%
- **Convergência com o Relatório Focus do Banco Central**

---


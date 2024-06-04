# prrEcopesca: Interactive Analysis and Visualization of Fish Populations' Spawning Potential Ratio
The prrEcopesca package is an enhanced, interactive customization of the LBSPR package, designed to facilitate 
the analysis and visualization of the Spawning Potential Ratio (SPR) of fish populations. This package compares observed 
size structure with target size structure, and it is ideal for marine biologists, ecologists, and fishery managers who 
need to monitor and evaluate the sustainability of fish populations.

## Package Installation Instructions
### Prerequisites for Installing the Package
* It is recommended to use **RStudio** preferably.
* Make sure you have the `devtools` or `remotes` packages properly installed.
### How to Install
* If you have the `devtools` package installed, use the following command:
>devtools::install_github("FelipeCadilho/prrEcopesca")

* If you have the `remotes` package installed, use the following command:
>remotes::install_github("FelipeCadilho/prrEcopesca")

## Prerequisites to Run the Package
### Database
* The data file must contain distance data in the first column and elements in the subsequent columns.
* The file can have the extensions `.xls`, `.xlsx`, `.csv`, `.txt`, or be of type `data.frame` in R.
* It is necessary to inform the column separator character for data files with extensions `.csv` and `.txt`.

## Step-by-step to Execute the Package
1. Set the working directory in RStudio to the directory where the data file is located.
  * Set the directory using the `setwd()` command or through the Files panel in the `Files` tab.
>Example 1: setwd("C:/User/UserPC/Documents")

>Example 2: https://youtu.be/QzSV8wvA1Do

2. To load the package functions, execute the command `library(prrEcopesca)` in RStudio.
3. Run the `prrEcopesca(2)` function to initiate the analysis.
4. Answer the program's questions to customize the analysis according to your needs.

## Main Features

### Interactive Data Entry 
Provides user-friendly questionnaires to facilitate data input, making it easier for users to input and manage their data accurately.
### Customizable Bar Graphs 
Creates histograms that compare observed size structure with target size structure, with extensive customization options for labels, colors, titles, and more.
### SPR Analysis 
Includes calculations and visualizations of SPR to help understand the reproductive capacity of the population relative to a defined target.
User Interaction: Engages users through interactive elements, enhancing the user experience and ensuring precise data collection.

# prrEcopesca: Análise e Visualização Interativa do Potencial Remanescente Reprodutivo de Populações de Peixes
O pacote prrEcopesca é uma personalização aprimorada e interativa do pacote LBSPR, desenvolvido para facilitar a análise e visualização do 
Potencial Remanescente Reprodutivo (PRR) de populações de peixes. Este pacote compara a estrutura de tamanho observada com a estrutura de tamanho alvo e 
é ideal para biólogos marinhos, ecologistas e gestores de pesca que precisam monitorar e avaliar a sustentabilidade das populações de peixes.

## Instruções de instalação do pacote
### Pré-requisitos para instalar o pacote
* É recomendado utilizar o **RStudio** preferencialmente.
* Certifique-se de ter os pacotes `devtools` ou `remotes` devidamente instalados.
### Como instalar
* Caso tenha o pacote `devtools` instalado, utilize o seguinte comando:
>devtools::install_github("FelipeCadilho/prrEcopesca")

* Caso tenha o pacote `remotes` instalado, utilize o seguinte comando:
>remotes::install_github("FelipeCadilho/prrEcopesca")

## Pré-requisitos para executar o pacote
### Base de dados
* O arquivo de dados deve conter os dados de distância na primeira coluna e os elementos nas colunas subsequentes.
* O arquivo pode ter as extensões `.xls`, `.xlsx`, `.csv`, `.txt` ou ser do tipo objeto `data.frame` do R.
* É necessário informar o caractere separador de colunas do arquivo de dados com as extensões `.csv` e `.txt`.

## Passo a passo para executar o pacote
1. Configure a pasta de trabalho no RStudio para o diretório onde o arquivo de dados está localizado.
    - Configure o diretório usando o comando `setwd()` ou pelo painel de visualização na aba `Files`.
>Exemplo 1: setwd("C:/User/Usuário da Máquina/Documents")

>Exemplo 2:https://youtu.be/lnoQcMe63oA?t=156
2. Para carregar as funções do pacote, execute o comando `library(prrEcopesca)` no RStudio.
3. Execute a função `prrEcopesca()` para iniciar a análise.
4. Responda às perguntas do programa para personalizar a análise de acordo com suas necessidades.

## Funcionalidades Principais

### Entrada de Dados Interativa 
Fornece questionários amigáveis para facilitar a entrada de dados, tornando mais fácil para os usuários inserir e gerenciar seus dados com precisão.
### Gráficos de Barras Personalizáveis 
Cria histogramas que comparam a estrutura de tamanho observada com a estrutura de tamanho alvo, com amplas opções de personalização para rótulos, cores, títulos e mais.
### Análise do SPR (Spawning Potential Ratio) 
Inclui cálculos e visualizações do SPR para ajudar a entender a capacidade reprodutiva da população em relação a uma meta definida.
Interatividade com o Usuário: Engaja os usuários através de elementos interativos, melhorando a experiência do usuário e garantindo uma coleta de dados precisa.

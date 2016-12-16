* Introdução


/Programming Collective Intelligence: Building Smart Web 2.0 Applications/ de Toby Segaran é um nome feliz. Conjura, de modo preciso, os motivos que levam esta introdução ao aprendizado de máquinas a ser tão empolgante: obter conhecimentos novos a partir de fatos compartilhados por um coletivo. 

O que é tão cativante nisto? É que todos os dados são, de certa forma, de pleno conhecimento de todos. Afinal, qualquer um pode, hoje em dia, saber quais páginas seus amigos curtem no Facebook; qualquer site pode conhecer o que seus usuários compram ou acessam. São informações que são ricas e, ao mesmo tempo, inúteis. São inúteis sozinhas, sem serem coletadas e analisadas, mas com o devido tratamento, essas informações batidas podem dar a luz à novos conhecimentos sobre o comportamente, as preferências e as opiniões de vasta quantidade de pessoas.

Este trabalho é uma tentativa de implementar em Racket um sistema de recomendação, originalmente implementado em Python no segundo capítulo do /Programming Collective Intelligence/ (PCI). Ele foi tornado possível a partir do trabalho de muitos: lectures do curso Mining Massive Datasets dos professores Rajaraman e Ullman, de Stanford, a implementação parcial do PCI do blog /I Need Closures/ de Richard Cook, dois papers sobre sistemas de recomendação e, literalmente, centenas de perguntas e respostas no StackOverflow, Reddit e Quora.

O trabalho é estruturado da seguinte forma: (I) explicar os objetivos que pretendo alcançar no desenvolvimento deste código; (II) apresentar os dados empíricos que foram coletados para desafiar a robustez do código que, em sua implementação original, trabalha com dados hipotéticos e limitados; (III) apresentar o código, explicando seu funcionamento e comentando algumas escolhas de seu design; e (IV) apontar possíveis melhorias e desenvolvimentos possíveis do código.


* Objetivos

O código teve três critérios em seu desenvolvimento:

1. Buscar robustez. No PCI e no código de Richard Cook, o sistema de recomedação toma como input um conjunto de preferências limitado no próprio código (através de um dicionário, no PCI, e de uma lista associativa, em Cook). Considerei que isto era insuficiente, e optei por tentar melhorar o programa ao permitir que ele lidasse com diferentes datasets e, preferencialmente, com dados empíricos;

2. Beleza. Acredito que a beleza de um código é uma boa medida, embora não única e, talvez, a mais importante, de sua qualidade. Busquei programar neste sentido, ao evitar o uso de estados, funções e estruturas de dados que não fossem estritamente necessárias para o sistema de recomendações (exceto quando importantes para sua escalabilidade), escrever um código legível tanto em seus comentários quanto na escolha dos nomes e quantidades de parâmetros e procedimentos, seguir o recomendado pelas práticas de estilo em Racket, entre outras abordagens. No entanto, entendo que a beleza é o que mais falta a este trabalho, em uma autocrítica, fruto da óbvia inexperiência e da limitacão de tempo.

3. Modularização e abstração. Busquei trazer os ensinamentos dos capítulos 2 e 3 do /Structure and Interpretation of Computer Programs/ ao código. Um programa modular e com uso de abstrações é mais escalável e mais elegante.


* O sistema de recomendação



* Sobre os dados

Tanto o código do PCI quanto o código de Richard Cook recebem datasets de recomendações pré-determinados e já estruturados:

Como eu quero que o meu código possa lidar com diferentes datasets, implementei-o de modo a ler os dados em um arquivo .csv e organizá-lo em uma lista associativa (link):


Ponderei algumas opções de estrutura de dados: hash-table, vectors, structs e listas associativas. A última me pareceu a melhor opção, pois vectors tornavam mais complicado acessar as preferências dos diferentes usuários e structs e hash-tables traziam funcionalidades que não seriam utilizadas. 


* Possíveis melhorias e desenvolvimentos

* Conclusão
  

--------------------

* Planning
  
** Fontes

Programming Collective Intelligence, capítulo 2

Isinkayer, F.O.; Folajimi, Y.O.; Ojokoh, B.A. "Recommendation Systems: Principles, methods and evaluation" in: *Egyptian Informatics Journal 16 (261-273)*. Cairo: Elsevier, 2015.

Puglisi, Silvia; Parra-Arnau, Javier; Forné, Jordi; Rebollo-Monedero, David. "On content-based recommendation and user privacy in social-tagging systems" in: *Computer Standards & Interfaces 41 (17-27)*. Barcelona: Elsevier, 2015.

Lecture notes "Recommendation Systems" de Jeff Ullman para o MOOC "Mining Massive Datasets". Disponível em: <http://infolab.stanford.edu/~ullman/mmds/ch9.pdf>.

Ridwan, Mahmud. "Predicting Likes: Inside A Simple Recommendation Engine's Algorithms". Disponível em: <https://www.toptal.com/algorithms/predicting-likes-inside-a-simple-recommendation-engine>.

Quora Feed sobre Recommender Systems. Disponível em: <https://www.quora.com/topic/Recommender-Systems-1>.

** Issues

Robustness – When users can participate in the recommender system, the issue of fraud must be addressed.

** 

Explicit Data Gathering: o dataset vai ser obtido por meio de um questionário aos alunos

Dataset: 
1. Pode ser esparso (nem todos os usuários avaliaram todos os produtos)


Sistema de Rec:
1. Pode ser 1) baseado no conteúdo, 2) colaborativo ou 3) modelagem de fatores latentes

1) Recomendar items ao usuário X similares aos itens que o usuário X avaliou positivamente anteriormente. Vantagens: a) não precisa de dados de outros usuários, b) é útil para usuários com preferências peculiares, c) items impopulares ou novos não são prejudicados pelo sistema. Desvantagens: a) dificuldade em se criar as características apropriadas aos items e às preferências, b) pouca diversidade nas recomendações; c) "cold-start" para os novos usuários.

2)

Precisa de uma métrica de similaridade entre os conjuntos de preferências dos usuários; Capture a intuição de hierarquia de similaridades (e.g. sim(A,C) > sim (A, B)); Saiba lidar com vetores de preferências desiguais;

Opções para a métrica: similaridade jacquartiana, coseno do ângulo entre rA e rB (problema, trata pref. vazias como 0 (sol. normalização das prefs por meio da subtração destas prefs pela média do row)); c a norm. ainda permite a distinção entre "preferências de alta confiança" e as demais. Outra opção: utilizar o conjunto de usuários k que também avaliarem o item i e, a partir daí, fazer recomendação para x (técnica da vizinhança). 



* Notes PCI

1. Usar as preferências de um grupo de pessoas para fazer recomendações a outro grupo de pessoas
2. Noção de items, usuários, avaliações e recomendações
3. Algoritmos de filtragem colaborativa costumam procurar um grande número de pessoas e encontrar um conjunto menor com preferências similares às do usuário x. Ele vai olhar para outros items que eles gostam e combiná-los para criar uma lista hierárquica de sugestões.
4. O primeiro desafio é como representar diferentes pessoas e suas preferências (em python, usa-se nested dictionaries; suponho que em racket o jeito natural seja [nested] hash-tables)
5. O segundo desafio é determinar como as pessoas são similares quanto às suas preferências. A ideia é comparar cada pessoa com cada outra pessoa, calculando um similarity score (há vários modos de se fazer isso: ver notas das lectures acima e mais dois outros métodos: distância euclidiana e correlação de Pearson)
6. O similarity score pode ser utilizado em uma função que hierarquiza os demais users em relação ao user x
7. Score os items por meio de um score ponderado (similarity do user y em relação ao user x * avaliação de y sobre item w)
8. A técnica acima se chama 'user-based collaborative filtering'
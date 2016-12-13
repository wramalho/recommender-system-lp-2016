
* Introdução

* Sobre os dados

* O código

* Resultados

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


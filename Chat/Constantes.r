
## ---------------------------------------------------------- ##
## Tabelas de dados estáticos usadas pelo algoritmo DES.      ##
## Muitas dessas tabelas são baseadas em permutações de bits, ##
## em que o índice da matriz corresponde ao bit de saída e o  ##
## valor indica qual bit da entrada deve ser usado.           ##
## ---------------------------------------------------------- ##

## ########################################################## ##
## ############## Permutações de alto nível ################# ##
## ########################################################## ##

## ---------------------------------------------------------- ##
## Permutação Inicial: o bloco de mensagens é permutado       ##
## por essa permutação no início do algoritmo                 ##
## ---------------------------------------------------------- ##

permIni = c(58, 50, 42, 34, 26, 18, 10,  2,
    60, 52, 44, 36, 28, 20, 12,  4,
    62, 54, 46, 38, 30, 22, 14,  6,
    64, 56, 48, 40, 32, 24, 16,  8,
    57, 49, 41, 33, 25, 17,  9,  1,
    59, 51, 43, 35, 27, 19, 11,  3,
    61, 53, 45, 37, 29, 21, 13,  5,
    63, 55, 47, 39, 31, 23, 15,  7)

## ---------------------------------------------------------- ##
## Permutação Final: o resultado final é permutado por essa   ##
## permutação para gerar o bloco final de texto cifrado       ##
## ---------------------------------------------------------- ##

permFim = c(40,  8, 48, 16, 56, 24, 64, 32,
    39,  7, 47, 15, 55, 23, 63, 31,
    38,  6, 46, 14, 54, 22, 62, 30,
    37,  5, 45, 13, 53, 21, 61, 29,
    36,  4, 44, 12, 52, 20, 60, 28,
    35,  3, 43, 11, 51, 19, 59, 27,
    34,  2, 42, 10, 50, 18, 58, 26,
    33,  1, 41,  9, 49, 17, 57, 25)

## ########################################################## ##
## ############## Permutações de alto nível ################# ##
## ########################################################## ##

## ---------------------------------------------------------- ##
## Permutação de Expansão: a função Feistel começa aplicando  ##
## essa permutação em seu meio bloco de entrada de 32 bits    ##
## para criar um valor "expandido" de 48 bits.                ##
## ---------------------------------------------------------- ##

expPerm = c(
    32,  1,  2,  3,  4,  5,
     4,  5,  6,  7,  8,  9, 
     8,  9, 10, 11, 12, 13,
    12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21,
    20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29,
    28, 29, 30, 31, 32,  1
)

## ---------------------------------------------------------- ##
## Caixas de Substituição: uma etapa crucial na função        ##
## Feistel é executar substituições de bits de acordo com     ##
## esta tabela. Um valor de 48 bits é divido em seções de 6   ##
## bits e cada seção é permutada em um valor de 6 bits        ##
## diferente, de acordo com essas oito tabelas.               ##
## (Uma tabela para cada seção)                               ##
## ---------------------------------------------------------- ##

sBox1l0 = c(14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7)
sBox1l1 = c( 0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8)
sBox1l2 = c( 4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0)
sBox1l3 = c(15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13)

sBox1 = list()
sBox1 = c(sBox1, list(sBox1l0, sBox1l1, sBox1l2, sBox1l3))

sBox2l0 = c(15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10)
sBox2l1 = c( 3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5)
sBox2l2 = c( 0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15)
sBox2l3 = c(13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9)

sBox2 = list()
sBox2 = c(sBox2, list(sBox2l0, sBox2l1, sBox2l2, sBox2l3))

sBox3l0 = c(10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8)
sBox3l1 = c(13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1)
sBox3l2 = c(13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7)
sBox3l3 = c( 1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12)

sBox3 = list()
sBox3 = c(sBox3, list(sBox3l0, sBox3l1, sBox3l2, sBox3l3))

sBox4l0 = c( 7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15)
sBox4l1 = c(13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9)
sBox4l2 = c(10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4)
sBox4l3 = c( 3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14)

sBox4 = list()
sBox4 = c(sBox4, list(sBox4l0, sBox4l1, sBox4l2, sBox4l3))

sBox5l0 = c( 2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9)
sBox5l1 = c(14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6)
sBox5l2 = c( 4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14)
sBox5l3 = c(11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3)

sBox5 = list()
sBox5 = c(sBox5, list(sBox5l0, sBox5l1, sBox5l2, sBox5l3))

sBox6l0 = c(12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11)
sBox6l1 = c(10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8)
sBox6l2 = c( 9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6)
sBox6l3 = c( 4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13)

sBox6 = list()
sBox6 = c(sBox6, list(sBox6l0, sBox6l1, sBox6l2, sBox6l3))

sBox7l0 = c( 4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1)
sBox7l1 = c(13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6)
sBox7l2 = c( 1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2)
sBox7l3 = c( 6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12)

sBox7 = list()
sBox7 = c(sBox7, list(sBox7l0, sBox7l1, sBox7l2, sBox7l3))

sBox8l0 = c(13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7)
sBox8l1 = c( 1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2)
sBox8l2 = c( 7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8)
sBox8l3 = c( 2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11)

sBox8 = list()
sBox8 = c(sBox8, list(sBox8l0, sBox8l1, sBox8l2, sBox8l3))

sBox = list()
sBox = c(sBox, list(sBox1, sBox2, sBox3, sBox4, sBox5, sBox6, sBox7, sBox8))

rm(sBox1l0, sBox1l1, sBox1l2, sBox1l3)
rm(sBox2l0, sBox2l1, sBox2l2, sBox2l3)
rm(sBox3l0, sBox3l1, sBox3l2, sBox3l3)
rm(sBox4l0, sBox4l1, sBox4l2, sBox4l3)
rm(sBox5l0, sBox5l1, sBox5l2, sBox5l3)
rm(sBox6l0, sBox6l1, sBox6l2, sBox6l3)
rm(sBox7l0, sBox7l1, sBox7l2, sBox7l3)
rm(sBox8l0, sBox8l1, sBox8l2, sBox8l3)
rm(sBox1, sBox2, sBox3, sBox4, sBox5, sBox6, sBox7, sBox8)

## rm -> remove objetos
## sBox[[1]][[1]][[1]] -> pega o primeiro elemento da primeira lista que está
## dentro da primeira lista

## ---------------------------------------------------------- ##
## Permutação "P": a função Feistel termina aplicando essa    ##
## permutação de 32 bits ao resultado da substituição da      ##
## caixa S, a fim de distribuir os bits de saída por 6 caixas ##
## S diferentes na próxima rodada
## ---------------------------------------------------------- ##

pBox = c(
    16,  7, 20, 21, 
    29, 12, 28, 17, 
     1, 15, 23, 26, 
     5, 18, 31, 10,
     2,  8, 24, 14, 
    32, 27,  3,  9,
    19, 13, 30,  6, 
    22, 11,  4, 25)

## ########################################################## ##
## #### Permutações relacionadas à geração de subchave ###### ##
## ########################################################## ##

## ---------------------------------------------------------- ##
## Permutação "PC1": a chave de 64 bits fornecida é permutada ##
## de acordo com esta tabela em uma chave de 56 bits. É por   ##
## isso que o DES é apenas um algoritmo de 56 bits, mesmo que ##
## você forneça 64 bits do material principal                 ##
## ---------------------------------------------------------- ##

pc1 = c(
   57, 49, 41, 33, 25, 17,  9,
    1, 58, 50, 42, 34, 26, 18,
   10,  2, 59, 51, 43, 35, 27,
   19, 11,  3, 60, 52, 44, 36,
   63, 55, 47, 39, 31, 23, 15,
    7, 62, 54, 46, 38, 30, 22,
   14,  6, 61, 53, 45, 37, 29,
   21, 13,  5, 28, 20, 12,  4)

## ---------------------------------------------------------- ##
## Permutação "PC2": o processo de geração de subchae aplica  ##
## essa permutação para transformar seu valor de chave de     ##
## 56 bits em execução no conjunto final de 16 subchaves      ##
## de 48 bits                                                 ##
## ---------------------------------------------------------- ##

pc2 = c(
   14, 17, 11, 24,  1,  5,
    3, 28, 15,  6, 21, 10,
   23, 19, 12,  4, 26,  8,
   16,  7, 27, 20, 13,  2,
   41, 52, 31, 37, 47, 55,
   30, 40, 51, 45, 33, 48,
   44, 49, 39, 56, 34, 53,
   46, 42, 50, 36, 29, 32)

## ---------------------------------------------------------- ##
## Subchave de Rotação: parte do processo de geração de       ##
## subchave envolve rotacionar certas seções de bits do       ##
## material principal por um ou dois bits para esquerda. Esta ##
## tabela especifica quantos bits devem ser rotacionados para ##
## a esquerda para uma das 16 etapas                          ##
## ---------------------------------------------------------- ##

shifts = c(1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1)

## ---------------------------------------------------------- ##
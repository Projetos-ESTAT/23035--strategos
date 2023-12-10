source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

####### importando banco #######

banco <- read_excel("banco/strategos - senado.xlsx")

## ordem labels
ordem_5 <- c("Discordo \nTotalmente","Discordo \nParcialmente","Neutro",
             "Concordo \nParcialmente","Concordo \nTotalmente")
#ordem_5 <- c("Concordo \nTotalmente","Concordo \nParcialmente","Neutro",
#             "Discordo \nParcialmente","Discordo \nTotalmente")

####### análise 1 - Devemos investir em educação para prevenção do uso problemático de drogas #######

an1 <- banco %>% 
  select(`Partido/Federação política (disposto na planilha)`,
         `Devemos investir em educação para prevenção do uso problemático de drogas
`) %>% mutate(
  "Devemos investir em educação para prevenção do uso problemático de drogas" = case_when(
    `Devemos investir em educação para prevenção do uso problemático de drogas
` == 1 ~ "Discordo \nTotalmente",
    `Devemos investir em educação para prevenção do uso problemático de drogas
` == 2 ~ "Discordo \nParcialmente",
    `Devemos investir em educação para prevenção do uso problemático de drogas
` == 3 ~ "Neutro",
    `Devemos investir em educação para prevenção do uso problemático de drogas
` == 4 ~ "Concordo \nParcialmente",
    `Devemos investir em educação para prevenção do uso problemático de drogas
` == 5 ~ "Concordo \nTotalmente"
  )
)

## tabela de contingência

round(
  prop.table(
    table(factor(an1$`Partido/Federação política (disposto na planilha)`), 
          factor(an1$`Devemos investir em educação para prevenção do uso problemático de drogas`, levels = ordem_5)), 1), 4)*100

table(factor(an1$`Partido/Federação política (disposto na planilha)`), 
      factor(an1$`Devemos investir em educação para prevenção do uso problemático de drogas`, levels = ordem_5))


# gráfico de colunas empilhadas -----

## manipulações para o gráfico
an1 <- as.data.frame(table(
  an1$`Partido/Federação política (disposto na planilha)`, 
  an1$`Devemos investir em educação para prevenção do uso problemático de drogas`))

ggplot(an1, 
       aes(x = Var1, 
           y = Freq, 
           fill = factor(Var2, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Partido/Federação política", y = "Frequência") +
  theme_estat() +
  scale_fill_manual(values =  cores_estat, name = "Devemos investir em educação para \nprevenção do uso problemático de drogas")+
  scale_x_discrete(
    labels=c("MDB", "NOVO", "PDT", "PL", "PODEMOS", "PP", "PSD", "PSDB/\nCIDADANIA", "PT/PV/\nPCdoB", "REPUBLI-\nCANOS", "UNIÃO")
  ) + 
  theme(axis.text = ggplot2::element_text(colour = "black", size = 8),
        text = element_text(family = "sans", size = 10))
#ggsave("colunas-bi-1.pdf", width = 158, height = 93, units = "mm")

####### análise 2 - Devemos adotar políticas sociais para pessoas que usam drogas #######

an2 <- banco %>% 
  select(`Partido/Federação política (disposto na planilha)`,
         `Devemos adotar políticas sociais para pessoas que usam drogas
`) %>% mutate(
           "Devemos adotar políticas sociais para pessoas que usam drogas" = case_when(
              `Devemos adotar políticas sociais para pessoas que usam drogas
` == 1 ~ "Discordo \nTotalmente",
              `Devemos adotar políticas sociais para pessoas que usam drogas
` == 2 ~ "Discordo \nParcialmente",
              `Devemos adotar políticas sociais para pessoas que usam drogas
` == 3 ~ "Neutro",
              `Devemos adotar políticas sociais para pessoas que usam drogas
` == 4 ~ "Concordo \nParcialmente",
              `Devemos adotar políticas sociais para pessoas que usam drogas
` == 5 ~ "Concordo \nTotalmente"
  )
)

## tabela de contingência

round(
  prop.table(
    table(factor(an2$`Partido/Federação política (disposto na planilha)`), 
          factor(an2$`Devemos adotar políticas sociais para pessoas que usam drogas`, levels = ordem_5)), 1), 4)*100

table(factor(an2$`Partido/Federação política (disposto na planilha)`), 
      factor(an2$`Devemos adotar políticas sociais para pessoas que usam drogas`, levels = ordem_5))


# gráfico de colunas empilhadas -----

## manipulações para o gráfico
an2 <- as.data.frame(table(
  an2$`Partido/Federação política (disposto na planilha)`, 
  an2$`Devemos adotar políticas sociais para pessoas que usam drogas`))

ggplot(an2, 
       aes(x = Var1, 
           y = Freq, 
           fill = factor(Var2, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Partido/Federação política", y = "Frequência") +
  theme_estat() +
  scale_fill_manual(values =  cores_estat, name = "Devemos adotar políticas sociais \npara pessoas que usam drogas")+
  scale_x_discrete(
    labels=c("MDB", "NOVO", "PDT", "PL", "PODEMOS", "PP", "PSD", "PSDB/\nCIDADANIA", "PT/PV/\nPCdoB", "REPUBLI-\nCANOS", "UNIÃO")
  ) + 
  theme(axis.text = ggplot2::element_text(colour = "black", size = 8),
        text = element_text(family = "sans", size = 10))
#ggsave("colunas-bi-2.pdf", width = 158, height = 93, units = "mm")


####### análise 3 - Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado #######

an3 <- banco %>% 
  select(`Partido/Federação política (disposto na planilha)`,
         `Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado
`) %>% mutate(
  "Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado" = case_when( 
    `Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado
` == 1 ~ "Discordo \nTotalmente",
    `Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado
` == 2 ~ "Discordo \nParcialmente",
    `Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado
` == 3 ~ "Neutro",
    `Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado
` == 4 ~ "Concordo \nParcialmente",
    `Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado
` == 5 ~ "Concordo \nTotalmente"
  )
)

## tabela de contingência

round(
  prop.table(
    table(factor(an3$`Partido/Federação política (disposto na planilha)`), 
          factor(an3$`Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado`, levels = ordem_5)), 1), 4)*100

table(factor(an3$`Partido/Federação política (disposto na planilha)`), 
      factor(an3$`Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado`, levels = ordem_5))


# gráfico de colunas empilhadas -----

## manipulações para o gráfico
an3 <- as.data.frame(table(
  an3$`Partido/Federação política (disposto na planilha)`, 
  an3$`Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado`))

ggplot(an3, 
       aes(x = Var1, 
           y = Freq, 
           fill = factor(Var2, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Partido/Federação política", y = "Frequência") +
  theme_estat() +
  scale_fill_manual(values =  cores_estat, 
                    name = "Pessoas que tenham problemas com \nuso de drogas devem ter o direito ao \ntratamento financiado pelo Estado")+
  scale_x_discrete(
    labels=c("MDB", "NOVO", "PDT", "PL", "PODEMOS", "PP", "PSD", "PSDB/\nCIDADANIA", "PT/PV/\nPCdoB", "REPUBLI-\nCANOS", "UNIÃO")
  ) + 
  guides(fill=guide_legend(ncol=2, byrow=TRUE)) +
  theme(axis.text = ggplot2::element_text(colour = "black", size = 8),
        text = element_text(family = "sans", size = 10))
#ggsave("colunas-bi-3.pdf", width = 158, height = 93, units = "mm")


####### análise 4 - Cada tipo de droga deve ter uma regulação específica definida pelo Estado #######

an4 <- banco %>% 
  select(`Partido/Federação política (disposto na planilha)`,
         `Cada tipo de droga deve ter uma regulação específica definida pelo Estado
`) %>% mutate(
  "Cada tipo de droga deve ter uma regulação específica definida pelo Estado" = case_when( 
    `Cada tipo de droga deve ter uma regulação específica definida pelo Estado
` == 1 ~ "Discordo \nTotalmente",
    `Cada tipo de droga deve ter uma regulação específica definida pelo Estado
` == 2 ~ "Discordo \nParcialmente",
    `Cada tipo de droga deve ter uma regulação específica definida pelo Estado
` == 3 ~ "Neutro",
    `Cada tipo de droga deve ter uma regulação específica definida pelo Estado
` == 4 ~ "Concordo \nParcialmente",
    `Cada tipo de droga deve ter uma regulação específica definida pelo Estado
` == 5 ~ "Concordo \nTotalmente"
  )
)

## tabela de contingência

round(
  prop.table(
    table(factor(an4$`Partido/Federação política (disposto na planilha)`), 
          factor(an4$`Cada tipo de droga deve ter uma regulação específica definida pelo Estado`, levels = ordem_5)), 1), 4)*100

table(factor(an4$`Partido/Federação política (disposto na planilha)`), 
      factor(an4$`Cada tipo de droga deve ter uma regulação específica definida pelo Estado`, levels = ordem_5))


# gráfico de colunas empilhadas -----

## manipulações para o gráfico
an4 <- as.data.frame(table(
  an4$`Partido/Federação política (disposto na planilha)`, 
  an4$`Cada tipo de droga deve ter uma regulação específica definida pelo Estado`))

ggplot(an4, 
       aes(x = Var1, 
           y = Freq, 
           fill = factor(Var2, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Partido/Federação política", y = "Frequência") +
  theme_estat() +
  scale_fill_manual(values =  cores_estat, 
                    name = "Cada tipo de droga deve ter uma regulação \nespecífica definida pelo Estado")+
  scale_x_discrete(
    labels=c("MDB", "NOVO", "PDT", "PL", "PODEMOS", "PP", "PSD", "PSDB/\nCIDADANIA", "PT/PV/\nPCdoB", "REPUBLI-\nCANOS", "UNIÃO")
  ) + 
  guides(fill=guide_legend(ncol=2, byrow=TRUE)) +
  theme(axis.text = ggplot2::element_text(colour = "black", size = 8),
        text = element_text(family = "sans", size = 10))
#ggsave("colunas-bi-4.pdf", width = 158, height = 93, units = "mm")


####### análise 5 - Pequenos e grandes traficantes devem ter penas diferentes #######

an5 <- banco %>% 
  select(`Partido/Federação política (disposto na planilha)`,
         `Pequenos e grandes traficantes devem ter penas diferentes
`) %>% mutate(
  "Pequenos e grandes traficantes devem ter penas diferentes" = case_when( 
    `Pequenos e grandes traficantes devem ter penas diferentes
` == 1 ~ "Discordo \nTotalmente",
    `Pequenos e grandes traficantes devem ter penas diferentes
` == 2 ~ "Discordo \nParcialmente",
    `Pequenos e grandes traficantes devem ter penas diferentes
` == 3 ~ "Neutro",
    `Pequenos e grandes traficantes devem ter penas diferentes
` == 4 ~ "Concordo \nParcialmente",
    `Pequenos e grandes traficantes devem ter penas diferentes
` == 5 ~ "Concordo \nTotalmente"
  )
)

## tabela de contingência

round(
  prop.table(
    table(factor(an5$`Partido/Federação política (disposto na planilha)`), 
          factor(an5$`Pequenos e grandes traficantes devem ter penas diferentes`, levels = ordem_5)), 1), 4)*100

table(factor(an5$`Partido/Federação política (disposto na planilha)`), 
      factor(an5$`Pequenos e grandes traficantes devem ter penas diferentes`, levels = ordem_5))


# gráfico de colunas empilhadas -----

## manipulações para o gráfico
an5 <- as.data.frame(table(
  an5$`Partido/Federação política (disposto na planilha)`, 
  an5$`Pequenos e grandes traficantes devem ter penas diferentes`))

ggplot(an5, 
       aes(x = Var1, 
           y = Freq, 
           fill = factor(Var2, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Partido/Federação política", y = "Frequência") +
  theme_estat() +
  scale_fill_manual(values =  cores_estat, 
                    name = "Pequenos e grandes traficantes \ndevem ter penas diferentes")+
  scale_x_discrete(
    labels=c("MDB", "NOVO", "PDT", "PL", "PODEMOS", "PP", "PSD", "PSDB/\nCIDADANIA", "PT/PV/\nPCdoB", "REPUBLI-\nCANOS", "UNIÃO")
  ) + 
  guides(fill=guide_legend(ncol=3, byrow=TRUE)) +
  theme(axis.text = ggplot2::element_text(colour = "black", size = 8),
        text = element_text(family = "sans", size = 10))
#ggsave("colunas-bi-5.pdf", width = 158, height = 93, units = "mm")

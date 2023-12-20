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

####################### BANCO SENADO ####################### 

####### importando banco #######

banco <- read_excel("banco/strategos - senado.xlsx")

## ordem labels
ordem_5 <- c("Discordo \nTotalmente","Discordo \nParcialmente","Neutro",
             "Concordo \nParcialmente","Concordo \nTotalmente")
#ordem_5 <- c("Concordo \nTotalmente","Concordo \nParcialmente","Neutro",
#             "Discordo \nParcialmente","Discordo \nTotalmente")

####### análise 2 #######
# referente às afirmativas:
# Devemos investir em educação para prevenção do uso problemático de drogas
# Devemos adotar políticas sociais para pessoas que usam drogas
# Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado
# Cada tipo de droga deve ter uma regulação específica definida pelo Estado
# Pequenos e grandes traficantes devem ter penas diferentes

an2 <- banco[,c(2:5,8)]

an2 <- an2 %>% 
  reshape2::melt(variable.name = "afirmativas", value.name = "respostas")

an2 <- an2 %>%  mutate( "respostas" = case_when( 
  respostas == 1 ~ "Discordo \nTotalmente",
  respostas == 2 ~ "Discordo \nParcialmente",
  respostas == 3 ~ "Neutro",
  respostas == 4 ~ "Concordo \nParcialmente",
  respostas == 5 ~ "Concordo \nTotalmente"
)
)

## tabela com frequencias e porcentagens
table(factor(an2$afirmativas), 
      factor(an2$respostas, levels = ordem_5))

round(
  prop.table(
    table(factor(an2$afirmativas), 
          factor(an2$respostas, levels = ordem_5)), 1), 4)*100

## gráfico
an2 <- an2 %>%
  group_by(afirmativas, respostas) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

ggplot(an2, aes(x = afirmativas, y = freq, fill = factor(respostas, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Afirmativas", y = "Frequência relativa") +
  scale_x_discrete(labels = wrap_format(30)) +
  #guides(fill=guide_legend(title="Proficiência")) +
  theme_estat() +
  scale_fill_manual(name = "Respostas", values =  cores_estat) +
  theme(legend.position = "right") +
  coord_flip()
#ggsave("colunas-an2.pdf", width = 158, height = 93, units = "mm")

####### análise 4 #######
# referente às afirmativas:
# Deve-se regulamentar a produção e o comércio das drogas hoje ilícitas para diminuir o poder do tráfico
# Deve-se regulamentar a maconha e tributar seu comércio para arrecadação de impostos
# As drogas ilícitas devem ser legalizadas para proteger a população negra da violência

an4 <- banco[,c(12:14)]

an4 <- an4 %>% 
  reshape2::melt(variable.name = "afirmativas", value.name = "respostas")

an4 <- an4 %>%  mutate( "respostas" = case_when( 
  respostas == 1 ~ "Discordo \nTotalmente",
  respostas == 2 ~ "Discordo \nParcialmente",
  respostas == 3 ~ "Neutro",
  respostas == 4 ~ "Concordo \nParcialmente",
  respostas == 5 ~ "Concordo \nTotalmente"
)
)

## tabela com frequencias e porcentagens
table(factor(an4$afirmativas), 
      factor(an4$respostas, levels = ordem_5))

round(
  prop.table(
    table(factor(an4$afirmativas), 
          factor(an4$respostas, levels = ordem_5)), 1), 4)*100

## gráfico
an4 <- an4 %>%
  group_by(afirmativas, respostas) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

ggplot(an4, aes(x = afirmativas, y = freq, fill = factor(respostas, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Afirmativas", y = "Frequência relativa") +
  scale_x_discrete(labels = wrap_format(30)) +
  #guides(fill=guide_legend(title="Proficiência")) +
  theme_estat() +
  scale_fill_manual(name = "Respostas", values =  cores_estat) +
  theme(legend.position = "right") +
  coord_flip()
#ggsave("colunas-an4.pdf", width = 158, height = 93, units = "mm")


####### análise 5 #######
# referente às afirmativas:
# Forças policiais e o sistema penal seriam mais eficientes se as drogas fossem legalizadas
# A violência que resulta da proibição das drogas afeta mais as populações negras e pobres
# A proibição das drogas não causa impacto ambiental 
# A proibição das drogas aumenta a violência entre populações indígenas, quilombolas, ribeirinhas

an5 <- banco[,c(21:24)]

an5 <- an5 %>% 
  reshape2::melt(variable.name = "afirmativas", value.name = "respostas")

an5 <- an5 %>%  mutate( "respostas" = case_when( 
  respostas == 1 ~ "Discordo \nTotalmente",
  respostas == 2 ~ "Discordo \nParcialmente",
  respostas == 3 ~ "Neutro",
  respostas == 4 ~ "Concordo \nParcialmente",
  respostas == 5 ~ "Concordo \nTotalmente"
)
)

## tabela com frequencias e porcentagens
table(factor(an5$afirmativas), 
      factor(an5$respostas, levels = ordem_5))

round(
  prop.table(
    table(factor(an5$afirmativas), 
          factor(an5$respostas, levels = ordem_5)), 1), 4)*100

## gráfico
an5 <- an5 %>%
  group_by(afirmativas, respostas) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

ggplot(an5, aes(x = afirmativas, y = freq, fill = factor(respostas, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Afirmativas", y = "Frequência relativa") +
  scale_x_discrete(labels = wrap_format(30)) +
  #guides(fill=guide_legend(title="Proficiência")) +
  theme_estat() +
  scale_fill_manual(name = "Respostas", values =  cores_estat) +
  theme(legend.position = "right") +
  #theme(axis.text=element_text(size=8), legend.text = element_text(size=8),legend.title=element_text(size=8.5),
  #      axis.title.y=element_text(size=9), axis.title.x=element_text(size=9)) +
  coord_flip()
#ggsave("colunas-an5.pdf", width = 158, height = 93, units = "mm")

####### análise 6 #######
# referente às afirmativas:
# Populações impactadas pela proibição das drogas devem ser prioritariamente beneficiadas nos modelos de regulação
# Recursos da tributação de drogas ilegais, como maconha, devem prioritariamente beneficiar pessoas e comunidades afetadas pela violência e prisões relacionadas às drogas.
# Se a maconha for legalizada, pessoas que já foram presas por conta da proibição devem ter prioridade no licenciamento para produção e comercialização de produtos à base da substância.

an6 <- banco[,c(25:27)]

an6 <- an6 %>% 
  reshape2::melt(variable.name = "afirmativas", value.name = "respostas")

an6 <- an6 %>%  mutate( "respostas" = case_when( 
  respostas == 1 ~ "Discordo \nTotalmente",
  respostas == 2 ~ "Discordo \nParcialmente",
  respostas == 3 ~ "Neutro",
  respostas == 4 ~ "Concordo \nParcialmente",
  respostas == 5 ~ "Concordo \nTotalmente"
)
)

## tabela com frequencias e porcentagens
table(factor(an6$afirmativas), 
      factor(an6$respostas, levels = ordem_5))

round(
  prop.table(
    table(factor(an6$afirmativas), 
          factor(an6$respostas, levels = ordem_5)), 1), 4)*100

## gráfico
an6 <- an6 %>%
  group_by(afirmativas, respostas) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

ggplot(an6, aes(x = afirmativas, y = freq, fill = factor(respostas, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Afirmativas", y = "Frequência relativa") +
  scale_x_discrete(labels = wrap_format(33)) +
  #guides(fill=guide_legend(title="Proficiência")) +
  theme_estat() +
  scale_fill_manual(name = "Respostas", values =  cores_estat) +
  theme(legend.position = "right") +
  coord_flip()
#ggsave("colunas-an6.pdf", width = 158, height = 93, units = "mm")

####################### BANCO CÂMARA ####################### 


####### importando banco #######

df <- read_excel("banco/Câmara.xlsx")

## ordem labels
ordem_5 <- c("Discordo \nTotalmente","Discordo \nParcialmente","Neutro",
             "Concordo \nParcialmente","Concordo \nTotalmente")

####### análise 2 #######
# referente às afirmativas:
# Devemos investir em educação para prevenção do uso problemático de drogas
# Devemos adotar políticas sociais para pessoas que usam drogas
# Pessoas que tenham problemas com uso de drogas devem ter o direito ao tratamento financiado pelo Estado
# Cada tipo de droga deve ter uma regulação específica definida pelo Estado
# Pequenos e grandes traficantes devem ter penas diferentes

an2 <- df[,c(1:4,7)]

an2 <- an2 %>% 
  reshape2::melt(variable.name = "afirmativas", value.name = "respostas")

an2 <- an2 %>%  mutate( "respostas" = case_when( 
  respostas == 1 ~ "Discordo \nTotalmente",
  respostas == 2 ~ "Discordo \nParcialmente",
  respostas == 3 ~ "Neutro",
  respostas == 4 ~ "Concordo \nParcialmente",
  respostas == 5 ~ "Concordo \nTotalmente"
)
)

## tabela com frequencias e porcentagens
table(factor(an2$afirmativas), 
      factor(an2$respostas, levels = ordem_5))

round(
  prop.table(
    table(factor(an2$afirmativas), 
          factor(an2$respostas, levels = ordem_5)), 1), 4)*100

## gráfico
an2 <- an2 %>%
  na.omit() %>% 
  group_by(afirmativas, respostas) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

ggplot(an2, aes(x = afirmativas, y = freq, fill = factor(respostas, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Afirmativas", y = "Frequência relativa") +
  scale_x_discrete(labels = wrap_format(30)) +
  #guides(fill=guide_legend(title="Proficiência")) +
  theme_estat() +
  scale_fill_manual(name = "Respostas", values =  cores_estat) +
  theme(legend.position = "right") +
  coord_flip()
#ggsave("colunas-an2.pdf", width = 158, height = 93, units = "mm")

####### análise 3 #######
# referente às afirmativas:
# Adultos devem ter o direito de usar drogas mesmo que, com isso, corram riscos
# A proibição causa mais danos à saúde pública do que o uso de drogas em si
# Pacientes devem ter o direito de plantar cannabis para produzir seu próprio tratamento
# Associações de pacientes devem ter o direito de plantar cannabis para produzir o tratamento para seus associados
# O tratamento com psicodélicos que tiverem efeitos terapêuticos comprovados devem ser permitidos

an3 <- df[,c(5:6,8:10)]

an3 <- an3 %>% 
  reshape2::melt(variable.name = "afirmativas", value.name = "respostas")

an3 <- an3 %>%  mutate( "respostas" = case_when( 
  respostas == 1 ~ "Discordo \nTotalmente",
  respostas == 2 ~ "Discordo \nParcialmente",
  respostas == 3 ~ "Neutro",
  respostas == 4 ~ "Concordo \nParcialmente",
  respostas == 5 ~ "Concordo \nTotalmente"
)
)

## tabela com frequencias e porcentagens
table(factor(an3$afirmativas), 
      factor(an3$respostas, levels = ordem_5))

round(
  prop.table(
    table(factor(an3$afirmativas), 
          factor(an3$respostas, levels = ordem_5)), 1), 4)*100

## gráfico
an3 <- an3 %>%
  na.omit() %>% 
  group_by(afirmativas, respostas) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

ggplot(an3, aes(x = afirmativas, y = freq, fill = factor(respostas, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Afirmativas", y = "Frequência relativa") +
  scale_x_discrete(labels = wrap_format(40)) +
  #guides(fill=guide_legend(title="Proficiência")) +
  theme_estat() +
  scale_fill_manual(name = "Respostas", values =  cores_estat) +
  theme(legend.position = "right") +
  coord_flip()
#ggsave("colunas-an3.pdf", width = 158, height = 93, units = "mm")

####### análise 4 #######
# referente às afirmativas:
# Deve-se regulamentar a produção e o comércio das drogas hoje ilícitas para diminuir o poder do tráfico
# Deve-se regulamentar a maconha e tributar seu comércio para arrecadação de impostos
# As drogas ilícitas devem ser legalizadas para proteger a população negra da violência

an4 <- df[,c(11:13)]

an4 <- an4 %>% 
  reshape2::melt(variable.name = "afirmativas", value.name = "respostas")

an4 <- an4 %>%  mutate( "respostas" = case_when( 
  respostas == 1 ~ "Discordo \nTotalmente",
  respostas == 2 ~ "Discordo \nParcialmente",
  respostas == 3 ~ "Neutro",
  respostas == 4 ~ "Concordo \nParcialmente",
  respostas == 5 ~ "Concordo \nTotalmente"
)
)

## tabela com frequencias e porcentagens
table(factor(an4$afirmativas), 
      factor(an4$respostas, levels = ordem_5))

round(
  prop.table(
    table(factor(an4$afirmativas), 
          factor(an4$respostas, levels = ordem_5)), 1), 4)*100

## gráfico
an4 <- an4 %>%
  na.omit() %>% 
  group_by(afirmativas, respostas) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

ggplot(an4, aes(x = afirmativas, y = freq, fill = factor(respostas, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Afirmativas", y = "Frequência relativa") +
  scale_x_discrete(labels = wrap_format(30)) +
  #guides(fill=guide_legend(title="Proficiência")) +
  theme_estat() +
  scale_fill_manual(name = "Respostas", values =  cores_estat) +
  theme(legend.position = "right") +
  coord_flip()
#ggsave("colunas-an4.pdf", width = 158, height = 93, units = "mm")

####### análise 5 #######
# referente às afirmativas:
# Forças policiais e o sistema penal seriam mais eficientes se as drogas fossem legalizadas
# A violência que resulta da proibição das drogas afeta mais as populações negras e pobres
# A proibição das drogas não causa impacto ambiental 
# A proibição das drogas aumenta a violência entre populações indígenas, quilombolas, ribeirinhas

an5 <- df[,c(14:17)]

an5 <- an5 %>% 
  reshape2::melt(variable.name = "afirmativas", value.name = "respostas")

an5 <- an5 %>%  mutate( "respostas" = case_when( 
  respostas == 1 ~ "Discordo \nTotalmente",
  respostas == 2 ~ "Discordo \nParcialmente",
  respostas == 3 ~ "Neutro",
  respostas == 4 ~ "Concordo \nParcialmente",
  respostas == 5 ~ "Concordo \nTotalmente"
)
)

## tabela com frequencias e porcentagens
table(factor(an5$afirmativas), 
      factor(an5$respostas, levels = ordem_5))

round(
  prop.table(
    table(factor(an5$afirmativas), 
          factor(an5$respostas, levels = ordem_5)), 1), 4)*100

## gráfico
an5 <- an5 %>%
  na.omit() %>% 
  group_by(afirmativas, respostas) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

ggplot(an5, aes(x = afirmativas, y = freq, fill = factor(respostas, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Afirmativas", y = "Frequência relativa") +
  scale_x_discrete(labels = wrap_format(30)) +
  #guides(fill=guide_legend(title="Proficiência")) +
  theme_estat() +
  scale_fill_manual(name = "Respostas", values =  cores_estat) +
  theme(legend.position = "right") +
  #theme(axis.text=element_text(size=8), legend.text = element_text(size=8),legend.title=element_text(size=8.5),
  #      axis.title.y=element_text(size=9), axis.title.x=element_text(size=9)) +
  coord_flip()
#ggsave("colunas-an5.pdf", width = 158, height = 93, units = "mm")

####### análise 6 #######
# referente às afirmativas:
# Populações impactadas pela proibição das drogas devem ser prioritariamente beneficiadas nos modelos de regulação
# Recursos da tributação de drogas ilegais, como maconha, devem prioritariamente beneficiar pessoas e comunidades afetadas pela violência e prisões relacionadas às drogas.
# Se a maconha for legalizada, pessoas que já foram presas por conta da proibição devem ter prioridade no licenciamento para produção e comercialização de produtos à base da substância.

an6 <- df[,c(18:20)]

an6 <- an6 %>% 
  reshape2::melt(variable.name = "afirmativas", value.name = "respostas")

an6 <- an6 %>%  mutate( "respostas" = case_when( 
  respostas == 1 ~ "Discordo \nTotalmente",
  respostas == 2 ~ "Discordo \nParcialmente",
  respostas == 3 ~ "Neutro",
  respostas == 4 ~ "Concordo \nParcialmente",
  respostas == 5 ~ "Concordo \nTotalmente"
)
)

## tabela com frequencias e porcentagens
table(factor(an6$afirmativas), 
      factor(an6$respostas, levels = ordem_5))

round(
  prop.table(
    table(factor(an6$afirmativas), 
          factor(an6$respostas, levels = ordem_5)), 1), 4)*100

## gráfico
an6 <- an6 %>%
  na.omit() %>% 
  group_by(afirmativas, respostas) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = freq %>% percent()
  )

ggplot(an6, aes(x = afirmativas, y = freq, fill = factor(respostas, levels = ordem_5))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(x = "Afirmativas", y = "Frequência relativa") +
  scale_x_discrete(labels = wrap_format(33)) +
  #guides(fill=guide_legend(title="Proficiência")) +
  theme_estat() +
  scale_fill_manual(name = "Respostas", values =  cores_estat) +
  theme(legend.position = "right") +
  coord_flip()
#ggsave("colunas-an6.pdf", width = 158, height = 93, units = "mm")


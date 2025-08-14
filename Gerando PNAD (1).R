library(PNADcIBGE)
library(dplyr)
library(stringr)

clean = function(data)
{# Start: limpando os dados

  empreendedores1 = data

  x = base::data.frame(empreendedores1$pweights)

  x1 = base::data.frame(empreendedores1$variables)

  x1$weight = x$empreendedores1.pweights

  x1 = x1 %>%
    dplyr::mutate(
      empreendedor = dplyr::case_when(
        V4012 %in% c("Empregador", "Conta pr�pria") ~ "Empreendedor",
        TRUE ~ NA
      )
    ) %>%
    dplyr::filter(!is.na(empreendedor)) %>%
    dplyr::select(
      -c(
        #Pesos Replicados
        "V1028001", "V1028002", "V1028003", "V1028004", "V1028005", "V1028006",
        "V1028007", "V1028008", "V1028009", "V1028010", "V1028011", "V1028012",
        "V1028013", "V1028014", "V1028015", "V1028016", "V1028017", "V1028018",
        "V1028019", "V1028020", "V1028021", "V1028022", "V1028023", "V1028024",
        "V1028025", "V1028026", "V1028027", "V1028028", "V1028029", "V1028030",
        "V1028031", "V1028032", "V1028033", "V1028034", "V1028035", "V1028036",
        "V1028037", "V1028038", "V1028039", "V1028040", "V1028041", "V1028042",
        "V1028043", "V1028044", "V1028045", "V1028046", "V1028047", "V1028048",
        "V1028049", "V1028050", "V1028051", "V1028052", "V1028053", "V1028054",
        "V1028055", "V1028056", "V1028057", "V1028058", "V1028059", "V1028060",
        "V1028061", "V1028062", "V1028063", "V1028064", "V1028065", "V1028066",
        "V1028067", "V1028068", "V1028069", "V1028070", "V1028071", "V1028072",
        "V1028073", "V1028074", "V1028075", "V1028076", "V1028077", "V1028078",
        "V1028079", "V1028080", "V1028081", "V1028082", "V1028083", "V1028084",
        "V1028085", "V1028086", "V1028087", "V1028088", "V1028089", "V1028090",
        "V1028091", "V1028092", "V1028093", "V1028094", "V1028095", "V1028096",
        "V1028097", "V1028098", "V1028099", "V1028100", "V1028101", "V1028102",
        "V1028103", "V1028104", "V1028105", "V1028106", "V1028107", "V1028108",
        "V1028109", "V1028110", "V1028111", "V1028112", "V1028113", "V1028114",
        "V1028115", "V1028116", "V1028117", "V1028118", "V1028119", "V1028120",
        "V1028121", "V1028122", "V1028123", "V1028124", "V1028125", "V1028126",
        "V1028127", "V1028128", "V1028129", "V1028130", "V1028131", "V1028132",
        "V1028133", "V1028134", "V1028135", "V1028136", "V1028137", "V1028138",
        "V1028139", "V1028140", "V1028141", "V1028142", "V1028143", "V1028144",
        "V1028145", "V1028146", "V1028147", "V1028148", "V1028149", "V1028150",
        "V1028151", "V1028152", "V1028153", "V1028154", "V1028155", "V1028156",
        "V1028157", "V1028158", "V1028159", "V1028160", "V1028161", "V1028162",
        "V1028163", "V1028164", "V1028165", "V1028166", "V1028167", "V1028168",
        "V1028169", "V1028170", "V1028171", "V1028172", "V1028173", "V1028174",
        "V1028175", "V1028176", "V1028177", "V1028178", "V1028179", "V1028180",
        "V1028181", "V1028182", "V1028183", "V1028184", "V1028185", "V1028186",
        "V1028187", "V1028188", "V1028189", "V1028190", "V1028191", "V1028192",
        "V1028193", "V1028194", "V1028195", "V1028196", "V1028197", "V1028198",
        "V1028199", "V1028200"
      )
    )

  base::return(x1)

}# End: limpando os dados

Bancos = base::list()

all_quero = base::list(
  c(2025, 1),
  c(2024, 3)
)

for( i in 1:base::length(all_quero))
{# Start: importando separadamente

  base::print(base::paste0(all_quero[[i]][1], "/", all_quero[[i]][2]))

  temp = PNADcIBGE::get_pnadc(year = all_quero[[i]][1], quarter = all_quero[[i]][2])

  Bancos[[i]] = clean(temp)

  base::rm(temp)

  base::names(Bancos)[i] = base::paste0(all_quero[[i]][1], "/", all_quero[[i]][2])

}# End: importando separadamente

PNAD = Bancos %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(id = 1:base::nrow(.)) %>%
  dplyr::relocate(id, 1)


PNAD1=PNAD[,c("weight","Ano","Trimestre","UF","V2007","V2009","V2010","VD2002" ,"VD3004","V3002","V3001",
              "V4040","V2005","V403422","V4039","V403312","V4032","V4012","V4019","V4020","V4016","V40171",
              "V3003A","V3009A","V4013","V40403","V4022")]
fa <- function(x) iconv(x, to = "ASCII//TRANSLIT")
PNAD1$UF2=fa(PNAD1$UF)
PNAD1$Regiao=ifelse(
  PNAD1$UF %in% c("Goi�s", "Mato Grosso", "Mato Grosso do Sul","Distrito Federal"),"Centro-Oeste",
  ifelse(PNAD1$UF %in% c("Paran�", "Rio Grande do Sul", "Santa Catarina"), "Sul",
         ifelse(PNAD1$UF %in% c("Esp�rito Santo", "Minas Gerais", 'Rio de Janeiro',"S�o Paulo"),"Sudeste",
                ifelse(PNAD1$UF %in% c("Alagoas",'Bahia','Cear�','Maranh�o','Para�ba','Pernambuco','Piau�',
                                       'Rio Grande do Norte', "Sergipe"),"Nordeste",
                       ifelse(PNAD1$UF %in% c("Acre",'Amap�','Amazonas',"Par�",'Rond�nia','Roraima',"Tocantins"),"Norte","NA")))))

PNAD1$Escolaridade=NA
PNAD1$peso=PNAD1$weight
PNAD1$peso1=as.numeric(stringr::str_replace_all(PNAD1$weight,",","."))
PNAD1$weight=as.numeric(stringr::str_replace_all(PNAD1$weight,",","."))


PNAD1%>%group_by(Ano, Trimestre)%>%summarise(sum(weight))
anyNA(PNAD1$V403312)
PNAD1$V403312=ifelse(is.na(PNAD1$V403312),"",PNAD1$V403312)

PNAD11=PNAD1%>%filter(Trimestre==1,Ano==2025)
PNAD2=PNAD1%>%filter(Trimestre==2)
PNAD3=PNAD1%>%filter(Trimestre==3)
PNAD4=PNAD1%>%filter(Trimestre==4)

PNAD1%>%group_by(Ano)%>%
  summarise(n=sum(weight))
con<-file('PNAD - Reduzido - 2025_1.csv',encoding="ISO-8859-1")
write.csv2(PNAD11,file=con)

##Depois tem que entrar no excel e transformar as colunas weight e V403312 e numerico e depois salvar
## o arquivo em csv separado por virgula UTF


## R code to load data sets and plot points
## call packages

source("R/packages.R")
source("R/functions.R")



# occurrence data
benthos_DF_eMOF <- read.csv(here ("DwC_output",
                                  "PELD_iloc_benthos",
                                  "DF_eMOF.txt"),sep=",",encoding= "UTF-8")


# event core
benthos_event_core<-read.csv(here ("DwC_output", 
                                   "PELD_iloc_benthos",
                                   "event_core.txt"),sep=",", encoding= "UTF-8",
                             row.names=NULL)


# matching event IDs to find site and locality (variables_we_want)
#variables_we_want <- c("locality","eventDate")
benthos_TS_data <- benthos_event_core [match (benthos_DF_eMOF$eventID,
                                              benthos_event_core$eventID),
                                       ]

# bind the occurrence data
benthos_TS_data<- cbind (benthos_TS_data,
                         benthos_DF_eMOF)


# load occ id table
benthos_DF_occ2 <- read.csv(here ("DwC_output", 
                                  "PELD_iloc_benthos",
                                  "DF_occ.txt"),sep=",",encoding= "UTF-8")

# bind taxon
benthos_TS_data <- cbind(benthos_TS_data,
                         scientificName=benthos_DF_occ2$scientificName)

# speciesName first up
benthos_TS_data$scientificName <-firstup(benthos_TS_data$scientificName)



# table
require(reshape2)
MusSpp_PELD <- dcast (formula = eventYear  ~scientificName,
       value.var = 'measurementValue',
       drop=T,
       fun.aggregate = mean,
       data=benthos_TS_data)



# data to plot
# peld
dat_peld <-melt (MusSpp_PELD[,c(1,grep("Mussismilia", colnames(MusSpp_PELD)))],
                 id.vars = "eventYear" )



# ----------------------------------------------------- 
# benthos (Ronaldo Francini-Filho)
# variables we want from here





# occurrence data
benthos_DF_eMOF_RF <- read.csv(here ("DwC_output",
                                     "RFrancini_timeSeries_abrolhos",
                                     "DF_eMOF_benthos.csv"),sep=",",encoding= "UTF-8",
                               row.names=NULL)


# event core
benthos_event_core_RF <-  read.csv(here ("DwC_output", 
                                         "RFrancini_timeSeries_abrolhos",
                                         "event_core_benthos.csv"),sep=",", encoding= "UTF-8",
                                   row.names=NULL)



# matching event IDs to find site and locality (variables_we_want)
benthos_TS_data_RF <- benthos_event_core_RF [match (benthos_DF_eMOF_RF$eventID,
                                                    benthos_event_core_RF$eventID),
                                             ]
benthos_TS_data_RF$eventDate<-substr(benthos_TS_data_RF$eventDate,1,4) # only year

# bind the occurrence data
benthos_TS_data_RF<- cbind (benthos_TS_data_RF,
                            benthos_DF_eMOF_RF)



# table
MusSpp_abrolhos <- dcast (formula = year  ~scientificName,
                      value.var = 'measurementValue',
                      drop=T,
                      fun.aggregate = mean,
                      data=benthos_TS_data_RF)




## ===========================================================
## benthos





benthos_DF_eMOF_ross <- read.csv(here ("DwC_output",
                                       "GLongo_NRoss_spatialData",
                                       "DF_eMOF_benthos.csv"),sep=",",
                                 encoding= "UTF-8",
                                 row.names=NULL)

# event core
benthos_event_core_ross <-  read.csv(here ("DwC_output", 
                                           "GLongo_NRoss_spatialData",
                                           "event_core_benthos.csv"),sep=",", 
                                     encoding= "UTF-8")


# matching event IDs to find site and locality (variables_we_want)
benthos_SN_data_ross <-benthos_event_core_ross [match (benthos_DF_eMOF_ross$eventID,
                                                       benthos_event_core_ross$eventID),
]
# bind the occurrence data
benthos_SN_data_ross<- cbind (benthos_SN_data_ross,
                              benthos_DF_eMOF_ross)



# table
Sid_RN <- dcast (formula = year  ~scientificName,
                 value.var = 'measurementValue',
                 drop=T,
                 fun.aggregate = mean,
                 data=benthos_SN_data_ross)


# data to plot
# peld
dat_peld <-melt (MusSpp_PELD[,c(1,grep("Mussismilia", colnames(MusSpp_PELD)))],
      id.vars = "eventYear" )
colnames(dat_peld) <- c("year", "variable", "value")

# abrolhos
dat_abrolhos <- (melt (MusSpp_abrolhos[,c(1,grep("Mussismilia", colnames(MusSpp_abrolhos)))],
       id.vars = "year" ))

# RN
dat_RN <- (melt (Sid_RN[,c(1,grep("Siderastrea", colnames(Sid_RN)))],
                       id.vars = "year" ))


# bind data to plot
data_to_plot <- rbind (
  cbind (dat_peld,
       data = "peld"),

  cbind (dat_abrolhos,
      data = "abrolhos"),
  cbind (dat_RN,
         data = "RN")
)

# replace NA by zero
data_to_plot$value[is.nan(data_to_plot$value)] <- 0

# plot
require (ggplot2)
ggplot (data_to_plot, aes (x= year, 
                           y=(value),
                           group=data))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x,k=1, bs="cs"))+
  facet_wrap(~variable,scales="fixed")+
  scale_fill_viridis_d() + 
  ylim (c(0,7)) + 
  ylab ("Cover") + 
  xlab ("Year")






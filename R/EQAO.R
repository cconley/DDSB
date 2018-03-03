#conditional model
#
#Use:   df <- IEPFull.EQAO(df)
#
#Function determines whether the ISD data frame is grade 3, 6, 9 or 10 and creates a new IEP column of IEP labels
# * Note: OSSLT returns IEPcode column

EQAO_IEPrecode <- function(x){
  ifelse("ROverallLevel" %in% colnames(x),
         {x$IEP <- paste0(x$SIF_IEP,
                          x$SIF_IPRC_Behaviour,
                          x$SIF_IPRC_Autism,
                          x$SIF_IPRC_Deaf,
                          x$SIF_IPRC_Language,
                          x$SIF_IPRC_Speech,
                          x$SIF_IPRC_Learning,
                          x$SIF_IPRC_Giftedness,
                          x$SIF_IPRC_MildIntellectual,
                          x$SIF_IPRC_Developmental,
                          x$SIF_IPRC_Physical,
                          x$SIF_IPRC_Blind,
                          x$SIF_IPRC_Multiple)
         x$IEP <- ifelse(x$IEP == "0000000000000", "No IEP",
                         ifelse(x$IEP == "1000000000000", "IEP no IPRC",
                                ifelse(x$IEP == "1100000000000", "Behaviour",
                                       ifelse(x$IEP == "1010000000000", "Autism",
                                              ifelse(x$IEP == "1001000000000", "Deaf",
                                                     ifelse(x$IEP == "1000100000000", "Language",
                                                            ifelse(x$IEP == "1000010000000", "Speech",
                                                                   ifelse(x$IEP == "1000001000000","Learning",
                                                                          ifelse(x$IEP == "1000000100000","Giftedness",
                                                                                 ifelse(x$IEP == "1000000010000","MildIntellectual",
                                                                                        ifelse(x$IEP == "1000000001000","Developmental",
                                                                                               ifelse(x$IEP == "1000000000100","Physical",
                                                                                                      ifelse(x$IEP == "1000000000010","Blind",
                                                                                                             ifelse(x$IEP == "1000000000001","Multiple","BadCode")
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
                         )
         )},
         ifelse("MathClassWhen" %in% colnames(x),
                {x$IEP <- paste0(x$SIF_IEP,
                                 x$SIF_IPRCBehaviour,
                                 x$SIF_IPRCAutism,
                                 x$SIF_IPRCDeaf,
                                 x$SIF_IPRCLanguage,
                                 x$SIF_IPRCSpeech,
                                 x$SIF_IPRCLearning,
                                 x$SIF_IPRCGifted,
                                 x$SIF_IPRCIntellectual,
                                 x$SIF_IPRCDevelopmental,
                                 x$SIF_IPRCPhysical,
                                 x$SIF_IPRCBlind,
                                 x$SIF_IPRCMultiple)
                x$IEP <- ifelse(x$IEP == "0000000000000", "No IEP",
                                ifelse(x$IEP == "1000000000000", "IEP no IPRC",
                                       ifelse(x$IEP == "1100000000000", "Behaviour",
                                              ifelse(x$IEP == "1010000000000", "Autism",
                                                     ifelse(x$IEP == "1001000000000", "Deaf",
                                                            ifelse(x$IEP == "1000100000000", "Language",
                                                                   ifelse(x$IEP == "1000010000000", "Speech",
                                                                          ifelse(x$IEP == "1000001000000","Learning",
                                                                                 ifelse(x$IEP == "1000000100000","Giftedness",
                                                                                        ifelse(x$IEP == "1000000010000","MildIntellectual",
                                                                                               ifelse(x$IEP == "1000000001000","Developmental",
                                                                                                      ifelse(x$IEP == "1000000000100","Physical",
                                                                                                             ifelse(x$IEP == "1000000000010","Blind",
                                                                                                                    ifelse(x$IEP == "1000000000001","Multiple","BadCode")
                                                                                                             )
                                                                                                      )
                                                                                               )
                                                                                        )
                                                                                 )
                                                                          )
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
                )},
                ifelse("OSSLTOutcome" %in% colnames(x),
                       {x$IEPcode <- paste0(x$IEP,
                                            x$IPRCExBehaviour,
                                            x$IPRCExAutism,
                                            x$IPRCExDeaf,
                                            x$IPRCExLanguage,
                                            x$IPRCExSpeech,
                                            x$IPRCExLearning,
                                            x$IPRCExGiftedness,
                                            x$IPRCExMildIntellectual,
                                            x$IPRCExDevelopmental,
                                            x$IPRCExPhysical,
                                            x$IPRCExBlind,
                                            x$IPRCExMultiple)
                       x$IEPcode <- ifelse(x$IEPcode == "0000000000000", "No IEP",
                                           ifelse(x$IEPcode == "1000000000000", "IEP no IPRC",
                                                  ifelse(x$IEPcode == "1100000000000", "Behaviour",
                                                         ifelse(x$IEPcode == "1010000000000", "Autism",
                                                                ifelse(x$IEPcode == "1001000000000", "Deaf",
                                                                       ifelse(x$IEPcode == "1000100000000", "Language",
                                                                              ifelse(x$IEPcode == "1000010000000", "Speech",
                                                                                     ifelse(x$IEPcode == "1000001000000","Learning",
                                                                                            ifelse(x$IEPcode == "1000000100000","Giftedness",
                                                                                                   ifelse(x$IEPcode == "1000000010000","MildIntellectual",
                                                                                                          ifelse(x$IEPcode == "1000000001000","Developmental",
                                                                                                                 ifelse(x$IEPcode == "1000000000100","Physical",
                                                                                                                        ifelse(x$IEPcode == "1000000000010","Blind",
                                                                                                                               ifelse(x$IEPcode == "1000000000001","Multiple","BadCode")
                                                                                                                        )
                                                                                                                 )
                                                                                                          )
                                                                                                   )
                                                                                            )
                                                                                     )
                                                                              )
                                                                       )
                                                                )
                                                         )
                                                  )
                                           )
                       )},  x$IEPcode <- "Unknown File Format"
                )
         )
  )


  return(x)
}

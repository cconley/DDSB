#' conditional model
#'
#' Use:   df <- EQAO_IEPrecode(df)
#'
#' Function determines whether the ISD data frame is grade 3, 6, 9 or 10 and creates a new IEPcode column of IEP labels


EQAO_IEPrecode <- function(x){
  ifelse("ROverallLevel" %in% colnames(x),
         {x$IEPcode <- ifelse(x$SIF_IPRC_Behaviour == "1", "Behaviour",
                                   ifelse(x$SIF_IPRC_Autism == "1", "Autism",
                                          ifelse(x$SIF_IPRC_Deaf == "1", "Deaf",
                                                 ifelse(x$SIF_IPRC_Language == "1", "Language",
                                                        ifelse(x$SIF_IPRC_Speech == "1", "Speech",
                                                               ifelse(x$SIF_IPRC_Learning == "1", "Learning",
                                                                      ifelse(x$SIF_IPRC_Giftedness == "1", "Giftedness",
                                                                             ifelse(x$SIF_IPRC_MildIntellectual == "1", "MildIntellectual",
                                                                                    ifelse(x$SIF_IPRC_Developmental == "1", "Development",
                                                                                           ifelse(x$SIF_IPRC_Physical == "1", "Physical",
                                                                                                  ifelse(x$SIF_IPRC_Blind =="1", "Blind",
                                                                                                         ifelse(x$SIF_IPRC_Multiple == "1", "Multiple",NA)
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
                {x$IEPcode <- ifelse(x$SIF_IPRCBehaviour == "1", "Behaviour",
                                 ifelse(x$SIFIPRC_Autism == "1", "Autism",
                                        ifelse(x$SIF_IPRCDeaf == "1", "Deaf",
                                               ifelse(x$SIF_IPRCLanguage == "1", "Language",
                                                      ifelse(x$SIF_IPRCSpeech == "1", "Speech",
                                                             ifelse(x$SIF_IPRCLearning == "1", "Learning",
                                                                    ifelse(x$SIF_IPRCGiftedness == "1", "Giftedness",
                                                                           ifelse(x$SIF_IPRCMildIntellectual == "1", "MildIntellectual",
                                                                                  ifelse(x$SIF_IPRCDevelopmental == "1", "Development",
                                                                                         ifelse(x$SIF_IPRCPhysical == "1", "Physical",
                                                                                                ifelse(x$SIF_IPRCBlind =="1", "Blind",
                                                                                                       ifelse(x$SIF_IPRCMultiple == "1", "Multiple",NA)
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
                       {x$IEPcode <- ifelse(x$IPRCExBehaviour == "1", "Behaviour",
                                        ifelse(x$IPRCEx_Autism == "1", "Autism",
                                               ifelse(x$IPRCExDeaf == "1", "Deaf",
                                                      ifelse(x$IPRCExLanguage == "1", "Language",
                                                             ifelse(x$IPRCExSpeech == "1", "Speech",
                                                                    ifelse(x$IPRCExLearning == "1", "Learning",
                                                                           ifelse(x$IPRCExGiftedness == "1", "Giftedness",
                                                                                  ifelse(x$IPRCExMildIntellectual == "1", "MildIntellectual",
                                                                                         ifelse(x$IPRCExDevelopmental == "1", "Development",
                                                                                                ifelse(x$IPRCExPhysical == "1", "Physical",
                                                                                                       ifelse(x$IPRCExBlind =="1", "Blind",
                                                                                                              ifelse(x$IPRCExMultiple == "1", "Multiple",NA)
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

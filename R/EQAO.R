#' For use with EQAO ISD files
#'
#' Collapsing IEP categories into a single column
#'
#' Use:   df <- EQAO_IEPrecode(df)
#'
#' Function determines whether the ISD data frame is grade 3, 6, 9 or 10 and creates a new IEPcode column of IEP labels
#' Adapted from drsimonj'b blog post: https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2


ISD_IEPrecode <- function(x){
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
                                                                                                         ifelse(x$SIF_IPRC_Multiple == "1", "Multiple","No IEP")
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
                                                                                                       ifelse(x$SIF_IPRCMultiple == "1", "Multiple","No IEP")
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
                                                                                                              ifelse(x$IPRCExMultiple == "1", "Multiple","No IEP")
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



#' For use with all raw secondary EQAO ISD files
#'
#' Use:   df <- EQAO_SecCourse(df)
#'
#' Cleaning all Secondary Course type labels
#'
#' Function determines whether the ISD is elementary or secondary assessments and recodes

ISD_SecCourse <- function(x){
  ifelse("LevelOfStudyLanguage" %in% colnames(x),
         {x$re.course <- ifelse(x$LevelOfStudyLanguage == "-2", "Ambiguous",
                                ifelse(x$LevelOfStudyLanguage == "-1", "Missing",
                                       ifelse(x$LevelOfStudyLanguage == "0", "NA",
                                              ifelse(x$LevelOfStudyLanguage == "1", "Academic",
                                                     ifelse(x$LevelOfStudyLanguage == "2", "Applied",
                                                            ifelse(x$LevelOfStudyLanguage == "3", "Locally Developed",
                                                                   ifelse(x$LevelOfStudyLanguage == "4", "ESL/ELD",
                                                                          ifelse(x$LevelOfStudyLanguage == "5", "Other","BadCode")
                                                                   )
                                                            )
                                                     )
                                              )
                                       )
                                )
         )},
         ifelse("Program" %in% colnames(x),
                {x$re.course <- ifelse(x$Program =="1", "Applied",
                                       ifelse(x$Program == "2", "Academic", "BadCode")
                )}, x$re.course <- "Elementary - Not Applicable"
         )
  )
  return(x)
}


#' For use with all raw EQAO ISD file
#'
#' Cleaning ELL field names
#'
#' Use:   df <- EQAO_ELL(df)
#'
#' Function determines whether the ISD is elementary or secondary assessments and recodes

ISD_ELL <- function(x){
  ifelse("Background_ESLELD_ALFPDF" %in% colnames(x),
         {x$ELLcode <- ifelse(x$Background_ESLELD_ALFPDF == "-1", "Missing",
                             ifelse(x$Background_ESLELD_ALFPDF == "0", "Not ELL",
                                    ifelse(x$Background_ESLELD_ALFPDF == "1", "ELL",
                                           ifelse(x$Background_ESLELD_ALFPDF == "2", NULL,
                                                  ifelse(x$Background_ESLELD_ALFPDF == "3", NULL,"BadCode"
                                                  )
                                           )
                                    )
                             )
         )},
         ifelse("ESLELD_ALFPDF" %in% colnames(x),
                {x$EQAOcode <- ifelse(x$ESLELD_ALFPDF == "-1", "Missing",
                                    ifelse(x$ESLELD_ALFPDF == "0", "Not ELL",
                                           ifelse(x$ESLELD_ALFPDF == "1", "ELL",
                                                  ifelse(x$ESLELD_ALFPDF == "2", NULL,
                                                         ifelse(x$ESLELD_ALFPDF == "3", NULL,"BadCode"
                                                         )
                                                  )
                                           )
                                    )
                )},  x$EQAOcode <- "Unknown File Format"
         )
  )
  return(x)
}


#' For use with all raw Elementary EQAO ISD file
#'
#' Cleaning FI labels
#'
#' Use:   df <- EQAO_FI(df)
#'
#' French Immersion is an elementary distinction.  Secondary is addressed through course types
#'
#' Function determines whether the ISD is elementary or secondary assessments and recodes

ISD_FI <- function(x){
  ifelse("Background_FrenchImmersion" %in% colnames(x),
         {x$FIcode <- ifelse(x$Background_FrenchImmersion == "-1", "Missing",
                            ifelse(x$Background_FrenchImmersion == "0", "Not FI",
                                   ifelse(x$Background_FrenchImmersion == "1", "FI (A)",
                                          ifelse(x$Background_FrenchImmersion == "2", "FI (B)",
                                                 ifelse(x$Background_FrenchImmersion == "3", "FI (C)",
                                                        ifelse(x$Background_FrenchImmersion == "4", "FI (G6)", "BadCode"
                                                        )
                                                 )
                                          )
                                   )
                            )
         )},  x$FIcode <- "Secondary - Not Applicable"
  )
  return(x)
}



#For use with all raw EQAO ISD file
#
#Use:   df <- EQAO_Gender(df)
#
#Gender is a standard field used in all ISDs and it not assessment specific

ISD_Gender <- function(x){
  x$Gendercod <- ifelse(x$Gender == "-1", "Missing",
                        ifelse(x$Gender == "1", "Male",
                               ifelse(x$Gender == "2", "Female", "BadCode")
                        )
  )
  return(x)
}


ISD_Achieve <- function(x) {
  x$Reading <- ifelse(x$ROverallLevel == "B", "No Data",
                      ifelse(x$ROverallLevel == "P", "Pending",
                             ifelse(x$ROverallLevel == "Q", "Not Required",
                                    ifelse(x$ROverallLevel %in% c("R", "W"), "Withheld",
                                           ifelse(x$ROverallLevel == "X", "Exempt", x$ROverallLevel)))))

  x$Writing <- ifelse(x$WOverallLevel == "B", "No Data",
                      ifelse(x$WOverallLevel == "P", "Pending",
                             ifelse(x$WOverallLevel == "Q", "Not Required",
                                    ifelse(x$ROverallLevel %in% c("R", "W"), "Withheld",
                                           ifelse(x$WOverallLevel == "X", "Exempt", x$ROverallLevel)))))

  x$Math <- ifelse(x$MOverallLevel == "B", "No Data",
                   ifelse(x$MOverallLevel == "P", "Pending",
                          ifelse(x$MOverallLevel == "Q", "Not Required",
                                 ifelse(x$MOverallLevel %in% c("R", "W"), "Withheld",
                                        ifelse(x$MOverallLevel == "X", "Exempt", x$ROverallLevel)))))

  return(x)

}



ISD_AchieveFctr <- function(x) {
  x$Reading <- ifelse(x$ROverallLevel == "B", "No Data",
                      ifelse(x$ROverallLevel == "P", "Pending",
                             ifelse(x$ROverallLevel == "Q", "Not Required",
                                    ifelse(x$ROverallLevel %in% c("R", "W"), "Withheld",
                                           ifelse(x$ROverallLevel == "X", "Exempt", x$ROverallLevel)))))
    x$Reading <- factor(x$Reading, levels = c("Not Required", "No Data",  "Exempt", "Withheld", "Pending", "1", "2", "3", "4"))

  x$Writing <- ifelse(x$WOverallLevel == "B", "No Data",
                      ifelse(x$WOverallLevel == "P", "Pending",
                             ifelse(x$WOverallLevel == "Q", "Not Required",
                                    ifelse(x$ROverallLevel %in% c("R", "W"), "Withheld",
                                           ifelse(x$WOverallLevel == "X", "Exempt", x$ROverallLevel)))))
    x$Writing <- factor(x$Writing, levels = c("Not Required", "No Data",  "Exempt", "Withheld", "Pending", "1", "2", "3", "4"))

  x$Math <- ifelse(x$MOverallLevel == "B", "No Data",
                        ifelse(x$MOverallLevel == "P", "Pending",
                               ifelse(x$MOverallLevel == "Q", "Not Required",
                                      ifelse(x$MOverallLevel %in% c("R", "W"), "Withheld",
                                             ifelse(x$MOverallLevel == "X", "Exempt", x$ROverallLevel)))))
    x$Math <- factor(x$Math, levels = c("Not Required", "No Data",  "Exempt", "Withheld", "Pending", "1", "2", "3", "4"))

        return(x)

  }

#' Examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#' geom_point(size = 4) +
#'   DDSB_scale_color()
#'
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#'   geom_point(size = 4, alpha = .6) +
#'   DDSB_scale_color(discrete = FALSE, palette = "cool")
#'
#' ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#'   geom_bar() +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#'   DDSB_scale_fill(palette = "mixed", guide = "none")

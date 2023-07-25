
# Tabla índices de abundancia ----
tb1<-function(label_tb,archivo.Rdata,caption1){
  
  load(archivo.Rdata)
  
  dataInd<-data.frame(years,reclasobs,pelacesobs,mphobs)
  
  kbl(dataInd, booktabs = T,format = "latex",position="h!",escape = F,align="c",
      format.args=list(big.mark = '.'),
      col.names = linebreak(c("Año\ncalendario ",
                              "Biomasa crucero\nde verano\n(toneladas)",
                              "Biomasa crucero\nde otoño\n(toneladas)",
                              "Biomasa desovante\nMPDH\n(toneladas)"),align="c"),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)
}

# tabla capturas ----
tb2<-function(label_tb,bioyear,desembarque,porcDesc_actualizado,CapturaDescartada,CapturaTotal,especie,caption1){
  
  dataDes_y_descar<-data.frame("Año"=bioyear,
                               "Desembarques"=round(desembarque,0),
                               "Pdescarte"=porcDesc_actualizado,
                               "Capturadesc"=round(CapturaDescartada,0),
                               "Capturatotal"=round(CapturaTotal,0))
  
  kbl(dataDes_y_descar, booktabs = T,format = "latex",position="h!",align="c",
      format.args=list(big.mark = '.'),escape=F,
      col.names = linebreak(c("Año\nbiológico ",
                              "Desembarques\n(toneladas)",
                              "Porcentaje\nDescarte",
                              "Captura\ndescartada\n(toneladas)",
                              "Captura\ntotal\n(toneladas)"),align="c"),   
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)
}

# tabla variables poblacionales ----
tb3<-function(label_tb,Rdata_H1,Rdata_H2,caption1){
  
  load(Rdata_H1); VarPob_H1<-Var
  load(Rdata_H2); VarPob_H2<-Var
  
  VarPobl1<- cbind(yearsbiol,
                   format(round(c(VarPob_H1[VarPob_H1$indicador=='BD',]$value,NA),0),nsmall=0, big.mark="."),
                   format(round(VarPob_H2[VarPob_H2$indicador=='BD',]$value,0),nsmall=0, big.mark="."),
                   format(round(c(VarPob_H1[VarPob_H1$indicador=='BT',]$value,NA),0),nsmall=0, big.mark="."),
                   format(round(VarPob_H2[VarPob_H2$indicador=='BT',]$value,0),nsmall=0, big.mark="."),
                   format(round(c(VarPob_H1[VarPob_H1$indicador=='Rt',]$value,NA),0),nsmall=0, big.mark="."),
                   format(round(VarPob_H2[VarPob_H2$indicador=='Rt',]$value,0),nsmall=0, big.mark="."),
                   formatC(round(c(VarPob_H1[VarPob_H1$indicador=='Ft',]$value,NA),3),decimal.mark = ",",digits = 3),
                   formatC(round(VarPob_H2[VarPob_H2$indicador=='Ft',]$value,3),decimal.mark = ",",digits = 3))
  
  # TABLA
  VarPobl1 %>%
  kbl(booktabs = T,format = "latex",position="h!",escape = F,align="c",
      col.names = linebreak(c("biológico",
                              "Hito 1","Hito 2","Hito 1","Hito 2","Hito 1","Hito 2","Hito 1","Hito 2"),align="c"),
  caption = paste(label_tb,caption1,especie,region,sep=" ")) %>%
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=8)%>%
    add_header_above(c('Año'=1,
                        "Biomasa\ndesovante"=2,
                        "Biomasa total"=2,
                        "Reclutamientos"=2,
                        "Mortalidad\npor pesca"=2))
}

tb3_H3<-function(label_tb,Rdata_H1,Rdata_H2,Rdata_H3,caption1){

  load(Rdata_H1); VarPob_H1<-Var
  load(Rdata_H2); VarPob_H2<-Var
  load(Rdata_H3); VarPob_H3<-Var
  
  VarPobl1<- cbind(yearsbiol,
                   format(round(c(VarPob_H1[VarPob_H1$indicador=='BD',]$value,NA),0),nsmall=0, big.mark="."),
                   format(round(VarPob_H2[VarPob_H2$indicador=='BD',]$value,0),nsmall=0, big.mark="."),
                   format(round(VarPob_H3[VarPob_H3$indicador=='BD',]$value,0),nsmall=0, big.mark="."),
                   format(round(c(VarPob_H1[VarPob_H1$indicador=='BT',]$value,NA),0),nsmall=0, big.mark="."),
                   format(round(VarPob_H2[VarPob_H2$indicador=='BT',]$value,0),nsmall=0, big.mark="."),
                   format(round(VarPob_H3[VarPob_H3$indicador=='BT',]$value,0),nsmall=0, big.mark="."),
                   format(round(c(VarPob_H1[VarPob_H1$indicador=='Rt',]$value,NA),0),nsmall=0, big.mark="."),
                   format(round(VarPob_H2[VarPob_H2$indicador=='Rt',]$value,0),nsmall=0, big.mark="."),
                   format(round(VarPob_H3[VarPob_H3$indicador=='Rt',]$value,0),nsmall=0, big.mark="."),
                   formatC(round(c(VarPob_H1[VarPob_H1$indicador=='Ft',]$value,NA),3),decimal.mark = ",",digits = 3),
                   formatC(round(VarPob_H2[VarPob_H2$indicador=='Ft',]$value,3),decimal.mark = ",",digits = 3),
                   formatC(round(VarPob_H3[VarPob_H3$indicador=='Ft',]$value,3),decimal.mark = ",",digits = 3))
  
  # TABLA
  opts<-options(knitr.kable.NA="")
  VarPobl1 %>%
    kbl(booktabs = T,format = "latex",position="h!",escape = F,align="c",
        col.names = linebreak(c("biológico",
                                "Hito 1","Hito 2","Hito 3","Hito 1","Hito 2","Hito 3",
                                "Hito 1","Hito 2","Hito 3","Hito 1","Hito 2","Hito 3"),align="c"),
        caption = paste(label_tb,caption1,especie,region,sep=" ")) %>%
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=8)%>%
    add_header_above(c('Año'=1,
                       "Biomasa\ndesovante"=3,
                       "Biomasa total"=3,
                       "Reclutamientos"=3,
                       "Mortalidad\npor pesca"=3))
}
# tabla puntos biológicos de referencia ----
tb4<-function(label_tb,pbrHito1,pbrHito2,especie,caption1){
  
  etapas<-c("Paso 1", "Paso 1", "Paso 2","Paso 2","Paso 3", "Paso 3", "Paso 4", "Paso 5", "Paso 6")
  
  pbrs<-c("$BD_{promedio}$",
          "$F_{mh}$",
          "$\\%BDPR_{F_{mh}}$",
          "$\\%BDPR_{F_{RMS}}$",
          "$\\%BD_{F_{mh}}$",
          "$\\%BD_{F_{RMS}}$",
          "$BD_{0}$",
          "$BD_{55\\%}$",
          "$BD_{27,5\\%}$")
  
  calculo<-c("Promedio de la serie histórica",
             "Mediana de la serie histórica",
             rep("Cálculo de la curva de biomasa por recluta (BDPR)",2),
             "$\\%BDPR_{F_{mh}}-5\\%$",
             "$\\%BDPR_{F_{RMS}}-5\\%$",
             "$BD_{0}=BD_{promedio}/\\%BD_{F_{mh}}$",
             "$BD_{RMS}=BD_{0}*\\%BD_{F_{RMS}}$",
             "$BD_{LIM}=BD_{0}*\\%BD_{F_{LIM}}$")
 
  tablapbrs<-data.frame(etapas,calculo,pbrs,pbrHito1,pbrHito2)

  #-----------
  # TABLA 
  #-----------
  kbl(tablapbrs, booktabs = T,format = "latex",position="h!",align="cllc",escape = FALSE,
      col.names = linebreak(c('Etapas','Cálculo','Aproximación','Hito 1','Hito 2'),align="c"),
      caption = paste(label_tb,caption1,especie,region,sep=" "))%>%
    collapse_rows(columns = 1:2,latex_hline = "major", valign = "middle")%>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=8)%>% 
    add_header_above(c(" " = 3,"Asesorías" = 3))
}

tb4_H3<-function(label_tb,pbrHito1,pbrHito2,pbrHito3,especie,caption1){
  
  etapas<-c("Paso 1", "Paso 1", "Paso 2","Paso 2","Paso 3", "Paso 3", "Paso 4", "Paso 5", "Paso 6")
  
  pbrs<-c("$BD_{promedio}$",
          "$F_{mh}$",
          "$\\%BDPR_{F_{mh}}$",
          "$\\%BDPR_{F_{RMS}}$",
          "$\\%BD_{F_{mh}}$",
          "$\\%BD_{F_{RMS}}$",
          "$BD_{0}$",
          "$BD_{55\\%}$",
          "$BD_{27,5\\%}$")
  
  calculo<-c("Promedio de la serie histórica",
             "Mediana de la serie histórica",
             rep("Cálculo de la curva de biomasa por recluta (BDPR)",2),
             "$\\%BDPR_{F_{mh}}-5\\%$",
             "$\\%BDPR_{F_{RMS}}-5\\%$",
             "$BD_{0}=BD_{promedio}/\\%BD_{F_{mh}}$",
             "$BD_{RMS}=BD_{0}*\\%BD_{F_{RMS}}$",
             "$BD_{LIM}=BD_{0}*\\%BD_{F_{LIM}}$")
  
  tablapbrs<-data.frame(etapas,calculo,pbrs,pbrHito1,pbrHito2,pbrHito3)

  
  #-----------
  # TABLA 
  #-----------
  kbl(tablapbrs, booktabs = T,format = "latex",position="h!",align="cllc",escape = FALSE,
      col.names = linebreak(c('Etapas','Cálculo','Aproximación','Hito 1','Hito 2','Hito 3'),align="c"),
      caption = paste(label_tb,caption1,especie,region,sep=" "))%>%
    collapse_rows(columns = 1:2,latex_hline = "major", valign = "middle")%>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=8)%>% 
    add_header_above(c(" " = 3,"Asesorías" = 3))
}

tb5<-function(label_tb,pbrs,especie,caption1){
#attach(pbrs)  
 
Tabla4.1<-rbind("Año biológico"=pbrs$yearStatus,
                "$F_{RMS}$ $(año^{-1})$"=formatC(round(pbrs$FRMS,2), decimal.mark = ","),
                "$BD_{RMS}$ (mil t)"=round(pbrs$BRMS/10^3,0),
                "$BD_{LIM}$ (mil t)"=round(pbrs$BLIM/10^3,0),
                "$p(BD_{last}<BD_{RMS})^1$"=formatC(round(pbrs$pa,3), decimal.mark = ","),
                "$p(F_{last}>F_{RMS})^2$"=formatC(round(pbrs$pb,3), decimal.mark = ","),
                "$p(sobre-explotación)^3$"=formatC(round(pbrs$pc,3), decimal.mark = ","),
                "$p(agotado/colapsado)^4$"=formatC(round(pbrs$pd,3), decimal.mark = ","),
                "$p(sobrepesca)^5$"=formatC(round(pbrs$pe,3), decimal.mark = ","))

colnames(Tabla4.1)<-pbrs$Hitos

footnote<-c("Probabilidad que $BD$ del año más reciente sea menor a $BD_{RMS}$ según el diagrama de fase",
            "Probabilidad que $F$ del año más reciente sea mayor a $F_{RMS}$ según el diagrama de fase",
            "Probabilidad de estar en sobreexplotación = $p(0,5<BD_{last}/BD_{RMS}<0,9)$",
            "Probabilidad de estar en colapso =$p(BD_{last}/BD_{RMS}<0,5)$",
            "Probabilidad de estar en sobrepesca = $p(F_{last}/F_{RMS}>1,1)$")

# TABLA 


kbl(Tabla4.1, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
    caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
  kable_styling(latex_options = c("striped"),
                full_width = FALSE,font_size=9) %>%
  footnote(number = footnote,threeparttable = T,escape = FALSE,fixed_small_size = TRUE) %>%
  column_spec(1, width = "25em") %>% 
  add_header_above(c(" " = 1,"Asesorías" = 3))

}

# Tabla indicadores de estatus

tb6<-function(label_tb,Rdata_H1,Rdata_H2,caption1){

  load(Rdata_H1); IndStatus_H1<-IndStatus
  load(Rdata_H2); IndStatus_H2<-IndStatus
  
  Estatus2<- data.frame(
    'Años'=yearsbiol,
    "F_FRMS_h1"=formatC(round(c(IndStatus_H1[IndStatus_H1$indicador=='F_FRMS',]$value,NA),3), decimal.mark = ","),
    "F_FRMS_h2"=formatC(round(c(IndStatus_H2[IndStatus_H2$indicador=='F_FRMS',]$value),3), decimal.mark = ","),
    "BD_BDRMS_h1"=formatC(round(c(IndStatus_H1[IndStatus_H1$indicador=='BD_BDRMS',]$value,NA),3), decimal.mark = ","),
    "BD_BDRMS_h2"=formatC(round(c(IndStatus_H2[IndStatus_H2$indicador=='BD_BDRMS',]$value),3), decimal.mark = ","),
    "Y_BT_h1"=formatC(round(c(IndStatus_H1[IndStatus_H1$indicador=='Y_BT',]$value,NA),3), decimal.mark = ","),
    "Y_BT_h2"=formatC(round(c(IndStatus_H2[IndStatus_H2$indicador=='Y_BT',]$value),3), decimal.mark = ","),
    "C_N_h1"=formatC(round(c(IndStatus_H1[IndStatus_H1$indicador=='C_N',]$value,NA),3), decimal.mark = ","),
    "C_N_h2"=formatC(round(c(IndStatus_H2[IndStatus_H2$indicador=='C_N',]$value),3), decimal.mark = ","))

kbl(Estatus2, booktabs = T,format = "latex",position="h!",align="c",escape = F,
    col.names = linebreak(c('biológico','Hito 1','Hito 2','Hito 1','Hito 2','Hito 1','Hito 2','Hito 1','Hito 2'),align="c"),
    caption = paste(label_tb,caption1,especie,region,sep=" ")) %>%
  kable_styling(latex_options = c("striped"),
                full_width = FALSE,font_size=8) %>%
  add_header_above(c("Año" = 1,
                     "$F/F_{RMS}$"=2,
                     "$BD/BD_{RMS}$"=2,
                     "$Y/BT$"=2,
                     "$C\\\\#/N\\\\#$"=2), escape = F)

}

tb6_H3<-function(label_tb,Rdata_H1,Rdata_H2,Rdata_H3,caption1){
  
  load(Rdata_H1); IndStatus_H1<-IndStatus
  load(Rdata_H2); IndStatus_H2<-IndStatus
  load(Rdata_H3); IndStatus_H3<-IndStatus
  
  Estatus2<- data.frame(
    'Años'=yearsbiol,
    "F_FRMS_h1"=formatC(round(c(IndStatus_H1[IndStatus_H1$indicador=='F_FRMS',]$value,NA),3), decimal.mark = ","),
    "F_FRMS_h2"=formatC(round(c(IndStatus_H2[IndStatus_H2$indicador=='F_FRMS',]$value),3), decimal.mark = ","),
    "F_FRMS_h3"=formatC(round(c(IndStatus_H3[IndStatus_H3$indicador=='F_FRMS',]$value),3), decimal.mark = ","),
    "BD_BDRMS_h1"=formatC(round(c(IndStatus_H1[IndStatus_H1$indicador=='BD_BDRMS',]$value,NA),3), decimal.mark = ","),
    "BD_BDRMS_h2"=formatC(round(c(IndStatus_H2[IndStatus_H2$indicador=='BD_BDRMS',]$value),3), decimal.mark = ","),
    "BD_BDRMS_h3"=formatC(round(c(IndStatus_H3[IndStatus_H3$indicador=='BD_BDRMS',]$value),3), decimal.mark = ","),
    "Y_BT_h1"=formatC(round(c(IndStatus_H1[IndStatus_H1$indicador=='Y_BT',]$value,NA),3), decimal.mark = ","),
    "Y_BT_h2"=formatC(round(c(IndStatus_H2[IndStatus_H2$indicador=='Y_BT',]$value),3), decimal.mark = ","),
    "Y_BT_h3"=formatC(round(c(IndStatus_H3[IndStatus_H3$indicador=='Y_BT',]$value),3), decimal.mark = ","),
    "C_N_h1"=formatC(round(c(IndStatus_H1[IndStatus_H1$indicador=='C_N',]$value,NA),3), decimal.mark = ","),
    "C_N_h2"=formatC(round(c(IndStatus_H2[IndStatus_H2$indicador=='C_N',]$value),3), decimal.mark = ","),
    "C_N_h3"=formatC(round(c(IndStatus_H3[IndStatus_H3$indicador=='C_N',]$value),3), decimal.mark = ","))
  
  kbl(Estatus2, booktabs = T,format = "latex",position="h!",align="c",escape = F,
      col.names = linebreak(c('biológico','Hito 1','Hito 2','Hito 3','Hito 1','Hito 2','Hito 3',
                                          'Hito 1','Hito 2','Hito 3','Hito 1','Hito 2','Hito 3'),align="c"),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>%
    kable_styling(latex_options = c("striped"),
                  full_width = FALSE,font_size=8) %>%
    add_header_above(c("Año" = 1,
                       "$F/F_{RMS}$"=3,
                       "$BD/BD_{RMS}$"=3,
                       "$Y/BT$"=3,
                       "$C\\\\#/N\\\\#$"=3), escape = F)
  
}

# Tablas proyección hito 1 ----

tb7_H1<-function(label_tb,Rdata_proy_Hito,escRecl,especie,region,caption1){
  
  load(Rdata_proy_Hito)
  
  probEstatus<-rbind("$R_{proy}$"=formatC(c(reps1a$Np[1],
                                            reps2a$Np[1],
                                            reps3a$Np[1],
                                            reps1a$Np[1],
                                            reps2a$Np[1],
                                            reps3a$Np[1])/1000, decimal.mark = ","),
                     "$F_{proy}=F_{RMS}$"=formatC(c(rep(FRMSp,6)), decimal.mark = ","),
                     "$BD_{RMS}$"=formatC(c(rep(BRMSp,6))/1000, decimal.mark = ","),
                     '$BD_{proy}$'=formatC(round(c(bds1[1],
                                                   bds2[1],
                                                   bds3[1],
                                                   bds1[2],
                                                   bds2[2],
                                                   bds3[2])/1000,0), decimal.mark = ","),
                     '$BD_{proy}/BD_{RMS}$'=formatC(round(c(RpRps1[1],
                                                            RpRps2[1],
                                                            RpRps3[1],
                                                            RpRps1[2],
                                                            RpRps2[2],
                                                            RpRps3[2]),2), decimal.mark = ","),
                     "$p(sobreexplotación)^1$"=formatC(round(c(pc1,
                                                               pc2,
                                                               pc3,
                                                               pc12,
                                                               pc22,
                                                               pc32),2), decimal.mark = ","),
                     "$p(agotado/colapsado)^2$"=formatC(round(c(pd1,
                                                                pd2,
                                                                pd3,
                                                                pd12,
                                                                pd22,
                                                                pd32),2), decimal.mark = ","))
  
  colnames(probEstatus)<-rep(escRecl,2)
  
  footnote2<-c("Probabilidad de estar en sobreexplotación = $p(0,5<BD_{proy}/BD_{RMS}<0,9)$",
               "Probabilidad de estar en colapso =$p(BD_{proy}/BD_{RMS}<0,5)$")
  
  #-----------
  # TABLA 
  #-----------
  kbl(probEstatus, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=8)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "Estatus proyectado 2022/23" = 3,"Estatus proyectado 2023/24" = 3),line = F)%>% 
    pack_rows(index = c("Hito 1" = 7))%>%
    footnote(number = footnote2,threeparttable = T,escape = FALSE,fixed_small_size = TRUE)
}

# Tablas proyección hito 2 ----

tb7_H2<-function(label_tb,Rdata,Rdata_proy_Hito,escRecl,especie,region,caption1){
  
  load(Rdata);pc;pd
  
  load(Rdata_proy_Hito)
  
  probEstatus<-rbind("$R_{actual/proy}$"=formatC(c(Rtp[nyears],
                                                   reps1a$Np[1],
                                                   reps2a$Np[1],
                                                   reps3a$Np[1])/1000, decimal.mark = ","),
                     "$F_{supuesto/proy}=F_{RMS}$"=formatC(round(c(rep(FRMSp,4)),2), decimal.mark = ","),
                     "$BD_{RMS}$"=formatC(c(rep(BRMSp,4))/1000, decimal.mark = ","),
                     '$BD_{actual/proy}$'=formatC(round(c(SSBp[nyears],
                                                          bds1[1],
                                                          bds2[1],
                                                          bds3[1])/1000,0), decimal.mark = ","),
                     '$BD_{actual/proy}/BD_{RMS}$'=formatC(round(c(SSBp[nyears]/BRMSp,
                                                                   RpRps1[1],
                                                                   RpRps2[1],
                                                                   RpRps3[1]),2), decimal.mark = ","),
                     "$p(sobreexplotación)^1$"=formatC(round(c(pc,
                                                               pc1,
                                                               pc2,
                                                               pc3),2), decimal.mark = ","),
                     "$p(agotado/colapsado)^2$"=formatC(round(c(pd,
                                                                pd1,
                                                                pd2,
                                                                pd3),2), decimal.mark = ","))
  
  colnames(probEstatus)<-c("$R_{2023}$",escRecl)
  
  footnote2<-c("Probabilidad de estar en sobreexplotación = $p(0,5<BD_{actual/proy}/BD_{RMS}<0,9)$",
               "Probabilidad de estar en colapso =$p(BD_{actual/proy}/BD_{RMS}<0,5)$")
  
  #-----------
  # TABLA 
  #-----------
  kbl(probEstatus, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=8)%>% 
    add_header_above(c(" " = 1,
                       "R actual" = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "Estatus actual 2022/23" = 1,"Estatus proyectado 2023/24" = 3),line = F)%>% 
    pack_rows(index = c("Hito 2" = 7))%>%
    footnote(number = footnote2,threeparttable = T,escape = FALSE,fixed_small_size = TRUE)
}


# Tablas proyección Estatus hito 3 ----

tb7_H3<-function(label_tb,RdataH2,RdataH3,Rdata_proy_HitoH2,Rdata_proy_HitoH3,escRecl,especie,region,caption1){
  
  
  # RdataH2<-Rdata_H2
  # RdataH3<-Rdata_H3
  # Rdata_proy_HitoH2<- Rdata_proy_Hito2
  # Rdata_proy_HitoH3<- Rdata_proy_Hito3
  
  # HITO 2 ----
  load(RdataH2);pc;pd
  
  load(Rdata_proy_HitoH2)
  
  probEstatusH2<-rbind("$R_{actual/proy}$"=formatC(c(Rtp[nyears],
                                                   reps1a$Np[1],
                                                   reps2a$Np[1],
                                                   reps3a$Np[1])/1000, decimal.mark = ","),
                     "$F_{supuesto/proy}=F_{RMS}$"=formatC(round(c(rep(FRMSp,4)),2), decimal.mark = ","),
                     "$BD_{RMS}$"=formatC(c(rep(BRMSp,4))/1000, decimal.mark = ","),
                     '$BD_{actual/proy}$'=formatC(round(c(SSBp[nyears],
                                                          bds1[1],
                                                          bds2[1],
                                                          bds3[1])/1000,0), decimal.mark = ","),
                     '$BD_{actual/proy}/BD_{RMS}$'=formatC(round(c(SSBp[nyears]/BRMSp,
                                                                   RpRps1[1],
                                                                   RpRps2[1],
                                                                   RpRps3[1]),2), decimal.mark = ","),
                     "$p(sobreexplotación)^1$"=formatC(round(c(pc,
                                                               pc1,
                                                               pc2,
                                                               pc3),2), decimal.mark = ","),
                     "$p(agotado/colapsado)^2$"=formatC(round(c(pd,
                                                                pd1,
                                                                pd2,
                                                                pd3),2), decimal.mark = ","))
  # HITO 3 ----
  load(RdataH3);pc;pd
  
  load(Rdata_proy_HitoH3)
  
  probEstatusH3<-rbind("$R_{actual/proy}$"=formatC(c(Rtp[nyears],
                                                   reps1a$Np[1],
                                                   reps2a$Np[1],
                                                   reps3a$Np[1])/1000, decimal.mark = ","),
                     "$F_{supuesto/proy}=F_{RMS}$"=formatC(round(c(rep(FRMSp,4)),2), decimal.mark = ","),
                     "$BD_{RMS}$"=formatC(c(rep(BRMSp,4))/1000, decimal.mark = ","),
                     '$BD_{actual/proy}$'=formatC(round(c(SSBp[nyears],
                                                          bds1[1],
                                                          bds2[1],
                                                          bds3[1])/1000,0), decimal.mark = ","),
                     '$BD_{actual/proy}/BD_{RMS}$'=formatC(round(c(SSBp[nyears]/BRMSp,
                                                                   RpRps1[1],
                                                                   RpRps2[1],
                                                                   RpRps3[1]),2), decimal.mark = ","),
                     "$p(sobreexplotación)^1$"=formatC(round(c(pc,
                                                               pc1,
                                                               pc2,
                                                               pc3),2), decimal.mark = ","),
                     "$p(agotado/colapsado)^2$"=formatC(round(c(pd,
                                                                pd1,
                                                                pd2,
                                                                pd3),2), decimal.mark = ","))
  
  probEstatus<-rbind(probEstatusH2,probEstatusH3)
  
  colnames(probEstatus)<-c("$R_{2023}$",escRecl)
  
  footnote2<-c("Probabilidad de estar en sobreexplotación = $p(0,5<BD_{actual/proy}/BD_{RMS}<0,9)$",
               "Probabilidad de estar en colapso =$p(BD_{actual/proy}/BD_{RMS}<0,5)$")
  
  #-----------
  # TABLA 
  #-----------
  kbl(probEstatus, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=8)%>% 
    add_header_above(c(" " = 1,
                       "R actual" = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "Estatus actual 2022/23" = 1,"Estatus proyectado 2023/24" = 3),line = F)%>% 
    pack_rows(index = c("Hito 2" = 7,"Hito 3" = 7))%>%
    footnote(number = footnote2,threeparttable = T,escape = FALSE,fixed_small_size = TRUE)
}





# Tabla de proyección Capturas ----
# Hito 1 ----
tb8_H1<-function(label_tb,Rdata_proy_HitoH1,escRecl,especie,region,caption1){
  
  load(Rdata_proy_HitoH1)
  
  Capturaproy<-rbind('Supuesto Captura proyectada 2022/2023'=c("-",round(c(cs1[1],cs2[1],cs3[1])/1000,0)),
                     'Supuesto Captura primer semestre 2023'=c("-",round(c(cs1[1]*0.7,cs2[1]*0.7,cs3[1]*0.7)/1000,0)),
                     'Supuesto Captura proyectada 2023/2024'=c("-",round(c(cs1[2],cs2[2],cs3[2])/1000,0)),
                     'Supuesto Captura Segundo Semestre 2023'=c("-",round(c(cs1[2]*0.3,cs2[2]*0.3,cs3[2]*0.3)/1000,0)),
                     'Captura año calendario 2023'=c("-",round(c((cs1[1]*0.7)+(cs1[2]*0.3),
                                                   (cs2[1]*0.7)+(cs2[2]*0.3),
                                                   (cs3[1]*0.7)+(cs3[2]*0.3))/1000,0)))
  
  colnames(Capturaproy)<-c("$R_{2023}$",escRecl)
  
  #-----------
  # TABLA 
  #-----------
  kbl(Capturaproy, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=8)%>% 
    add_header_above(c("Estimación de Captura por Hito" = 1,
                       "R actualizado" = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    pack_rows(index = c("Hito 1" = 5))
}


# Hito 2 ----
tb8_H2<-function(label_tb,Rdata_proy_Hito,escRecl,especie,region,caption1){
  
  load(Rdata_proy_Hito)
  
  Capturaproy<-rbind('Supuesto Captura actual 2022/23'=c(round((cs0[1])/1000,0),"-","-","-"),
                     'Supuesto Captura primer semestre 2023'=c(round((cs0[1]*0.7)/1000,0),"-","-","-"),
                     'Supuesto Captura proyectada 2023/24'=c("-",round(c(cs1[1],cs2[1],cs3[1])/1000,0)),
                     'Supuesto Captura segundo semestre 2023'=c("-",round(c(cs1[1]*0.3,cs2[1]*0.3,cs3[1]*0.3)/1000,0)),
                     'Captura año calendario 2023'=c("-",round(c((cs0[1]*0.7)+(cs1[1]*0.3),
                                                         (cs0[1]*0.7)+(cs2[1]*0.3),
                                                         (cs0[1]*0.7)+(cs3[1]*0.3))/1000,0)))
  
  colnames(Capturaproy)<-c("$R_{2023}$",escRecl)
  
  #-----------
  # TABLA 
  #-----------
  kbl(Capturaproy, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=8)%>% 
    add_header_above(c("Estimación de Captura por Hito" = 1,
                       "R actualizado" = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    pack_rows(index = c("Hito 2" = 5))
}



# Hito 3 ----
tb8_H3<-function(label_tb,Rdata_proy_HitoH1,Rdata_proy_HitoH2,Rdata_proy_HitoH3,escRecl,especie,region,caption1){
  
  # HITO 1 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH1)
  
  CapturaproyH1<-rbind('Supuesto Captura proyectada 2022/2023'=c("-",round(c(cs1[1],cs2[1],cs3[1])/1000,0)),
                     'Supuesto Captura primer semestre 2023'=c("-",round(c(cs1[1]*0.7,cs2[1]*0.7,cs3[1]*0.7)/1000,0)),
                     'Supuesto Captura proyectada 2023/2024'=c("-",round(c(cs1[2],cs2[2],cs3[2])/1000,0)),
                     'Supuesto Captura Segundo Semestre 2023'=c("-",round(c(cs1[2]*0.3,cs2[2]*0.3,cs3[2]*0.3)/1000,0)),
                     'Captura año calendario 2023'=c("-",round(c((cs1[1]*0.7)+(cs1[2]*0.3),
                                                                 (cs2[1]*0.7)+(cs2[2]*0.3),
                                                                 (cs3[1]*0.7)+(cs3[2]*0.3))/1000,0)))
  # HITO 2 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH2)
  
  CapturaproyH2<-rbind('Supuesto Captura actual 2022/23'=c(round((cs0[1])/1000,0),"-","-","-"),
                     'Supuesto Captura primer semestre 2023'=c(round((cs0[1]*0.7)/1000,0),"-","-","-"),
                     'Supuesto Captura proyectada 2023/24'=c("-",round(c(cs1[1],cs2[1],cs3[1])/1000,0)),
                     'Supuesto Captura segundo semestre 2023'=c("-",round(c(cs1[1]*0.3,cs2[1]*0.3,cs3[1]*0.3)/1000,0)),
                     'Captura año calendario 2023'=c("-",round(c((cs0[1]*0.7)+(cs1[1]*0.3),
                                                         (cs0[1]*0.7)+(cs2[1]*0.3),
                                                         (cs0[1]*0.7)+(cs3[1]*0.3))/1000,0)))
  
  # HITO 3 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH3)
  
  CapturaproyH3<-rbind('Captura Oficial 2022/23'=c(round(cs0d/1000,0),"-","-","-"),
                     'Captura Oficial primer semestre 2023'=c(round(cs01erS/1000,0),"-","-","-"),
                     'Supuesto Captura proyectada 2023/24'=c("-",round(c(cs1[1],cs2[1],cs3[1])/1000,0)),
                     'Supuesto Captura segundo semestre 2023'=c("-",round(c(cs1[1]*0.3,cs2[1]*0.3,cs3[1]*0.3)/1000,0)),
                     'Captura año calendario 2023'=c("-",round(c(cs0d+(cs1[1]*0.3),
                                                         cs0d+(cs2[1]*0.3),
                                                         cs0d+(cs3[1]*0.3))/1000,0)))
  #------------------------------------------------------------------------------------------------
  Capturaproy<-rbind(CapturaproyH1,CapturaproyH2,CapturaproyH3)
  
  colnames(Capturaproy)<-c("$R_{2023}$",escRecl)
  
  #-----------
  # TABLA 
  #-----------
  kbl(Capturaproy, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=8)%>% 
    add_header_above(c("Estimación de Captura por Hito" = 1,
                       "R actualizado" = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    pack_rows(index = c("Hito 1" = 5,"Hito 2" = 5,"Hito 3" = 5))
}


# CBA supuesto 70%-30% sin descuento descarte ----
# Hito 1 ----
tb9_H1<-function(label_tb,Rdata_proy_HitoH1,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  
  load(Rdata_proy_HitoH1)
  tCBA<-t(data.frame(CBAp_sept,CBApstd_sept,CBA_sept))
  colnames(tCBA)  <-escRecl
  row.names(tCBA) <-c('mean','sd',percentiles)
  #-----------
  # TABLA 
  #-----------
  kbl(round(tCBA,0), booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023" = 3))
  
}

# Hito 2 ----
tb9_H2<-function(label_tb,Rdata_proy_HitoH2,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  load(Rdata_proy_HitoH2)
  tCBA<-t(data.frame(CBAp_H2y3,CBApstd_H2y3,CBA_H2y3))
  colnames(tCBA)  <-escRecl
  row.names(tCBA) <-c('mean','sd',percentiles)
  #-----------
  # TABLA 
  #-----------
  kbl(round(tCBA,0), booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023" = 3))
  
}
# Hito 3 ----
tb9_H3<-function(label_tb,Rdata_proy_HitoH1,Rdata_proy_HitoH2,Rdata_proy_HitoH3,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  
  # HITO 1 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH1)
  tCBAH1<-t(data.frame(CBAp_sept,CBApstd_sept,CBA_sept))
  row.names(tCBAH1) <-c('mean','sd',percentiles)
  # HITO 2 ---------------------------------------------------------------------------------------- 
  load(Rdata_proy_HitoH2)
  tCBAH2<-t(data.frame(CBAp_H2y3,CBApstd_H2y3,CBA_H2y3))
  row.names(tCBAH2) <-c('mean','sd',percentiles)
  # HITO 3 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH3)
  tCBAH3<-t(data.frame(CBAp_H2y3,CBApstd_H2y3,CBA_H2y3))
  row.names(tCBAH3) <-c('mean','sd',percentiles)
  #------------------------------------------------------------------------------------------------
  tCBA<-rbind(tCBAH1,tCBAH2,tCBAH3)
  
  colnames(tCBA)  <-escRecl
  #----------- 
  # TABLA 
  #-----------
  kbl(round(tCBA,0), booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023" = 3))%>% 
    pack_rows(index = c("Hito 1" = 7,"Hito 2" = 7,"Hito 3" = 7))
  
}


# CBA supuesto 70%-30% y descuento descarte ----
# Hito 1----
tb10_anch_H1<-function(label_tb,Rdata_proy_HitoH1,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  # HITO 1 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH1)
  tCBAd<-t(data.frame(CBApd_sept,CBApdstd_sept,CBAd_sept))
  row.names(tCBAd)<-c('mean','sd',percentiles)
  colnames(tCBAd)<-escRecl
  #-----------
  # TABLA 
  #-----------
  kbl(round(tCBAd,0), booktabs = T,format = "latex",position="h!",escape = FALSE,align="c",
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023 - 2\\% descarte" = 3))
  
}

tb10_anch_H2<-function(label_tb,Rdata_proy_HitoH2,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%") 
  # HITO 2 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH2)
  tCBAd<-t(data.frame(CBApd_H2y3,CBApdstd_H2y3,CBAd_H2y3))
  row.names(tCBAd)<-c('mean','sd',percentiles)
  colnames(tCBAd)<-escRecl
  #-----------
  # TABLA 
  #-----------
  kbl(round(tCBAd,0), booktabs = T,format = "latex",position="h!",escape = FALSE,align="c",
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023 - 2\\% descarte" = 3))
  
}

tb10_anch_H3<-function(label_tb,Rdata_proy_HitoH1,Rdata_proy_HitoH2,Rdata_proy_HitoH3,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%") 
  # HITO 1 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH1)
  tCBAdH1<-t(data.frame(CBApd_sept,CBApdstd_sept,CBAd_sept))
  row.names(tCBAdH1)<-c('mean','sd',percentiles)
  # HITO 2 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH2)
  tCBAdH2<-t(data.frame(CBApd_H2y3,CBApdstd_H2y3,CBAd_H2y3))
  row.names(tCBAdH2)<-c('mean','sd',percentiles)
  # HITO 3 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH3)
  tCBAdH3<-t(data.frame(CBApd_H2y3,CBApdstd_H2y3,CBAd_H2y3))
  row.names(tCBAdH3)<-c('mean','sd',percentiles)
  
  tCBAd<-rbind(tCBAdH1,tCBAdH2,tCBAdH3)
  colnames(tCBAd)<-escRecl
  #-----------
  # TABLA 
  #-----------
  kbl(round(tCBAd,0), booktabs = T,format = "latex",position="h!",escape = FALSE,align="c",
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023 - 2\\% descarte" = 3))%>% 
    pack_rows(index = c("Hito 1" = 7,"Hito 2" = 7,"Hito 3" = 7))
  
}
#==================================================================================================
# RESGUARDO CBA ----
#==================================================================================================
# HITO 1 ----
tb11_H1<-function(label_tb,Rdata_proy_HitoH1,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  load(Rdata_proy_HitoH1)
  buffer2<-data.frame(t(buffer))
  row.names(buffer2)<-percentiles
  colnames(buffer2)<-escRecl

  # TABLA 
  kbl(buffer2, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      format.args=list(decimal.mark = ','),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=9)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "Resguardo" = 3))
}
# HITO 2 ----
tb11_H2<-function(label_tb,Rdata_proy_HitoH2,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%") 
  load(Rdata_proy_HitoH2)
  buffer2<-data.frame(t(buffer))
  row.names(buffer2)<-percentiles
  colnames(buffer2)<-escRecl

  # TABLA 
  kbl(buffer2, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      format.args=list(decimal.mark = ','),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "Resguardo" = 3))
}

# HITO 3----
tb11_H3<-function(label_tb,Rdata_proy_HitoH1,Rdata_proy_HitoH2,Rdata_proy_HitoH3,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%") 
  # HITO 1 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH1)
  buffer2H1<-data.frame(t(buffer))
  row.names(buffer2H1)<-percentiles
  # HITO 2 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH2)
  buffer2H2<-data.frame(t(buffer))
  row.names(buffer2H2)<-percentiles
  # HITO 3 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH3)
  buffer2H3<-data.frame(t(buffer))
  row.names(buffer2H3)<-percentiles
  
  buffer2<-rbind(buffer2H1,buffer2H2,buffer2H3)
  colnames(buffer2)<-escRecl
  
  # TABLA 
  kbl(buffer2, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      format.args=list(decimal.mark = ','),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "Resguardo" = 3))%>% 
    pack_rows(index = c("Hito 1" = 5,"Hito 2" = 5,"Hito 3" = 5))
  
  
}



#  Considera el descuento del desembarque  segundo  semestre año previo y  remanente  de cuota año previo

tb8b_H2<-function(label_tb,Rdata_proy_HitoH2,escRecl,especie,region,caption1){
  
  
  load(Rdata_proy_HitoH2)
  
  Capturaproy<-rbind('Año biológico proyectada 2022/23'=c(round((cs0[1])/1000,0),"-","-","-"),
                     'Desembarque segundo semestre 2022'=c(round(desem2doSem/1000,0),"-","-","-"),
                     'Saldo  cuota 2022'=c(round(remanente/1000,0),"-","-","-"),
                     'Primer Semestre 2023'=c(round((cs0R[1])/1000,0),"-","-","-"),
                     'Año biológico proyectada 2023/24'=c("-",round(c(cs1[1],cs2[1],cs3[1])/1000,0)),
                     'Segundo Semestre 2023'=c("-",round(c(cs1[1]*0.3,cs2[1]*0.3,cs3[1]*0.3)/1000,0)),
                     'Año calendario 2023'=c("-",round(c((cs0R[1])+(cs1[1]*0.3),
                                                         (cs0R[1])+(cs2[1]*0.3),
                                                         (cs0R[1])+(cs3[1]*0.3))/1000,0)))
  
  colnames(Capturaproy)<-c("$R_{2023}$",escRecl)
  
  #-----------
  # TABLA 
  #-----------
  kbl(Capturaproy, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c("Captura" = 1,
                       "R actual" = 1,
                       "Escenarios de reclutamiento" = 3),line = F)
}

tb8b_H3<-function(label_tb,Rdata_proy_HitoH1,Rdata_proy_HitoH2,Rdata_proy_HitoH3,escRecl,especie,region,caption1){
  
  pd<-0.98 #porcentaje de descarte
  # HITO 1 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH1)
  
  CapturaproyH1<-rbind(
    '$Captura_{2022/23}$ = $F_{RMS}$'=c("-",round(c((cs1[1]*pd),(cs2[1]*pd),(cs3[1]*pd))/1000,0)),
    '$Captura_{2doSem_{2022}} = 30\\%Captura_{2022/23}$'=c("-",round(c((cs1[1]*pd)*0.3,
                                                                       (cs2[1]*pd)*0.3,
                                                                       (cs3[1]*pd)*0.3)/1000,0)),
    '$Captura_{1erSem_{2023}} = 70\\%Captura_{2022/23}$'=c("-",round(c((cs1[1]*pd)*0.7,
                                                                       (cs2[1]*pd)*0.7,
                                                                       (cs3[1]*pd)*0.7)/1000,0)),
    '$Remanente_{cuota2022}$ (sin antecedentes)'=c("-","-","-","-"),
    '$Captura_{2023/24}$ = $F_{RMS}$'=c("-",round(c((cs1[2]*pd),(cs2[2]*pd), (cs3[2]*pd))/1000,0)),
    '$Captura_{2doSem_{2023}} = 30\\%Captura_{2023/24}$' =c("-",round(c((cs1[2]*pd)*0.3,
                                                                        (cs2[2]*pd)*0.3,
                                                                        (cs3[2]*pd)*0.3)/1000,0)),
    'Captura año calendario 2023'=c("-",round(c(((cs1[1]*pd)*0.7)+((cs1[2]*pd)*0.3),
                                                ((cs2[1]*pd)*0.7)+((cs2[2]*pd)*0.3),
                                                ((cs3[1]*pd)*0.7)+((cs3[2]*pd)*0.3))/1000,0)))
  # HITO 2 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH2)
  
  CapturaproyH2<-rbind(
    '$Captura_{2022/23}$ = $F_{RMS}$'=c(round((cs0d[1])/1000,0),"-","-","-"),
    '$Captura_{2doSem_{2022}} = Desembarque_{2doSem_{2022}}$'=c(round(desem2doSem/1000,0),"-","-","-"),
    '$Captura_{1erSem_{2023}} = Captura_{2022/23} -  Desembarque_{2doSem_{2022}}$'=c(round((cs0d[1]-desem2doSem)/1000,0),"-","-","-"),
    '$Remanente_{cuota 2022}$ (autorizado por ley)' =c(round(remanente/1000,0),"-","-","-"),
    '$Captura Corregida_{1erSem_{2023}} = Captura_{1erSem_{2023}} - Remanente_{cuota 2022}$'=c(round(cs01erSR/1000,0),"-","-","-"),
    '$Captura_{2023/24}$ = $F_{RMS}$'=c("-",round(c((cs1[1]*pd),
                                                    (cs2[1]*pd),
                                                    (cs3[1]*pd))/1000,0)),
    '$Captura_{2doSem_{2023}} = 30\\%Captura_{2023/24}$'=c("-",round(c((cs1[1]*pd)*0.3,
                                                                       (cs2[1]*pd)*0.3,
                                                                       (cs3[1]*pd)*0.3)/1000,0)),
    'Captura año calendario 2023'=c("-",round(c(cs01erSR+((cs1[1]*pd)*0.3),
                                                cs01erSR+((cs2[1]*pd)*0.3),
                                                cs01erSR+((cs3[1]*pd)*0.3))/1000,0)))
  # HITO 3 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH3)
  
  CapturaproyH3<-rbind(
    '$Captura_{2022/23}$ = $F_{2022/23}$'=c(round((cs0d[1])/1000,0),"-","-","-"),
    '$Captura_{2doSem_{2022}} = Desembarque_{2doSem_{2022}}$'=c(round(desem2doSem/1000,0),"-","-","-"),
    '$Captura_{1erSem_{2023}} = Desembarque_{1erSem_{2023}}$'=c(round((cs0d[1]-desem2doSem)/1000,0),"-","-","-"),
    '$Remanente_{cuota 2022}$ (consumido a mayo 2023)' =c(round(remanente/1000,0),"-","-","-"),
    '$Captura Corregida_{1erSem_{2023}} = Captura_{1erSem_{2023}} - Remanente_{cuota 2022}$'=c(round(cs01erSR/1000,0),"-","-","-"),
    '$Captura_{2023/24}$ = $F_{RMS}$' =c("-",round(c((cs1[1]*pd),(cs2[1]*pd),(cs3[1]*pd))/1000,0)),
    '$Captura_{2doSem_{2023}} = 30\\%Captura_{2023/24}$'=c("-",round(c((cs1[1]*pd)*0.3,
                                                                       (cs2[1]*pd)*0.3,
                                                                       (cs3[1]*pd)*0.3)/1000,0)),
    'Captura año calendario 2023' =c("-",round(c(cs01erSR+((cs1[1]*pd)*0.3),
                                                 cs01erSR+((cs2[1]*pd)*0.3),
                                                 cs01erSR+((cs3[1]*pd)*0.3))/1000,0)))

  Capturaproy<-rbind(CapturaproyH1,CapturaproyH2,CapturaproyH3)
  
  colnames(Capturaproy)<-c("$R_{2023}$",escRecl)
  
  #-----------
  # TABLA 
  #-----------
  kbl(Capturaproy, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c("Criterios por Hito" = 1,
                       "R actualizado" = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    pack_rows(index = c("Hito 1" = 7,"Hito 2" = 8,"Hito 3" = 8))%>% 
    row_spec(c(7,15,23),italic = TRUE,bold=TRUE) %>% 
    footnote(number = "Captura año calendario 2023 = $Captura_{1erSem_{2023}}$ + $Captura_{2doSem_{2023}}$",
             threeparttable = T,escape = FALSE,fixed_small_size = TRUE)
}


# CBA considerando el descuento del desembarque 2do semestre y saldo cuota año previo ----
# Hito 2 ----
tb9B_H2<-function(label_tb,Rdata_proy_HitoH2,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  load(Rdata_proy_HitoH2)
  tCBA<-t(data.frame(CBAp_H2y3D2SR,CBApstd_H2y3D2SR,CBA_H2y3D2SR))
  colnames(tCBA)  <-escRecl
  row.names(tCBA) <-c('mean','sd',percentiles)
  #-----------
  # TABLA 
  #-----------
  kbl(round(tCBA,0), booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023" = 3))
  
}
# Hito 3 ----
tb9B_H3<-function(label_tb,Rdata_proy_HitoH1,Rdata_proy_HitoH2,Rdata_proy_HitoH3,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  # HITO 1 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH1)
  tCBAH1<-t(data.frame(CBAp_sept,CBApstd_sept,CBA_sept))
  row.names(tCBAH1) <-c('mean','sd',percentiles)
  # HITO 2 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH2)
  tCBAH2<-t(data.frame(CBAp_H2y3D2SR,CBApstd_H2y3D2SR,CBA_H2y3D2SR))
  row.names(tCBAH2) <-c('mean','sd',percentiles)
  # HITO 3 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH3)
  tCBAH3<-t(data.frame(CBAp_H2y3D2SR,CBApstd_H2y3D2SR,CBA_H2y3D2SR))
  row.names(tCBAH3) <-c('mean','sd',percentiles)
  
  tCBA<-rbind(tCBAH1,tCBAH2,tCBAH3)
  colnames(tCBA)  <-escRecl
  # TABLA ----
  kbl(round(tCBA,0), booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023" = 3))%>% 
    pack_rows(index = c("Hito 1" = 7,"Hito 2" = 7,"Hito 3" = 7))
  
}


# CBA descuento descarte y remanente ----
# Hito 2 ----
tb10B_anch_H2<-function(label_tb,Rdata_proy_HitoH2,escRecl,especie,region,caption1){
  
  load(Rdata_proy_HitoH2)
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  tCBAd<-t(data.frame(CBApd_H2y3D2SdR,CBApdstd_H2y3D2SdR,CBAd_H2y3D2SdR))
  colnames(tCBAd)<-escRecl
  row.names(tCBAd)<-c('mean','sd',percentiles)
# TABLA ----
  kbl(round(tCBAd,0), booktabs = T,format = "latex",position="h!",escape = FALSE,align="c",
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023 - 2\\% descarte" = 3))
  
}


# CBA descuento descarte y remanente ----
# Hito 3 ----
tb10B_anch_H3<-function(label_tb,Rdata_proy_HitoH1,Rdata_proy_HitoH2,Rdata_proy_HitoH3,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  # HITO 1 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH1)
  tCBAdH1<-t(data.frame(CBApd_sept,CBApdstd_sept,CBAd_sept))
  row.names(tCBAdH1)<-c('mean','sd',percentiles)
  # HITO 2 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH2)
  tCBAdH2<-t(data.frame(CBApd_H2y3D2SdR,CBApdstd_H2y3D2SdR,CBAd_H2y3D2SdR))
  row.names(tCBAdH2)<-c('mean','sd',percentiles)
  
  # HITO 3 ----------------------------------------------------------------------------------------
  load(Rdata_proy_HitoH3)
  tCBAdH3<-t(data.frame(CBApd_H2y3D2SdR,CBApdstd_H2y3D2SdR,CBAd_H2y3D2SdR))
  row.names(tCBAdH3)<-c('mean','sd',percentiles)
  
  tCBAd<-rbind(tCBAdH1,tCBAdH2,tCBAdH3)
  colnames(tCBAd)<-escRecl
  
  # TABLA ----
  kbl(round(tCBAd,0), booktabs = T,format = "latex",position="h!",escape = FALSE,align="c",
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023 - 2\\\\% descarte" = 3))%>% 
    pack_rows(index = c("Hito 1" = 7,"Hito 2" = 7,"Hito 3" = 7))
  
}


# Resguardo ----
# Hito 2 ----
tb11B_H2<-function(label_tb,Rdata_proy_HitoH2,escRecl,especie,region,caption1){
  
  load(Rdata_proy_HitoH2)
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  buffer2<-data.frame(t(bufferD2SR))
  colnames(buffer2)<-escRecl
  row.names(buffer2)<-percentiles
  # TABLA ----
  kbl(buffer2, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      format.args=list(decimal.mark = ','),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "Resguardo" = 3))
}

# Hito 3 ----
tb11B_H3<-function(label_tb,Rdata_proy_HitoH1,Rdata_proy_HitoH2,Rdata_proy_HitoH3,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  load(Rdata_proy_HitoH1)
  buffer2H1<-data.frame(t(buffer))
  row.names(buffer2H1)<-percentiles
  
  load(Rdata_proy_HitoH2)
  buffer2H2<-data.frame(t(bufferD2SR))
  row.names(buffer2H2)<-percentiles
  
  load(Rdata_proy_HitoH3)
  buffer2H3<-data.frame(t(bufferD2SR))
  row.names(buffer2H3)<-percentiles
  
  buffer2<-rbind(buffer2H1,buffer2H2,buffer2H3)
  
  colnames(buffer2)<-escRecl
  
  # TABLA ----
  kbl(buffer2, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      format.args=list(decimal.mark = ','),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>%
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "Resguardo" = 3))%>% 
    pack_rows(index = c("Hito 1" = 5,"Hito 2" = 5,"Hito 3" = 5))
}


# Diferencia CBA Hito 1 vs Hito 2
tb12_anch_H2<-function(label_tb,Rdata_proy_Hito1,Rdata_proy_Hito2,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  load(Rdata_proy_Hito1)
  CBAd_sept
  
  load(Rdata_proy_Hito2)
  CBAd_H2y3
  
  tabladiferencias<--round(1-(CBAd_H2y3/CBAd_sept),2)*100
  colnames(tabladiferencias)<-percentiles
  row.names(tabladiferencias)<-escRecl
  # TABLA ----
  kbl(round(t(tabladiferencias),0), booktabs = T,format = "latex",position="h!",escape = FALSE,align="c",
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023 - 2\\% descarte" = 3))
  
}

tb13_anch_H2<-function(label_tb,Rdata_proy_Hito1,Rdata_proy_Hito2,escRecl,especie,region,caption1){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  load(Rdata_proy_Hito1)
  CBAd_sept
  
  load(Rdata_proy_Hito2)
  CBAd_H2y3D2SdR
  
  tabladiferencias<--round(1-(CBAd_H2y3D2SdR/CBAd_sept),2)*100
  colnames(tabladiferencias)<-percentiles
  row.names(tabladiferencias)<-escRecl
  # TABLA ----
  kbl(round(t(tabladiferencias),0), booktabs = T,format = "latex",position="h!",escape = FALSE,align="c",
      format.args=list(big.mark = '.'),
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>% 
    add_header_above(c(" " = 1,
                       "Escenarios de reclutamiento" = 3),line = F)%>% 
    add_header_above(c(" " = 1,
                       "CBA 2023 - 2\\% descarte" = 3))
  
}


# escenarios de supuesto de captura  Hito 2 ----

tb14<-function(label_tb,comp,especie){
  #attach(pbrs)  
  
  Tabla4.1<-rbind("Año biológico"=comp$yearStatus,
                  "$F_{2022/23}$ $(año^{-1})$"=formatC(round(comp$Flast,2), decimal.mark = ","),
                  "$BD_{2022/23}$ (mil t)"=round(comp$BDlast/10^3,0),
                  "$F_{2022/23}/F_{RMS}$ "=formatC(round(comp$F_Frms,2), decimal.mark = ","),
                  "$BD_{2022/23}/BD_{RMS}$ "=formatC(round(comp$BD_BDrms,2), decimal.mark = ","),
                  "$p(BD_{2022/23}<BD_{RMS})^1$"=formatC(round(comp$pa,3), decimal.mark = ","),
                  "$p(F_{2022/23}>F_{RMS})^2$"=formatC(round(comp$pb,3), decimal.mark = ","),
                  "$p(sobre-explotación)^3$"=formatC(round(comp$pc,3), decimal.mark = ","),
                  "$p(agotado/colapsado)^4$"=formatC(round(comp$pd,3), decimal.mark = ","),
                  "$p(sobrepesca)^5$"=formatC(round(comp$pe,3), decimal.mark = ","))
  
  colnames(Tabla4.1)<-comp$Hitos
  
  footnote<-c("Probabilidad que $BD$ del año más reciente sea menor a $BD_{RMS}$ según el diagrama de fase",
              "Probabilidad que $F$ del año más reciente sea mayor a $F_{RMS}$ según el diagrama de fase",
              "Probabilidad de estar en sobreexplotación = $p(0,5<BD_{last}/BD_{RMS}<0,9)$",
              "Probabilidad de estar en colapso =$p(BD_{last}/BD_{RMS}<0,5)$",
              "Probabilidad de estar en sobrepesca = $p(F_{last}/F_{RMS}>1,1)$")
  
  # TABLA ----
  
  caption1<-"\\ Comparación entre el caso base y escenarios alternativos de 
  captura del año biológico actuallas estimaciones de mortalidad por pesca, 
  biomasa desovante  del año biológico actual y 
probabilidades de estar bajo $BD_{RMS}$ y sobre $F_{RMS}$ y en sobreexplotación, 
colapsado o sobrepesca. "
  
  kbl(Tabla4.1, booktabs = T,format = "latex",position="h!",align="c",escape = FALSE,
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>% 
    kable_styling(latex_options = c("striped"),
                  full_width = FALSE,font_size=9) %>%
    footnote(number = footnote,threeparttable = T,escape = FALSE,fixed_small_size = TRUE) %>%
    column_spec(1, width = "25em") %>% 
    add_header_above(c(" " = 1,"Escenarios" = 5))
  
}

#comparación CBA criterio 1 ----
tb10_anch_C1<-function(label_tb,escRecl,especie,region){
  
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  escenarios <-c(rep("S1",5),rep("S2",5),rep("S3",5),rep("S4",5),rep("Caso base",5))
  
  load(Rdata_proy_Hito2)
  CBAd_H2y3S0 <- t(CBAd_H2y3)
  load(Rdata_proy_Hito2S1)
  CBAd_H2y3S1 <- t(CBAd_H2y3)
  load(Rdata_proy_Hito2S2)
  CBAd_H2y3S2 <- t(CBAd_H2y3)
  load(Rdata_proy_Hito2S3)
  CBAd_H2y3S3 <- t(CBAd_H2y3)
  load(Rdata_proy_Hito2S4)
  CBAd_H2y3S4 <- t(CBAd_H2y3)
  
  
  tCBAd<-data.frame(escenarios,percentiles,
                    format(round(rbind(CBAd_H2y3S1,CBAd_H2y3S2,
                                       CBAd_H2y3S3,CBAd_H2y3S4,CBAd_H2y3S0),0),big.mark = '.'))
  
  
  colnames(tCBAd)<-c("Escenarios","Percentiles",escRecl)
  #row.names(tCBAd)<-escenarios
  
  caption1 <-"Estimación de CBA 2023 de los cuatro escenarios de supuesto de captura 2022/23
              considerando el 
              descuento del 2\\% de descarte. Criterio 1."

  # TABLA ----
  kbl(tCBAd, booktabs = T,format = "latex",position="h!",escape = FALSE,align="c",
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>%
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>%
    add_header_above(c(" " = 2,
                       "Escenarios de reclutamiento" = 3),line = F)%>%
    add_header_above(c(" " = 2,
                       "CBA 2023 - 2\\% descarte" = 3))
  
}

#comparación CBA criterio 2 ----
tb10_anch_C2<-function(label_tb,escRecl,especie,region){
  
  percentiles<-c("10\\%","20\\%","30\\%","40\\%","50\\%")
  escenarios <-c(rep("S1",5),rep("S2",5),rep("S3",5),rep("S4",5),rep("Caso base",5))
  
  load(Rdata_proy_Hito2)
  CBAd_H2y3S0 <- t(CBAd_H2y3D2SdR)
  load(Rdata_proy_Hito2S1)
  CBAd_H2y3S1 <- t(CBAd_H2y3D2SdR)
  load(Rdata_proy_Hito2S2)
  CBAd_H2y3S2 <- t(CBAd_H2y3D2SdR)
  load(Rdata_proy_Hito2S3)
  CBAd_H2y3S3 <- t(CBAd_H2y3D2SdR)
  load(Rdata_proy_Hito2S4)
  CBAd_H2y3S4 <- t(CBAd_H2y3D2SdR)
  
  
  tCBAd<-data.frame(escenarios,percentiles,
                    format(round(rbind(CBAd_H2y3S1,CBAd_H2y3S2,
                                       CBAd_H2y3S3,CBAd_H2y3S4,CBAd_H2y3S0),0),big.mark = '.'))
  
  
  colnames(tCBAd)<-c("Escenarios","Percentiles",escRecl)
  #row.names(tCBAd)<-escenarios
  
  caption1 <-"Estimación de CBA 2023 de los cuatro escenarios de supuesto de captura 2022/23
              considerando el 
              descuento del 2\\% de descarte. Criterio 2."
  
  # TABLA ----
  kbl(tCBAd, booktabs = T,format = "latex",position="h!",escape = FALSE,align="c",
      caption = paste(label_tb,caption1,especie,region,sep=" ")) %>%
    kable_styling(latex_options = c("striped", "condensed"),
                  full_width = FALSE,font_size=10)%>%
    add_header_above(c(" " = 2,
                       "Escenarios de reclutamiento" = 3),line = F)%>%
    add_header_above(c(" " = 2,
                       "CBA 2023 - 2\\% descarte" = 3))
  
}


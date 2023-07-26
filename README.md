Anchoveta_CBA2023


Para la actualización HITO 3 del archivo de datos:

- Cálculo del desembarque en año biológico 2022/2023, esto es el desembarque acumulado de julio a diciembre 2022 (segundo semestre) más el desembarque acumulado de enero a junio 2023 (primer semestre) de la flota total (artesanal e industrial). 
- Cálculo de la captura descartada en año biológico 2022/2023, esto es aplicar el %descarte (4% sardina y 2% anchoveta) al desembarque en año biológico 2022/23.  Esta captura descartada se debe sumar al desembarque anteriormente calculado para obtener la Captura2022/23 que se ingresa en el archivo .dat.
- Composición de edad de la flota total del año biológico 2022/23
- Pesos medios a la edad de la flota total del año biológico 2022/23
- Cálculo de pesos iniciales a la edad de la flota total de año biológico 2022/23
- Cálculo del promedio de los últimos 5 años  de los pesos medios
- Cálculo del promedio de los últimos 5 años de los pesos iniciales
- Biomasa estimada por el crucero de otoño 2023
- Composición de edad del crucero de otoño 2023
- Actualización de los tamaños de muestra de la flota, crucero de verano y otoño (utilizar función llamada NM_Ian() del código .Rmd que genera el informe)
- Actualización de Fmediana (utilizar función llamada Crms_Fmedian() del código .Rmd que genera el informe)
- Actualizar perfil de verosimilitud (utilizar función Verosimilitud() del código .Rmd que genera el informe)
- Actualizar análisis retrospectivo analítico (utilizar función Retrospectivo() del código .Rmd que genera el informe).
- Actualizar análisis retrospectivo histórico


Escenarios de sensibilidad a la actualización de los datos de entrada:

Esta sección la debemos incorporar como parte del diagnóstico. Para estos escenarios les recomiendo crear una carpeta especial para ello.

- **S1**=MAE0323, caso base asesoría de marzo 2023
- **S2**=S1 + Actualiza de Captura 2022/23 
- **S3**=S2 + Actualiza composición de edad de la flota 2022/23
- **S4**=S3 + Actualiza pesos medios e iniciales a la edad de la flota 2022/23
- **S5**=S4 + Actualiza biomasa del crucero de otoño 2023
- **S6**=S5 + Actualiza composición de edad del crucero de otoño 2023
- **S7**=S6 + Actualiza tamaños de muestra flota, crucero de verano y otoño (MAE723)




# Research project

 Applied Research I: Political Economy - ECO-40211 
 Fall 2021
 ITAM
 
El documento principal, "David Rojas, 2021 - Covid cases in Latvia.pdf", se encuentra en la carpeta \docs

El documento Rmd para generar este pdf, "4pager_20211121.Rmd", se encuentra en la carpeta \src\rmd\

Esta investigación identificó eventos que podrían ser instrumentalizados de manera propagandística en el contexto geopolítico actual de los países de la región báltica. Esta tabla se encuentra en un documento de excel en la siguiente ruta \data\raw\Eventos.xlsx

La base de datos con que se realizan las regresiones se exporta en la carpeta \data\processed
 
Las regresiones se ejecutan en la función \src\calculos.R y \src\calculos_con_controles.R

La base se preprocesa en \src\depurar_data.R y las librerías necesarias se encuentras listadas en \src\librerias.R

El cógido para generar las gráficas por separado se encuentra en la carpeta \src\ggplots

En la carpeta \src\anterior se encuentran variaciones de los cálculos por grupo étnico que no llegaron al reporte final.

En la carpeta \data\interim hay resultados de regresiones por lengua (en formato RData) que no llegaron al artículo final pero que podrían aportar.
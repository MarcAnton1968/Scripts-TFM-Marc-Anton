# Scripts-TFM-Marc-Anton
Scripts Treball Final de Master Bioinformatica i Bioestadistica de Marc Anton on s'analitza la sensibilitat del LPI-cat a la distribució de les espècies i la cobertura del seu mostratge

Em aquest repositori s'hi han dipositat els següents scripts:
1. DataCurRelAnalisiPipeline.Rmd.- És el pipeline de càrrega de dades i d'anàlisi de la primera part del TFM on s'analitza la relació entre la distribució de les espècies i la cobertura del mostratge que es fa d'elles en relació a la seva tendència, als valors extrems de la mateixa i a l'error estàndard que dú associat aquesta tendència.
2. Sesitivity_analisi.R.- És un script on es calculen diversos indicadors LPI-cat extraient espècies mal i ben distribuides i s'avalua fins a quin punt representen un canvi en l'indicador amb l'objectiu d'establir la sensibilitat d'aquest canvi en l'indicacdor LPI-cat. Aquest script crida a un altre que és el que fa els càlculs de l'indicador. És un script fet per Statistics Netherland que es va difondre entre tots els participants al projecte PECBMS (https://pecbms.info/) del qual l'ICO en forma part
3. MSI_23_TFM.R.- És l'script que  genera els  indicadors LPI-cat. Ho fa calculant una mitjana geomètrica dels índexs anuals de les tendències de caada espècie i generant uns intervals de confiança per iteracions de Montecarlo que també generen una línia suavitzada de l'indicador.
4. ouputs_informe_final.R.- Conté el codi per fer taules i gràfics per l'informe final

En el repositori també hi ha el html resultatnt de l'execució del rmd


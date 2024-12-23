#Ponderamos por departamento (solo para CABA y año 2010)
join_CODGEO_filter = join_CODGEO %>% select(ano, CODGEO, provincia, Provincia, departamen, Peso_al_nacer, personas, hogares, viv_part, Total, geometry) 

join_CODGEO_2010 = join_CODGEO_filter %>% filter(ano == 2010)

join_CODGEO_2010_CABA =join_CODGEO_2010 %>% filter(Provincia == '02')


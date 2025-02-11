
library(tidyverse)
# library(lubridate)

### Import raw database

base_raw = read.xlsx("base prevencion en accion - 2023.xlsx")
# # 
# write.csv(base_raw, "base prevencion en accion - 2023.csv", row.names = F)
# 
# base_raw <- read.csv("base prevencion en accion - 2023.csv", check.names = F)
# base_raw = read.csv("base prevencion en accion 07-12-22.csv", check.names = F)

### Variable selection

base = base_raw %>%
  select(Caso:`1_version`, `1_total`:`2_version`,`2_total`:`3_version`,`3_total`:`3_coradmin`) 


# Reorganize data from wide to long format

base_long = base %>% 
  pivot_longer(!Caso:OrdenTo, names_to = c("Time", ".value"),names_sep = "\\_") 


# Tidy data

base_long = base_long%>%   
  mutate(across(hsinic: hsfinc, convertToDateTime)) %>%
  # mutate(across(hsinic: hsfinc, lubridate::hms)) %>%  
  # mutate(across(hsinic: hsfinc, lubridate::as_datetime)) %>%              # Transform vector with date time to date class             
  mutate(RT = as.numeric(hsfinc - hsinic)/60  ) %>%                                # Create resolution times variable
  mutate(hs = as.numeric(if_else(Time == "1", hsfinc,                    #Put final times of Baseline and initial times of T1 in the same column
                                 if_else(Time == "2", hsinic, NA_Date_)))) %>%  
  group_by(Caso) %>% 
  mutate(Intervention_time = ifelse(Time == "1", diff(hs)/60, NA)) %>%      # Calculate intervention time with difference between Baseline and T1
  select(-hs) %>% 
  ungroup() %>% 
  mutate(across(c(ocpadre, ocmadre, Escuela), as.factor)) %>%             # Coerce character variables into factor variables
  mutate(Group = as.factor(case_when(                                     # Rename and re code levels for treatment groups
    grest == 1 ~ "Talk",
    grest == 2 ~ "Abbreviated workshop",
    grest == 3 ~ "Workshop"
  ))) %>%
  mutate(Group = factor(Group, levels = c("Talk", "Abbreviated workshop", "Workshop"))) %>% 
  mutate(Time = as.factor(case_when(                                     # Rename and re code levels for treatment groups
    Time == 1 ~ "Baseline",
    Time == 2 ~ "T1",
    Time == 3 ~ "T2"
  ))) %>% 
  mutate(ocmadre = as.factor(case_when(
    ocmadre == 12	~ "Director de empresa, Dueño de comercio (grande)",
    ocmadre == 11	~ "Profesional",
    ocmadre == 10	~ "Propietario de pequeña empresa o comercio",
    ocmadre == 8	~ "Técnico, docente, enfermero",
    ocmadre == 7	~ "Empleado administrativo, vendedor, patio de comidas, recepcionista",
    ocmadre == 6	~ "Pequeño productor autónomo, trabajador autónomo especializado",
    ocmadre == 4	~ "Obrero calificado, cadete, chofer, cocinero, mozo, mantenimiento, portero, ayudante de..., distribuidor de bebidas, albañil, modista, mecánico",
    ocmadre == 2	~ "Obrero no calificado, barrendero, costurero",
    ocmadre == 1	~ "Trabajador inestable, empleada doméstica, vendedor ambulante, changas, cosechas, venta en feria, producciones en el hogar no estable",
    ocmadre == 0	~ "Desocupado, no trabaja, ama de casa, jefes de hogar"
  ))) %>% 
  mutate(ocpadre = as.factor(case_when(
    ocpadre == 12	~ "Director de empresa, Dueño de comercio (grande)",
    ocpadre == 11	~ "Profesional",
    ocpadre == 10	~ "Propietario de pequeña empresa o comercio",
    ocpadre == 8	~ "Técnico, docente, enfermero",
    ocpadre == 7	~ "Empleado administrativo, vendedor, patio de comidas, recepcionista",
    ocpadre == 6	~ "Pequeño productor autónomo, trabajador autónomo especializado",
    ocpadre == 4	~ "Obrero calificado, cadete, chofer, cocinero, mozo, mantenimiento, portero, ayudante de..., distribuidor de bebidas, albañil, modista, mecánico",
    ocpadre == 2	~ "Obrero no calificado, barrendero, costurero",
    ocpadre == 1	~ "Trabajador inestable, empleada doméstica, vendedor ambulante, changas, cosechas, venta en feria, producciones en el hogar no estable",
    ocpadre == 0	~ "Desocupado, no trabaja, ama de casa, jefes de hogar"
  ))) %>%
  # mutate(Baseline =)
  rename("Score" = "total") %>%                                        # Rename performance variables 
  rename("Proportion" = "coradmin") %>% 
  rename("Case" = "Caso") 


  
baseline.data = base_long %>% 
  filter(Time == "Baseline") %>% 
  mutate(Baseline.Score = Score,
         Baseline.Proportion = Proportion,
         Baseline.RT = RT) %>% 
  select(Case, Baseline.Score, Baseline.Proportion, Baseline.RT)


base_source = left_join(base_long, baseline.data)



## Data about conceptual knowledge

# Reorganize data from wide to long format


base_concept = base_raw %>% 
  select(Caso, grest,`1_hsinic`:`3_coradmin`) %>%
  pivot_longer(!c(Caso, grest), names_to = c("Time", ".value"),names_sep = "\\_") %>% 
  filter(!is.na(admin))


# Tidy data

base_concept = base_concept%>%                                                    
  mutate(across(hsinic: hsfinc, lubridate::as_datetime)) %>%                  # Transform vector with date time to date class             
  mutate(RT = as.numeric(hsfinc - hsinic)) %>%                                # Create resolution times variable
  mutate(hs = as.numeric(if_else(Time == "1", hsfinc,                         #Put final times of Baseline and initial times of T1 in the same column
                                 if_else(Time == "2", hsinic, NA_Date_)))) %>%  
  group_by(Caso) %>% 
  mutate(Intervention_time = ifelse(Time == "1", diff(hs)/60, NA)) %>%        # Calculate intervention time with difference between Baseline and T1
  select(-hs) %>% 
  ungroup() %>% 
  
  mutate(Group = as.factor(case_when(                                     # Rename and re code levels for treatment groups
    grest == 1 ~ "Talk",
    grest == 2 ~ "Abb. Workshop",
    grest == 3 ~ "Workshop"
  ))) %>%
  mutate(Group = factor(Group, levels = c("Talk", "Abb. Workshop", "Workshop"))) %>% 
  mutate(Time = as.factor(case_when(                                     # Rename and re code levels for treatment groups
    Time == 1 ~ "Baseline",
    Time == 2 ~ "T1",
    Time == 3 ~ "T2"
  ))) %>% 
  select(Caso:version, total:Group, everything())


base_long_concept = base_concept %>% 
  pivot_longer(!c(Caso:Group), names_to = "Pregunta", values_to = "Respuesta") %>% 
  rename(Pregunta_short = Pregunta) %>% 
  mutate(Pregunta = case_when(
    
    Pregunta_short == "repelentes" ~ "Does the use of repellents help avoid mosquito bites? ",                                                                           
    Pregunta_short == "selva"      ~ "Does the mosquito that spreads dengue fever breed more in the jungle than in the city?",                                                                                              
    Pregunta_short == "mortal"     ~ "Can dengue kill you?",                                                                                          
    Pregunta_short == "tirar"      ~ "To avoid mosquitoes, do you have to throw away containers that collect stagnant water?",                                                           
    Pregunta_short == "mangaco"    ~ "To avoid mosquito bites, do you have to wear short-sleeved clothing?",                                                                        
    Pregunta_short == "espalda"    ~ "Do mosquitoes bite more on the face and back than on the legs and arms?",                                                                   
    Pregunta_short == "arroyos"    ~ "Does the aedes aegypti mosquito lay its eggs in streams?",                                                                                     
    Pregunta_short == "tapar"      ~ "If a container collects water and it cannot be emptied, should we cover it so that mosquitoes do not breed there?", 
    Pregunta_short == "lluvia"     ~ "After it rains, do we have to eliminate the containers with water in them to avoid mosquito breeding?",                        
    Pregunta_short == "persona"    ~ "Besides mosquitoes, can people also spread dengue fever?",                                                                                              
    Pregunta_short == "larva"      ~ "Before flying, is the mosquito a larva that lives in water?",                                                                            
    Pregunta_short == "aspirina"   ~ "If you think you may have dengue, should you take an aspirin?",              
    Pregunta_short == "manchas"    ~ "Is the aedes aegypti black with white spots and stripes on its body and legs?",                                              
    Pregunta_short == "dolor"      ~ "Does dengue cause fever and body aches?",                                                                                               
    Pregunta_short == "mediodia"   ~ "Does the mosquito that transmits dengue bite at noon?"                                                                           
    
  )) 







#Problema 1, a)




#Problema1, b)
# Funcție pentru calcularea sumei elementelor unui vector
suma <- function(vector) {
  sum_vector <- 0
  for (i in 1:length(vector))
  {
    sum_vector <- sum_vector + vector[i]
  }
  return (sum_vector)
}

# Funcție pentru calcularea mediei elementelor unui vector
medie <- function(vector){
  return (suma(vector) / length(vector))
}

# Funcție pentru calcularea abaterii standard a elementelor unui vector
abatere_standard <- function(vector){
  suma_var2 <- 0
  medie_x <- medie(vector)
  for (i in 1:length(vector)){
    suma_var2 <- suma_var2 + (vector[i] - medie_x) ^ 2
  }
  suma_var2 <- suma_var2 / (length(vector) - 1)
  return (sqrt(suma_var2))
}

#test student
test_student <- function(vector, c) {
  n <- length(vector)
  x_bar <- mean(vector)
  s <- sd(vector)
  t <- (x_bar - c) / (s / sqrt(n))
  return (t)
}
# Funcție pentru calcularea coeficientului de corelatie liniara intre 2 variabile
correlation_coefficient <- function(vector1, vector2) {
  n <- length(vector1)
  sum_xy <- sum(vector1 * vector2)
  sum_x <- sum(vector1)
  sum_y <- sum(vector2)
  sum_x_squared <- sum(vector1^2)
  sum_y_squared <- sum(vector2^2)
  numarator <- n * sum_xy - sum_x * sum_y
  numitor <- sqrt((n * sum_x_squared - sum_x^2) * (n * sum_y_squared - sum_y^2))
  correlation_coefficient <- numarator / numitor
  return (correlation_coefficient)
}
#Problema 1 c)
#solutia 1 calcul:
sol1 = function(x)
{
  vanzari=matrix(x, nrow=12, ncol=4)
  medii <-colMeans(vanzari)
  return (cbind(c("Magazin 1", "Magazin 2", "Magazin 3", "Magazin 4"), medii))
}

#solutia 2 calcul:
sol2 =function(x)
{
  vanzari=matrix(x,nrow=12,ncol=4)
  medii <- apply(vanzari, 2, mean)
  return (medii)
}


#Problema 1 d)
# Setul de date cu informații de bază despre piloți
driver_info <- data.frame(
  driver = c("Hamilton", "Verstappen", "Bottas", "Pérez", "Norris"),
  team = c("Mercedes", "Red Bull", "Mercedes", "Red Bull", "McLaren"),
  country = c("United Kingdom", "Netherlands", "Finland", "Mexico", "United Kingdom"),
  age = c(37, 24, 32, 31, 22),
  races = c(274, 95, 174, 198, 41)
)

# Setul de date cu informații suplimentare despre piloți
driver_stats <- data.frame(
  driver = c("Hamilton", "Verstappen", "Bottas", "Pérez", "Norris"),
  points = c(94, 80, 47, 44, 41),
  wins = c(2, 1, 0, 0, 0),
  poles = c(1, 1, 0, 0, 0)
)

# Setul de date cu informații despre echipamentele de cursă
team_info <- data.frame(
  team = c("Mercedes", "Red Bull", "McLaren"),
  engine = c("Mercedes", "Honda", "Mercedes"),
  chassis = c("Mercedes", "Red Bull", "McLaren")
)

# Adăugăm coloane noi la setul de date cu informații de bază
driver_info <- cbind(driver_info, driver_stats[, c("points", "wins", "poles")])

# Combinăm setul de date cu informații despre echipamentele de cursă cu setul de date combinat
driver_info <- merge(driver_info, team_info, by = "team")

# Combinăm setul de date cu informații de bază și echipamentele de cursă cu setul de date de performanță
driver_performance <- data.frame(
  driver = c("Hamilton", "Verstappen", "Bottas", "Pérez", "Norris"),
  team = c("Mercedes", "Red Bull", "Mercedes", "Red Bull", "McLaren"),
  lap_time = c(79.203, 79.284, 79.368, 79.459, 79.536),
  top_speed = c(348.6, 347.9, 347.0, 346.3, 345.6)
)

driver_info_performance <- inner_join(driver_info, driver_performance, by = c("driver", "team"))

# Afișăm setul de date final
driver_info_performance

#Problema 2
library(magrittr)
library(readr)
library(dplyr)
chess_players <- read.csv("C:/Users/Raul/Downloads/top_chess_players_aug_2020.csv/top_chess_players_aug_2020.csv")

#transformam vectorii aferenti variabilelor categoriale in variabile de tip factor
chess_players$Name <-as.factor(chess_players$Name)
chess_players$Federation <-as.factor(chess_players$Federation)
chess_players$Gender <-as.factor(chess_players$Gender)
chess_players$Title <-as.factor(chess_players$Title)

#folosim tapply pentru a determina in variabila mean_Blitz_rating valoarea medie a ratingului jucatorilor pentru fiecare federatie in parte
#folosim na.rm= TRUE pentru a exclude jucatorii care nu au rating
mean_Blitz_rating <- tapply(chess_players$Blitz_rating, chess_players$Federation, function(x) mean(x, na.rm = TRUE))
print(mean_Blitz_rating)

#folosim aggregate pentru a agrega jucatorii dupa 2 variabile, genul si federatia, si de a calcula pentru toate combinatiile de gen-federatie ratingul mediu la rapid
#la fel, am folosit parametrul na.rm=TRUE pentru a omite jucatorii fara rating la categoria Blitz
mean_rapid_rating <- aggregate(chess_players$Rapid_rating, by=list(chess_players$Title, chess_players$Gender), FUN=function(x) mean(x, na.rm=TRUE))
print(mean_rapid_rating)
#filtram jucatorii din baza de date, ii extragem doar pe cei din USA cu rating mai mare de 2700 si barbati
#prin comanda select, alegem sa extragem din baza de date doar anumite detalii legate de jucatori, in acest caz numele federatia si ratingul standard
us_players <- chess_players %>%
  filter(Federation == "USA" & Standard_Rating >= 2700 & Gender=="M" ) %>%
  select(Name, Federation, Standard_Rating)
print(us_players)

#prin mutate creeam o noua coloana care sa contina varsta aproximativa a jucatorilor in anul 2023 (avem doar anul nasterii)
#folosim select pentru a extrage numele si varsta
chess_players %>%
  mutate(age = 2023 - Year_of_birth ) %>%
  select(Name, age)

#filtram jucatorele care sunt active si din federatiile principale, adica Rusia, China si USA
#le aranjam in ordine descrescatoare dupa ratingul de la rapid si prin select extragem doar anumite coloane pe care le consideram relevante
chess_players %>%
  filter(Inactive_flag!="i" & Gender=="F" & Federation %in% c("RUS", "CHN", "USA")) %>%
  arrange(desc(Rapid_rating)) %>%
  select(Name, Rapid_rating, Title, Federation)


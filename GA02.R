library(GA)
 
#ustawienie parametrów algorytmu ewolucyjnego

min_x = -2
max_x = 2
min_y = -2
max_y = 2
pop_size = 20
pc = 0.2
pm = 0.1
maxiter = 100
seed = 1
sleep = -1

#funkcja optymalizowana

Funkcjaa <- function(x,y)
{
  (x^2 - y^2) - (1 -x)^2
}
 #funkcja dopasowania

fitness = function(x) Funkcjaa(x[1], x[2])

# wykres funkcji



x <- y <- seq(-2,2, by = 0.2)
f <- outer(x, y, Funkcjaa)
persp3D(x, y, f , theta =50, phi =20, col.palette =bl2gr.colors)
#filled.contour(x, y, f, color.palette = bl2gr.colors)

# funkcja monitoruj¹ca dzia³anie algorytmu

monitor <- function(obj)
{
  contour(x, y, f, drawlabels = FALSE, col = grey(0.5))
  title(paste("iteration = ", obj@iter), font.main = 1)
  points(obj@population, pch = 20, col = 2)
  Sys.sleep(0.2)
}

# urchomienie algorytmu

GA <- ga(
  type = "real-valued"
  ,fitness = fitness
  ,lower = c(min_x,min_y)
  ,upper = c(max_x,max_y)
  ,popSize = pop_size
  ,pcrossover = pc
  ,pmutation = pm
  ,maxiter = maxiter
  ,keepBest = TRUE
  ,seed = seed
)

#wyœwietlanie podsumowanie
summary(GA)

# wykres - najlepiej dopasowany osobnik
abline(v = GA@solution, lty =3 )

#wykres - mediana
plot(GA)

#wyœwietlenie wartoœci najlepszego przystosowania osiagniêtego w ka¿dej iteracji
GA@bestSol


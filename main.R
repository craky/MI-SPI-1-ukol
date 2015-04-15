# http://en.wikipedia.org/wiki/Exponential_distribution#Generating_exponential_variates
K = 5 # jmeno
L = 6 # prijmeni
n = K*20
u=runif(n, min=0, max=1) # Generuje rovnoměrné rozdělení
x=-log(1-u)/L # Exp rozdělení z rovnoměrného inverzní distr. fcí.

#################################################################
# 1.2
#hist(u)
hist(x, breaks=5*K, freq=FALSE) # Histogram našich hodnot
xWidth=max(x)-min(x) # Rozpětí hodnot
xGrid=seq(min(x)-0.2*xWidth,max(x)+0.2*xWidth,length=30) # vytvoří hodnoty kvantilů (?)
lines (xGrid,dexp(xGrid, rate=L), col='red') # přiloží graf hustoty k histogramu

################################################################
# 1.3
plot(ecdf(x), verticals=TRUE, do.points = FALSE, main="Distribuční funkce") # Opsáno ze zadáni, graf emp. exp. fce
lines (xGrid,pexp(xGrid, rate = L), col='red')  # přiloží graf teoretické fce

################################################################
# 1.4
y=rexp(1000, rate=L)
qqplot(x, y)
abline(0,1, col='red', lwd=2)

###############################################################
# 2.1
lambda = function(t) { 100 + 50*exp(-(t - 420)^2/(3600*L)) + 100*exp(-(L*(-30*L+t-480)^2)/360000)}
t=seq(0,24*60-1)                                   #Počet minut ve dnu, počítáno od nuly
# !!! V zadání je 50*exp, v příkladu je 50/exp !!!
plot (t,lambda(t), lty="solid", lwd=3, type='l')   # Vykreslení lambdy (type je malý L, ne jednička)

###############################################################
# 2.2
# Sestrojíme K*10 hodnot z exp rozdělení pro dané lambda

event = numeric(K*10)             # Pole událostí
delta = 1 / 1000                  # Delta t
tau   = 0                         # Aktuální bod na časové ose
i     = 1                         # Index

while (i<=K*10) {
  current_time = tau %/% 1        # Přiřadíme t dolní celou část tau 
  tau = tau + delta               # Tau zvýšíme o delta t
  rand = runif(1, min=0, max=1)   # Náhodná hodnota
  if (rand < lambda(current_time) * delta) {
    event[i] = tau                # S danou pravděpodobností nastane událost
    i = i + 1                     # Zvýšíme index
  }
}

eventGrid = numeric(K*10)         # Mřížka nul pro vykreslení grafu
plot (event, eventGrid)           # Vykreslení grafu časů příchodů

#################################################################
# 2.3

per_minute = numeric(24*60)       # Vektor uschovávající počet příchodů za minutu
delta      = 1 / 1000             # Delta t
j          = 0                    # Index
tau        = 0                    # Aktuální bod na časové ose

# Bacha běží to fakt dlouho, nezvyšujte moc deltu !!!
while (j<24*60) {
  j = tau %/% 1                   # Přiřadíme j dolní celou část tau 
  tau = tau + delta               # Tau zvýšíme o delta t
  rand = runif(1, min=0, max=1)   # Náhodná hodnota
  if (rand < lambda(j) * delta) { # S danou pravděpodobností nastane událost
    per_minute[j+1] = per_minute[j+1] + 1
  }
}

write(per_minute2, file = "per_minute.txt", ncolumns = 1, append = FALSE)

# Doporučovaný histogram nepotřebuju, mám hodnoty v per_minute
plot (t,per_minute, lwd=1, type='l')   # Vykreslí experimentálně zjištěná data
lines (t,lambda(t), lwd=3, col='red')  # Přiloží graf teoretické fce



############################################################################
# 3
kuryr = K/(K+L)                   # Pravděpodobnost, že si vezmou kurýra
per_minute_kuryr = numeric(24*60) # Vektor uschovávající počet odvozů kurýrem

i = 1                             # Index
while (i<=24*60) {
  j = 1                           # Index
  while (j<=per_minute[i]) {
    rand = runif(1, min=0, max=1) # Náhodná hodnota
    if (rand < kuryr) {           # S danou pravděpodobností zvolí kurýra
      per_minute_kuryr[i] = per_minute_kuryr[i] + 1
    }
    j = j + 1
  }
  i = i + 1
}

per_minute_posta = per_minute - per_minute_kuryr  # Jednoduchý doplněk

write(per_minute_kuryr, file = "per_minute_kuryr.txt", ncolumns = 1, append = FALSE)
write(per_minute_posta, file = "per_minute_posta.txt", ncolumns = 1, append = FALSE)

plot  (t,per_minute_posta,    lwd=1, col='grey', type='l') # Vykreslí experimentálně zjištěná data
lines (t,lambda(t)*(1-kuryr), lwd=3, col='red')            # Přiloží graf teoretické fce

lines (t,per_minute_kuryr, lwd=1, col='grey', type='l')    # Vykreslí experimentálně zjištěná data
lines (t,lambda(t)*kuryr,  lwd=3, col='blue')              # Přiloží graf teoretické fce
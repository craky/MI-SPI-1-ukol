# http://en.wikipedia.org/wiki/Exponential_distribution#Generating_exponential_variates
K = nchar('Tomas') # jmeno
L = nchar('Nesrovnal') # prijmeni

#################################################################
# 1

n = K*20
u=runif(n, min=0, max=1) # Generuje n náhodných hodnot z UNIF(0,1)
x=-log(1-u)/L # Exp rozdělení z rovnoměrného inverzní distr. fcí.

#################################################################
# 1.2
hist(u, freq=FALSE)
hist(x, breaks=5*K, freq=FALSE) # Histogram našich hodnot
#help(hist) freq --> logical;
#if TRUE, the histogram graphic is a representation of frequencies, the counts component of the result;
#if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one).
xWidth=max(x)-min(x) # Rozpětí hodnot
xGrid=seq(min(x)-0.2*xWidth,max(x)+0.2*xWidth,length=n) # vytvoří hodnoty kvantilů (?)
lines (xGrid,dexp(xGrid, rate=L), col='red') # přiloží graf hustoty k histogramu

################################################################
# 1.3
plot(ecdf(x), verticals=TRUE, do.points = FALSE, main="Distribuční funkce") # Opsáno ze zadáni, graf emp. exp. fce
lines (xGrid,pexp(xGrid, rate = L), col='red')  # přiloží graf teoretické fce

################################################################
# 1.4
y=rexp(n, rate=L) # hodnoty ze stejného rozdělení jako se kterým porovnáváme
qqplot(x, y)
abline(0,1, col='red', lwd=2)
#Pokud jsou hodnoty obou rozdělení stejné, ležely by na červené čáře. Hodnoty nejdál od čáry se nejvíce vychylují.

################################################################
# 1.5

################################################################
# 1.6

chisq.test(x, p=y/sum(y)) 

# Chi-squared test for given probabilities
# data:  x
# X-squared = 58.0799, df = 99, p-value = 0.9997
# Warning message:
# In chisq.test(x, p = y/sum(y)) : Chi-squared approximation may be incorrect

#TODO: najit ks.test a zkusit to podle toho

###############################################################
# 2.1

# opsano z eduxu: 2.I
lambda = function(t){100+50*exp(-(t-420)^2/(3600*L))+100*exp(-(L*(-30*L+t-480)^2)/360000)}
# prvni perioda
t=seq(0,24*60-1)
#TODO: popsat osu v levo
plot(t,lambda(t),lty="solid",lwd=3,type='l', main="Intenzita přístupů za den")
###############################################################
# 2.2

p = K*10 # prichod
event = numeric(p) #array TODO: prejmenovat
s = 10^-6 # step TODO: vypocitat z lamdy
t = 0 # time = Aktuální čas
i = 0 # iterator

# Cyklus simuluje ubíhající čas (po skocích delta) a na základě
# funkce lambda generuje příchod zákazníka
while(i < p) {
  if (runif(1, min=0, max=1) < lambda(t) * s) {
    event[i] = t # oznacime cas udalosti
    i = i + 1                     # Zvýšíme index
  }
  t = t + s
} 
plot(event, numeric(p))

#################################################################
# 2.3

per_minute = numeric(24*(60))    # Vektor uschovávající počet příchodů za minutu
delta      = 1 / 1000             # Delta t
j          = 0                    # Index
tau        = 0                    # Aktuální bod na časové ose

# Bacha běží to fakt dlouho, nezvyšujte moc deltu !!!
# Cyklus simuluje ubíhající čas (po skocích delta) a zvyšuje četnost příchodu na základě fce lambda
# Četnost výskytů se akumuluje do "binů" šířky jedné minuty
while (j<24*60) {
  j = tau %/% 1                   # Přiřadíme j dolní celou část tau 
  tau = tau + delta               # Tau zvýšíme o delta t
  rand = runif(1, min=0, max=1)   # Náhodná hodnota
  if (rand < lambda(j) * delta) { # S danou pravděpodobností nastane událost
    per_minute[j+1] = per_minute[j+1] + 1
  }
}

write(per_minute, file = "per_minute.txt", ncolumns = 1, append = FALSE)

# Doporučovaný histogram nepotřebuju, mám hodnoty v per_minute
plot (t,per_minute, lwd=1, type='l')   # Vykreslí experimentálně zjištěná data
lines (t,lambda(t), lwd=3, col='red')  # Přiloží graf teoretické fce



############################################################################
# 3
kuryr = K/(K+L)                   # Pravděpodobnost, že si vezmou kurýra
per_minute_kuryr = numeric(24*60) # Vektor uschovávající počet odvozů kurýrem

i = 1                             # Index
# Cyklus prochází všechny minutové výskyty a spočíta pravděpodobnosti, že zákazník použije kurýra
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

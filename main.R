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

lambda = function(t){100+50*exp(-(t-420)^2/(3600*L))+100*
                       exp(-(L*(-30*L+t-480)^2)/360000)}
# prvni perioda
t=seq(0,3*(24*60)-1)
#TODO: popsat osu v levo
plot(t,lambda(t%%(24*60)),lty="solid",lwd=3,type='l',
     main="Intenzita přístupů za den",
     ylab="Příchody za minutu",
     xlab="Čas t v minutách")
text(0, 200, "Den 1", cex=0.6, pos=4)
text(24*60, 200, "Den 2", cex=0.6, pos=4)
text(24*120, 200, "Den 3", cex=0.6, pos=4)
abline(h = 0, v = 01)
abline(h = 0, v = 24*60)
abline(h = 0, v = 24*120)
###############################################################
# 2.2

p = K*10 # prichod
event = numeric(p) #array TODO: prejmenovat
s = 10^-(2.4) # step
t = 0 # time = Aktuální čas
i = p # iterator

# Cyklus simuluje ubíhající čas (po skocích delta) a na základě
# funkce lambda generuje příchod zákazníka
while (0 < i) {
  if (runif(1, min=0, max=1) < lambda(t) * s) {
    i = i - 1
    event[i] = t # oznacime cas udalosti
  }
  t = t + s
} 
plot(event, numeric(p))

#################################################################
# 2.3

day = 24*60
cetnosti_za_minutu = numeric(day) # pro cely den
day_seq = seq(0,day-1)
`+<-` <- `+`
t = 0
while (t < day) {
  if (runif(1, min=0, max=1) < lambda(t) * s) {
    `+`(cetnosti_za_minutu[(t %/% 1)+1])<-1
  }
  t = t + s
}

# edux style - nefunguje to a chce se mi brecet
h = hist(cetnosti_za_minutu, plot=FALSE, breaks=seq(0,300))
plot(h$mids, h$counts, type="l")
lines(day_seq,lambda(day_seq), lwd=3, col='red')

# fitwiki style
plot(day_seq, cetnosti_za_minutu, lwd=1, type='l',
      main="Četnosti příchodů za celý den",
      ylab="Příchody za minutu",
      xlab="Čas t v minutách")
lines(day_seq,lambda(day_seq), lwd=3, col='red')

############################################################################
# 3
kuryr = K/(K+L)                   # Pravděpodobnost, že si vezmou kurýra
za_minutu_kuryr = numeric(day) # Vektor uchovávající počet odvozů kurýrem po minutach
za_minutu_posta = numeric(day) # Vektor uchovávající počet odvozů poštou po minutach

i = 1                             # Index
# Cyklus prochází všechny minutové výskyty a spočíta pravděpodobnosti, že zákazník použije kurýra
while (i<=day) {
  j = 1                           # Index
  while (j<=cetnosti_za_minutu[i]) {
    rand = runif(1, min=0, max=1) # Náhodná hodnota [0,1]
    if (rand < kuryr) {           # S danou pravděpodobností zvolí kurýra
      za_minutu_kuryr[i] = za_minutu_kuryr[i] + 1
    }else{                        # Pokud nezvolí kurýra zvolí si poštu
      za_minutu_posta[i] = za_minutu_posta[i] + 1
    }
    j = j + 1
  }
  i = i + 1
}


plot  (day_seq,za_minutu_posta,    lwd=1, col='grey', type='l') # Vykreslí experimentálně zjištěná data
lines (day_seq,lambda(day_seq)*(1-kuryr), lwd=3, col='red')            # Přiloží graf teoretické fce

lines (day_seq,za_minutu_kuryr, lwd=1, col='grey', type='l')    # Vykreslí experimentálně zjištěná data
lines (day_seq,lambda(day_seq)*kuryr,  lwd=3, col='blue')              # Přiloží graf teoretické fce

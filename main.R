# MP-SPI 2015 ukol 1
# zdrojovy kod
# autori:
#   nesrotom - 107 - Nesrovnal Tomas
#   krakovoj - 103 - Krakora Vojtech
#   sabattom - 103 - Sabata Tomas
#################################################################
`+<-` <- `+` # operator += 
`-<-` <- `-` # operator -=
#################################################################
# NASTAVENI PROMENNYCH K, L
K=nchar('Tomas') # |jmeno|, K = 5
L=nchar('Nesrovnal') # |prijmeni|, L =10
#################################################################
# 1.1
# http://en.wikipedia.org/wiki/Exponential_distribution#Generating_exponential_variates
n=K*20
u=runif(n, min=0, max=1) # Generuje n náhodných hodnot z UNIF(0,1)
x=-log(1-u)/L # Exp rozdělení z rovnoměrného inverzní distr. fcí.
#################################################################
# 1.2
hist(u,freq=FALSE,
     main="TODO1",
     ylab="TODO2",
     xlab="TODO3")
hist(x,breaks=5*K,freq=FALSE
     main="TODO1",
     ylab="TODO2",
     xlab="TODO3") # Histogram našich hodnot
#help(hist) freq --> logical;
#if TRUE, the histogram graphic is a representation of frequencies, the counts component of the result;
#if FALSE, probability densities, component density, are plotted (so that the histogram has a total area of one).
xWidth=max(x)-min(x) # Rozpětí hodnot
xGrid=seq(min(x)-0.2*xWidth,max(x)+0.2*xWidth,length=n) # vytvoří hodnoty kvantilů (?)
lines(xGrid,dexp(xGrid,rate=L),col="red") # přiloží graf hustoty k histogramu
################################################################
# 1.3 Opsáno ze zadáni, graf emp. exp. fce
plot(ecdf(x),verticals=TRUE,do.points=FALSE,
     main="Distribuční funkce",
     ylab="TODO4",
     xlab="TODO5") 
lines(xGrid,pexp(xGrid,rate=L),col="red")
################################################################
# 1.4
y=rexp(n,rate=L) # hodnoty ze stejného rozdělení jako se kterým porovnáváme
qqplot(x,y)
abline(0,1,col="red",lwd=2)
#Pokud jsou hodnoty obou rozdělení stejné, ležely by na červené čáře. Hodnoty nejdál od čáry se nejvíce vychylují.
################################################################
# 1.5
################################################################
# 1.6 TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO 
chisq.test(x, p=y/sum(y)) 

# Chi-squared test for given probabilities
# data:  x
# X-squared = 58.0799, df = 99, p-value = 0.9997
# Warning message:
# In chisq.test(x, p = y/sum(y)) : Chi-squared approximation may be incorrect

#TODO: najit ks.test a zkusit to podle toho

###############################################################
# 2.1
day=24*60
t=seq(0,3*day-1) # prvni perioda
plot(t,lambda(t%%(24*60)),lty="solid",lwd=3,type="l",
     main="Intenzita přístupů za den",
     ylab="Příchody za minutu",
     xlab="Čas t v minutách")
text(0,200,"Den 1",cex=0.6,pos=4)
text(24*60,200,"Den 2",cex=0.6,pos=4)
text(24*120,200,"Den 3",cex=0.6,pos=4)
abline(h=0,v=0)
abline(h=0,v=24*60)
abline(h=0,v=24*120)
###############################################################
# 2.2
p=K*10 # pocet prichodu
cas_prichodu=numeric(p)
s=10^-3 # krok v case = 1/1000 minuty
t=0 # time = Aktuální čas
# Cyklus simuluje ubíhající čas (po skocích delta) a na základě
# funkce lambda generuje příchod zákazníka
i = p # budeme delat p prichodu
while (0 < i) {
  # FIXME: tady to chce chytrej komentar jak to funguje
  if (runif(1, min=0, max=1) < lambda(t) * s) {
    `-`(i)<-1 # odebereme jeden prichod
    cas_prichodu[i] = t # oznacime cas udalosti
  }
  `+`(t)<-s # udelame krok v case
}
# aby byl graf jako na eduxu, vypiseme spolu s polem
# cas_prichodu i pole nul
plot(cas_prichodu, numeric(p))
#################################################################
# 2.3
cetnosti_za_minutu=numeric(day) # pro cely den
day_seq=seq(0,day-1)
lamda_day_seq=lambda(day_seq)
# 2.3 EDUX -----------------------------------------------------
# potrebujeme, zarovnat hodnoty lamdy mezi 0-1. to udelame tak
# ze vysledek lamda(x) vydelime maximalni hodnotou
max_lambda = max(lambda(day_seq))
scaled_lambda = function(t){lambda(t)/max_lambda}
arrivals = NULL;
posledni_prichod = 0
while (posledni_prichod < day) {
  `+`(posledni_prichod)<-rexp(1,rate=max_lambda)
  if (runif(1, min=0, max=1) < (scaled_lambda(posledni_prichod))){
    arrivals = append(arrivals, posledni_prichod)
  }
} 
hi = hist(arrivals, plot=FALSE, breaks=day)
plot(hi$counts[hi$mids], type="l", col='gray',
     xlab="Čas t (v minutach)",
     ylab="Příchody za minutu")
lines (day_seq, lamda_day_seq, type="l", lwd=3, col="red")
# 2.3 VYLEPSENO ------------------------------------------------
# V "cestnosti_za_minutu[m]" bude pocet prichodu
# za minutu "m".
# Cely den projdu po krocich "s" a pokud nahodne
# vygenerovane cislo bude pod mez, pridam prisup
# k dane minute. Protoze krok "s" je mensi nez
# minuta, je potreba oriznout desetinnou cast
# od casu "t", abysme dostali cele minuty.
t = day
while (0 < t) {
  if (runif(1, min=0, max=1) < lambda(t) * s) {
    `+`(cetnosti_za_minutu[(t %/% 1)+1])<-1
  }
  `-`(t)<-s
}
plot(day_seq, cetnosti_za_minutu, lwd=1, type='l',
     main="Četnosti příchodů za celý den",
     ylab="Příchody za minutu",
     xlab="Čas t v minutách")
lines(day_seq,lamda_day_seq, lwd=3, col='red')
############################################################################
# 3
vezme_kuryra=K/(K+L)  # Ze všech zákazníků K/(K+L) použije kurýrní službu
za_minutu_kuryr=numeric(day) 
za_minutu_posta=numeric(day)
# Cyklus prochází všechny minutové výskyty a spočíta pravděpodobnosti,
# že zákazník použije kurýra
m=day
while (0 < m) { # pro kazdou minutu po cely den
  o=cetnosti_za_minutu[m] # pocet objednavek za minutu m
  while(0<o){ # pro vsechny objednavky za tu minutu
    if(runif(1,min=0,max=1)<vezme_kuryra){ # zvoli kuryra?
      `+`(za_minutu_kuryr[m])<-1
    }else{ # nebo postu?
      `+`(za_minutu_posta[m])<-1
    }
    `-`(o)<-1
  }
  `-`(m)<-1
}
plot(day_seq,za_minutu_kuryr,lwd=1,col='lightgreen',type='l',
     ylim=c(10,150),
     main="Způsoby dodávky",
     ylab="Četnosti příchodů objednávek",
     xlab="Čas t v minutách")
lines(day_seq,lamda_day_seq*vezme_kuryra,lwd=3,col='green')
lines(day_seq,za_minutu_posta,lwd=1, col='lightpink',type='l')
# pravdepodovnost, ze vezmou postu je doplnkem k tomu,
# ze vezmou kuryra
lines(day_seq,lamda_day_seq*(1-vezme_kuryra),lwd=3,col='red')
legend(x=1200,y=150,c("kurýr","pošta"),cex=.8, 
       col=c("green","red"),lty=c(1,1))
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
L=nchar('Nesrovnal') # |prijmeni|, L = 10
#################################################################
n = 20;         # nastaveni dle eduxu
alpha = 0.01;   # nastaveni dle eduxu
# ?rnorm
# ?t.test
x = rnorm(n, mean=10.5, sd=1.3);

hypothesisTest = t.test(x, mu=10, conf.level = 1-alpha);
#Testujem hypotezu zdali ma rozdeleni X stredni hodnotu 10
print(hypothesisTest); # Printing of the result is useful if you

# A
?qt
# kritická hodnota
                                  # parametr lower.tail podle eduxu?
criticalValue <- qt(.995,n-1);    # moc nechapu proc tam je .995 a ne .99 (mozna kvuli oboustranosti)
#Stred interavlu tedy prumer
xmean <- mean(x);
#odchylka
stdDev <- sd(x);
sqrtn <- sqrt(n);
?c
intv <- criticalValue*stdDev/sqrtn

confint = c(xmean - intv, xmean + intv)
print(hypothesisTest$conf.int -confint)
#neb je to 0 0 tak se zda ze interval mame dobre.

#B I
in_interval <- function(x, interval){
  stopifnot(length(interval) == 2L)
  interval[1] < x & x < interval[2]
}
mu <- 10
in_interval(mu, confint)
# in_interval= true, Hypotézu že 10 je střední hodnotou rozdělení X nezamítáme.
# pravděpodobnost chyby nevíme
# Ha zamítáme s pravděpodobností chyby aplha


#C I
testT = (mean(x) - 10)/sqrt(var(x)/(n-1))
nint=c(-qt(.995, n-1), +qt(.995, n-1))
in_interval(testT, nint)
# True takže nEzamítáme.

greater=t.test(x, mu=10, alternative = "greater", conf.level = 1-alpha);
less=t.test(x, mu=10, alternative = "less", conf.level = 1-alpha);
print(greater)
print(less)

#jelikoz zamítnutí nám dá více inforamce, a odhad prumeru je 10.6 tak
volíme greater
#neb máme vetsší pravdepodobnost zamítnutí
#B I
criticalValue <- qt(.990,n-1);
xmean <- mean(x);
stdDev <- sd(x);
sqrtn <- sqrt(n);
intv <- criticalValue*stdDev/sqrtn

confint = c(  xmean - intv, Inf  )
print(greater$conf.int)
print(confint)
#Intervali jsou stejne takze super.
in_interval(mu, confint)
#true -> H0 bohuzel nezamitame, HA zamitame s pravdepodobnosti chyby 1 procento.

#C II
testT = (mean(x) - 10)/sqrt(var(x)/(n-1))
nint=c(-qt(.990, n-1), Inf)
print(nint)
in_interval(testT, nint)
# Ani studentuv test nam nedovolil hypotezu zamitnout.



# 2 I
# a
n = 20;
alpha = 0.01
x = rnorm(n, mean=10, sd=1)
error = rnorm(n, mean=0.5, sd=0.8306624)
y = x + error
?t.test
result=t.test(x, y=y, paired = TRUE, alternative = "less", conf.level = 1-alpha)
print(result)
#zamitame Hypotezu HO a nezamitame Ha, s tim že pravdepodobnost chyba zamitnuti Ho
#je 1 procento.

# b
#Jde o prevedeni paroveho testu na jednovyberovy test.
#
diff = x - y
#alternativni hypoteza je mu(diff) je mensi nez 0.
muDiff=mean(diff)
criticalValue <- qt(.990,n-1);
xmean <- mean(diff);
stdDev <- sd(diff);
sqrtn <- sqrt(n);
intv <- criticalValue*stdDev/sqrtn
confint = c(-Inf  ,xmean + intv  )
print(confint)
in_interval(muDiff, confint)
# Nase hodnota lezi v intervalu tudiz neprekvapive nezamitame H0.


#2 II
n1 = 20;
n2 = 25;
alpha = 0.01
x=rnorm(n1, mean=10, sd=1.3)
y=rnorm(n2, mean=11.25, sd=1.3)
t.test(x, y=y, paired = FALSE, var.equal = TRUE, conf.level = 1-alpha)
#OD tede veskere hypotezi zamitae s pravdepodobnosti chyby 50 procent



#Nase modifiakce pro jednostrany test je velmi primocara
tmp=t.test(x, y=y, paired = FALSE, var.equal = TRUE, conf.level =
             1-alpha, alternative="less")
#I nyni s duverou ve vlastni usudek kostatujeme ze hypotezu Ha nezamitame zatimco hypotezu Ho zamitame.


# b
n=length(x)
m=length(y)
df=m+n-2
qt(.990,df);
?qt
Sx= var(x)
Sy= var(y)
Sxy <-sqrt(((Sx*(n-1)) + (Sy*(m-1))) / df )
print(Sxy)

#a nyni se pustimne do velkeho T
T= (  (mean(x)-mean(y)) / (Sxy*sqrt((1/n)+(1/m)) )    )
print(tmp)
print(T -tmp$t )
print(T)

p_value = pt(T, df)
print(p_value)
print(p_value < alpha)
# p_value je mensi nez alpha, tudiz testu verime

#2 III

# a
n1 = 20;
n2 = 25;
alpha = 0.01
x=rnorm(n1, mean=10, sd=1.3)
y=rnorm(n2, mean=11.28, sd=1.2)

t.test(x, y=y, paired = FALSE, var.equal = FALSE, conf.level = 1-alpha, alternative="less")

# Dle provedeneho T testu opet zamitame hypotezu H0 
# a nezamitame Ha. Nasemu rozhodnuti verime s pravdepodobnosti 99%.

#b

Sx = var(x)
Sy = var(y)
df = ((Sx/n1 + Sy/n2)^2)/(((Sx/n1)^2 / (n1-1)) + ((Sy/n2)^2 / (n2-1)))
Sxy = sqrt(Sx/n1 + Sy/n2)
T = (mean(x) - mean(y))/Sxy

p_value = pt(T, df)

print(paste("Df=", df, sep=""))
print(paste("T=", T, sep=""))
print(paste("P value=", p_value, sep=""))
print(p_value < alpha)

#3
#I
sequenceLength = 2000000;
x = runif(sequenceLength, 0, 100)
print(system.time(sort(x)))
#II
sampleSize = L*40;
time1 = time2 = numeric(sampleSize); # Declare an array
for(i in 1:sampleSize){
  x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
  # Measure sort times. The user-space time is at system.time(...)[1]
  # Inside system.time we must use x1 <- value and not x = value. The latter syntax is reserved for parameters.
  time1[i] = system.time(x1 <- sort(x, method = "quick"),  gcFirst = TRUE)[1];
  time2[i] = system.time(x2 <- sort(x, method = "shell"), gcFirst = TRUE)[1];
}

#jako Ho volíme že algoritmy stetřídí data za stejnou dobu
#jako Ha volíme že algoritmy stetřídí data za odlišnou dobu
diff=time1-time2
mean(diff)
alfa=K/100

less=t.test(diff, mu=0, alternative = "less", conf.level = 1-alpha);
print(less)
# S pravdebností chyby 1 procento tedy zamítám hypotézu Ho tedy to že by byli
#stejně rychlé, takže algoritujs je vskutku rychlejší
# Použili jseme jednostrný tes neb nás zajímalo jen zdali je stejně rychlý nebo rychlejší.

#III
for(i in 1:sampleSize){
  x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
  time1[i] = system.time(x1 <- sort(x, method = "quick"),  gcFirst = TRUE)[1];
}
sampleSize2 = L*35;
for(i in 1:sampleSize2){
  x = runif(sequenceLength, 0, 100); # Generate the sequence to be sorted
  time2[i] = system.time(x2 <- sort(x, method = "shell"), gcFirst = TRUE)[1];
}
result=t.test(time1, y=time2, paired = FALSE, var.equal = FALSE,
              conf.level = 1-alpha, , alternative = "less")
print(result$conf.int)
in_interval(0,result$conf.int)
#Výsledky obou tetsů se schodůjí a my tedy nejse zaskočeni ajen si pochvalujeme náš kvalitní úsudek.
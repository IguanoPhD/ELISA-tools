cutoff = 0.8686231                   #Enter the cutoff value here
OD = read.table("clipboard")
Dilutions = c(100,200,400,800,1600,3200,6400,12800)

regresiones = function(objeto) {
    obs = data.frame(Dilutions, objeto)
    
    simple = lm(Dilutions ~ ., data = obs)
    log10 = lm(log10(Dilutions)~ ., data = obs)
    reciprocal = lm(1/Dilutions~ ., data = obs)
    
    Titre_1 = coef(simple)[1] + (cutoff)*coef(simple)[2]
    
    A = coef(log10)[1] + (cutoff)*coef(log10)[2]
    Titre_2 = 10^A
    B = coef(reciprocal)[1] + (cutoff)*coef(reciprocal)[2]
    Titre_3=1/B
    
    c(Titre_1, Titre_2, Titre_3)
}

sapply(OD, regresiones)

simple = function(x) {
    obs = data.frame(Dilutions, x)
    summary(lm(Dilutions~ ., data = obs))
}
log_10 = function(x) {
    obs = data.frame(Dilutions, x)
    summary(lm(log10(Dilutions)~ ., data = obs))
}
reciprocal = function(x) {
    obs = data.frame(Dilutions, x)
    summary(lm(1/Dilutions~ ., data = obs))
}

lapply(OD, simple)
lapply(OD, log_10)
lapply(OD, reciprocal)

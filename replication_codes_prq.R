### Replication codes for Castanho Silva, Jungkunz, Helbling, and Littvay,
### 'An Empirical Comparison of Seven Populist Attitudes Scales'
### forthcoming in Political Research Quarterly.

# Load the required libraries

library(lavaan) # v0.6-2
library(mirt) # v1.29
library(semTools) # v0.5-0
library(lattice) # v0.20-35
library(gridExtra) # v2.3
library(psych) # v1.8.4
library(ggplot2) # v3.1.0
library(corrplot) # v0.84

# Read the dataset and column names, which are not there since these data have also to be read in Mplus:
data<-read.csv('data.merged_mar17.csv',header=F)

varnames<-unlist(strsplit(readLines('varnames_mar17.txt'), '\t'))
names(data)<-varnames

data[data==-999]<-NA


# Recode trust variables to make it more intuitive:
data$t_party.r<-6-data$t_party
data$t_parl.r<-6-data$t_parl
data$t_gov.r<-6-data$t_gov

## Table 1:
### CFA on the pooled data for each scale:
# Akkerman:
akkerman.cfa<-'akk =~ akker1 + akker2 + akker3 + akker4 + akker5 + akker6'
akkerman.cfa.fit<-cfa(data=data,model=akkerman.cfa,estimator='mlr',missing='fiml')
summary(akkerman.cfa.fit,fit.measures=T,standardized=T)
mean(c(.614,.669,.547,.690,.639,.486))

## CSES:
data$cses2.r<-6-data$cses2
cses.cfa<-'cses =~ akker6 + cses1 + cses2.r + cses3 + cses4 + cses5 + akker2'
cses.cfa.fit<-cfa(data=data,model=cses.cfa,estimator='mlr',missing='fiml')
summary(cses.cfa.fit,fit.measures=T,standardized=T)
mean(c(.461,.750,.441,.635,.084,.726,.484))

## Oliver and Rahn:
# First recode manich1, which was measured in 1-7 to a 1-5 scale, to match the others:
range1 <- function(x){ (x - min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)) * (5 - (1)) + 1 }
data$ow_me4<-range1(data$manich1)

or.cfa<-'
antiel =~ ow_ae1 + ow_ae2 + ow_ae3 + ow_ae4 + ow_ae5..
mistexp =~ ow_me1 + ow_me2 + ow_me3 + ow_me4
nataff =~ ow_na1 + ow_na2 + ow_na3'
or.cfa.fit<-cfa(data=data,model=or.cfa,estimator='mlr',missing='fiml')
summary(or.cfa.fit,fit.measures=T,standardized=T)
mean(c(.461,.595,.643,.596,.260,.667,.552,.052,.386,.299,.346,.513))

## Elchardus and Spruyt
es.cfa<-'es =~ es1 + es2 + es3 + es4'
es.cfa.fit<-cfa(data=data,model=es.cfa,estimator='mlr',missing='fiml')
summary(es.cfa.fit,fit.measures=T,standardized=T)
mean(c(.431,.594,.784,.130))

## Schulz et al. 2017
nccr.cfa<-'
antiel =~ nccr_ant1 + nccr_ant2 + nccr_ant3
sov =~ nccr_sov1 + nccr_sov2 + akker2
hom =~ nccr_hom1 + nccr_hom2 + nccr_hom3
pop =~ antiel + sov + hom'
nccr.cfa.fit<-cfa(data=data,model=nccr.cfa,estimator='mlr',missing='fiml')
summary(nccr.cfa.fit,fit.measures=T,standardized=T)
mean(c(.643,.545,.504,.761,.721,.683,.555,.701,.504,.708,.985,.550))

## Stanley 2011
stanley.cfa<-'sta =~ stanley1 + stanley2 + stanley3 + stanley4 + stanley5 + stanley6 + stanley7 + stanley8
method =~ stanley3 + stanley8
method ~~ 0*sta'
stanley.cfa.fit<-cfa(data=data,model=stanley.cfa,estimator='mlr',missing='fiml')
summary(stanley.cfa.fit,fit.measures=T,standardized=T)
mean(c(.507,.435,.314,.309,.511,.009,.10,.18))

## Castanho Silva et al:
data$simple8.r<-8-data$simple8
data$rwpop8.r<-8-data$rwpop8
data$manich13.r<-8-data$manich13

model.cfa.castanho<-'
antiel =~ antiel23 + rwpop8.r + antiel21
people =~ gewill17 + simple8.r + gewill3
manich =~ manich15 + manich13.r + manich14
method =~ gewill3 + b1*antiel23 +  b1*antiel21 + b1*gewill17 + b1*manich15 + b1*manich14
method ~~ 0*antiel + 0*people + 0*manich
'
castanho.cfa.fit<-cfa(data=data,model=model.cfa.castanho,estimator='mlr',missing='fiml')

summary(castanho.cfa.fit,fit.measures=T,standardized=T)

mean(c(.598,.552,.587,.627,.376,.543,.645,.413,.275))


## Dropping the worst indicator:

# Akkerman:
akkerman.cfa1<-'akk =~ akker1 + akker2 + akker3 + akker4 + akker5 '
akkerman.cfa.fit<-cfa(data=data,model=akkerman.cfa1,estimator='mlr',missing='fiml')
summary(akkerman.cfa.fit,fit.measures=T,standardized=T)
mean(c(.641,.616,.525,.610,.617))

## CSES:
cses.cfa1<-'cses =~ akker6 + cses1 + cses2.r + cses3 + cses5 + akker2'
cses.cfa.fit<-cfa(data=data,model=cses.cfa1,estimator='mlr',missing='fiml')
summary(cses.cfa.fit,fit.measures=T,standardized=T)
mean(c(.458,.751,.447,.633,.728,.481))

## Oliver and Rahn:
# First recode manich1, which was measured in 1-7 to a 1-5 scale, to match the others:
or.cfa1<-'
antiel =~ ow_ae1 + ow_ae2 + ow_ae3 + ow_ae4 + ow_ae5..
mistexp =~ ow_me1 + ow_me2 + ow_me4
nataff =~ ow_na1 + ow_na2 + ow_na3'
or.cfa.fit<-cfa(data=data,model=or.cfa1,estimator='mlr',missing='fiml')
summary(or.cfa.fit,fit.measures=T,standardized=T)
mean(c(.461,.594,.644,.596,.262,.680,.555,.386,.299,.348,.511))

## Elchardus and Spruyt
es.cfa1<-'es =~ b1*es1 + b1*es2 + es3'
es.cfa.fit<-cfa(data=data,model=es.cfa1,estimator='mlr',missing='fiml')
summary(es.cfa.fit,fit.measures=T,standardized=T)
mean(c(.462,.551,.805))

## Schulz et al. 2017
nccr.cfa1<-'
antiel =~ nccr_ant1 + nccr_ant2
sov =~ nccr_sov1 + nccr_sov2 + akker2
hom =~ nccr_hom1 + nccr_hom2 + nccr_hom3
pop =~ antiel + sov + hom'
nccr.cfa.fit<-cfa(data=data,model=nccr.cfa1,estimator='mlr',missing='fiml')
summary(nccr.cfa.fit,fit.measures=T,standardized=T)
mean(c(.622,.533,.762,.721,.682,.556,.701,.503,.747,.984,.550))

## Stanley 2011
stanley.cfa1<-'sta =~ stanley1 + stanley2 + stanley3 + stanley4 + stanley5 + stanley7 + stanley8
method =~ stanley3 + stanley8
method ~~ 0*sta'
stanley.cfa.fit<-cfa(data=data,model=stanley.cfa1,estimator='mlr',missing='fiml')
summary(stanley.cfa.fit,fit.measures=T,standardized=T)
mean(c(.506,.433,.315,.308,.513,.102,.179))

## Castanho Silva et al:
model.cfa.castanho1<-'
antiel =~ antiel23 + rwpop8.r + antiel21
people =~ gewill17 + simple8.r + gewill3
manich =~ manich15 + manich13.r 
method =~ gewill3 + b1*antiel23 +  b1*antiel21 + b1*gewill17 + b1*manich15 
method ~~ 0*antiel + 0*people + 0*manich
'
castanho.cfa.fit<-cfa(data=data,model=model.cfa.castanho1,estimator='mlr',missing='fiml')
summary(castanho.cfa.fit,fit.measures=T,standardized=T)
mean(c(.578,.550,.567,.601,.381,.522,.705,.407))


## TABLE 2:

### Akkerman:
akkerman.mgcfa<-'pop =~ akker1 + akker2 + akker3 + akker4 + akker5 + akker6'
measurementInvariance(model=akkerman.mgcfa,data=data,estimator='mlr',missing='fiml',
                      group = 'country')
### CSES
cses.mgcfa<-'pop =~ akker6 + cses1 + cses2 + cses3 + cses4 + cses5 + akker2'
measurementInvariance(model=cses.mgcfa,data=data,estimator='mlr',missing='fiml',
                      group = 'country')

### Oliver and Rahn:
or.mgcfa<-'
antiel =~ ow_ae1 + ow_ae2 + ow_ae3 + ow_ae4 + ow_ae5..
mistexp =~ ow_me1 + ow_me2 + ow_me3 + ow_me4
nataff =~ ow_na1 + ow_na2 + ow_na3'
measurementInvariance(model=or.mgcfa,data=data,estimator='mlr',missing='fiml',
                      group = 'country')

### Elchardus and Spruyt:
es.mgcfa<-'pop =~ es1 + es2 + es3 + es4'
measurementInvariance(model=es.mgcfa,data=data,estimator='mlr',missing='fiml',
                      group = 'country')

### Schulz et al:
nccr.mgcfa<-'
antiel =~ nccr_ant1 + nccr_ant2 + nccr_ant3
sov =~ nccr_sov1 + nccr_sov2 + akker2
hom =~ nccr_hom1 + nccr_hom2 + nccr_hom3
pop =~ antiel + sov + hom'
measurementInvariance(model=nccr.mgcfa,data=data,estimator='mlr',missing='fiml',
                      group = 'country')

### Stanley:
stanley.mgcfa<-'
pop =~ stanley1 + stanley2 + stanley3 + stanley4 + stanley5 + stanley6 + stanley7 + stanley8
method =~ stanley3 + stanley8
method ~~ 0*pop'
measurementInvariance(model=stanley.mgcfa,data=data,estimator='mlr',missing='fiml',
                      group = 'country')

## Castanho Silva:
castanho.mgcfa<-'
antiel =~ antiel23 + rwpop8.r + antiel21
people =~ gewill17 + simple8.r + gewill3
manich =~ manich15 + manich13.r + manich14
method =~ gewill3 + b1*antiel23 +  b1*antiel21 + b1*gewill17 + b1*manich15 + b1*manich14
method ~~ 0*antiel + 0*people + 0*manich
'
measurementInvariance(model=castanho.mgcfa,data=data,estimator='mlr',missing='fiml',group='country')


### Figures 1 and 2:

## Akkerman:
bwtheme <- standard.theme(color=FALSE)
bwtheme$fontsize$text=10

akker.irt<-mirt(data[,c('akker1','akker2','akker3','akker4','akker5','akker6')],1,itemtype='grsm',
                   technical = list(removeEmptyRows=T))

p.akker<-plot(akker.irt, type = 'infoSE', main='Information and SE',
               facet_items=F)
p.akker<-update(p.akker,sub='Akkerman et al.',par.settings=bwtheme)
p.akker

## CSES:

cses.irt<-mirt(data[,c('akker6','akker2','cses1','cses2','cses3','cses4','cses5')],1,itemtype='grsm',
               technical = list(removeEmptyRows=T))

p.cses<-plot(cses.irt, type = 'infoSE', main='Information and SE',
              facet_items=F)
p.cses<-update(p.cses,sub='CSES',par.settings=bwtheme)
p.cses

## Elchardus and Spruyt:
es.irt<-mirt(data[,c('es1','es2','es3','es4')],1,itemtype='grsm',
               technical = list(removeEmptyRows=T))

p.es<-plot(es.irt, type = 'infoSE', main='Information and SE',
             facet_items=F)
p.es<-update(p.es,sub='Elchadus and Spruyt',par.settings=bwtheme)
p.es

## Stanley:
stanley.irt<-mirt(data[,c('stanley1','stanley2','stanley3','stanley4',
                          'stanley5','stanley6','stanley7','stanley8')],1,itemtype='grsm',
             technical = list(removeEmptyRows=T))

p.stanley<-plot(stanley.irt, type = 'infoSE', main='Information and SE',
           facet_items=F)
p.stanley<-update(p.stanley,sub='Stanley',par.settings=bwtheme)
p.stanley

## Schulz:
nccr.ant.irt<-mirt(data=data[,c('nccr_ant1','nccr_ant2','nccr_ant3')],1,itemtype='grsm',
                  technical = list(removeEmptyRows=T))

p.nccr.ant<-plot(nccr.ant.irt, type = 'infoSE',
                facet_items=F)
p.nccr.ant<-update(p.nccr.ant,sub='Schulz et al. - Antielitism',main=NULL,par.settings=bwtheme)
p.nccr.ant

nccr.sov.irt<-mirt(data=data[,c('nccr_sov1','nccr_sov2','akker2')],1,itemtype='grsm',
               technical = list(removeEmptyRows=T))
p.nccr.sov<-plot(nccr.sov.irt, type = 'infoSE',
             facet_items=F)
p.nccr.sov<-update(p.nccr.sov,sub='Schulz et al. - Sovereignty',main=NULL,par.settings=bwtheme)
p.nccr.sov

nccr.hom.irt<-mirt(data=data[,c('nccr_hom1','nccr_hom2','nccr_hom3')],1,itemtype='grsm',
               technical = list(removeEmptyRows=T))
p.nccr.hom<-plot(nccr.hom.irt, type = 'infoSE',
             facet_items=F)
p.nccr.hom<-update(p.nccr.hom,sub='Schulz et al. - Homogeneity',main=NULL,par.settings=bwtheme)
p.nccr.hom


## Oliver and Rahn:
or.ant.irt<-mirt(data=data[,c('ow_ae1', 'ow_ae2','ow_ae3','ow_ae4','ow_ae5..')],1,itemtype='graded',
                 technical = list(removeEmptyRows=T))
p.or.ant<-plot(or.ant.irt, type = 'infoSE',facet_items=F)
p.or.ant<-update(p.or.ant, sub = 'Oliver and Rahn - Anti-elitism', main=NULL, par.settings=bwtheme)

or.me.irt<-mirt(data=data[,c('ow_me1', 'ow_me2','ow_me3','ow_me4')],1,itemtype='graded',
                 technical = list(removeEmptyRows=T))
p.or.me<-plot(or.me.irt, type = 'infoSE',facet_items=F)
p.or.me<-update(p.or.me, sub = 'Oliver and Rahn - Mistrust Experts', main=NULL, par.settings=bwtheme)

or.na.irt<-mirt(data=data[,c('ow_na1', 'ow_na2','ow_na3')],1,itemtype='graded',
                 technical = list(removeEmptyRows=T))
p.or.na<-plot(or.na.irt, type = 'infoSE',facet_items=F)
p.or.na<-update(p.or.na, sub = 'Oliver and Rahn - National Affiliation',main=NULL, par.settings=bwtheme)
p.or.na

## Castanho Silva:
model.antiel<-mirt(data[,c('antiel23','rwpop8.r','antiel21')],1,itemtype='grsm',
                   technical = list(removeEmptyRows=T))

p.antiel<-plot(model.antiel, type = 'infoSE',
               facet_items=F)
p.antiel<-update(p.antiel,sub='Castanho Silva et al. - Antielitism',main=NULL,par.settings=bwtheme)
p.antiel

model.people<-mirt(data[,c('gewill17','simple8.r','gewill3')],1,itemtype='grsm',
                   technical = list(removeEmptyRows=T))

p.people<-plot(model.people, type = 'infoSE', sub='People-centrism',
               facet_items=F)
p.people<-update(p.people,sub='Castanho Silva et al. - People',main=NULL,par.settings=bwtheme)
p.people

model.manich<-mirt(data[,c('manich15','manich13','manich14')],1,itemtype='grsm',
                   technical = list(removeEmptyRows=T))
p.manich<-plot(model.manich, type = 'infoSE',
               facet_items=F)
p.manich<-update(p.manich, main=NULL,sub='Castanho Silva et al. - Manichaean',par.settings=bwtheme)
p.manich


pdf('infocurve_single.pdf')
grid.arrange(p.akker,p.cses,p.es,p.stanley,ncol=2)
dev.off()

pdf('infocurve_multi.pdf', height=6, width=8)
grid.arrange(p.antiel,p.nccr.ant,p.or.ant,p.people,p.nccr.hom,p.or.na,p.manich,p.nccr.sov,p.or.me,ncol=3)
dev.off()


##### External validity: Countries with populists: France, Greece, Italy, Spain, ~U.S., UK, Mexico

## Get the factor scores and multiplicative index for Castanho Silva:
cfa.castanho<-'antiel =~ antiel23 + rwpop8.r + antiel21
people =~ gewill17 + simple8.r + gewill3
manich =~ manich15 + manich13.r + manich14
method =~ gewill3 + b1*antiel23 +  b1*antiel21 + b1*gewill17 + b1*manich15 + b1*manich14
method ~~ 0*antiel + 0*people + 0*manich'
fit.castanho<-cfa(model=cfa.castanho,data=data,estimator='mlr',missing='fiml')

range01 <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))}

dimensions<-predict(fit.castanho)[,1:3]

dimensions<-apply(dimensions,2,range01)

pop<-dimensions[,1]*dimensions[,2]*dimensions[,3]

data$pop.castanho<-pop

## TABLE 3:
### Correlate each with conspiracy and political trust (separately, model won't fit with all together):

## Akkerman:
cfa.ext.akker<-'akker =~ akker1 + akker2 + akker3 + akker4 + akker5 + akker6
trust =~ t_party.r + t_parl.r + t_gov.r
consp =~ cmq1 + cmq2 + cmq3 + cmq4 + cmq5 
'
akker.ext.fit<-cfa(model=cfa.ext.akker,data=data,estimator='mlr',missing='fiml')
summary(akker.ext.fit,standardized=T,fit.measures=T)

## CSES:
cfa.ext.cses<-'cses =~ akker6 + cses1 + cses2.r + cses3 + cses4 + cses5 + akker2
trust =~ t_party.r + t_parl.r + t_gov.r
consp =~ cmq1 + cmq2 + cmq3 + cmq4 + cmq5 
'
cses.ext.fit<-cfa(model=cfa.ext.cses,data=data,estimator='mlr',missing='fiml')
summary(cses.ext.fit,standardized=T,fit.measures=T)

## Elchardus and Spruyt:
cfa.ext.es<-'es =~ es1 + es2 + es3 + es4
trust =~ t_party.r + t_parl.r + t_gov.r
consp =~ cmq1 + cmq2 + cmq3 + cmq4 + cmq5 
'
es.ext.fit<-cfa(model=cfa.ext.es,data=data,estimator='mlr',missing='fiml')
summary(es.ext.fit,standardized=T,fit.measures=T)

## Schulz et al.:
cfa.ext.nccr<-'nccr.antiel =~ nccr_ant1 + nccr_ant2 + nccr_ant3
nccr.sov =~ nccr_sov1 + nccr_sov2 + akker2
nccr.hom =~ nccr_hom1 + nccr_hom2 + nccr_hom3
nccr.pop =~ nccr.antiel + nccr.sov + nccr.hom
trust =~ t_party.r + t_parl.r + t_gov.r
consp =~ cmq1 + cmq2 + cmq3 + cmq4 + cmq5 
'
nccr.ext.fit<-cfa(model=cfa.ext.nccr,data=data,estimator='mlr',missing='fiml')
summary(nccr.ext.fit,standardized=T,fit.measures=T)


## Stanley
cfa.ext.stanley<-'stanley =~ stanley1 + stanley2 + stanley3 + stanley4 + stanley5 + stanley6 + stanley7 + stanley8
method.st =~ stanley3 + stanley8
method.st ~~ 0*stanley
trust =~ t_party.r + t_parl.r + t_gov.r
consp =~ cmq1 + cmq2 + cmq3 + cmq4 + cmq5 
'
stanley.ext.fit<-cfa(model=cfa.ext.stanley,data=data,estimator='mlr',missing='fiml')
summary(stanley.ext.fit,standardized=T,fit.measures=T)

## Castanho Silva aggregate:
data$pop.r<-data$pop.castanho*100 ## Otherwise the variance is too small in comparison to the rest
cfa.ext.castanho<-'trust =~ t_party.r + t_parl.r + t_gov.r
consp =~ cmq1 + cmq2 + cmq3 + cmq4 + cmq5
popla =~ pop.r
'
castanho.ext.fit<-cfa(model=cfa.ext.castanho,data=data,estimator='mlr',missing='fiml')
summary(castanho.ext.fit,standardized=T,fit.measures=T)

## Castanho Silva three dimensions:
cfa.ext.castanho3<-'antiel =~ antiel23 + rwpop8.r + antiel21
people =~ gewill17 + simple8.r + gewill3
manich =~ manich15 + manich13.r + manich14
method =~ gewill3 + b1*antiel23 +  b1*antiel21 + b1*gewill17 + b1*manich15 + b1*manich14
method ~~ 0*antiel + 0*people + 0*manich
trust =~ t_party.r + t_parl.r + t_gov.r
consp =~ cmq1 + cmq2 + cmq3 + cmq4 + cmq5
'
castanho3.ext.fit<-cfa(model=cfa.ext.castanho3,data=data,estimator='mlr',missing='fiml')
summary(castanho3.ext.fit,standardized=T,fit.measures=T)


## Oliver Rahn:
cfa.ext.or<-'or.antiel =~ ow_ae1 + ow_ae2 + ow_ae3 + ow_ae4 + ow_ae5..
or.mistexp =~ ow_me1 + ow_me2 + ow_me3 + ow_me4
or.nataff =~ ow_na1 + ow_na2 + ow_na3
trust =~ t_party.r + t_parl.r + t_gov.r
consp =~ cmq1 + cmq2 + cmq3 + cmq4 + cmq5'
or.ext.fit<-cfa(model=cfa.ext.or,data=data,estimator='mlr',missing='fiml')
summary(or.ext.fit,standardized=T,fit.measures=T)


## FIGURE 3:

or.ae.cfa<-'or.antiel =~ ow_ae1 + ow_ae2 + ow_ae3 + ow_ae4 + ow_ae5..'
or.me.cfa<-'or.mistexp =~ ow_me1 + ow_me2 + ow_me3 + ow_me4'
or.na.cfa<-'or.nataff =~ ow_na1 + ow_na2 + ow_na3'

cast.ant<-'c.ant =~ antiel23 + rwpop8.r + antiel21'
cast.pp<-'c.pp =~ gewill17 + simple8.r + gewill3'
cast.man<-'c.man =~ manich15 + manich13.r + manich14'

## Unidimensional:

model1<-list(akkerman.cfa,cses.cfa,es.cfa,or.ae.cfa,or.me.cfa,or.na.cfa,cast.ant,cast.pp,cast.man,stanley.cfa)


correlation.matrix<-matrix(NA,ncol=11,nrow=11)
m<-c(1:length(model1))
for(c in 1:length(model1)){
  model2<-model1[-c]
  for(r in 1:length(model2)){
cfa.model<-paste(model1[[c]],'\n',model2[[r]],sep='')
cfa.fit<-cfa(model=cfa.model, data=data, estimator='mlr',missing='fiml')
correlation.matrix[r,c]<-cov2cor(inspect(cfa.fit, 'est')$psi)[[2]]
}
}

# Add the line with NCCR:
for(i in 2:length(model1)){
  cfa.model<-paste(nccr.cfa,'\n',model1[[i]],sep='')
  cfa.fit<-cfa(model=cfa.model, data=data, estimator='mlr',missing='fiml',auto.cov.lv.x=T)
  correlation.matrix[10,i]<-cov2cor(inspect(cfa.fit, 'est')$psi)[[20]]
}

# Getting the one for Stanley (different indexing nr because of the method factor)
  cfa.model<-paste(nccr.cfa,'\n',model1[[10]],sep='')
  cfa.fit<-cfa(model=cfa.model, data=data, estimator='mlr',missing='fiml',auto.cov.lv.x=T)
  correlation.matrix[10,10]<-cov2cor(inspect(cfa.fit, 'est')$psi)[[23]]

# With Akkerman, which doesn't give a positive-definite lv.cor matrix because of the collinearity between scales...
  cfa.model<-paste(nccr.cfa,'\n',model1[[1]],sep='')
  cfa.fit<-cfa(model=cfa.model, data=data, estimator='mlr',missing='fiml',auto.cov.lv.x=T)
  correlation.matrix[10,1]<-cov2cor(inspect(cfa.fit, 'est')$psi)[[20]]

# Cast with all:
  
for(i in 1:10){
  cfa.model<-paste('lapop=~pop.castanho\n',model1[[i]],sep='')
  cfa.fit<-cfa(model=cfa.model, data=data, estimator='mlr',missing='fiml',auto.cov.lv.x=T)
  correlation.matrix[11,i]<-cov2cor(inspect(cfa.fit, 'est')$psi)[[2]]
}

# Cast with Schulz:
cfa.model<-paste(nccr.cfa,'\nlapop=~pop.castanho',sep='')
  cfa.fit<-cfa(model=cfa.model, data=data, estimator='mlr',missing='fiml',auto.cov.lv.x=T)
  correlation.matrix[11,11]<-cov2cor(inspect(cfa.fit, 'est')$psi)[[20]]

# Fix the matrix to look like a proper triangular correlation matrix:
correlation.matrix<-rbind(matrix(rep(1,11),nrow=1,ncol=11),correlation.matrix)
correlation.matrix<-cbind(correlation.matrix,matrix(rep(1,12),nrow=12,ncol=1))

for(i in 1:12){
  correlation.matrix[i,i]<-1
}

correlation.matrix[upper.tri(correlation.matrix)]<-0
cormat.full<-correlation.matrix+t(correlation.matrix)

colnames(cormat.full)<-c("Akkerman et al.", "CSES", "Elchardus/Spruyt", "Oliver/Rahn: Anti-elitism", "Oliver/Rahn: Experts",
                              "Oliver/Rahn: National",
                     "Cast: Anti-elitism", "Cast: People", "Cast: Manichean Outlook", "Stanley", "Schulz", "Cast: Aggregate")
rownames(cormat.full)<-c("Akkerman et al.", "CSES", "Elchardus/Spruyt", "Oliver/Rahn: Anti-elitism", "Oliver/Rahn: Experts",
                              "Oliver/Rahn: National",
                     "Cast: Anti-elitism", "Cast: People", "Cast: Manichean Outlook",  "Stanley", "Schulz","Cast: Aggregate")

# Fix those above 1:

cormat.full[cormat.full > 1]<-1

col_bw <- colorRampPalette(c("grey1", "white", "black")) 

pdf("corrplot_scales.pdf", width = 11,height = 9)
corrplot(cormat.full, order="hclust",
         col=col_bw(10), tl.cex=0.8, tl.col = "black", 
         na.label = ".", shade.col="black", addgrid.col=TRUE,
         type="upper", method="shade", mar=c(0,0,0,0))
dev.off()


#### FIGURES 4 AND 5:

## First, execute all the .inp Mplus files in the folder predictions. Then run the following:

##Curves with predicted probabilities of voting:

## Country list:

# 1 = US; 2 = Brazil; 3 = Mexico; 4 = Spain; 5 = Italy; 6 = UK; 7 = FRA; 8 = GRE; 9 = IRE

## France:  FN: 1; PG: 2

# Spain: Podemos = 3

# Mexico: Morena = 8

# Italy: M5S = 1, LN = 3; FI = 4; LN and FI have too few obs

# Greece: SYRIZA = 4, ANEL = 7


## Plot akkerman:

akker_fra<-read.delim('predictions/akker_france.dat',header=F)
akker_ita<-read.delim('predictions/akker_italy.dat',header=F)
akker_gre<-read.delim('predictions/akker_greece.dat',header=F)
akker_mex<-read.delim('predictions/akker_mexico.dat',header=F)
akker_spa<-read.delim('predictions/akker_spain.dat',header=F)
akker_uk<-read.delim('predictions/akker_uk.dat',header=F)

p.akker<-ggplot(data=akker_fra,aes(x = V1, y = V2))+geom_line()+
  geom_line(data=akker_ita,aes(x=V1, y=V2),linetype = 'dotted')+
  geom_line(data=akker_gre,aes(x=V1, y=V2),linetype = 'dashed')+
  geom_line(data=akker_mex,aes(x=V1, y=V2),linetype = 'dotdash')+
  geom_line(data=akker_spa,aes(x=V1, y=V2),linetype = '12345678')+
  geom_line(data=akker_uk,aes(x=V1, y=V2),linetype = 'twodash')+
  annotate('text', x = max(akker_fra$V1)+0.1, y = max(akker_fra$V2), label='FRA',size=3)+
  annotate('text', x = max(akker_ita$V1)+0.1, y = max(akker_ita$V2), label='ITA',size=3)+
  annotate('text', x = max(akker_gre$V1)+0.1, y = min(akker_gre$V2), label='GRE',size=3)+
  annotate('text', x = max(akker_mex$V1)+0.1, y = max(akker_mex$V2), label='MEX',size=3)+
  annotate('text', x = max(akker_spa$V1)+0.1, y = max(akker_spa$V2), label='SPA',size=3)+
  annotate('text', x = max(akker_uk$V1)+0.1, y = max(akker_uk$V2), label='UK',size=3)+
  ylab('Populist Identification Probability')+xlab('Level of Populist Attitudes')+
  ggtitle('Akkerman et al. 2014')+ylim(c(0,1))+
  theme(panel.grid.major.y = element_line(colour='gray85'), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),#axis.line.x = element_line(colour = 'black'),
        axis.line = element_line(colour = 'black'), axis.text.x = element_text(colour = 'black'),axis.text.y = element_text(colour = 'black'),
        text = element_text(size = 10)
        )
p.akker

## Plot CSES:

cses_fra<-read.delim('predictions/cses_france.dat',header=F)
cses_ita<-read.delim('predictions/cses_italy.dat',header=F)
cses_gre<-read.delim('predictions/cses_greece.dat',header=F)
cses_mex<-read.delim('predictions/cses_mexico.dat',header=F)
cses_spa<-read.delim('predictions/cses_spain.dat',header=F)
cses_uk<-read.delim('predictions/cses_uk.dat',header=F)

p.cses<-ggplot(data=cses_fra,aes(x = V1, y = V2))+geom_line()+
  geom_line(data=cses_ita,aes(x=V1, y=V2),linetype = 'dotted')+
  geom_line(data=cses_gre,aes(x=V1, y=V2),linetype = 'dashed')+
  geom_line(data=cses_mex,aes(x=V1, y=V2),linetype = 'dotdash')+
  geom_line(data=cses_spa,aes(x=V1, y=V2),linetype = '12345678')+
  geom_line(data=cses_uk,aes(x=V1, y=V2),linetype = 'twodash')+
  annotate('text', x = max(cses_fra$V1)+0.1, y = max(cses_fra$V2), label='FRA', size=3)+
  annotate('text', x = max(cses_ita$V1)+0.1, y = max(cses_ita$V2), label='ITA', size=3)+
  annotate('text', x = max(cses_gre$V1)+0.1, y = min(cses_gre$V2), label='GRE', size=3)+
  annotate('text', x = max(cses_mex$V1)+0.1, y = max(cses_mex$V2), label='MEX', size=3)+
  annotate('text', x = max(cses_spa$V1)+0.1, y = max(cses_spa$V2), label='SPA', size=3)+
  annotate('text', x = max(cses_uk$V1)+0.1, y = max(cses_uk$V2), label='UK', size=3)+
  ylab('Populist Identification Probability')+xlab('Level of Populist Attitudes')+
  ggtitle('CSES Module 5')+ylim(c(0,1))+
  theme(panel.grid.major.y = element_line(colour='gray85'), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),#axis.line.x = element_line(colour = 'black'),
        axis.line = element_line(colour = 'black'), axis.text.x = element_text(colour = 'black'),axis.text.y = element_text(colour = 'black'),
        text = element_text(size=10)
  )
p.cses


#### Castanho Silva:

france<-subset(data, country == 7)
italy<-subset(data, country == 5)
spain<-subset(data, country == 4)
mexico<-subset(data, country == 3)
greece<-subset(data, country == 8)
uk<-subset(data, country == 6)

cfa.castanho<-'antiel =~ antiel23 + rwpop8.r + antiel21
people =~ gewill17 + simple8.r + gewill3
manich =~ manich15 + manich13.r + manich14
method =~ gewill3 + b1*antiel23 +  b1*antiel21 + b1*gewill17 + b1*manich15 + b1*manich14
method ~~ 0*antiel + 0*people + 0*manich'


fscores<-function(data,model){
  fit<-cfa(model=model, data=data,estimator='mlr',missing='fiml')
  dimensions<-predict(fit)[,1:3]
  
  dimensions<-apply(dimensions,2,range01)
  
  pop<-dimensions[,1]*dimensions[,2]*dimensions[,3]
  return(pop)
}

france_populism<-fscores(france,cfa.castanho)
italy_populism<-fscores(italy,cfa.castanho)
mexico_populism<-fscores(mexico,cfa.castanho)
uk_populism<-fscores(uk, cfa.castanho)
## Spanish and Greek lv.cov matrix is not pos def wiht the previous spec. Fix the cor to 0 for model to converge
cfa.castanho.spain<-'antiel =~ antiel23 + rwpop8.r + antiel21
people =~ gewill17 + simple8.r + gewill3
manich =~ manich15 + manich13.r + manich14
method =~ gewill3 + b1*antiel23 +  b1*antiel21 + b1*gewill17 + b1*manich14 + b1*manich15
method ~~ 0*antiel + 0*people + 0*manich
people ~~ 0*manich'
spain_populism<-fscores(spain,cfa.castanho.spain)

cfa.castanho.greece<-'antiel =~ antiel23 + rwpop8.r + antiel21
people =~ gewill17 + simple8.r + gewill3
manich =~ manich15 + manich13.r + manich14
method =~ gewill3 + b1*antiel23 +  b1*antiel21 + b1*gewill17 + b1*manich14 + b1*manich15
method ~~ 0*antiel + 0*people + 0*manich
manich ~~ 0*people'

greece_populism<-fscores(greece,cfa.castanho.greece)

france.data<-data.frame(cbind(france_populism,france$pid))
italy.data<-data.frame(cbind(italy_populism,italy$pid))
spain.data<-data.frame(cbind(spain_populism,spain$pid))
mexico.data<-data.frame(cbind(mexico_populism,mexico$pid))
greece.data<-data.frame(cbind(greece_populism,greece$pid))
uk.data<-data.frame(cbind(uk_populism, uk$pid))

# France:

colnames(france.data)[2]<-'pid'

france.data[france.data == -999]<-NA
france.data$fn<-0
france.data$fn[france.data$pid == 1]<-1
france.data$fn[is.na(france.data$pid)]<-NA
france.data$fg<-0
france.data$fg[france.data$pid == 2]<-1
france.data$fg[is.na(france.data$pid)]<-NA
france.data$populist<-ifelse(france.data$fn == 1 | france.data$fg == 1, 1, 0)
france.data$populist[is.na(france.data$pid)]<-NA

# Scale to have a mean of 0 and variance of 1 and be in the same scale as the other scales
france.data$france_populism.r<-scale(france.data$france_populism)
france.data<-na.omit(france.data)
france.pred<-glm(populist~france_populism.r,data=france.data,family='binomial'(link='probit'))$fitted

## Italy:
colnames(italy.data)[2]<-'pid'

italy.data[italy.data == -999]<-NA
italy.data$mcs<-0
italy.data$mcs[italy.data$pid == 1]<-1
italy.data$mcs[is.na(italy.data$pid)]<-NA
italy.data$ln<-0
italy.data$ln[italy.data$pid == 3]<-1
italy.data$ln[is.na(italy.data$pid)]<-NA
italy.data$fi<-0
italy.data$fi[italy.data$pid == 4]<-1
italy.data$fi[is.na(italy.data$pid)]<-NA
italy.data$populist<-ifelse(italy.data$mcs == 1 | italy.data$fi == 1 | italy.data$ln == 1, 1, 0)
italy.data$populist[is.na(italy.data$pid)]<-NA

# Scale to have a mean of 0 and variance of 1 and be in the same scale as the other scales
italy.data$italy_populism.r<-scale(italy.data$italy_populism)
italy.data<-na.omit(italy.data)
italy.pred<-glm(populist~italy_populism.r,data=italy.data,family='binomial'(link='probit'))$fitted


## Greece:
colnames(greece.data)[2]<-'pid'

greece.data[greece.data == -999]<-NA
greece.data$syriza<-0
greece.data$syriza[greece.data$pid == 4]<-1
greece.data$syriza[is.na(greece.data$pid)]<-NA
greece.data$anel<-0
greece.data$anel[greece.data$pid == 7]<-1
greece.data$anel[is.na(greece.data$pid)]<-NA
greece.data$populist<-ifelse(greece.data$syriza == 1 | greece.data$anel == 1, 1, 0)
greece.data$populist[is.na(greece.data$pid)]<-NA

# Scale to have a mean of 0 and variance of 1 and be in the same scale as the other scales
greece.data$greece_populism.r<-scale(greece.data$greece_populism)
greece.data<-na.omit(greece.data)
greece.pred<-glm(populist~greece_populism.r,data=greece.data,family='binomial'(link='probit'))$fitted


# Spain:
colnames(spain.data)[2]<-'pid'

spain.data[spain.data == -999]<-NA
spain.data$podemos<-0
spain.data$podemos[spain.data$pid == 3]<-1
spain.data$podemos[is.na(spain.data$pid)]<-NA

# Scale to have a mean of 0 and variance of 1 and be in the same scale as the other scales
spain.data$spain_populism.r<-scale(spain.data$spain_populism)
spain.data<-na.omit(spain.data)
spain.pred<-glm(podemos~spain_populism.r,data=spain.data,family='binomial'(link='probit'))$fitted


# Mexico:
colnames(mexico.data)[2]<-'pid'

mexico.data[mexico.data == -999]<-NA
mexico.data$morena<-0
mexico.data$morena[mexico.data$pid == 8]<-1
mexico.data$morena[is.na(mexico.data$pid)]<-NA

# Scale to have a mean of 0 and variance of 1 and be in the same scale as the other scales
mexico.data$mexico_populism.r<-scale(mexico.data$mexico_populism)
mexico.data<-na.omit(mexico.data)
mexico.pred<-glm(morena~mexico_populism.r,data=mexico.data,family='binomial'(link='probit'))$fitted

# UK:
colnames(uk.data)[2]<-'pid'
uk.data[uk.data == -999]<-NA
uk.data$ukip<-0
uk.data$ukip[uk.data$pid == 7]<-1
uk.data$ukip[is.na(uk.data$pid)]<-NA

# Scale to have a mean of 0 and variance of 1 and be in the same scale as the other scales
uk.data$uk_populism.r<-scale(uk.data$uk_populism)
uk.data<-na.omit(uk.data)
uk.pred<-glm(ukip~uk_populism.r,data=uk.data,family='binomial'(link='probit'))$fitted


## Plot

data.it.castanho<-data.frame(cbind(italy.data$italy_populism.r,italy.pred))
data.fr.castanho<-data.frame(cbind(france.data$france_populism.r,france.pred))
data.gr.castanho<-data.frame(cbind(greece.data$greece_populism.r,greece.pred))
data.mx.castanho<-data.frame(cbind(mexico.data$mexico_populism.r,mexico.pred))
data.sp.castanho<-data.frame(cbind(spain.data$spain_populism.r,spain.pred))
data.uk.castanho<-data.frame(cbind(uk.data$uk_populism.r,uk.pred))



p.castanho<-ggplot(data=data.fr.castanho,aes(x = V1, y = france.pred))+geom_line()+
  geom_line(data=data.it.castanho,aes(x=V1, y=italy.pred),linetype = 'dotted')+
  geom_line(data=data.gr.castanho,aes(x=V1, y=greece.pred),linetype = 'dashed')+
  geom_line(data=data.mx.castanho,aes(x=V1, y=mexico.pred),linetype = 'dotdash')+
  geom_line(data=data.sp.castanho,aes(x=V1, y=spain.pred),linetype = '12345678')+
  geom_line(data=data.uk.castanho,aes(x=V1, y=uk.pred), linetype='twodash')+
  annotate('text', x = max(data.fr.castanho$V1)+0.2, y = max(data.fr.castanho$france.pred), label='FRA', size=3)+
  annotate('text', x = max(data.it.castanho$V1)+0.2, y = max(data.it.castanho$italy.pred), label='ITA', size=3)+
  annotate('text', x = max(data.gr.castanho$V1)+0.2, y = min(data.gr.castanho$greece.pred)+0.05, label='GRE', size=3)+
  annotate('text', x = max(data.mx.castanho$V1)+0.2, y = max(data.mx.castanho$mexico.pred), label='MEX', size=3)+
  annotate('text', x = max(data.sp.castanho$V1)+0.2, y = max(data.sp.castanho$spain.pred), label='SPA', size=3)+
  annotate('text', x = max(data.uk.castanho$V1)+0.2, y = max(data.uk.castanho$uk.pred), label ='UK', size=3)+
  ylab('Populist Voting Probability')+xlab('Level of Populist Attitudes')+
  ggtitle('Castanho Silva et al. 2016')+ylim(c(0,1))+
  theme(panel.grid.major.y = element_line(colour='gray85'), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),#axis.line.x = element_line(colour = 'black'),
        axis.line = element_line(colour = 'black'), axis.text.x = element_text(colour = 'black'),axis.text.y = element_text(colour = 'black'),
        text = element_text(size = 10)
  )
p.castanho


## Stanley:

## Plot Stanley:

stanley_fra<-read.delim('predictions/stanley_france.dat',header=F)
stanley_ita<-read.delim('predictions/stanley_italy.dat',header=F)
stanley_gre<-read.delim('predictions/stanley_greece.dat',header=F)
stanley_mex<-read.delim('predictions/stanley_mexico.dat',header=F)
stanley_spa<-read.delim('predictions/stanley_spain.dat',header=F)
stanley_uk<-read.delim('predictions/stanley_uk.dat', header=F)

p.stanley<-ggplot(data=stanley_fra,aes(x = V1, y = V2))+geom_line()+
  geom_line(data=stanley_ita,aes(x=V1, y=V2),linetype = 'dotted')+
  geom_line(data=stanley_gre,aes(x=V1, y=V2),linetype = 'dashed')+
  geom_line(data=stanley_mex,aes(x=V1, y=V2),linetype = 'dotdash')+
  geom_line(data=stanley_spa,aes(x=V1, y=V2),linetype = '12345678')+
  geom_line(data=stanley_uk, aes(x=V1, y=V2),linetype = 'twodash')+
  annotate('text', x = max(stanley_fra$V1)+0.1, y = max(stanley_fra$V2), label='FRA', size=3)+
  annotate('text', x = max(stanley_ita$V1)+0.1, y = max(stanley_ita$V2), label='ITA', size=3)+
  annotate('text', x = max(stanley_gre$V1)+0.1, y = max(stanley_gre$V2), label='GRE', size=3)+
  annotate('text', x = max(stanley_mex$V1)+0.1, y = max(stanley_mex$V2), label='MEX', size=3)+
  annotate('text', x = max(stanley_spa$V1)+0.1, y = max(stanley_spa$V2), label='SPA', size=3)+
  annotate('text', x = max(stanley_uk$V1)+0.1, y = max(stanley_uk$V2), label = 'UK', size=3)+
  ylab('Populist Identification Probability')+xlab('Level of Populist Attitudes')+
  ggtitle('Stanley 2011')+ylim(c(0,1))+
  theme(panel.grid.major.y = element_line(colour='gray85'), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),#axis.line.x = element_line(colour = 'black'),
        axis.line = element_line(colour = 'black'), axis.text.x = element_text(colour = 'black'),axis.text.y = element_text(colour = 'black'),
        text = element_text(size=10)
  )
p.stanley


## Elchardus / Spruyt

es_fra<-read.delim('predictions/es_france.dat',header=F)
es_ita<-read.delim('predictions/es_italy.dat',header=F)
es_gre<-read.delim('predictions/es_greece.dat',header=F)
es_mex<-read.delim('predictions/es_mexico.dat',header=F)
es_spa<-read.delim('predictions/es_spain.dat',header=F)
es_uk<-read.delim('predictions/es_uk.dat',header=F)

p.es<-ggplot(data=es_fra,aes(x = V1, y = V2))+geom_line()+
  geom_line(data=es_ita,aes(x=V1, y=V2),linetype = 'dotted')+
  geom_line(data=es_gre,aes(x=V1, y=V2),linetype = 'dashed')+
  geom_line(data=es_mex,aes(x=V1, y=V2),linetype = 'dotdash')+
  geom_line(data=es_spa,aes(x=V1, y=V2),linetype = '12345678')+
  geom_line(data=es_uk,aes(x=V1, y=V2),linetype = 'twodash')+
  annotate('text', x = max(es_fra$V1)+0.1, y = max(es_fra$V2), label='FRA',size=3)+
  annotate('text', x = max(es_ita$V1)+0.1, y = max(es_ita$V2), label='ITA',size=3)+
  annotate('text', x = max(es_gre$V1)+0.1, y = min(es_gre$V2), label='GRE',size=3)+
  annotate('text', x = max(es_mex$V1)+0.1, y = max(es_mex$V2), label='MEX',size=3)+
  annotate('text', x = max(es_spa$V1)+0.1, y = max(es_spa$V2), label='SPA',size=3)+
  annotate('text', x = max(es_uk$V1)+0.1, y = max(es_uk$V2), label='UK',size=3)+
  ylab('Populist Identification Probability')+xlab('Level of Populist Attitudes')+
  ggtitle('Elchardus/Spruyt 2016')+ylim(c(0,1))+
  theme(panel.grid.major.y = element_line(colour='gray85'), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),#axis.line.x = element_line(colour = 'black'),
        axis.line = element_line(colour = 'black'), axis.text.x = element_text(colour = 'black'),axis.text.y = element_text(colour = 'black'),
        text = element_text(size=10)
  )
p.es

## Schulz et al. 2017

nccr_fra<-read.delim('predictions/nccr_france.dat',header=F)
nccr_ita<-read.delim('predictions/nccr_italy.dat',header=F)
nccr_gre<-read.delim('predictions/nccr_greece.dat',header=F)
nccr_mex<-read.delim('predictions/nccr_mexico.dat',header=F)
nccr_spa<-read.delim('predictions/nccr_spain.dat',header=F)
nccr_uk<-read.delim('predictions/nccr_uk.dat',header=F)

p.nccr<-ggplot(data=nccr_fra,aes(x = V1, y = V2))+geom_line()+
  geom_line(data=nccr_ita,aes(x=V1, y=V2),linetype = 'dotted')+
  geom_line(data=nccr_gre,aes(x=V1, y=V2),linetype = 'dashed')+
  geom_line(data=nccr_mex,aes(x=V1, y=V2),linetype = 'dotdash')+
  geom_line(data=nccr_spa,aes(x=V1, y=V2),linetype = '12345678')+
  geom_line(data=nccr_uk,aes(x=V1, y=V2),linetype = 'twodash')+
  annotate('text', x = max(nccr_fra$V1)+0.1, y = max(nccr_fra$V2), label='FRA',size=3)+
  annotate('text', x = max(nccr_ita$V1)+0.1, y = max(nccr_ita$V2), label='ITA',size=3)+
  annotate('text', x = max(nccr_gre$V1)+0.1, y = max(nccr_gre$V2), label='GRE',size=3)+
  annotate('text', x = max(nccr_mex$V1)+0.1, y = max(nccr_mex$V2), label='MEX',size=3)+
  annotate('text', x = max(nccr_spa$V1)+0.1, y = max(nccr_spa$V2), label='SPA',size=3)+
  annotate('text', x = max(nccr_uk$V1)+0.1, y = max(nccr_uk$V2), label='UK',size=3)+
  ylab('Populist Identification Probability')+xlab('Level of Populist Attitudes')+
  ggtitle('Schulz et al. 2017')+ylim(c(0,1))+
  theme(panel.grid.major.y = element_line(colour='gray85'), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),#axis.line.x = element_line(colour = 'black'),
        axis.line = element_line(colour = 'black'), axis.text.x = element_text(colour = 'black'),axis.text.y = element_text(colour = 'black'),
        text = element_text(size=10)
  )
p.nccr

pdf('pid_probs.pdf')
grid.arrange(p.akker,p.castanho,p.nccr,p.stanley,p.es,p.cses,ncol=2)
dev.off()


### Oliver Rahn:
# Anti-elitism
or_ae_fra<-read.delim('predictions/or_ae_france.dat',header=F)
or_ae_ita<-read.delim('predictions/or_ae_italy.dat',header=F)
or_ae_gre<-read.delim('predictions/or_ae_greece.dat',header=F)
or_ae_mex<-read.delim('predictions/or_ae_mexico.dat',header=F)
or_ae_spa<-read.delim('predictions/or_ae_spain.dat',header=F)
or_ae_uk<-read.delim('predictions/or_ae_uk.dat',header=F)

p.or.ae<-ggplot(data=or_ae_fra,aes(x = V1, y = V2))+geom_line()+
  geom_line(data=or_ae_ita,aes(x=V1, y=V2),linetype = 'dotted')+
  geom_line(data=or_ae_gre,aes(x=V1, y=V2),linetype = 'dashed')+
  geom_line(data=or_ae_mex,aes(x=V1, y=V2),linetype = 'dotdash')+
  geom_line(data=or_ae_spa,aes(x=V1, y=V2),linetype = '12345678')+
  geom_line(data=or_ae_uk,aes(x=V1, y=V2),linetype = 'twodash')+
  annotate('text', x = max(or_ae_fra$V1)+0.1, y = max(or_ae_fra$V2), label='FRA',size=3)+
  annotate('text', x = max(or_ae_ita$V1)+0.1, y = max(or_ae_ita$V2), label='ITA',size=3)+
  annotate('text', x = max(or_ae_gre$V1)+0.1, y = max(or_ae_gre$V2), label='GRE',size=3)+
  annotate('text', x = max(or_ae_mex$V1)+0.1, y = max(or_ae_mex$V2), label='MEX',size=3)+
  annotate('text', x = max(or_ae_spa$V1)+0.1, y = max(or_ae_spa$V2), label='SPA',size=3)+
  annotate('text', x = max(or_ae_uk$V1)+0.1, y = max(or_ae_uk$V2), label='UK',size=3)+
  ylab('Populist Identification Probability')+xlab('Level of Anti-elitism')+
  ggtitle('Oliver and Rahn 2016')+ylim(c(0,1))+
  theme(panel.grid.major.y = element_line(colour='gray85'), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),#axis.line.x = element_line(colour = 'black'),
        axis.line = element_line(colour = 'black'), axis.text.x = element_text(colour = 'black'),axis.text.y = element_text(colour = 'black'),
        text = element_text(size=10)
  )
p.or.ae

# Mistrust of experts:
or_me_fra<-read.delim('predictions/or_me_france.dat',header=F)
or_me_ita<-read.delim('predictions/or_me_italy.dat',header=F)
or_me_gre<-read.delim('predictions/or_me_greece.dat',header=F)
or_me_mex<-read.delim('predictions/or_me_mexico.dat',header=F)
or_me_spa<-read.delim('predictions/or_me_spain.dat',header=F)
or_me_uk<-read.delim('predictions/or_me_uk.dat',header=F)

p.or.me<-ggplot(data=or_me_fra,aes(x = V1, y = V2))+geom_line()+
  geom_line(data=or_me_ita,aes(x=V1, y=V2),linetype = 'dotted')+
  geom_line(data=or_me_gre,aes(x=V1, y=V2),linetype = 'dashed')+
  geom_line(data=or_me_mex,aes(x=V1, y=V2),linetype = 'dotdash')+
  geom_line(data=or_me_spa,aes(x=V1, y=V2),linetype = '12345678')+
  geom_line(data=or_me_uk,aes(x=V1, y=V2),linetype = 'twodash')+
  annotate('text', x = max(or_me_fra$V1)+0.1, y = max(or_me_fra$V2), label='FRA',size=3)+
  annotate('text', x = max(or_me_ita$V1)+0.1, y = max(or_me_ita$V2), label='ITA',size=3)+
  annotate('text', x = max(or_me_gre$V1)+0.1, y = max(or_me_gre$V2), label='GRE',size=3)+
  annotate('text', x = max(or_me_mex$V1)+0.1, y = max(or_me_mex$V2), label='MEX',size=3)+
  annotate('text', x = max(or_me_spa$V1)+0.1, y = max(or_me_spa$V2), label='SPA',size=3)+
  annotate('text', x = max(or_me_uk$V1)+0.1, y = max(or_me_uk$V2), label='UK',size=3)+
  ylab('Populist Identification Probability')+xlab('Level of Mistrust of Experts')+
  ggtitle('Oliver and Rahn 2016')+ylim(c(0,1))+
  theme(panel.grid.major.y = element_line(colour='gray85'), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),#axis.line.x = element_line(colour = 'black'),
        axis.line = element_line(colour = 'black'), axis.text.x = element_text(colour = 'black'),axis.text.y = element_text(colour = 'black'),
        text = element_text(size=10)
  )
p.or.me

# National affiliation
or_na_fra<-read.delim('predictions/or_na_france.dat',header=F)
or_na_ita<-read.delim('predictions/or_na_italy.dat',header=F)
or_na_gre<-read.delim('predictions/or_na_greece.dat',header=F)
or_na_mex<-read.delim('predictions/or_na_mexico.dat',header=F)
or_na_spa<-read.delim('predictions/or_na_spain.dat',header=F)
or_na_uk<-read.delim('predictions/or_na_uk.dat',header=F)

p.or.na<-ggplot(data=or_na_fra,aes(x = V1, y = V2))+geom_line()+
  geom_line(data=or_na_ita,aes(x=V1, y=V2),linetype = 'dotted')+
  geom_line(data=or_na_gre,aes(x=V1, y=V2),linetype = 'dashed')+
  geom_line(data=or_na_mex,aes(x=V1, y=V2),linetype = 'dotdash')+
  geom_line(data=or_na_spa,aes(x=V1, y=V2),linetype = '12345678')+
  geom_line(data=or_na_uk,aes(x=V1, y=V2),linetype = 'twodash')+
  annotate('text', x = max(or_na_fra$V1)+0.1, y = max(or_na_fra$V2), label='FRA',size=3)+
  annotate('text', x = max(or_na_ita$V1)+0.1, y = max(or_na_ita$V2), label='ITA',size=3)+
  annotate('text', x = max(or_na_gre$V1)+0.1, y = max(or_na_gre$V2), label='GRE',size=3)+
  annotate('text', x = max(or_na_mex$V1)+0.1, y = min(or_na_mex$V2), label='MEX',size=3)+
  annotate('text', x = max(or_na_spa$V1)+0.1, y = min(or_na_spa$V2), label='SPA',size=3)+
  annotate('text', x = max(or_na_uk$V1)+0.1, y = max(or_na_uk$V2), label='UK',size=3)+
  ylab('Populist Identification Probability')+xlab('Level of National Affiliation')+
  ggtitle('Oliver and Rahn 2016')+ylim(c(0,1))+
  theme(panel.grid.major.y = element_line(colour='gray85'), panel.grid.minor = element_blank(), 
        panel.background = element_blank(),#axis.line.x = element_line(colour = 'black'),
        axis.line = element_line(colour = 'black'), axis.text.x = element_text(colour = 'black'),axis.text.y = element_text(colour = 'black'),
        text = element_text(size=10)
  )
p.or.na

pdf('oliverrahn.pdf',height=3,width=9)
grid.arrange(p.or.ae,p.or.me,p.or.na,ncol=3)
dev.off()

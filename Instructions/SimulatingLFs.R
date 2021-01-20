
#building a growth matrix

p = bio.lobster::load.environment()
require(fields)

la()

	som=SoMplot(cols=tim.colors(5),ltys=2:6,graphic='png')


#	TempModelling = TempModel(areas = 'subarea')
	#TempModelPlot(TempModelling,xlim=c(1980,2017),depths=c(5,25,50),Area=c("27N","27S", "29", "30","31A","31B", "32", "33E", "33W"),graphic='R')
	p$TempModel = TempModelling$Model

#	MoltModelling = moltModel(p,redo.dd=F)
	p$moltModel = MoltModelling
	#moltModelPlot(p$moltModel,graphic='png')


p$lfas = c( "30") # specify lfas in 2 batches

####### Base
p$F=0
	p = getSimList(p,sex=1)

	simMolt(p,continuous.recruitment=T)

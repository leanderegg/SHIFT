########################### conductR #####################################
##########################################################################
#### an R program to measure plant hydraulic conductance with an #########
#### electronic balance or  pipette. #####################################
##########################################################################
#### Written by Duncan D. Smith 2012-2018 ################################
#### free to use and alter for all non-commercial purposes  ##############
#### Conceptually based on John Sperry's conduct.ver1.xls ################
##########################################################################

#require(tcltk)									#load tcltk ##############

##########################################################################

##Computer-specific settings - *three* things ############################
kwd="/Users/user/Desktop/conductR"					#working directory ###########
.Tcl('set port /dev/cu.usbserial-D309R1DL
') 	#port number #############

library(here)

#source(here("ConductR", "conductR_18.R")) ###############
	#run the above line (without #) to 'load' the program
##########################################################################

##For Indra's computer and the old Sartorius:
#   /dev/cu.usbserial-D309R1DL

#.Tcl('set serial [open /dev/cu.usbserial-D309R1DL r+]' )
#.Tcl('fconfigure $serial -mode \"9600,o,7,1\" -blocking 0 -handshake xonxoff')

##Communication/Plot Settings#############################################
btyp=c("sar","met","ohaus","sci")[1]	#balance type (print code & parse)
.Tcl('set baud 9600') 							#baud rate ###############
.Tcl('set prty o') 								#parity ##################
.Tcl('set dbit 7') 								#data bits ###############
.Tcl('set sbit 1') 								#stop bits ###############
if(btyp=="sar") .Tcl('set prnt \x1B\x50\xD\xA') #Sar print code ##########
################################################### (Esc-P-CR-LF) ########
if(btyp=="met") .Tcl('set prnt \x53\x49\xD\xA') #Met print code ##########
################################################### (S-I-CR-LF) ##########
if(btyp=="ohaus") .Tcl('set prnt \x49\x50') #Ohaus print code ############
################################################### (I-P) ################
if(btyp=="sci") .Tcl('set prnt \x53\x45\x4E\x44\xD\xA') #Scientech code ##
################################################### (S-E-N-D-CR-LF) ######
par(mar=c(5,5,1,1))							#plot margins etc ############
##########################################################################

##Location Settings#######################################################
elev=13		#elevation (masl)			##############################
lat=34.4139629			#latitude (deg)				##############################
##########################################################################

##CONDUCTANCE FUNCTION####################################################
cond=function(numbr,sp,Ptreat.MPa=0,runmean=10,dt= 15,resl=0.0001,mxplot=10,
	manul=F,testmode=F,ovrl=0.4,kleaf=F,nmin=3,fix=F){
	####################################
	ver="v18"
	Tsoln.C=25 	#default solution temp (C) - will standardize to 20	##############
	grav = 0.0005777*abs(lat)+9.78033 #gravity (m/s2) corrected for latitude
	grav =-0.000003077*elev+grav #g corrected for elevation
	#resl= resl*1000
	rmcls=c("cyan") #colors for running means
	cls=c("black") #colors for individual measures
	ndec=-floor(log(resl,10)) #number of decimal places
	if(kleaf){
		#manul=T
		nmin=1
	}
	tst=0
	wf=NULL #which file (when there are multiple). This will get defined later
	setwd(kwd)
	if(length(Ptreat.MPa)==2) Ptreat.MPa=round(pcent(Ptreat.MPa[1],Ptreat.MPa[2]),4)
	nomID=paste(sp,numbr,Ptreat.MPa,"MPa") #use this name to identify this particular cond measurement
	stemID=paste(sp,numbr) #use this to identify the stem (for the purpose of recalling d and l)
	allbal=allt=alldt=allQ=allrmQ=NULL
	inst=F #is there an inset plotted?
	cnt=0 #yes i have a counter :/
	if(!manul) { #define units of water in g or ml for balance or pipette method, respectively.
		if(!kleaf) yl="flow rate, Q (mg/s)" else yl="flow rate into leaf, Q (mg/s)"
		qu="g"
		kun="(g / s MPa)"
		mlt=1000 #multipler to turn g into mg
	}else{
		qu="ml"
		yl= "flow rate, Q (ml/s)"
		kun="(ml / s MPa)"
		mlt=1 #multiplier to keep ml as ml
	}
	if(kleaf){
		xlb="pressure difference (MPa)"
		nmin=1
	}else{
		xlb="pressure (cm)"
	} #end if !manul
	if(!fix){ #make measurements or skip to looking at data and calculating conductance
		fini <- tclVar(0) #set an initial value
		if(!manul) {
			###MAKE AN EXIT BUTTON#########
			tt <- tktoplevel() #make a window
			f1 <- tkframe(tt) #not sure if this is needed
			tkpack(tkbutton(tt, text='Done!', command=function() tclvalue(fini)<-1),side='left', anchor='s') #add the button
			print("CLICK DONE! WHEN FINISHED - DO NOT HIT ESC!")
		}
		###############################

		####READ THE BALANCE UNTIL THE BUTTON IS PRESSED########
		while(tclvalue(fini)==0){
			if(!manul){ #read balance or skip to manual mode
				if(!testmode){ #F = TESTING W/OUT BALANCE
					tst=NA
					while(is.na(tst)){
						.Tcl('set serial [open $port r+]' )  #open the connection
						.Tcl('fconfigure $serial -mode \"$baud,$prty,$dbit,$sbit\"  -blocking 0 -handshake xonxoff') #configure: the quoted part is baud rate, parity, data bits, stop bits
						.Tcl('puts $serial $prnt') #tell it to print. Nothing happens
						.Tcl('flush $serial') #force the command through
						Sys.sleep(0.1) #wait a bit for a response
						tst=.Tcl('return [read $serial]') #read the value
						.Tcl('close $serial') #close the connection. done
						#print(as.character(tst))
						tst=decod(tst,btyp,ndec)
					}
				}else{
					tst=tst+sample(5,1)*resl
					#tst=prtnd[cnt+1] #prtnd is all balance measures from a previous flowdata.csv
				}
			}else{ #manual (pipette) mode
				if(length(allbal)==0){
					npt=readline(paste("ready to start! increment =",resl, "ml (press enter) "))
					tst=0
				}else{
					npt=readline("record n increments; blank=1; d=done ")
					if(npt=="d"){
						tclvalue(fini)=1
					}else{
						if(npt=="") tst=allbal[1]+resl else tst=allbal[1]+resl*as.numeric(npt)
					}
				}
			}
			if(tclvalue(fini)==0){
				##record values
				allbal=c(tst*mlt,allbal)
				allt=c(as.numeric(Sys.time()),allt)
				alldt=c(allt[1]-allt[2],alldt)
				allQ=c((allbal[1]-allbal[2])/alldt[1],allQ)
				if(kleaf & !manul) allQ[1]=-allQ[1]
				allrmQ=c(mean(allQ[1:runmean]),allrmQ)
				cnt=cnt+1

				if(cnt>1){ #need at least two readings to calculate Q
					if(cnt==2){ #MAKE FIRST PLOT
						if(manul) dt=alldt[1]
						if(allQ[1]==0) yrng=c(-resl,resl)/dt*mlt else yrng=range(allQ[1]+c(-resl,resl)/dt*mlt)
						xrng=c(allt[1],allt[1]+dt*mxplot*2)
						trng=range(allt)
						if(diff(trng)>120) xl="elapsed time (min)" else xl="elapsed time (s)"
						plot(allQ~allt,xlim=xrng,ylim=yrng,xaxt="n",pch=19,
							ylab=yl,xlab=xl,main=nomID,col=cls[1])
						usr=par("usr")
						if(diff(trng)>120){
							xlbl=pretty((usr[1:2]-min(allt))/60)
							xat=xlbl*60+min(allt)
						}else{
							xlbl=pretty(usr[1:2]-min(allt))
							xat=xlbl+min(allt)
						}
						axis(1,xat,xlbl)
						if(!manul) resbar(resl/dt*mlt) #add resolution bars
					}else{
						if(allt[1]>usr[2] | allQ[1]<usr[3] | allQ[1]>usr[4]){ #is a new plot needed?
							#rng=1:min(c(length(allQ),mxplot)) #replot all or n=mxplot
							if(allt[1]>usr[2]) { #x is past right side. Shift x as needed
								rng=1:min(c(cnt,mxplot))
								xrng=c(min(allt[rng]),max(allt[rng])+dt*mxplot)
							}else{ #y is too high or low, keep same x
								xrng=usr[1:2]
								rng=1:sum(allt>usr[1])
							}
							trng=range(allt)
							if(diff(trng)>120) xl="elapsed time (min)" else xl="elapsed time (s)"
							plot(allQ[rng]~allt[rng],xlim=xrng,
								#ylim=range(c(usr[3:4],allQ[rng]),na.rm=T),xaxt="n",type="l",
								ylim=range(c(allQ,yrng),na.rm=T),xaxt="n",type="l",
								ylab=yl,xlab=xl,main=nomID,xaxs="i",col=cls[1])
							points(allQ[rng]~allt[rng],pch=19,col=cls[1])
							lines(allrmQ[rng]~allt[rng],col=rmcls[1],lwd=2)
							#points(allrmQ[rng]~allt[rng],pch=19,col=rmcls[1])
							usr=par("usr")
							# add elapsed time on axis
							if(diff(trng)>120){
							xlbl=pretty((usr[1:2]-min(allt))/60)
							xat=xlbl*60+min(allt)
							}else{
								xlbl=pretty(usr[1:2]-min(allt))
								xat=xlbl+min(allt)
							}
							axis(1,xat,xlbl)
							#
							if(!manul) resbar(resl/dt*mlt) #add resolution bars
							if(ovrl>0 & cnt>(2*mxplot)){#add an inset
								if(allrmQ[1]<((usr[4]-usr[3])*0.5+usr[3])){
									yirng=c(usr[4]-ovrl*(usr[4]-usr[3]),usr[4]) #top left
								}else{
									yirng=c(usr[3],usr[3]+ovrl*(usr[4]-usr[3])) #bottom left
								}
								xirng=c(usr[1],usr[1]+ovrl*(usr[2]-usr[1]))

								polygon(xirng[c(1,1,2,2)],yirng[c(1,2,2,1)],col="white")
								dxr=(diff(range(head(allt,-runmean))))
								dyr=(diff(range(c(allQ,yrng),na.rm=T)))
								#miny=min(head(allQ,-1))-resl/dt
								miny=min(c(yrng,allQ),na.rm=T)
								xscl=(cnt-runmean)/(cnt-runmean+mxplot)*0.95
								sx=((allt-min(head(allt,-runmean)))/dxr*xscl+0.04)*(xirng[2]-xirng[1])+
									xirng[1]
								sy=((allrmQ-miny)/dyr*0.92+0.04)*(yirng[2]-yirng[1])+
									yirng[1]
								lines(sy~sx,col=rmcls[1])
								inst=T #inset has been made and can be added to later

								##check if scale is right
								if(F){
									yu=usr[c(3,3,4,4)]
									xu=usr[c(1,2,2,1)]
									#points(yu~xu,pch=19,col=3)
									sx=((xu-min(head(allt,-runmean)))/dxr*xscl+0.04)*(xirng[2]-xirng[1])+
										xirng[1]
									sy=((yu-miny)/dyr*0.92+0.04)*(yirng[2]-yirng[1])+
										yirng[1]
									points(sy~sx,cex=0.5,col=3,pch=19)
								}
							}
						}else{ #just add to existing plot
							points(allQ[1]~allt[1],pch=19,col=cls[1])
							#points(allrmQ[1]~allt[1],pch=19,col=rmcls[1])
							segments(c(allt[2],allt[2]),c(allQ[2], allrmQ[2]), c(allt[1], allt[1]), c(allQ[1], allrmQ[1]), col=c(cls[1],rmcls[1]), lwd=c(1,2))
							if(inst){ #add to inset if it exists
								sx=((allt[1:2]-min(head(allt,-runmean)))/dxr*xscl+0.04)*(xirng[2]-
									xirng[1])+xirng[1]
								sy=((allrmQ[1:2]-miny)/dyr*0.92+0.04)*(yirng[2]-yirng[1])+
									yirng[1]
								lines(sy~sx,col=rmcls[1])
							}
						}
					}
				}
			}
			now=as.numeric(Sys.time())
			if(!manul){
				while(now<(allt[1]+dt) & tclvalue(fini)==0){ #wait for the next reading or the button
					#Sys.sleep(0.1)
					now=as.numeric(Sys.time())
				}
			}
		##################################################
		} #done with reading
		if(!manul) tkdestroy(tt) #destroy the button
		text(allt,allrmQ,1:length(allQ)) #label the running means
		if(inst){ #add numbers to inset if it exists
			sx=((allt-min(head(allt,-runmean)))/dxr*xscl+0.04)*(xirng[2]-
				xirng[1])+xirng[1]
			sy=((allrmQ-miny)/dyr*0.92+0.04)*(yirng[2]-yirng[1])+
				yirng[1]
			wch=tail(seq(0,length(allQ),10),-1)
			text(sx[wch],sy[wch],(1:length(allQ))[wch],cex=0.4)
			points(sx,sy,cex=0.1,pch=19,col="black")
		}
		kpr=readline("which point to keep? (blank=last) ")
		if(kpr=="") kpr=1 else kpr=as.numeric(kpr)
		tst=T
		#if(!kleaf){ #not sure why I had this specific to !kleaf
			while(tst){ #sometimes I input pressure instead of what to keep - this gives an extra chance
				if(kpr==0 | kpr!=as.integer(kpr) | kpr>(length(allQ)-runmean)){ #kpr cannot be zero, a non-integer, or greater than the number of recorded running means
					kpr=readline("INVALID ANSWER: which point to keep? (blank=last) ")
					if(kpr=="") kpr=1 else kpr=as.numeric(kpr)
				}else tst=F
			}
		#}
		if(kleaf) {
			p.in=readline("infeed pressure (cm) ")
			tst=T
			while(tst){ #just in case you left this blank, here's another try (until you input something, non-numbers will be accepted but error out later)
				if(p.in==""){
						p.in=readline("INVALID ANSWER: infeed pressure? (cm) ")
				}else tst=F
			}
		}
		if(!kleaf) p.t=readline("ending pressure? (cm) ") else p.t=readline("leaf pressure (MPa) ") #hydraulic head
		tst=T
		while(tst){ #just in case you left this blank, here's another try (until you input something, non-numbers will be accepted but error out later)
			if(is.na(as.numeric(p.t))){ #blank and letters will error
				if(!kleaf) {
					p.t=readline("INVALID ANSWER: ending pressure? (cm) ")
				}else{
					p.t=readline("INVALID ANSWER: leaf pressure? (MPa) ")
				}
			}else tst=F
		}
		p.t=as.numeric(p.t)
		#convert to MPa after checking temp

		#find exisitng file for this species
		fls=list.files()
		appnd=paste("flowdata")#,ver,sep="")
		if(manul) appnd=paste(appnd,"manul")
		if(kleaf) appnd=paste(appnd,"kleaf")
		appnd=paste(appnd,".csv",sep="")
		fls=fls[grep(paste("",sp,appnd),fls)] #should be zero or one file left
					#adding space before sp helps filter out files with similar names
		if(length(fls)>1) {
			print(paste("Warning: more than one",sp,"flowdata file in working directory"))
			wf=readline(paste("Choose: ",paste("(",1:length(fls),") ",fls,"; ",sep="",collapse="")))
			if(wf=="") wf=0 else wf=as.numeric(wf)
			tst=	wf<1 | wf>length(fls)
			while(tst){
				wf=readline(paste("INVALID ANSWER Choose: ",paste("(",1:length(fls),") ",fls,"; ",sep="",collapse="")))
				if(wf=="") wf=0 else wf=as.numeric(wf)
				tst=wf<1 | wf>length(fls)
			}
			fls=fls[as.numeric(wf)]
			print("Consider moving other files to another directory")
		} else wf=1

		jr=julr(,2)
		alltdec=(allt-jr[2])/86400+jr[1]
		if(!kleaf){
			if(length(fls)==0){
				tmp=NULL
				d.mm=as.numeric(readline("no previous diameter. d.mm= "))
				tst=T
				while(tst){
					if(is.na(d.mm)) d.mm=as.numeric(readline("INVALID ANSWER: d.mm= ")) else tst=F
				}
				l.mm=as.numeric(readline("no previous length. l.mm= "))
				tst=T
				while(tst){
					if(is.na(l.mm)) l.mm=as.numeric(readline("INVALID ANSWER: l.mm= ")) else tst=F
				}
			}else{
				tmp=read.csv(fls[1]) #read in previous data
				prevt=tail(tmp$Tsoln.C,1)
				tmp=tmp[tmp$stemID==stemID,] #limit to this stem
				if(dim(tmp)[1]>0){ #existing stem
					d.mm=tail(tmp$d.mm,1)
					l.mm=tail(tmp$l.mm,1)
					#print(paste("stored d.mm=",d.mm,sep=""))
					#print(paste("stored l.mm=",l.mm,sep=""))

					#tstd=readline(paste("previous d.mm=",tail(tmp$d.mm,1),
					#	" blank=use, otherwise d.mm= "))
					tstl=readline(paste("previous l.mm=",tail(tmp$l.mm,1),
						" blank=use, otherwise l.mm= "))
					#if(tstd=="") d.mm=tail(tmp$d.mm,1) else d.mm=as.numeric(tstd)
					if(tstl=="") l.mm=tail(tmp$l.mm,1) else l.mm=as.numeric(tstl)

				}else{ #new stem
					d.mm=as.numeric(readline("no previous diameter. d.mm= "))
					tst=T
					while(tst){
						if(is.na(d.mm)) d.mm=as.numeric(readline("INVALID ANSWER: d.mm= ")) else tst=F
					}
					l.mm=as.numeric(readline("no previous length. l.mm= "))
					tst=T
					while(tst){
						if(is.na(l.mm)) l.mm=as.numeric(readline("INVALID ANSWER: l.mm= ")) else tst=F
					}
				}
			}#end if file exists
			if(length(fls)==0){
				prevt=Tsoln.C
			}
			tsol=readline(paste("previous Tsoln.C=",prevt," blank=use, otherwise Tsoln.C= "))
			if(tsol==""){
				tsol=prevt
			}else{
				tst=F
				while(tst){
					if(is.na(tsol)) tsol =as.numeric(readline("INVALID ANSWER: Tsoln.C= ")) else tst=F
				}
			}
			tsol=as.numeric(tsol)
			rho= 999.9+0.05545* tsol-0.007881* tsol ^2+0.00004204* tsol ^3 #density (kg/m3) based on temp####
			p.MPa=rho*grav*p.t/(100*1e6)

			flowdat=data.frame(sp,numbr,Ptreat.MPa,d.mm,l.mm,stemID,nomID,allt-min(allt), alltdec, alldt, allbal/mlt, allQ/mlt, allrmQ/mlt, p.cm=p.t, p.MPa,Tsoln.C=tsol,ver, keep=F,usefork=F)
		}else{ #(if kleaf)
			if(length(fls)==0){
				prevt=Tsoln.C
			}else{
				tmp=read.csv(fls[1]) #read in previous data
				prevt=tail(tmp$Tsoln.C,1)
			}
			tsol=readline(paste("previous Tsoln.C=",prevt," blank=use, otherwise Tsoln.C= "))
			if(tsol==""){
				tsol=prevt
			}else{
				tst=F
				while(tst){
					if(is.na(tsol)) tsol =as.numeric(readline("INVALID ANSWER: Tsoln.C= ")) else tst=F
				}
			}
			rho= 999.9+0.05545* tsol-0.007881* tsol ^2+0.00004204* tsol ^3 #density (kg/m3) based on temp####
			p.t=-abs(p.t) #leaf pressure must be negative
			p.in=as.numeric(p.in)
			p.MPa=rho*grav*p.in/(100*1e6)-p.t #convert from cm to MPa

			flowdat=data.frame(sp,numbr,Ptreat.MPa,stemID,nomID,allt-min(allt),alltdec, alldt, allbal/mlt, allQ/mlt, allrmQ/mlt, p.MPa, pleaf.MPa=rho*grav*p.in/(100*1e6)-p.MPa, Tsoln.C= tsol,ver,keep=F,usefork=F) #as above but no d.mm, l.mm or p.cm
		}#end if !kleaf
		flowdat$keep[kpr]=T
		names(flowdat)[1:6+ifelse(kleaf,5,7)]=c("t.s","t.day","dt.s",paste("bal",qu,sep="."), paste("Q",qu,"s",sep="."), paste("rmQ",qu,"s",sep="."))
		flowdat=flowdat[order(flowdat$t.s),]
		#assign("flowdat",flowdat,envir=.GlobalEnv)
		if(length(fls)==0){
			flowdat$measure=1
			nom=paste(Sys.Date(),sp,paste("flowdata"))#,ver,sep=""))
			if(manul) nom=paste(nom,"manul")
			if(kleaf) nom=paste(nom,"kleaf")
			nom=paste(nom,".csv",sep="")
			#nom=paste(Sys.time(),sp,"flowdata.csv",sep=" ")
			nom=gsub(":","-",nom) #Windows doesn't like colons in file names
			msg=paste("data written in NEW file as:",nom,"to",getwd())
		}else{
			nom=fls[1]
			tmp=read.csv(fls[1])
			if(sum(tmp$nomID==nomID)>0){
				flowdat$measure=max(tmp$measure[tmp$nomID==nomID])+1
			}else flowdat$measure=1
			flowdat=frbind(tmp,flowdat)
			msg=paste("data APPENDED to:",nom,"in",getwd())
		}
		adon=",fix=T)"
		if(kleaf) adon=paste(",kleaf=T",adon,sep="")
		if(manul) adon=paste(",manul=T",adon,sep="")
		print(paste("Make corrections using: cond('",numbr, "','", sp, "',", Ptreat.MPa, adon, sep=""))
		write.csv(flowdat,nom,row.names=F)
		print(msg)
	}#end if !fix

	##read in previous measurements on this stem (if they exist), plot and offer to calculate and store conductance if there are enough readings
	fls=list.files()
	appnd=paste("flowdata")#,ver,sep="")
	if(manul) appnd=paste(appnd,"manul")
	if(kleaf) appnd=paste(appnd,"kleaf")
	appnd=paste(appnd,".csv",sep="")
	fls=fls[grep(paste("",sp,appnd),fls)] #should one (or more) file left
	if(length(fls)==0) stop("no file to fix. check your input")
	if(is.null(wf)){ #this means you skipped straight here with fix=T. flowdata is read in twice for each measurement: first to save data and then to fix and/or see about calculating conductance. If there are mulitple files, I dont want to ask which one twice
		if(length(fls)>1) {
			print(paste("Warning: more than one",sp,"flowdata file in working directory"))
			wf=readline(paste("Choose: ",paste("(",1:length(fls),") ",fls,"; ",sep="",collapse="")))
			if(wf=="") wf=0 else wf=as.numeric(wf)
			tst=	wf<1 | wf>length(fls)
			while(tst){
				wf=readline(paste("INVALID ANSWER Choose: ",paste("(",1:length(fls),") ",fls,"; ",sep="",collapse="")))
				if(wf=="") wf=0 else wf=as.numeric(wf)
				tst=wf<1 | wf>length(fls)
			}
			#fls=fls[as.numeric(wf)]
			print("Consider moving other files to another directory")
		} else wf=1
	}
	fls=fls[wf] #wf chosen earlier
	tmp=read.csv(fls[1],stringsAsFactors=F)
	#'try adding fix
	if(fix){
		nmeas=unique(tmp$measure[tmp$nomID==nomID]) #in order of measurement - could differ from numerical order if fix has been used
		#print("it would be nice if these numbers matched with those plotted in Q vs P. Either make that plot use 'measure' or always make these numerical and sequential")
		wchm=readline(paste("which measure to fix (blank=skip to k calculation): ",paste(nmeas,collapse=", "),"? ",sep=" "))
		if(wchm!=""){
			wchm=as.numeric(wchm)
			ths=tmp$nomID==nomID & tmp$measure==wchm
			tsol=tmp$Tsoln.C[ths][1]
			rho= 999.9+0.05545* tsol-0.007881* tsol ^2+0.00004204* tsol ^3 #density (kg/m3) based on temp####

			if(!kleaf) {
				wht=readline("what to fix: (1) diam,  (2) length,  (3) end pressure,  (4) treat pressure,  (5) ID number,  (6) Temp ")
			}else{
				wht=readline("what to fix: (1) infeed pressure,  (2) leaf pressure,  (4) treat pressure,  (5) ID number,  (6) Temp ")
			}
			wht=as.numeric(wht)
			chng=F
			if(wht==5){ #change ID number
				ov=tmp$numbr[ths][1] #old value
				newv=readline(paste("old ID number: ",ov,"; correct ID number (no quotes)? ",sep=""))
				if(newv!=""){
					prex=tmp$numbr==newv #preexisting measures with this number
					tmp$numbr[ths]=newv
					tmp$stemID[ths]=paste(tmp$sp[ths], tmp$numbr[ths])
					tmp$nomID[ths]=paste(tmp$stemID[ths], tmp$Ptreat.MPa[ths], "MPa")
					chng=T
				}
				if(!kleaf) print("Need to check diameter and length:")
			}
			if((wht==1 | wht==5) & !kleaf){
				ov=tmp$d.mm[ths][1]
				newv=readline(paste("old diam: ",ov,"; correct diameter (mm)? (blank=no change) ",sep=""))
				if(newv!=""){
					chng=T
					tmp$d.mm[ths]=as.numeric(newv)
				}
			}
			if((wht==2 | wht==5) & !kleaf){
				ov=tmp$l.mm[ths][1]
				newv=readline(paste("old length: ",ov,"; correct length (mm)? (blank=no change) ",sep=""))
				if(newv!=""){
					chng=T
					tmp$l.mm[ths]=as.numeric(newv)
				}
			}
			if(wht==3 & !kleaf){
				ov=tmp$p.cm[ths][1]
				newv=readline(paste("old end pressure: ",ov,"; correct end pressure (cm)? (blank=no change) ",sep=""))
				if(newv!=""){
					chng=T
					tmp$p.cm[ths]=as.numeric(newv)
					tmp$p.MPa[ths]=rho*grav*tmp$p.cm[ths]/(100*1e6)
				}
			}
			if(wht==4){
				ov=tmp$Ptreat.MPa[ths][1]
				newv=readline(paste("old treat pressure: ",ov,"; correct treat pressure (MPa)? (blank=no change) ",sep=""))
				if(newv!=""){
					chng=T
					prex=tmp$nomID==paste(tmp$sp[ths][1],tmp$numbr[ths][1],newv,"MPa")
					tmp$Ptreat.MPa[ths]=as.numeric(newv)
					tmp$nomID[ths]=paste(tmp$sp[ths], tmp$numbr[ths], tmp$Ptreat.MPa[ths], "MPa")
				}
			}
			if(wht==4 | wht==5){#need to update measure number and not have it conflict with exsiting
				if(sum(prex)>0){
					tmp$measure[ths]=max(tmp$measure[prex])+1
				}else tmp$measure[ths]=1
				if(tmp$measure[ths][1]!=wchm) print(paste("measure number changed to",tmp$measure[ths][1]))
			}
			if(wht==1 & kleaf){
				#p.t=-abs(p.t) #leaf pressure must be negative
				#p.in=as.numeric(p.in)
				#p.MPa=rho*grav*p.in/(100*1e6)-p.t #convert from cm to MPa
				#pleaf.MPa=rho*grav*p.in/(100*1e6)-p.MPa
				ov=(tmp$pleaf.MPa[ths][1]+tmp$p.MPa[ths][1])*(100*1e6)/(rho*grav)
				ov=round(ov,8)
				newv=readline(paste("old infeed pressure: ",ov,"; correct infeed pressure (cm)? (blank=no change) ",sep=""))
				if(newv!=""){
					chng=T
					tmp$p.MPa[ths]=rho*grav*as.numeric(newv)/(100*1e6)-tmp$pleaf.MPa[ths][1]
				}
			}
			if(wht==2 & kleaf){
				ov=tmp$pleaf.MPa[ths][1]
				newv=readline(paste("old leaf pressure: ",ov,"; correct leaf pressure (MPa)? (blank=no change) ",sep=""))
				if(newv!=""){
					chng=T
					newv=-abs(as.numeric(newv))
					tmp$p.MPa[ths]=tmp$p.MPa[ths][1]+tmp$pleaf.MPa[ths][1]-newv
					tmp$pleaf.MPa[ths]=newv
				}
			}
			if(wht==6){
				ov=tmp$Tsoln.C[ths][1]
				newv=readline(paste("old Temp: ",ov,"; correct Temp (C)? (blank=no change) ",sep=""))
				if(newv!=""){
					chng=T
					tmp$Tsoln.C[ths]=as.numeric(newv)
				}
			}

			if(chng){
				write.csv(tmp,fls[1],row.names=F)
				print("flowdata file updated")
			}else print("no change made")
		}
	} #end if fix
	#'end try adding fix
	smp=tmp[tmp$nomID==nomID,]
	if(sum(smp$keep)>1 | nmin==1){ #get at least two measurements to make a plot, unless 1 is ok
		rmq=smp$rmQ[smp$keep]
		dps.MPa=smp$p.MPa[smp$keep]
		tsol=tail(smp$Tsoln[smp$keep],1)
		if(!kleaf) dps=smp$p.cm[smp$keep] else {
			dps=dps.MPa
			plvs=smp$pleaf.MPa[smp$keep]
		}
		#if(diff(range(dps))==0) warning("need additional pressures")
		wch= !logical(length(rmq))
		#lab=1:length(rmq)
		lab=smp$measure[smp$keep]
		tst=T
		plot(smp$rmQ*mlt~I((smp$t.day-min(smp$t.day))*1440),type="l",ylab=paste("mean ",yl),xlab="elapsed time (min)")
		points(smp$rmQ[smp$keep]*mlt~I((smp$t.day[smp$keep]-min(smp$t.day))*1440),pch=19,col="red",cex=0.8)
		abline(h=0,lty=2)
		if(!manul) resbar(resl/dt*mlt)
		Sys.sleep(1)
		print("(go back one plot to see all mean flow rates)")
		if(nmin>1) {
			qrng=range(rmq)*mlt
			prng=range(dps)
		}else{
			qrng=range(c(0,rmq))*mlt
			prng=range(c(0,dps))
		}
		while(tst){
			plot(rmq*mlt~dps,xlab=xlb,ylab=paste("mean ",yl), main=nomID,type="n",ylim=qrng,xlim=prng)
			abline(h=0,col="grey70",lty=3)
			if(!manul) resbar(resl/dt*mlt) #add resolution bars
			text(dps,rmq*mlt,lab,col=ifelse(wch,"black","grey"))
			if(sum(wch)>1){
				reg=lm(rmq[wch]*mlt~dps[wch]) #fit to flow vs P(cm) - for the plot
				abline(reg)
				kreg=lm(rmq[wch]~dps.MPa[wch]) #fit to flow vs P(MPa) - for actual conductance
				tmk=summary(kreg)$coef[2]*((0.001713-0.00004565*tsol+0.000000505*tsol^2)/(0.001002))
				if(tmk>0) oom=log(tmk,10) else oom=log(-tmk,10) #get order of magnitude
				if(tmk>0) cn=2 else cn=1 #choose a corner to put k
				corner(cn,c(paste("k =",round(tmk,-oom+3),kun),
					as.expression(substitute(r^2==a,list(a=round(summary(reg)$r.square,3))))))
			}else{
				if(nmin==1 & sum(wch)>0){
					abline(a=0,b=rmq[wch]/dps[wch]*mlt)
					tmk=(rmq[wch]/dps.MPa[wch])*((0.001713-0.00004565*tsol+0.000000505*tsol^2)/(0.001002))
					if(tmk>0) oom=log(tmk,10) else oom=log(-tmk,10) #get order of magnitude
					if(tmk>0) cn=2 else cn=1 #choose a corner to put k
					corner(cn,c(paste("k =",round(tmk,-oom+3),kun),expression(paste(r^2,"=NA"))))
				}
			}

			inpt=readline("exclude points? (input number to exclude, blank = no change) ")
			if(inpt!=""){
				inpt=as.numeric(inpt)
				wch[inpt==lab]=F
			}else tst=F
		}

		if(sum(wch)>(nmin-1)){ #are there enough readings for getting k? (3=default)
			if(sum(wch)>1){
				reg=lm(rmq[wch]~dps.MPa[wch])
				k=summary(reg)$coef[2]*((0.001713-0.00004565*tsol+0.000000505*tsol^2)/(0.001002))  #correct for viscosity to 20 C
				kr2=summary(reg)$r.square
			}else{
				k=(rmq[wch]/dps.MPa[wch])*((0.001713-0.00004565*tsol+0.000000505*tsol^2)/(0.001002))
				kr2=NA
			}
			tst=readline("save conductance? (y = yes; blank = no) ")
			if(tst=="y"){
				tmp$usefork[tmp$nomID==nomID & tmp$keep][wch]=T #update which were used for conductance
				write.csv(tmp,fls[1],row.names=F) #and resave the file
				if(!kleaf){
					kdat=data.frame(t.day=tail(smp$t.day,1),sp,numbr,Ptreat.MPa,nomID,stemID,l.mm=tail(smp$l.mm,1),d.mm=tail(smp$d.mm,1),k,r2=kr2)
					kdat$K=kdat$k*kdat$l.mm
					kdat$Ks=kdat$K/(0.25*pi*kdat$d.mm^2)
					kdat$ver=ver
					names(kdat)[c(3,8,10,11)+1]=c(paste("P.MPa",sep="."),
						paste("k",qu,"s.MPa",sep="."),
						paste("K",qu,"mm.s.MPa",sep="."),
						paste("Ks",qu,"s.MPa.mm",sep="."))
				}else{ #for kleaf
					kdat=data.frame(t.day=tail(smp$t.day,1),sp,numbr,P.MPa=Ptreat.MPa,nomID, stemID,k, r2=kr2, Pleaf.MPa=tail(plvs[wch],1),ver)
					names(kdat)[7]=paste("k",qu,"s.MPa",sep=".")
				}
				fls=list.files()
				appnd=paste("condcalc")#,ver,sep="")
				if(manul) appnd=paste(appnd,"manul")
				if(kleaf) appnd=paste(appnd,"kleaf")
				appnd=paste(appnd,".csv",sep="")
				fls=fls[grep(paste("",sp,appnd),fls)]
				fls=fls[grep(paste("",sp,appnd),fls)] #there should be zero or one file left
				kdat$plc=NA

				if(length(fls)>1) {
					print(paste("Warning: more than one",sp,"condcalc file in working directory"))
					wf=readline(paste("Choose: ",paste("(",1:length(fls),") ",fls,"; ",sep="",collapse="")))
					if(wf=="") wf=0 else wf=as.numeric(wf)
					tst=	wf<1 | wf>length(fls)
					while(tst){
						wf=readline(paste("INVALID ANSWER Choose: ",paste("(",1:length(fls),") ",fls,"; ",sep="",collapse="")))
						if(wf=="") wf=0 else wf=as.numeric(wf)
						tst=wf<1 | wf>length(fls)
					}
					fls=fls[wf]
					print("Consider moving other files to another directory")
				}

				if(length(fls)==0){
					nom=paste(Sys.Date(),sp,paste("condcalc"))#,ver,sep=""))
					if(manul) nom=paste(nom,"manul")
					if(kleaf) nom=paste(nom,"kleaf")
					nom=paste(nom,".csv",sep="")
					#nom=paste(Sys.time(),sp,"condcalc.csv",sep=" ")
					nom=gsub(":","-",nom) #Windows doesn't like colons in file names
					print(paste("data written in NEW file as:",nom,"to",getwd()))
					#kdat$plc=0
				}else{
					tmp=read.csv(fls[1])
					nom=fls[1]
					#prv=tmp[tmp$stemID==stemID,]
					#if(dim(prv)[1]==0){
					#	kdat$plc=0
					#}else{
					#	kdat$plc=100*(1-kdat$K.g.mm.s.MPa/prv$K.g.mm.s.MPa[prv$P.MPa==min(prv$P.MPa)])
					#}
					kdat=frbind(tmp,kdat)
					print(paste("data APPENDED to:",nom,"in",getwd()))
				}
				sam=kdat$stemID==stemID # same stem
				if(!kleaf){
					isK=substring(names(kdat),1,2)=="K."
					kmaxref=abs(kdat$P.MPa)[sam]==min(abs(kdat$P.MPa)[sam]) #which has most favorable P
					kdat$plc[sam]=100*(1-kdat[sam,isK]/kdat[sam,isK][tail(which(kmaxref),1)]) #recalc all plc values for this stem - using most recent ref if there are multiples
				}
				write.csv(kdat,nom,row.names=F)
				return(kdat)
			}
		}
	}
}
#################################################################################
##########SHOWVULN FUNCTION######################################################
showvuln=function(sp,yax=c("k.","K.","Ks")[3],as.plc=F,horiz=90,retrn=F){
	setwd(kwd)
	fls=list.files()
	fls=fls[grep(".csv",fls)]
	fls=fls[grep(paste("",sp,paste("condcalc",sep="")),fls)] #should be zero or one file left
	nfl=length(fls)
	if(nfl==0) stop(paste("species not found in",kwd))
	if(nfl>1) wch=as.numeric(readline(paste("which file:" ,paste("(",1:nfl,") ",fls,",",sep="",collapse=" ")," "))) else wch=1
	dat=read.csv(fls[wch])
	kleaf=length(grep("kleaf",fls[wch]))>0
	if(kleaf) yax="k."
	if(kleaf & as.plc) stop("PLC not calculated for leaves")
	noms=names(dat)
	xis=which(noms=="P.MPa")
	yis=which(yax==substring(noms,1,2))
	dat$cls=as.numeric(as.factor(dat$stemID))
	dat$Y=dat[,yis]
	dat$X=dat[,xis]
	numbrs=unique(dat$numbr)
	dat$stemID=as.character(dat$stemID)
	if(as.plc){
		plot(plc~X,dat,xlab=noms[xis],ylab="PLC",pch=19,col=cls,ylim=range(c(0,plc)))
		abline(h=horiz,col="grey90")
		for(i in numbrs) lines(plc~X,dat[dat$numbr==i,],col=cls)
		legend("topleft",sort(unique(dat$stemID)),pch=19,col=1:sum(!duplicated(dat$stemID)),bty="n")
	}else{
		plot(Y~X,dat,xlab=noms[xis],ylab=noms[yis],pch=19,col=cls,ylim=range(c(0,Y)))
		for(i in numbrs) lines(Y~X,dat[dat$numbr==i,],col=cls)
		legend("topright",sort(unique(dat$stemID)),pch=19,col=1:sum(!duplicated(dat$stemID)),bty="n")
	}
	if(retrn) return(dat)
}

#################################################################################
##########INTERNAL FUNCTIONS#####################################################
corner=function(xy,text,ofst=0.01,cex=1,wrt=c(NULL,"x","y"),spcadj=1,...) {
	usr=par("usr")
	dimen=par("pin")
	shorty=which(dimen==min(dimen))[1]
	if(wrt[1]=="x" & shorty==2) shorty=1
	if(wrt[1]=="y" & shorty==1) shorty=2
	xconv=(usr[2]-usr[1])/dimen[1]
	yconv=(usr[4]-usr[3])/dimen[2]
	if(shorty==1){
		xofst=ofst
		xinch=ofst*(usr[2]-usr[1])/xconv
		yunits=xinch*yconv
		yofst=yunits/(usr[4]-usr[3])
	}else{
		yofst=ofst
		yinch=ofst*(usr[4]-usr[3])/yconv
		xunits=yinch*xconv
		xofst=xunits/(usr[2]-usr[1])
	}
	if(xy=="topleft" | xy=="topright" | xy==1 | xy==2) {
		y=(1-yofst)*usr[4]+yofst*usr[3]
		ud=1
	}else{
		y=(yofst)*usr[4]+(1-yofst)*usr[3]
		ud=0
	}
	if(xy=="topleft" | xy=="bottomleft" | xy==2 | xy==3) {
		x=(xofst)*usr[2]+(1-xofst)*usr[1]
		lr=0
	}else{
		x=(1-xofst)*usr[2]+xofst*usr[1]
		lr=1
	}
	if(length(text)>1){
		cin=par("cin")[2]*spcadj
		if(ud==1){
			y=y-cex*cin*yconv*1*(1:length(text)-1)
		}else{
			y=sort(y+cex*cin*yconv*1*(1:length(text)-1),T)
		}
	}
	if(par()$xlog==T) x=10^x
	if(par()$ylog==T) y=10^y
	text(x,y,text,cex=cex,adj=c(lr,ud),...)
}

resbar=function(res){
	usr=par("usr")
	yunit=c(floor(usr[3]/res),ceiling(usr[4]/res))
	yat=yunit*res
	yat=seq(yat[1],yat[2],2*res)
	xat=usr[1]+0.98*(usr[2]-usr[1]) #bar occupies 2% of right hand side
	sq=seq(1,,5,length(yat))
	xx=numeric(5*(length(yat)))
	xx[sq+4]=NA
	yy=xx
	xx[sq]=xx[sq+3]=xat
	xx[sq+1]=xx[sq+2]=usr[2]
	yy[sq]=yy[sq+1]=yat
	yy[sq+2]=yy[sq+3]=yat+res
	polygon(xx,yy,col="grey90",border=NA)
	abline(v=usr[2]) #cover up the overlap onto the border
}

julr=function(rtime=NULL,opt=1){
	if(is.null(rtime)) rtime=Sys.time()
	ortime=rtime
	rtime=as.character(rtime)
	splt=unlist(strsplit(rtime," "))
	dayt=unlist(strsplit(splt[seq(1,length(splt),2)],"-"))
	tm=unlist(strsplit(splt[seq(2,length(splt),2)],":"))
	yr=as.numeric(dayt[seq(1,length(dayt),3)])
	mo=as.numeric(dayt[seq(2,length(dayt),3)])
	dy=as.numeric(dayt[seq(3,length(dayt),3)])
	hr=as.numeric(tm[seq(1,length(tm),3)])
	mn=as.numeric(tm[seq(2,length(tm),3)])
	ss=as.numeric(tm[seq(3,length(tm),3)])
	cdy=cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))
	jday=cdy[mo]+dy
	leap=(yr%%4==0 & yr%%100!=0) | yr%%400==0
	jday=jday+ifelse(leap & mo>2,1,0)
	jday=jday+(hr+(mn+ss/60)/60)/24
	if(opt!=1 & length(rtime)==1) return(c(jday,as.numeric(ortime))) else return(jday)
}

decod=function(x,typ=c("sar","met","ohaus","sci")[1],dec){
	x=as.character(x)
	if(length(x)>0){
		pos=1
		if(typ=="sar" | typ=="sci"){ #sci: thanks Greg
			if(x[2]=="-") pos=-1
			if(x[2]=="+" | x[2]=="-") wch=3 else wch=2
		}
		if(typ=="met") wch=3
		if(typ=="ohaus") wch=16 #thanks Stephanie
		nd=nchar(unlist(strsplit(x[wch],"[.]"))[2])
		#print(nd)
		#print(dec)
		if(!is.na(nd)){
			if(nd==dec){
				x=as.numeric(x[wch])
				x=x*pos
			}else{
				x=NA
				message(paste("only",nd,"of",dec,"dec. points. Retrying..."))
			}
		}else{
			message(paste("unparsable input from balance:",x))
			x=NA
		}
	}else x=NA
	return(x)
}

frbind=function(d1,d2){
	if(!is.null(d1) & !is.null(d2)){
		n1=names(d1)
		n2=names(d2)
		nn1=length(n1)
		nn2=length(n2)
		u1=n1[is.na(match(n1,n2))]
		u2=n2[is.na(match(n2,n1))]
		if(length(u2)>0){
			for(i in 1:length(u2)) d1=cbind(d1,NA)
			names(d1)[nn1+1:length(u2)]=u2
		}
		if(length(u1)>0){
			for(i in 1:length(u1)) d2=cbind(d2,NA)
			names(d2)[nn2+1:length(u1)]=u1
		}
	}
	rbind(d1,d2)
}

#################################################################################
##########RPM to PRESSURE FUNCTION###############################################
pcent=function(rpm,rmax.mm,r=0) {
	rho=998.2 #assume 20C
	rho*(r^2-rmax.mm^2)/2*((rpm*2*pi)/(1e6*60))^2
}

#################################################################################
##########HYDRAULIC HEAD, HEIGHT TO PRESSURE FUNCTION############################
phead=function(p.cm,rho,grav) {
	rho*grav*p.cm/(100*1e6)
}

#################################################################################
##########PRESSURE to RPM FUNCTION###############################################
rpmcent=function(p,rmax.mm,r=0) {
	rho=998.2 #assume 20C
	(2*p/(rho*(r^2-rmax.mm^2)))^0.5*1e6*60/(2*pi)
}

#Belated version history:
#16: semi-revison so program asks for stem length every time
#17: decod altered to handle unparsable inputs (addresses the 'if (nd == dec)' error). Further, the function returns a message containing the offending input.
#18: fixed issue where running cond(...fix=T) and changing ID led to no change if diameter or length did not change. Added Ohaus capability.


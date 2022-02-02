# Makefile for the BEM-DC

FF = gfortran
FFLAGS = -O2 -fbounds-check -Wall
#FFLAGS = -O2
OPEN = 
RM = del   #in windows
#RM = rm    #in unix/linux/mac

all: bem

bem: parameters.o initbem.o functions.o bem.o plqdf1.o
	$(FF) -o bem $(FFLAGS) $(OPEN) parameters.o initbem.o functions.o bem.o plqdf1.o 

param.mod: parameters.o parameters.f03
	$(FF) -c parameters.f03

parameters.o: parameters.f03
	$(FF) -c parameters.f03

initbem.mod: param.mod initbem.f03
	$(FF) -c $(FFLAGS) $(OPEN) initbem.f03

initbem.o: param.mod initbem.f03
	$(FF) -c $(FFLAGS) $(OPEN) initbem.f03

functions.mod: param.mod functions.o functions.f03
	$(FF) -c $(FFLAGS) $(OPEN) functions.f03 
	
functions.o: param.mod functions.f03
	$(FF) -c $(FFLAGS) $(OPEN) functions.f03 

bem.o: param.mod initbem.mod functions.mod bem.f03
	$(FF) -c $(FFLAGS) $(OPEN) bem.f03 

plqdf1.o: plqdf1.f
	$(FF) -c $(FFLAGS) $(OPEN) plqdf1.f 

clean:	
	$(RM) bem *.mod *.o  
	echo Clean done

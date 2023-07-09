CXX =g++
FC =gfortran
LD =gfortran
#FC =f77
#LD =f77

CFLAGS=-c -Wall -Df2Fortran -std=c++11
FFLAGS=-c

LIBS=-lpacklib -lmathlib -lkernlib

LFLAGS=-Wl,-static -Wl,-dy -lnsl -lcrypt -ldl -lgfortran $(LIBS)

PARNIDIR=/home/tomoki/Tools/Parni

OBJECTS=integral.o\
	$(PARNIDIR)/avh_parni.o\
	$(PARNIDIR)/examples/avh_moncar.o\


#HEADERS=

#.f.o: 
#	$(FC) $(FFLAGS) -c $*.f -o $*.o

all:integral

integral.o: integral.cpp
	$(CXX) $(CFLAGS) integral.cpp

integral: $(OBJECTS)
	$(CXX) -o integral $(OBJECTS) $(LFLAGS)



avh_parni.o: $(PARNIDIR)/avh_parni.f $(PARNIDIR)/avh_parni.h
	$(FC) -c $(PARNIDIR)/avh_parni.f


avh_moncar.o: $(PARNIDIR)/examples/avh_moncar.f
	$(FC) -c $(PARNIDIR)/examples/avh_moncar.f
	

clean:
	rm -f *.o; rm -f $(PARNIDIR)/*.o;rm -f $(PARNIDIR)/examples/*.o; rm -f *.o

	

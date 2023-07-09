#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------


# PREC128 = quadrupole, PREC64 = double
PRECISION=PREC64
#PRECISION=PREC128

# define relevant paths
ifeq ($(PRECISION),PREC128)
CUBADIR=$(HOME)/Tools/Cuba-4.2-q
else
CUBADIR=$(HOME)/Tools/Cuba-4.2
endif

#HEPDIR=$(HOME)/utils/libraries/heplib
#MATHDIR=/usr/local/Wolfram/Mathematica/11.0/SystemFiles/IncludeFiles/C
#INCLUDE = $(shell gsl-config --cflags)
#LIBRARIES+= $(shell gsl-config --libs-without-cblas)
#LIBRARIES+=-L/usr/local/Wolfram/Mathematica/11.0/SystemFiles/Libraries/Linux-x86-64 \
-lmkl_def -lmkl_core -lWolframRTL_Minimal
#LIBRARIES+=-L/usr/lib/x86_64-linux-gnu -lpthread


# include and libraries
INCLUDE += -I$(CUBADIR)/include
#INCLUDE += -I$(HEPDIR) -I$(CUBADIR)/include

ifeq ($(PRECISION),PREC128)
  #LIBINTDIR=../libint128
  #LIBINT=$(LIBINTDIR)/libint128.a
  #LIBRARIES += -L$(CUBADIR)/lib -lcubaq -lquadmath
  LIBRARIES += -L$(CUBADIR) -lcubaq -lquadmath 
else
  #LIBINTDIR=../libint64
  #LIBINT=$(LIBINTDIR)/libint64.a
  LIBRARIES += -L$(CUBADIR) -lcuba -L$(LIBINTDIR) -lint64
endif



ifeq ($(PRECISION),PREC128)
SRC =  beamcalc.cc
else
SRC = beamcalc.cc
endif
 
OBJ = $(patsubst %.cc,%.o,$(SRC))
DEPS = $(patsubst %.cc,%.d,$(SRC))
EXE =  $(patsubst %.cc,%,$(SRC))


CXX=g++
CXXFLAGS= -Wall -O3 -MMD -MP $(INCLUDE) -fPIC  -D $(PRECISION)


all:	$(EXE) 


ifeq ($(PRECISION),PREC128)
beamcalc: beamcalc.o $(LIBINT)
	$(CXX) $(CXXFLAGS) -o $@ $@.o $(LIBRARIES)
else
beamcalc: beamcalc.o $(LIBINT)
	$(CXX) $(CXXFLAGS) -o $@ $@.o $(LIBRARIES)
endif



clean:
	rm -fv $(OBJ) $(EXE) *.a *.d *.o

distclean:
	rm -fv $(OBJ) $(EXE) *.a *.d *.o
	rm -fv integral?.*


%.o: %.cc 
	$(CXX) -c $(CXXFLAGS) -o $@ $(patsubst %.o, %.cc, $@)


# takes care of dependencies
-include ${DEPS}

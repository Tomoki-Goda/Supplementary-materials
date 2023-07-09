#include<iostream>
#include<cmath>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <limits>
#include <quadmath.h>
#include <vector>

#include"../beamcalc/integrals/integral1.hh"
#include"../beamcalc/integrals/integral2.hh"
#include"../beamcalc/integrals/integral3.hh"
#include"../beamcalc/integrals/integral4.hh"/*
#include"../beamcalc/integrals/integral5.hh"
#include"../beamcalc/integrals/integral6.hh"
#include"../beamcalc/integrals/integral7.hh"
#include"../beamcalc/integrals/integral8.hh"*/
/*
#include"../beamcalc/integrals/integral13.hh"
#include"../beamcalc/integrals/integral26.hh"
#include"../beamcalc/integrals/integral39.hh"


#include"../beamcalc/integrals/integral7.hh"
#include"../beamcalc/integrals/integral9.hh"
#include"../beamcalc/integrals/integral20.hh"
#include"../beamcalc/integrals/integral22.hh"
#include"../beamcalc/integrals/integral33.hh"
#include"../beamcalc/integrals/integral35.hh"


#include"../beamcalc/integrals/integral11.hh"
#include"../beamcalc/integrals/integral12.hh"
#include"../beamcalc/integrals/integral24.hh"
#include"../beamcalc/integrals/integral25.hh"
#include"../beamcalc/integrals/integral37.hh"
#include"../beamcalc/integrals/integral38.hh"



#include"../beamcalc/integrals/integral8.hh"
#include"../beamcalc/integrals/integral10.hh"
#include"../beamcalc/integrals/integral21.hh"
#include"../beamcalc/integrals/integral23.hh"
#include"../beamcalc/integrals/integral34.hh"
#include"../beamcalc/integrals/integral36.hh"


#include"../beamcalc/integrals/integral5.hh"
#include"../beamcalc/integrals/integral6.hh"
#include"../beamcalc/integrals/integral18.hh"
#include"../beamcalc/integrals/integral19.hh"
#include"../beamcalc/integrals/integral31.hh"
#include"../beamcalc/integrals/integral32.hh"
 

#include"../beamcalc/integrals/integral1.hh"
#include"../beamcalc/integrals/integral2.hh"
#include"../beamcalc/integrals/integral3.hh"
#include"../beamcalc/integrals/integral4.hh"


#include"../beamcalc/integrals/integral14.hh"
#include"../beamcalc/integrals/integral15.hh"
#include"../beamcalc/integrals/integral16.hh"
#include"../beamcalc/integrals/integral17.hh"

#include"../beamcalc/integrals/integral27.hh"
#include"../beamcalc/integrals/integral28.hh"
#include"../beamcalc/integrals/integral29.hh"

#include"../beamcalc/integrals/integral30.hh"*/


/*DO NOT FORGET TO CHANGE THE INITIALIZATION PART LATER IN THE FILE*/
#include"../beamcalc/integrals/integral1names.hh"
#include"../beamcalc/integrals/integral2names.hh"
#include"../beamcalc/integrals/integral3names.hh"
#include"../beamcalc/integrals/integral4names.hh"/*
#include"../beamcalc/integrals/integral5names.hh"
#include"../beamcalc/integrals/integral6names.hh"
#include"../beamcalc/integrals/integral7names.hh"
#include"../beamcalc/integrals/integral8names.hh"*/

/*
#include"../beamcalc/integrals/integral13names.hh"
#include"../beamcalc/integrals/integral26names.hh"
#include"../beamcalc/integrals/integral39names.hh"


#include"../beamcalc/integrals/integral7names.hh"
#include"../beamcalc/integrals/integral9names.hh"
#include"../beamcalc/integrals/integral20names.hh"
#include"../beamcalc/integrals/integral22names.hh"
#include"../beamcalc/integrals/integral33names.hh"
#include"../beamcalc/integrals/integral35names.hh"


#include"../beamcalc/integrals/integral11names.hh"
#include"../beamcalc/integrals/integral12names.hh"
#include"../beamcalc/integrals/integral24names.hh"
#include"../beamcalc/integrals/integral25names.hh"
#include"../beamcalc/integrals/integral37names.hh"
#include"../beamcalc/integrals/integral38names.hh"



#include"../beamcalc/integrals/integral8names.hh"
#include"../beamcalc/integrals/integral10names.hh"
#include"../beamcalc/integrals/integral21names.hh"
#include"../beamcalc/integrals/integral23names.hh"
#include"../beamcalc/integrals/integral34names.hh"
#include"../beamcalc/integrals/integral36names.hh"


#include"../beamcalc/integrals/integral5names.hh"
#include"../beamcalc/integrals/integral6names.hh"
#include"../beamcalc/integrals/integral18names.hh"
#include"../beamcalc/integrals/integral19names.hh"
#include"../beamcalc/integrals/integral31names.hh"
#include"../beamcalc/integrals/integral32names.hh"



#include"../beamcalc/integrals/integral1names.hh"
#include"../beamcalc/integrals/integral2names.hh"
#include"../beamcalc/integrals/integral3names.hh"
#include"../beamcalc/integrals/integral4names.hh"


#include"../beamcalc/integrals/integral14names.hh"
#include"../beamcalc/integrals/integral15names.hh"
#include"../beamcalc/integrals/integral16names.hh"
#include"../beamcalc/integrals/integral17names.hh"

#include"../beamcalc/integrals/integral27names.hh"
#include"../beamcalc/integrals/integral28names.hh"
#include"../beamcalc/integrals/integral29names.hh"

#include"../beamcalc/integrals/integral30names.hh"
*/


using namespace std;

int const maxndim=8;

extern "C" void avh_moncar_init_(int*);
extern "C" void avh_parni_init_(int*,int* ,int const*,int*,int*);
extern "C" void avh_parni_adapt_(int*,double*,double(*)[maxndim]);
extern "C" void avh_parni_collect_(int*,double*);
extern "C" void avh_moncar_collect_(int*,double*);
extern "C" double avh_parni_density_(int*,double(*)[maxndim]);
extern "C" void avh_parni_plot_(int*,int const*,int*);
extern "C" void avh_parni_marg_(int*,int const*,int*);
extern "C" void avh_parni_result_(int*);
extern "C" void avh_moncar_result_(int*,double*,double*,double*,double*);
extern "C" void avh_moncar_batch_(int*,double*);
extern "C" void avh_parni_generate_(int*,double(*)[maxndim]);

 

std::map<std::string , funcptr> functions;
string funcname;

//I am fully aware this is highly inelegant...
static double Integrand(const int *ndim, const double xx[],
  const int *ncomp, double ff[], void *userdata) {

  double* par = reinterpret_cast<double*>(userdata);
if(*ndim==8){
return (functions[funcname])(xx[0],xx[1],xx[2],xx[3],xx[4],xx[5],xx[6],xx[7],par);
} else if(*ndim==7){
return (functions[funcname])(xx[0],xx[1],xx[2],xx[3],xx[4],xx[5],xx[6],1,par);
} else if(*ndim==6){
return (functions[funcname])(xx[0],xx[1],xx[2],xx[3],xx[4],xx[5],1,1,par);
} else if(*ndim==5){
return (functions[funcname])(xx[0],xx[1],xx[2],xx[3],xx[4],1,1,1,par);
} else if(*ndim==4){
return (functions[funcname])(xx[0],xx[1],xx[2],xx[3],1,1,1,1,par);
} else if(*ndim==3){
return (functions[funcname])(xx[0],xx[1],xx[2],1,1,1,1,1,par);
} else if(*ndim==2){
return (functions[funcname])(xx[0],xx[1],1,1,1,1,1,1,par);
} else{
return (functions[funcname])(xx[0],1,1,1,1,1,1,1,par);
}

};




long int nev;
int nopt,ianalyse,itask,nbat_s,nbatch,iev,nchmax,sampling,estimating;
int const nunit=21;
double wght,xx[maxndim],ave,sig,eff,rev,ww,avh_parni_density,cauchy_dnst;

int number;// Introduced simply because I don't know how to pass a number to fortran.
double auxname;

int main(int argc,char* argv[]){
	InputDat inputdat(argv[1]);
	funcname=inputdat.value<string>("funcname");
	string outfile=inputdat.value<string>("outfile");
	const int ndim   = inputdat.value<int>("ndim");
	const int ncomp = 1;

	//Initialize	
	//fill_map();
 // Intitialization
fill_map_integral1();
fill_map_integral2();
fill_map_integral3();
fill_map_integral4();/*
fill_map_integral5();
fill_map_integral6();
fill_map_integral7();
fill_map_integral8();*/

/*
fill_map_integral13();
fill_map_integral26();
fill_map_integral39();


fill_map_integral7();
fill_map_integral9();
fill_map_integral20();
fill_map_integral22();
fill_map_integral33();
fill_map_integral35();


fill_map_integral11();
fill_map_integral12();
fill_map_integral24();
fill_map_integral25();
fill_map_integral37();
fill_map_integral38();



fill_map_integral8();
fill_map_integral10();
fill_map_integral21();
fill_map_integral23();
fill_map_integral34();
fill_map_integral36();


fill_map_integral5();
fill_map_integral6();
fill_map_integral18();
fill_map_integral19();
fill_map_integral31();
fill_map_integral32();




fill_map_integral1();
fill_map_integral2();
fill_map_integral3();
fill_map_integral4();


fill_map_integral14();
fill_map_integral15();
fill_map_integral16();
fill_map_integral17();

fill_map_integral27();
fill_map_integral28();
fill_map_integral29();

fill_map_integral30();*/


/*inline void fill_map() {
 functions["int25s15t1m1m1"]=int25s15t1m1m1;
};*/
	
        //nev=inputdat.value<long long int>("maxeval");
        //nopt = inputdat.value<long long int>("nstart");
	nev=2e+6;
        nopt = 1e+5;
	
	//can't pass number
	number=1; //all
	avh_moncar_init_(&number);
	number=2;//again can't pass nuber
	avh_moncar_init_(&number);//after optimisation

	//Initialize parni for integration task
	sampling = 1; //label for parni
	itask = 2;//for integration, the options are 1,2,3,11,12,13
	nbat_s =(int)pow((double)nopt,0.5);
	nchmax=1000;//number of channels, 0 is for indefinite.
	avh_parni_init_(&sampling,&itask,&ndim,&nbat_s,&nchmax);
	
        double epsrel=1e-3;
        double epsabs=1e-1;
        
	
	// Start MC-Loop
	iev=1;
	std::cout<< funcname <<"  "<<ndim<<std::endl;
	while(iev<=nev && ((abs(sig/ave)>epsrel && sig>epsabs) || rev<nopt)){//stop the loop when the relative accuracy reaches 0.1%
	//while(iev<=nev ){
	//for(int iev=1;iev<=nev;++iev){
	       //generate x
	       avh_parni_generate_(&sampling,&xx);
	       //calculate the integrand
               double ff[ncomp];
	       wght=Integrand(&ndim, xx,&ncomp,ff,nullptr);
	       //wght=Integrand(&ndim, xx,&ncomp,&ff,nullptr);
               //wght= (*ff)[1];
	       //calculate the MC-weight
	       wght=wght/avh_parni_density_(&sampling,&xx);
	       //adapt PARNI, only during the first nopt events
	       if(iev<=nopt){avh_parni_adapt_(&sampling,&wght,&xx);};
	       //collection of events ater optimiztion
	       number=2;//can't pass number
	       if(iev>nopt){avh_moncar_collect_(&number,&wght);};
	       //collection of events including optimization
	       number=1;//same here
	       avh_moncar_collect_(&number,&wght);
	       //during optimizaton, batches of events contribute to the final result with weight wght
	       if(iev<=nopt && iev==(iev/nbat_s*nbat_s)){
		       wght=pow((((double)iev)/((double)nopt)),(double)1.0);
		       wght=min((double)1.0,wght);
		       number=1;//again, can't pass number
		       avh_moncar_batch_(&number,&wght);      
	       };
	       if((int)(iev%((int)(nopt)))==0&&iev!=0){//every nopt evaluations, print how it's progressing
	       number=2;
	       avh_moncar_result_(&number,&ave,&sig,&eff,&rev);
	       std::cout<<ave<<" +/- "<< sig<< " "<<rev<< " optimized  evaluations. "<<iev <<" loops." <<std::endl;};
	       
	       ++iev;
	       
	};//end of MC
        
	//results of integration
	//Include the final batch of events for the result including optimization, and return this results.
	
        auxname=(double)((nev-nopt)/(nbat_s)); 
	number=1;
	avh_moncar_batch_(&number,&auxname);
	avh_moncar_result_(&number,&ave,&sig,&eff,&rev);

	cout<<funcname<<endl;
	cout<<ave<<" +/- "<< sig<< " "<<rev<<" "<<eff<<endl;
	
	ofstream result(outfile.c_str());
	result << "# " << "Parni" << endl;
	result.precision(10);
	result<<"All "<<endl;
	result << "Nevents = "  << rev << endl;
	result << "efficiency = "   << (double) eff << endl;
	result << "result0 = " << (double) ave
         << " +/- "     << (double) sig << endl;


	number=2;
	avh_moncar_result_(&number,&ave,&sig,&eff,&rev);
	std::cout<<"optimized:" <<std::endl;
        std::cout<< funcname <<"=  "<<ave<<" +/- "<< sig<< " "<<rev<<" "<<eff<<std::endl;

	avh_parni_result_(&sampling);//optimization diagnostics

	

	result << "Optimized" << endl;
	result.precision(10);
	result << "Nevents = "  << rev << endl;
	result << "efficiency = "   << (double) eff << endl;
	result << "result = " << (double) ave
         << " +/- "     << (double) sig << endl;
        result << funcname << "_error " << (double) sig << endl;
	result << funcname << " " << (double) ave << endl;
	result.close();


	return 0;
}


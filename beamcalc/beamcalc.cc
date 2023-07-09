#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <iostream>
#include <string>
#include <limits>
#include <quadmath.h>
#include <vector>


#if PREC128
#include "/home/tomoki/Tools/Cuba-4.2-q/cubaq.h"
#else
#include "/home/tomoki/Tools/Cuba-4.2-l/cuba.h"
#endif

/* */
#include"integrals/integral13.hh"
#include"integrals/integral26.hh"
#include"integrals/integral39.hh"

/* */
#include"integrals/integral7.hh"
#include"integrals/integral9.hh"
#include"integrals/integral20.hh"
#include"integrals/integral22.hh"
#include"integrals/integral33.hh"
#include"integrals/integral35.hh"

/*  */
#include"integrals/integral11.hh"
#include"integrals/integral12.hh"
#include"integrals/integral24.hh"
#include"integrals/integral25.hh"
#include"integrals/integral37.hh"
#include"integrals/integral38.hh"


/*  */
#include"integrals/integral8.hh"
#include"integrals/integral10.hh"
#include"integrals/integral21.hh"
#include"integrals/integral23.hh"
#include"integrals/integral34.hh"
#include"integrals/integral36.hh"

/*   */
#include"integrals/integral5.hh"
#include"integrals/integral6.hh"
#include"integrals/integral18.hh"
#include"integrals/integral19.hh"
#include"integrals/integral31.hh"
#include"integrals/integral32.hh"


/*
#include"integrals/integral1.hh"
#include"integrals/integral2.hh"
#include"integrals/integral3.hh"
#include"integrals/integral4.hh"


#include"integrals/integral14.hh"
#include"integrals/integral15.hh"
#include"integrals/integral16.hh"
#include"integrals/integral17.hh"

#include"integrals/integral27.hh"
#include"integrals/integral28.hh"
#include"integrals/integral29.hh"

#include"integrals/integral30.hh"
*/










/*DO NOT FORGET TO CHANGE THE INITIALIZATION PART LATER IN THE FILE*/
/* */
#include"integrals/integral13names.hh"
#include"integrals/integral26names.hh"
#include"integrals/integral39names.hh"

/* */
#include"integrals/integral7names.hh"
#include"integrals/integral9names.hh"
#include"integrals/integral20names.hh"
#include"integrals/integral22names.hh"
#include"integrals/integral33names.hh"
#include"integrals/integral35names.hh"

/*  */
#include"integrals/integral11names.hh"
#include"integrals/integral12names.hh"
#include"integrals/integral24names.hh"
#include"integrals/integral25names.hh"
#include"integrals/integral37names.hh"
#include"integrals/integral38names.hh"


/*  */
#include"integrals/integral8names.hh"
#include"integrals/integral10names.hh"
#include"integrals/integral21names.hh"
#include"integrals/integral23names.hh"
#include"integrals/integral34names.hh"
#include"integrals/integral36names.hh"

/*   */
#include"integrals/integral5names.hh"
#include"integrals/integral6names.hh"
#include"integrals/integral18names.hh"
#include"integrals/integral19names.hh"
#include"integrals/integral31names.hh"
#include"integrals/integral32names.hh"


/*
#include"integrals/integral1names.hh"
#include"integrals/integral2names.hh"
#include"integrals/integral3names.hh"
#include"integrals/integral4names.hh"


#include"integrals/integral14names.hh"
#include"integrals/integral15names.hh"
#include"integrals/integral16names.hh"
#include"integrals/integral17names.hh"

#include"integrals/integral27names.hh"
#include"integrals/integral28names.hh"
#include"integrals/integral29names.hh"

#include"integrals/integral30names.hh"
*/



using namespace std;



//inline void fill_map() {
//  functions["int25s15t1m1m1"]=int25s15t1m1m1;
//  functions["int25s15t2m1m1"]=int25s15t2m1m1;
//  functions["int26s37t1m1m1"]=int26s37t1m1m1;
//  functions["int26s37t2m1m1"]=int26s37t2m1m1;
//  functions["int26s40t1m1m1"]=int26s40t1m1m1;
// functions["int26s40t2m1m1"]=int26s40t1m1m1;
// }


// global variables
std::map<std::string , funcptr> functions;
string funcname;



//-----------------------------------------------------------------------------
#ifdef PREC128
static int Integrand(const int *ndim, const __float128 xx[],
  const int *ncomp, __float128 ff[], void *userdata) {

  __float128* par = reinterpret_cast<__float128*>(userdata);

#else
static int Integrand(const int *ndim, const cubareal xx[],
  const int *ncomp, cubareal ff[], void *userdata) {

  double* par = reinterpret_cast<double*>(userdata);

#endif

#define f ff[0]
f=(functions[funcname])(xx[0],xx[1],xx[2],xx[3],xx[4],xx[5],xx[6],xx[7],par);


//std::cout<<"intts1t1m1m1 2 "<<(float)intts1t1m1m1(xx[0],xx[1],xx[2],xx[3],xx[4],xx[5],xx[6],xx[7], par)<<endl;

//std::cout << (double) x1 <<  " " << (double) x2 <<  " " << (double) x3 <<  " "
//       << (double) x4 <<  " " << (double) f << endl; 

return 0; 
}






//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
int main( int argc, char* argv[]) {

  //vector<double> l1  = l1int1sec1(0.0, 0.4, 0.0, 0.2, NULL);
  //vector<double> l2  = l2int1sec1(0.0, 0.4, 0.0, 0.2, NULL);
  //cout << l20int1sec1(0.2, 0.4, 0.3, 0.6, NULL) << endl;
  //cout << sp(l1,l2) << endl;
  //cout << spT(l1,l2) << endl;
  //cout << int1sec1cw0(0,0.4,0,0.2,NULL) << endl;
  //cout << int1sec1m2(0.3, 0.4, 0.3, 0.2, NULL) << endl;


  //---------------------------------------------------------------------------
  // input parameters
  //---------------------------------------------------------------------------
  InputDat inputdat(argv[1]);

  funcname = inputdat.value<string>("funcname");

  string outfile = inputdat.value<string>("outfile");


  string method     = inputdat.value<string>("method");
  const int ndim   = inputdat.value<int>("ndim");
  //#define ndim  inputdat.value<int>("ndim")

  #ifdef PREC128
  __float128 epsrel = 
             strtoflt128(inputdat.value<string>("epsrel").c_str(), nullptr);
  __float128 epsabs = 
             strtoflt128(inputdat.value<string>("epsabs").c_str(), nullptr);
  #else
  double epsrel = inputdat.value<double>("epsrel");
  double epsabs = inputdat.value<double>("epsabs");
  #endif

  long long int mineval   = inputdat.value<long long int>("mineval");
  long long int maxeval   = inputdat.value<long long int>("maxeval");
  long long int nstart    = inputdat.value<long long int>("nstart");
  long long int nincrease = inputdat.value<long long int>("nincrease");
  long long int nbatch    = inputdat.value<long long int>("nbatch");      

  int verbose = inputdat.value<int>("verbose");
  int gridno = inputdat.value<int>("gridno");

  int last = inputdat.value<int>("last");
  int seed = inputdat.value<int>("seed");

  #ifdef PREC128
  __float128 nnew   = strtoflt128(inputdat.value<string>("nnew").c_str(), nullptr);
  __float128 flatness = 
             strtoflt128(inputdat.value<string>("flatness").c_str(), nullptr);
  #else
  double nnew   = inputdat.value<double>("nnew");
  double flatness = inputdat.value<double>("flatness");
  #endif

  int nmin = inputdat.value<int>("nmin");

  int key = inputdat.value<int>("key");

  char* statefile = nullptr;
  void* userdata = nullptr;
  void* spin = nullptr;

  const long long int nvec = 1;
  const int ncomp = 1;


  //// Divonne-specific
  //int key1
  //int key2
  //int key3
  //int maxpass
  //__float128 border
  //__float128 maxchisq
  //__float128 mindeviation
  //long long int ngiven
  //int ldxgiven
  //long long int nextra


  int  fail = 1, nregions;
  long long int  neval;
  #ifdef PREC128
    __float128 integral[ncomp], error[ncomp], prob[ncomp];
  #else
    double integral[ncomp], error[ncomp], prob[ncomp];
  #endif


  //----------------------------------------------------------------------------
  // Intitialization
/* */
fill_map_integral13();
fill_map_integral26();
fill_map_integral39();

/* */
fill_map_integral7();
fill_map_integral9();
fill_map_integral20();
fill_map_integral22();
fill_map_integral33();
fill_map_integral35();

/*  */
fill_map_integral11();
fill_map_integral12();
fill_map_integral24();
fill_map_integral25();
fill_map_integral37();
fill_map_integral38();


/*  */
fill_map_integral8();
fill_map_integral10();
fill_map_integral21();
fill_map_integral23();
fill_map_integral34();
fill_map_integral36();

/*   */
fill_map_integral5();
fill_map_integral6();
fill_map_integral18();
fill_map_integral19();
fill_map_integral31();
fill_map_integral32();


/*
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

fill_map_integral30();
*/


  cout << funcname << endl;
  cout << ndim << endl;
  #if PREC128
    cout << "PRECISSION 128 " << method<<" "<< funcname<<endl;
  /* float epsrelp=epsrel;
   float epsabsp=epsabs;
   cout<<"ndim "<<ndim<<" ncomp "<<ncomp<<" nvec "<<nvec
       <<" epsrel "<< epsrelp<<" epsabs "<<epsabsp<<" verbose "<<verbose<<" seed "<<seed<<" mineval "<<mineval<<" maxeval "<<maxeval<<" nstart "<<nstart
       <<" nincrease "<<nincrease<<" nbatch "<<nbatch<<" gridno "<<gridno<<endl;*/

  #else
    cout << "PRECISSION 64 " << method<<" "<< funcname<<endl;
  #endif


//const int* ndimp=&ndim;
//__float128 xarr[8]={0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.9};
//__float128 farr[1]={0};
//const int* ncompp=&ncomp;


//cout<<ncompp<<ndimp<<endl;
//int outint=Integrand(ndimp,xarr,ncompp,farr,NULL);
//cout<<outint<<endl;

  //----------------------------------------------------------------------------
  // Integration 
  if (method == "Vegas") {
      llVegas(ndim, ncomp, Integrand, userdata, nvec,
              epsrel, epsabs, verbose, seed,
              mineval, maxeval, nstart, nincrease, 
              nbatch, gridno, statefile, spin,
              &neval, &fail, integral, error, prob);

  } else if (method == "Suave") {
      llSuave(ndim, ncomp, Integrand, userdata, nvec,
              epsrel, epsabs, verbose | last, seed,
              mineval, maxeval, nnew, nmin, flatness, statefile, spin,
              &nregions, &neval, &fail, integral, error, prob);

  } else if (method == "Cuhre") {
      llCuhre(ndim, ncomp, Integrand, nullptr, nvec,
              epsrel, epsabs, verbose,
              mineval, maxeval, key, nullptr, nullptr,
              &nregions, &neval, &fail, integral, error, prob);
      //llCuhre(ndim, ncomp, Integrand, userdata, nvec,
              //mineval, maxeval, key, statefile, spin,
  }



  //----------------------------------------------------------------------------
  // Saving results
  ofstream result(outfile.c_str());

  result << "# " << method << endl;
  result.precision(10);
  result << "neval = "  << neval << endl;
  result << "fail = "   << fail << endl;
  result << "prob = "   << (double) prob[0] << endl;
  result << "result = " << (double) integral[0]
         << " +/- "     << (double) error[0] << endl;
  result << funcname << " " << (double) integral[0] << endl;
  result.close();


  return 0;
}

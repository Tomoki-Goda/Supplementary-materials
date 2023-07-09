#ifndef __DEFS_HH__
#define __DEFS_HH__

#if PREC128
#include <quadmath.h>
#endif
#include <map>
#include <vector>
#include "InputDat.hh"

#if PREC128
  #define VARPREC __float128
#else
  #define VARPREC double
#endif

using namespace std;

#if PREC128
  #define Power(x,y) powq(x,y)
  #define Cos(x) cosq(x)
  #define Sqrt(x) sqrtq(x)
  #define ArcTanh(x) atanhq(x)
  #define Tan(x) tanq(x)
  #define Abs(x) absq(x)
#else
  #define Power(x,y) pow(x,y)
  #define Cos(x) cos(x)
  #define Sqrt(x) sqrt(x)
  #define ArcTanh(x) atanh(x)
  #define Tan(x) tan(x)
  #define Abs(x) abs(x)
#endif

#define Pi 3.1415926535897932385
#define E  2.7182818284590452354
#define EulerGamma 0.57721566490153286061
#define PolyGamma polygamma
#define Log(x) logarithm(x)
#define theta(x) HeavisideTheta(x)
#define EPS 1e-15


#if PREC128
typedef __float128 (*funcptr)(const __float128, const __float128,
                              const __float128, const __float128, 
                              const __float128, const __float128,
                              const __float128, const __float128, 
			      __float128*);
#else
typedef double (*funcptr)(const double x1, const double x2,
                          const double x3, const double x4, 
                          const double x6, const double x7,
                          const double x8, const double x9, 
			  double* par);
#endif

extern std::map<std::string , funcptr> functions;


#if PREC128
int HeavisideTheta(const __float128 arg) {
#else
int HeavisideTheta(const double arg) {
#endif

  if (arg>0) return 1;
  else return 0;
  
}

#if PREC128
__float128 polygamma(const __float128 x, const __float128 y) {
  const  __float128 eps = 1e-10;
#else
double polygamma(const double x, const double y) {
  const  double eps = 1e-10;
#endif


  if (x<eps && abs(y-0.5) < eps) {
    return -1.96351;
  } else if (abs(x-2)<eps && abs(y-0.5) < eps) {
    return -16.828796644234319996;
  } else if (abs(x-2)<eps && abs(y-1) < eps) {
    return -2.4041138063191885708;
  } else {
    cerr << "Error: problem in polygamma" << endl;
    exit(1);
  }
  
}

#if PREC128
__float128 logarithm(const __float128 x) {
  if (x>0) return logq(x);
#else
double logarithm(const double x) {
  if (x>0) return log(x);
#endif
  else return 0;
  
}

VARPREC sp(vector<VARPREC> v1, vector<VARPREC> v2) {
  VARPREC val = 
           v1[0]*v2[0] - v1[1]*v2[1] - v1[2]*v2[2] - v1[3]*v2[3] - v1[4]*v2[4];
  if(abs(val)<EPS) 
    return EPS;	  
  else
    return val;
}

VARPREC spT(vector<VARPREC> v1, vector<VARPREC> v2) {
  VARPREC val = v1[1]*v2[1] + v1[2]*v2[2] + v1[3]*v2[3];
  if(abs(val)<EPS) 
    return EPS;	  
  else
    return val;
}
vector<VARPREC> n1(VARPREC x1, VARPREC x2, VARPREC x3, VARPREC x4, 
                   VARPREC* par) { 
  vector<VARPREC> res = {1,0,0,0,1};
  return res;
}

vector<VARPREC> n2(VARPREC x1, VARPREC x2, VARPREC x3, VARPREC x4, 
                   VARPREC* par) {
  vector<VARPREC> res = {1,0,0,0,-1};
  return res;
}

#endif/*__DEFS_HH__ */

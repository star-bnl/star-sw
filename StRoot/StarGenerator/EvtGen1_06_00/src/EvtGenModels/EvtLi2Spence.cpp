
#include <math.h>

const double a1 = -0.250000000000000;
const double a2 = -0.111111111111111;
const double a3 = -0.010000000000000;
const double a4 = -0.017006802721088;
const double a5 = -0.019444444444444;
const double a6 = -0.020661157024793;
const double a7 = -0.021417300648069;
const double a8 = -0.021948866377231;
const double a9 = -0.022349233811171;
const double a10 = -0.022663689135191;
const double zeta2 = 1.644934066848226;

double li2spence(double x){

  double result = 0;
  double xr = x;
  double xi = 0;
  double r2 = xr * xr + xi * xi;

  double y, z, z2, sp;

  if ( r2 > 1. && (xr/r2) > 0.5 ) {

    y = (x - 1.0) / x;
    z = -log(1.0 - y);
    z2 = z*z;
    sp = z * ( 1.0 + a1 * z * ( 1.0 + a2 * z * ( 1.0 + a3 * z2 * 
         ( 1.0 + a4 * z2 * ( 1.0 + a5 * z2 * ( 1.0 + a6 * z2 * 
         ( 1.0 + a7 * z2 * ( 1.0 + a8 * z2 * ( 1.0 + a9 * z2 * 
         ( 1.0 + a10 * z2 ) ) ) ) ) ) ) ) ) ) + zeta2 - 
         log(x) * log( 1.0 - x ) + 0.5 * log(x) * log(x);
    result = sp;

  }
  else if ( r2 > 1 && (xr/r2) <= 0.5 ) {

    y = 1.0 / x;
    z = -log(1.0 - y);
    z2 = z * z;
    sp = -z * ( 1.0 + a1 * z * ( 1.0 + a2 * z * ( 1.0 + a3 * z2 * 
         ( 1.0 + a4 * z2 * ( 1.0 + a5 * z2 * ( 1.0 + a6 * z2 * 
         ( 1.0 + a7 * z2 * ( 1.0 + a8 * z2 * ( 1.0 + a9 * z2 * 
         ( 1.0 + a10 * z2) ) ) ) ) ) ) ) ) ) + zeta2 - 0.5*log(-x)*log(-x);
    result = sp;

  }
  else if ( r2 == 1 && xi == 0 ) {
   
    sp = zeta2;
    result = sp;
 
  }
  else if ( r2 <= 1 && xr > 0.5 ) {

    y = 1.0 - x;
    z = -log(1.0 - y);
    z2 = z * z;
    sp = -z * ( 1.0 + a1 * z * ( 1.0 + a2 * z * ( 1.0 + a3 * z2 * 
         ( 1.0 + a4 * z2 * ( 1.0 + a5 * z2 * ( 1.0 + a6 * z2 * 
         ( 1.0 + a7 * z2 * ( 1.0 + a8 * z2 * ( 1.0 + a9 * z2 * 
         ( 1.0 + a10 * z2) ) ) ) ) ) ) ) ) ) + zeta2 - log(x) * log(1.0 - x);
    result = sp;

  }
  else if ( r2 <= 1 && xr <= 0.5 ) {

    y = x;
    z = -log(1.0 - y);
    z2 = z * z;
    sp = z * ( 1.0 + a1 * z * ( 1.0 + a2 * z * ( 1.0 + a3 * z2 * 
         ( 1.0 + a4 * z2 * ( 1.0 + a5 * z2 * ( 1.0 + a6 * z2 * 
         ( 1.0 + a7 * z2 * ( 1.0 + a8 * z2 * ( 1.0 + a9 * z2 * 
         ( 1.0 + a10 * z2 ) ) ) ) ) ) ) ) ) );
    result = sp;

  }

  return result; 

}

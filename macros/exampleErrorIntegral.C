#include "TF1.h"
#include "TH1D.h"
#include <cmath>
#include <assert.h>
#include <iostream>

TF1 * fitFunc;  // fit funciton pointer (need to be globel since it is used by myFuncGradient )

const int NPAR = 2; // number of function parameters;

double f(double * x, double * p) { 
   // function used to fit the data
   return p[1]*std::sin( p[0] * x[0] ); 
}

double df_dPar(double * x, double * p) { 

   // derivative of the function w.r..t parameters

   // use analytical  derivatives 
//    double * par = fitFunc->GetParameters();
//    if (ipar == 0) 
//       return par[1]* x[0] * std::cos( par[0] * x[0] );
//    else if (ipar == 1) 
//       return std::sin( par[0] * x[0] );
//    else 
//       return 0; 
   

   // use calculated derivatives from TF1::GradientPar

   double grad[NPAR]; 
   // p is used to specify for which parameter the derivative is computed 
   int ipar = int(p[0] ); 
   assert (ipar >=0 && ipar < NPAR );

   assert(fitFunc);
   fitFunc->GradientPar(x, grad);


   return grad[ipar]; 
}


void exampleErrorIntegral() { 


   fitFunc = new TF1("f",f,0,1,NPAR); 
   TH1D * h1     = new TH1D("h1","h1",50,0,1); 

   double  par[NPAR] = { 3.14, 1.}; 
   fitFunc->SetParameters(par);

   for (int i = 0; i < 1000; ++i) { 
      double x = fitFunc->GetRandom();
      h1->Fill(x);
   }

   // vary a little the parameters
   fitFunc->SetParameter(0,3.);


   // fit the histogram 
   h1->Fit(fitFunc);

   h1->Draw();

   // calculate the integral 
   double integral = fitFunc->Integral(0,1);

   // calculate now the error (needs the derivatives of the function w..r.t the parameters)
   
   TF1 * deriv_par0 = new TF1("dfdp0",df_dPar,0,1,1);
   deriv_par0->SetParameter(0,0);


   TF1 * deriv_par1 = new TF1("dfdp1",df_dPar,0,1,1);
   deriv_par1->SetParameter(0,1.);


   double c0 = deriv_par0->Integral(0,1); 
   double c1 = deriv_par1->Integral(0,1); 

   double * epar = fitFunc->GetParErrors();

   double sigma_integral = std::sqrt( c0*c0 * epar[0]*epar[0] + c1*c1 * epar[1]*epar[1] ); 

   std::cout << "Integral = " << integral << " +/- " << sigma_integral << std::endl;
 
   double * p = fitFunc->GetParameters();
   double ic  = p[1]* (1-std::cos(p[0]) )/p[0];
   double c0c = p[1] * (std::cos(p[0]) + p[0]*std::sin(p[0]) -1.)/p[0]/p[0];
   double c1c = (1-std::cos(p[0]) )/p[0];

   double sic = std::sqrt( c0c*c0c * epar[0]*epar[0] + c1c*c1c * epar[1]*epar[1] ); 

   if ( std::fabs(sigma_integral-sic) > 1.E-6*sic ) 
      std::cout << " ERROR: test failed : different analytical  integral : " << ic << " +/- " << sic << std::endl;
   
}

int main() { 
   exampleErrorIntegral(); 
   return 0; 
}

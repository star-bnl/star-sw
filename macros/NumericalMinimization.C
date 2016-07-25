#include "Math/Functor.h"
#include "Math/GSLMinimizer1D.h"
#include "Riostream.h" 
double myfunc(double x ) { 
   return 1 + -4*x + 1*x*x; 
}
 
int NumericalMinimization()
{
   ROOT::Math::Functor1D func(&myfunc);
 
   // default (Brent)
   ROOT::Math::GSLMinimizer1D minBrent;
   minBrent.SetFunction(func,1,-10,10);
   minBrent.Minimize(100,0.01,0.01);
   cout << "Found minimum: x = " << minBrent.XMinimum() 
             << "  f(x) = " << minBrent.FValMinimum() << endl;
 
   // Golden Section
   ROOT::Math::GSLMinimizer1D minGold(ROOT::Math::Minim1D::kGOLDENSECTION);
   minGold.SetFunction(func,1,-10,10);
   minGold.Minimize(100,0.01,0.01);
   cout << "Found minimum: x = " << minGold.XMinimum() 
             << "  f(x) = " << minGold.FValMinimum() << endl;
 
   return 0;
}

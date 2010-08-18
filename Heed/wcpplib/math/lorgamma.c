#include <cmath>
#include "wcpplib/math/lorgamma.h"
#include "wcpplib/stream/prstream.h"
#include "wcpplib/util/FunNameStack.h"

double lorgamma_1(double betta)
{
  if(betta==0.0) return 0.0;
  if(betta>=1.0) 
  {
    mcout<<"double lorgamma_1(double betta): ERROR: betta>=1.0, betta="
	 <<betta<<"\n";
    spexit(mcerr);
  }
  betta*=betta;
  double g2_1=betta/(1-betta);
  //cout<<"g2_1="<<g2_1<<'\n';
  double gam=sqrt(g2_1+1);
  //cout<<"gam="<<gam<<'\n';
  return g2_1/(gam+1);
}

double lorbetta(double gamma_1)
{
  return sqrt(gamma_1 * (gamma_1+2)) / (gamma_1+1);
}

double lorbetta2(double gamma_1)
{
  double g=gamma_1+1;
  return (gamma_1 * (gamma_1+2)) / (g*g);
}

//double lorbetta(double momentum, double mass, double speed_of_light2)
//{
//  return momentum / sqrt(mass * mass * speed_of_light2 + momentum * momentum);
//}

double lorbetta(double momentum, double mass, double speed_of_light2)
{
  double x = (mass * mass * speed_of_light2) / (momentum * momentum);
  x = x + 1.0;
  x = 1.0 / x;
  x = sqrt( x );

  return x;
}

//double lorbetta_1(double momentum, double mass, double speed_of_light2)
//{
//  return momentum / sqrt(mass * mass * speed_of_light2 + momentum * momentum);
//}

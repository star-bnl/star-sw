/*********************************************************************
 * $Id: StRichOtherAlgorithms.cxx,v 2.0 2000/08/09 16:17:02 gans Exp $
 *
 *  Polia Distribution was written according to an algorithm
 *  proposed by Rob Veenhof, from CERN.
 *
 *  Gauss and Poisson come from the STAR Class Library and
 *  are initiated using the STAR HepJamesRandom engine.
 *  Flat comes from the C++ Standard Library.
 *
 * $Log: StRichOtherAlgorithms.cxx,v $
 * Revision 2.0  2000/08/09 16:17:02  gans
 * Readded Files That were not added in last CVS. Cosmetic Changes, naming convention
 * for StRichDrawableT(foo)
 *
 * Revision 1.2  2000/04/05 16:01:25  lasiuk
 * poisson(double) added
 *
 * Revision 1.1  2000/03/17 14:54:56  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 *********************************************************************/

#include "StRichOtherAlgorithms.h"

HepJamesRandom  Randoms::mEngine;
RandPoisson     Randoms::mPoisson(mEngine);
RandGauss       Randoms::mGauss(mEngine);
RandFlat        Randoms::mFlat(mEngine);

Randoms::Randoms()
{ /**/ }
   
Randoms::~Randoms()
{ /**/ }

int Randoms::Poisson(int n) const
{
    return mPoisson.shoot(n);
}

int Randoms::Poisson(double n) const
{
    return mPoisson.shoot(n);
}
  
double Randoms::Gauss(double mean, double std_dev) const
{ 
    return mGauss.shoot(mean,std_dev); 
}

int Randoms::Flat(int n) const
{
    return static_cast<int>(mFlat.shoot(n));
}

double Randoms::Flat(double width) const
{
    return mFlat.shoot(width);
}

double Randoms::Polia(double theta)
{
    float theta_plus_1 = theta + 1;   
    if (theta>-1) return (::rngama_(&theta_plus_1) / (theta_plus_1));
    else return 0;
}


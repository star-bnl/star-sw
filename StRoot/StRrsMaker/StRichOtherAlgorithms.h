/*******************************************OtherAlgorithms.h**\
 * $Id: StRichOtherAlgorithms.h,v 1.2 2000/01/25 22:02:21 lasiuk Exp $
 *  This file contains two small classes: Randoms encapsulates
 *  random number generation algorithms, such as poisson,
 *  gauss, flat and polia. MyRound rounds doubles to the nearest
 *  integer.   
 *
 *  Polia Distribution was written according to an algorithm
 *  proposed by Rob Veenhof, from CERN.
 *
 *  Gauss and Poisson come from the STAR Class Library and
 *  are initiated using the STAR HepJamesRandom engine.
 *  Flat comes from the C++ Standard Library.
 *
 * $Log: StRichOtherAlgorithms.h,v $
 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 * Revision 1.4  2000/03/13 18:03:37  lasiuk
 * nearest integer
 *
\**************************************************************/

 * Revision 1.2  2000/01/25 22:02:21  lasiuk
 * Second Revision
 *
 * Revision 1.5  2000/03/17 14:54:57  lasiuk
#ifndef ST_RICH_OTHER_ALGORITHMS_H
#define ST_RICH_OTHER_ALGORITHMS_H
 *********************************************************************/
#ifndef ST_RICH_RANDOM_H
#define ST_RICH_RANDOM_H

#include <time.h>
#include <iostream.h>
#include <stdlib.h>
#include <math.h>

// SCL
#include "Randomize.h"

    mGauss = new RandGauss( hjr );

  Randoms() { 
    HepJamesRandom hjr;

    mFlat    = new RandFlat(hjr);
    
    if ( !mPoisson ) cerr << "Error initializing SCL_RandPoisson!!!\n";
    if ( !mGauss ) cerr << "Error initializing SCL_RandGauss!!!\n";
    if ( !mFlat ) cerr << "Error initializing SCL_RandFlat!!!\n";
    
    srand( (unsigned)time(NULL) );
  }

  ~Randoms() {
    delete mPoisson;
    delete mGauss;
  }

  int Flat(int n) const { return ((int(rand())/RAND_MAX) * n); }
  double Flat(double n=1) const { return ((double(rand())/RAND_MAX) * n); }

  double Polia(double theta) {
    float theta_plus_1 = theta + 1;   
    if (theta>-1) return (::rngama_(&theta_plus_1) / (theta_plus_1));
    else return 0;
  }
    //double Flat(double n=1) const { return ((double(rand())/RAND_MAX) * n); }
    double Polia(double theta) {
  RandPoisson * mPoisson;
  RandGauss * mGauss;
    }
    double Flat(double width=1) const;

    RandPoisson* mPoisson;
    RandGauss*   mGauss;
    RandFlat*    mFlat;
};

struct MyRound {
  int operator()(double d) const {

    return static_cast<int>( ( ceil(a)-a > 0.5 ) ? floor(a) : ceil(a)); 
{
    return static_cast<int>( ( ceil(static_cast<double>(a))-a > 0.5 ) ?
			     floor(static_cast<double>(a)) : ceil(static_cast<double>(a)) ); 
}
#endif

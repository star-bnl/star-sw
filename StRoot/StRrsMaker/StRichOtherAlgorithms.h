/*********************************************************************
 * $Id: StRichOtherAlgorithms.h,v 1.6 2000/04/05 16:01:29 lasiuk Exp $
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
 * Revision 1.6  2000/04/05 16:01:29  lasiuk
 * poisson(double) added
 *
 * Revision 1.5  2000/03/17 14:54:57  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 *********************************************************************/
#ifndef ST_RICH_RANDOM_H
#define ST_RICH_RANDOM_H

#include <iostream.h>
#include <stdlib.h>
#include <math.h>

// SCL
#include "Randomize.h"

// CernLib
extern "C" float rngama_( float* );

class Randoms {
public:
    Randoms();
    ~Randoms();

    int Poisson(int n) const;
    int Poisson(double n) const;
    double Gauss(double mean =0, double std_dev =1) const;

    //int Flat(int n) const { return ((int(rand())/RAND_MAX) * n); }
    //double Flat(double n=1) const { return ((double(rand())/RAND_MAX) * n); }
    int Flat(int n) const;
    double Flat(double width=1) const;

    double Polia(double theta);

private:
    static HepJamesRandom mEngine;
    static RandPoisson    mPoisson;
    static RandGauss      mGauss;
    static RandFlat       mFlat;
};

template<class T>
inline int nearestInteger(T a)
{
    return static_cast<int>( ( ceil(static_cast<double>(a))-a > 0.5 ) ?
			     floor(static_cast<double>(a)) : ceil(static_cast<double>(a)) ); 
}
#endif

/***************************************************************************
 *
 * $Id: StRandom.hh,v 1.1 2000/03/16 16:29:14 ullrich Exp $
 *
 * Author: Thomas Ullrich, Mar 2000
 ***************************************************************************
 *
 * Description:  User friendly interface to CLHEP random generators.
 *
 ***************************************************************************
 *
 * $Log: StRandom.hh,v $
 * Revision 1.1  2000/03/16 16:29:14  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_RANDOM_HH
#define ST_RANDOM_HH

#include "Randomize.h"

class StRandom {
public:
    StRandom();
    ~StRandom();

    static void setSeed(long s);
    
    static double flat();
    static double flat(double w);
    static double flat(double a, double b);
    
    static long flatInt(long n);
    static long flatInt(long m, long n);

    static double exponential();
    static double exponential(double mean);

    static double gauss();
    static double gauss(double mean, double stdDev);

    static long   poisson(double mean);

    static double breitWigner(double a=1.0, double b=0.2);
    static double breitWigner(double a, double b, double c);
    static double breitWignerM2(double a=1.0, double b=0.2);
    static double breitWignerM2(double a, double b, double c);
    
private:
    static RanluxEngine    mEngine;
    static RandFlat        mFlat;
    static RandBreitWigner mBreitWigner;
    static RandExponential mExponential;
    static RandGauss       mGauss;
    static RandPoisson     mPoisson;
};

inline void StRandom::setSeed(long s) { mEngine.setSeed(s); }
inline double StRandom::flat() { return mFlat.shoot(); }
inline double StRandom::flat(double w) { return mFlat.shoot(w); }
inline double StRandom::flat(double a, double b) { return mFlat.shoot(a, b); }
inline  long StRandom::flatInt(long n) { return mFlat.shootInt(n); }
inline  long StRandom::flatInt(long m, long n) { return mFlat.shootInt(m,n); }
inline double StRandom::exponential() { return mExponential.shoot(); }
inline double StRandom::exponential(double mean) { return mExponential.shoot(mean); }
inline double StRandom::gauss() { return mGauss.shoot(0, 1); }
inline double StRandom::gauss(double mean, double stdDev) { return mGauss.shoot(mean, stdDev); }
inline long   StRandom::poisson(double mean) { return mPoisson.shoot(mean); }
inline  double StRandom::breitWigner(double a, double b) { return mBreitWigner.shoot(a, b); }
inline  double StRandom::breitWigner(double a, double b, double c) { return mBreitWigner.shoot(a, b, c); }
inline  double StRandom::breitWignerM2(double a, double b) { return mBreitWigner.shootM2(a, b); }
inline  double StRandom::breitWignerM2(double a, double b, double c) { return mBreitWigner.shootM2(a, b, c); }

#endif

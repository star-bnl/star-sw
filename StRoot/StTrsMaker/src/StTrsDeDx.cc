/*****************************************************************
 *
 * $Id: StTrsDeDx.cc,v 1.2 1999/01/15 10:57:45 lasiuk Exp $
 *
 * Author: brian Nov 20, 1997
 *
 *****************************************************************
 * Description:  calculates dE/dx using a given parmeterization
 *               based on CERN/NA49 work.  Cross checks in
 *               progress with GEANT, system test and P10 data
 *               He/C3H8 parameterization coming soon...
 *
 *****************************************************************
 *
 * $Log: StTrsDeDx.cc,v $
 * Revision 1.2  1999/01/15 10:57:45  lasiuk
 * exponential dist
 * unit check
 *
 * exponential dist
 * unit check
 *
 * Revision 1.1  1998/11/10 17:12:24  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.9  1998/11/08 17:30:01  lasiuk
 * allocators for SUN
 *
 * Revision 1.8  1998/11/02 22:48:21  lasiuk
 * attachment coefficient as data member
 *
 * Revision 1.7  1998/11/01 17:37:45  lasiuk
 * added diffusion coefficients (need them for transporter)
 *
 * Revision 1.6  1998/10/31 14:12:44  lasiuk
 * add mMeanFreePath data member for nextInteraction member function;
 * return energy in secondary member function;
 * use SystemOfUnits in default padLength;
 * name space considerations
 *
 * Revision 1.5  1998/10/22 14:56:30  lasiuk
 * Incorporate 1/E^n distribution for primaries
 *
 * Revision 1.4  1998/10/22 00:24:22  lasiuk
 * Oct 22
 *
 * Revision 1.3  1998/08/12 17:51:05  lasiuk
 * incorporate units; update Bethe-Bloch
 *
 * Revision 1.2  1998/08/10 15:06:22  lasiuk
 * random engine/distributions static
 *
 * Revision 1.1  1998/06/04 23:31:57  lasiuk
 * Initial Revision
 *
 ******************************************************************/
#ifndef ST_NO_EXCEPTIONS
#include <stdexcept>
#endif
#include <math.h>
// SCL
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#include "StTrsDeDx.hh"

HepJamesRandom  StTrsDeDx::mEngine;
RandFlat        StTrsDeDx::mFlatDistribution(mEngine);
  doInitialization();
    : mPadLength(pad),  mGas(gas)


    : mPadLength(pad)
{
    mGas = gas;
    if((gas != "Ne")  && (gas != "Ar") &&
	throw invalid_argument("Gas not currently Implemented.\nMust use either \"Ne\" or \"Ar\".");
#ifdef ST_USES_EXCEPTIONS
	cerr << gas << " gas not currently Implemented." << endl;
	throw invalid_argument("Gas not currently Implemented.\nMust use either \"Ne\" or \"Ar\" or \"P10\".");
#else
	cerr << gas.c_str() << " gas not currently Implemented." << endl;
	cerr << "Must use either: \"Ne\", \"Ar\", or \"P10\"." << endl;
	cerr << "Exitting..." << endl;
    }

void StTrsDeDx::doInitialization()
    if(gas == "Ne") {
    mKonstant = 0.1536*MeV*centimeter2/gram;
    
    if(mGas == "Ne") {
	mPairs    = 12.4/centimeter;     // in electrons/cm
	mIonize   = 21.6*eV;
	mW        = 36.6*eV;
	mExponent = 2.2;

	// ??? same as P10 currently  
	mSigmaTransverse   = 600*micrometer/sqrt(centimeter);
	mSigmaLongitudinal = 300*micrometer/sqrt(centimeter);
	
	mDensity  = 0.000839*gram/centimeter3;
    if(gas == "Ar") {
    }
    
    if(mGas == "Ar") {
	mPairs    = 28.0/centimeter;
	mIonize   = 16.6*eV;
	mW        = 28.6*eV;
	mExponent = 2.0;

	// ??? same as P10 currently  
	mSigmaTransverse   = 600*micrometer/sqrt(centimeter);
	mSigmaLongitudinal = 300*micrometer/sqrt(centimeter);
	
	mDensity  = 0.00166*gram/centimeter3;
    if((gas == "P10") || (gas == "p10")) {
    }

    if((mGas == "P10") || (mGas == "p10")) {
	mPairs    = 28.1/centimeter;
	mIonize   = 15.5*eV;
	mW        = 26.2*eV;
	mSigmaTransverse   = 600*micrometer/sqrt(centimeter);
	mSigmaLongitudinal = 300*micrometer/sqrt(centimeter);
	// ArCH4 (90:10) at 160 V/cm    From Alber et al. NIM (NA49)  
	mSigmaTransverse   = 633*micrometer/sqrt(centimeter);
	mSigmaLongitudinal = 370*micrometer/sqrt(centimeter);

	// For P10 (see writeup)
	mAttachment        = 10.2/(1.e-6*second*bar*bar);//5.167e-4 per microsecond 
	
	mDensity  = 0.001561*gram/centimeter3;
	mZa       =   .459;
    }

    ///////////////// TEMPORARY!!!!!!!!!!!!!
    mAttachment        = 10.2/(1.e-6*second*bar*bar);//5.167e-4 per microsecond 
    ////////////////////////////////////////////
    
    // Common to all
    mMeanFreePath = 1./mPairs;
    mEReduced     = 1 - mExponent;
    mEE           = pow(mIonize,mEReduced);
    mEndPoint     = 1000*mW;
    mAlfat        = mKonstant*gram/MeV/centimeter2*mZa*mPadLength/centimeter; 
}

StTrsDeDx::~StTrsDeDx() {/* nopt */}

void  StTrsDeDx::setPadLength(double len)
{
    mPadLength = len;
    mAlfat = mKonstant*gram/MeV/centimeter2*mZa*mPadLength/centimeter; 
}

double StTrsDeDx::nextInteraction() const
{
    // determines scaler distance of next interaction
    // according to mean free path
    return mExponentialDistribution.shoot(mMeanFreePath);
}

int StTrsDeDx::primary(double bg) const
{
    int numberOfPrimaries;
    if(bg == 1) {
	numberOfPrimaries =
	    mPoissonDistribution.shoot(mPairs*mPadLength);
    }
    else {
	numberOfPrimaries =
	    mPoissonDistribution.shoot(mPairs*mPadLength*betheBloch(bg));
    }
    return numberOfPrimaries;
}

int StTrsDeDx::secondary(double* primaryEnergy) const
{
    // Determined from the Energy distribution of the Primary Electrons
    // Generate a 1/E^n distribution where n is (mExponent)
    
    double energyDistribution =
	mFlatDistribution.shoot()*(mEE - pow(mEndPoint,mEReduced));

    *primaryEnergy =
	pow(mEE - energyDistribution,(1/mEReduced));
    
    double numberOfSecondaries = (*primaryEnergy - mIonize)/mW;

    // Keep a Physical Limit on the number of secondaries that is possible
    if(numberOfSecondaries > 1000)
	numberOfSecondaries = 1000;

    return (static_cast<int>(numberOfSecondaries));
}

#ifndef ST_NO_TEMPLATE_DEF_ARGS
void StTrsDeDx::electrons(vector<int>& sum, double bg) const
#else
    PR(StTrsDeDx::numberOfElectrons);
#endif
{
    PR(sum.size());
    if(sum.size() != StTrsDeDx::numberOfElectrons)
	sum.resize(StTrsDeDx::numberOfElectrons);
    //PR(sum.size());
    //copy(sum.begin(),sum.end()-1,ostream_iterator<int>(cout,","));
    // define number of primaries
    sum[StTrsDeDx::primaries] = this->primary(bg);
    
    //loop over primaries
    double primaryEnergy;
    sum[StTrsDeDx::secondaries] = 0;
    for(int ii=0; ii<sum[StTrsDeDx::primaries]; ii++) {
	sum[StTrsDeDx::secondaries] += this->secondary(&primaryEnergy);
    }
    
    sum[StTrsDeDx::total] =
	sum[StTrsDeDx::primaries] + sum[StTrsDeDx::secondaries];
}

double StTrsDeDx::betheBloch(double bg) const
{
    //
    // Plateau Parameterization
    // -- in general a function of the gas; and may have
    //    to be implemented as data members initialized
    //    in the constructor.
    
    // first corner
    double x0 = .426266;

    // plateau
    double x1 = 4.;

    // shape of rise
    double m = 2.5;

    double xa=log(1.649*mIonize/eV/(28.8*sqrt(mDensity*centimeter3/gram*mZa)));

    double f = 4.606*(xa-x0)/(pow((x1-x0),m));

    
    double bigx = log(bg)/log(10.);
    
    // Saturation term
    double d;
    if (bigx<x0)
	d=0;
    else if ((bigx>x0) && (bigx<x1))
	d=4.606*(bigx-xa)+f*(pow((x1-bigx),m));
    else if(bigx>x1)
	d=4.606*(bigx-xa);

    // Bethe-Bloch Parameterization:
    double k=.891;

    // Compute the minimum
    double bgMin = 3.;
    double te = k + 2*log(bgMin) - (sqr(bgMin)/(sqr(bgMin)+1)) - log(sqr(bgMin)/(sqr(bgMin)+1));
    double Io = mAlfat*((sqr(bgMin)+1)/sqr(bgMin))*(log((electron_mass_c2/MeV*mAlfat)/(sqr(mIonize)))+te);
      
    te = k + 2*log(bg) - (sqr(bg)/(sqr(bg)+1)) - log(sqr(bg)/(sqr(bg)+1)) - d;
    double I = mAlfat*((sqr(bg)+1)/sqr(bg))*(log((electron_mass_c2/MeV*mAlfat)/(sqr(mIonize)))+te);

    return (I/Io);
}

#if  defined(__sun) && ! defined(__GNUG__)
#else
    os << "==> " << mGas << endl;
#endif
    os << "mPairs     " << (mPairs*centimeter)     << " /cm" << endl;
    os << "mIonize    " << (mIonize/eV)            << " eV"  << endl;
    os << "mW         " << (mW/eV)                 << " eV"  << endl;
    os << "mEndPoint  " << (mEndPoint/keV)         << " keV" << endl;
    os << "mExponent  " << (mExponent)                       << endl;
    os << "mZa        " << mZa                               << endl;
    os << "--------------------------------------"           << endl;




/*****************************************************************
 *
 * $Id: StTrsDeDx.cc,v 1.18 2012/06/11 15:04:56 fisyak Exp $
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
 * Revision 1.18  2012/06/11 15:04:56  fisyak
 * std namespace
 *
 * Revision 1.17  2009/09/28 18:36:14  perev
 * Weird comparison fixed
 *
 * Revision 1.16  2004/05/03 23:31:12  perev
 * Possible non init WarnOff
 *
 * Revision 1.15  2003/09/02 17:59:19  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.14  2000/06/07 02:03:11  lasiuk
 * exit/abort ultimatum
 *
 * Revision 1.13  2000/01/10 23:14:30  lasiuk
 * Include MACROS for compatiblity with SUN CC5
 *
 * Revision 1.12  1999/10/22 00:00:13  calderon
 * -added macro to use Erf instead of erf if we have HP and Root together.
 * -constructor with char* for StTrsDedx so solaris doesn't complain
 * -remove mZeros from StTrsDigitalSector.  This causes several files to
 *  be modified to conform to the new data format, so only mData remains,
 *  access functions change and digitization procedure is different.
 *
 * Revision 1.11  1999/07/20 02:19:58  lasiuk
 * remove CVS merge conflicts
 *
 * Revision 1.10  1999/07/19 21:42:23  lasiuk
 * - add tss bethe-bloche parameterization for P10.  No saturation
 *   effects are included.
 *
 * Revision 1.9  1999/06/16 14:26:52  fisyak
 * Add flags for egcs on Solaris
 *
 * Revision 1.8  1999/04/07 00:51:46  lasiuk
 * refine diffusion coefficients for P10
 *
 * Revision 1.7  1999/03/17 17:11:30  lasiuk
 * comment out debug output
 *
 * Revision 1.6  1999/01/26 20:43:29  lasiuk
 * Jan 26
 *
 * Revision 1.5  1999/01/25 23:37:50  lasiuk
 * sun string
 *
 * Revision 1.4  1999/01/23 18:47:23  fisyak
 * Cleanup for SL98l
 *
 * Revision 1.3  1999/01/23 05:04:09  lasiuk
 * provide a default constructor
 *
 * Revision 1.2  1999/01/15 10:57:45  lasiuk
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
#include <string.h>
#ifndef ST_NO_EXCEPTIONS
#include <stdexcept>
#   if !defined(ST_NO_NAMESPACES)
        using std::invalid_argument;
#   endif
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
RandPoisson     StTrsDeDx::mPoissonDistribution(mEngine);
RandExponential StTrsDeDx::mExponentialDistribution(mEngine);

//static string trsDeDxDummy = "a";

StTrsDeDx::StTrsDeDx()
{
  mGas = "Ar";
  mPadLength = 1.95*centimeter;
  doInitialization();
}
StTrsDeDx::StTrsDeDx(const char* gas, double pad)
    : mPadLength(pad)
{
    mGas = gas;
//VP     if((gas != "Ne")  && (gas != "Ar") &&
//VP        (gas != "P10") && (gas != "p10")) {
    if (strcmp(gas, "Ne") && strcmp(gas,"Ar")
     && strcmp(gas,"P10") && strcmp(gas,"p10")) {
#ifdef ST_USES_EXCEPTIONS
	std::cerr << "oops" << endl;
	throw invalid_argument("Gas not currently Implemented.\nMust use either \"Ne\" or \"Ar\" or \"P10\".");
#else
	std::cerr << gas << " gas not currently Implemented." << endl;
	std::cerr << "Must use either: \"Ne\", \"Ar\", or \"P10\"." << endl;
	std::cerr << "Aborting..." << endl;
	abort();
#endif
    }
    doInitialization();
}
StTrsDeDx::StTrsDeDx(const string& gas, double pad)
    : mPadLength(pad)
{
    mGas = gas;
    if((gas != "Ne")  && (gas != "Ar") &&
       (gas != "P10") && (gas != "p10")) {
#ifdef ST_USES_EXCEPTIONS
	std::cerr << "oops" << endl;
	throw invalid_argument("Gas not currently Implemented.\nMust use either \"Ne\" or \"Ar\" or \"P10\".");
#else
	std::cerr << gas.c_str() << " gas not currently Implemented." << endl;
	std::cerr << "Must use either: \"Ne\", \"Ar\", or \"P10\"." << endl;
	std::cerr << "Aborting..." << endl;
	abort();
#endif
    }
    doInitialization();
}

void StTrsDeDx::doInitialization()
{
    mKonstant = 0.1536*MeV*centimeter2/gram;
    
    if(mGas == "Ne") {
	mPairs    = 12.4/centimeter;     // in electrons/cm
	mIonize   = 21.6*eV;
	mW        = 36.6*eV;
	mExponent = 2.2;

	// ??? same as P10 currently  
	mSigmaTransverse   = 600*micrometer/::sqrt(centimeter);
	mSigmaLongitudinal = 300*micrometer/::sqrt(centimeter);
	
	mDensity  = 0.000839*gram/centimeter3;
	mZa       =  .5;        // Z/A
    }
    
    if(mGas == "Ar") {
	mPairs    = 28.0/centimeter;
	mIonize   = 16.6*eV;
	mW        = 28.6*eV;
	mExponent = 2.0;

	// ??? same as P10 currently  
	mSigmaTransverse   = 600*micrometer/::sqrt(centimeter);
	mSigmaLongitudinal = 300*micrometer/::sqrt(centimeter);
	
	mDensity  = 0.00166*gram/centimeter3;
	mZa       =  .45;
    }

    if((mGas == "P10") || (mGas == "p10")) {
	mPairs    = 28.1/centimeter;
	mIonize   = 15.5*eV;
	mW        = 26.2*eV;
	mExponent = 2.0;

	// ArCH4 (90:10) at 160 V/cm    From Alber et al. NIM (NA49)  
	mSigmaTransverse   = 633*micrometer/::sqrt(centimeter);
	mSigmaLongitudinal = 370*micrometer/::sqrt(centimeter);

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
    mEE           = ::pow(mIonize,mEReduced);
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
	mFlatDistribution.shoot()*(mEE - ::pow(mEndPoint,mEReduced));

    *primaryEnergy =
	::pow(mEE - energyDistribution,(1/mEReduced));
    
    double numberOfSecondaries = (*primaryEnergy - mIonize)/mW;

    // Keep a Physical Limit on the number of secondaries that is possible
    if(numberOfSecondaries > 1000)
	numberOfSecondaries = 1000;

    return (static_cast<int>(numberOfSecondaries));
}

#ifndef ST_NO_TEMPLATE_DEF_ARGS
void StTrsDeDx::electrons(vector<int>& sum, double bg) const
#else
void StTrsDeDx::electrons(vector<int, allocator<int> >& sum, double bg) const    
#endif
{
    //PR(StTrsDeDx::numberOfElectrons);
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

    double xa=::log(1.649*mIonize/eV/(28.8*::sqrt(mDensity*centimeter3/gram*mZa)));

    double f = 4.606*(xa-x0)/(::pow((x1-x0),m));
    
    double bigx = ::log(bg)/::log(10.);
    
    // Saturation term
    double d=-999999.;
    if (bigx<x0)
	d=0;
    else if ((bigx>x0) && (bigx<x1))
	d=4.606*(bigx-xa)+f*(::pow((x1-bigx),m));
    else if(bigx>x1)
	d=4.606*(bigx-xa);

    // Bethe-Bloch Parameterization:
    double k=.891;

    // Compute the minimum
    double bgMin = 3.;
    double te = k + 2*::log(bgMin) - (sqr(bgMin)/(sqr(bgMin)+1)) - ::log(sqr(bgMin)/(sqr(bgMin)+1));
    double Io = mAlfat*((sqr(bgMin)+1)/sqr(bgMin))*(::log((electron_mass_c2/MeV*mAlfat)/(sqr(mIonize)))+te);
      
    te = k + 2*::log(bg) - (sqr(bg)/(sqr(bg)+1)) - ::log(sqr(bg)/(sqr(bg)+1)) - d;
    double I = mAlfat*((sqr(bg)+1)/sqr(bg))*(::log((electron_mass_c2/MeV*mAlfat)/(sqr(mIonize)))+te);

    return (I/Io);
}

double StTrsDeDx::betheBlochTSS(double p, double z, double m)
{
    //
    // Modified Bethe-Bloch formula, accounting for restricted energy loss.
    // Form taken from PDG.
    // The density effect correction term "delta" is not accounted for here.
    //  This should affect the relativistic rise.
    // p =  momentum (GeV/c)
    // z =  charge (e-)
    // m =  mass (GeV/c^2) of particle
    //float beta,gamma; //           ! relativistic velocity of particle

    double K = 0.3071*MeV/(gram/centimeter2);
    double me = 511.*keV;  // electron mass
    double emax = 50.*keV; // cut-off energy (usually some 10s of keV)

    //
    // Gas characteristics----------
    const int ncomp=2;
    double rhoP10  = 1.547e-3*gram/centimeter3; // P10 density
    double rhocomp[ncomp] = {1.66e-3*gram/centimeter3,
			    6.70e-4*gram/centimeter3}; // Ar. CH4 density
    double Acomp[ncomp]   = {39.95*gram/mole, 16.04*gram/mole};
    double Zcomp[ncomp]   = {18.0, 10.0}; //  A,Z of the two components in P10
    double I[ncomp]       = {188.0*eV, 41.7*eV}; //  Mean excitation energy for
                                                // components of P10
    double dedxComp[ncomp]; // dedx for this component

    double percentage[ncomp] = {90.0, 10.0};

    double energy = ::sqrt(p*p+m*m);
    double beta   = p/energy;
    double beta2  = beta*beta;
    double gamma  = energy/m;
    double z2     = z*z;

    double weight, prefactor, arg;
    for(int icomp=0; icomp<ncomp; icomp++) {
	weight =
	    (percentage[icomp]*Acomp[icomp]/(percentage[0]*Acomp[0]+percentage[1]*Acomp[1]));
	prefactor =
	    K*z2*Zcomp[icomp]/(Acomp[icomp]/(gram/mole)*beta2)*rhocomp[icomp]; 
     
	arg = beta*gamma*::sqrt(2.*me*emax)/I[icomp];
	dedxComp[icomp] =
	    (weight/rhocomp[icomp])*prefactor*(::log(arg)-beta2/2.);
    }
        
    return (rhoP10*(dedxComp[0] + dedxComp[1]));    
}

void StTrsDeDx::print(ostream& os) const
{
    os << "=========== StTrsDeDx ================"           << endl;
#if  defined(__sun) && ! defined(__GNUG__)
    os << "==> " << mGas.c_str() << endl;
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
    os << "mPadLength " << (mPadLength/centimeter) << " cm"  << endl;
    os << "======================================\n"         << endl;
}

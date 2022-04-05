/***************************************************************************
 *
 * $Id: StTrsSlowAnalogSignalGenerator.hh,v 1.7 2008/10/13 19:56:11 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrsSlowAnalogSignalGenerator.hh,v $
 * Revision 1.7  2008/10/13 19:56:11  fisyak
 * Account that Z-offset is sector dependent
 *
 * Revision 1.6  2003/09/02 17:59:16  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  1999/02/16 23:34:19  lasiuk
 * inline 2 functions
 * merge operations for speed up (after profiler0
 *
 * Revision 1.4  1999/01/18 21:01:42  lasiuk
 * use fractionSampled(); enumerated types for function selection
 *
 * Revision 1.3  1999/01/18 10:20:57  lasiuk
 * use integral to deposit total charge in time bin
 *
 * Revision 1.2  1998/11/16 14:49:43  lasiuk
 * add deltaResponse()
 *
 * Revision 1.1  1998/11/10 17:12:12  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.3  1998/11/04 18:47:18  lasiuk
 * signal sampler machinery
 *
 * Revision 1.2  1998/10/22 14:58:14  lasiuk
 * image charge returns double and uses PRF integral
 *
 * Revision 1.1  1998/06/30 22:54:09  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_SLOW_ANALOG_SIGNAL_GENERATOR_HH
#define ST_TRS_SLOW_ANALOG_SIGNAL_GENERATOR_HH

#include <Stiostream.h>
#include "StTrsAnalogSignalGenerator.hh"

class StTrsSlowAnalogSignalGenerator : public  StTrsAnalogSignalGenerator {
public:
    enum StDistribution {endo,
			 gatti,
			 dipole,
			 unknown};

    enum StSignal {delta,
		   symmetricGaussianApproximation,
		   symmetricGaussianExact,
		   asymmetricGaussianApproximation,
		   realShaper,
		   undefined};

public:
    ~StTrsSlowAnalogSignalGenerator();
    //StTrsSlowAnalogSignalGenerator(const StTrsSlowAnalogSignalGenerator&);
    //StTrsSlowAnalogSignalGenerator& operator=(const StTrsSlowAnalogSignalGenerator&);

    static StTrsAnalogSignalGenerator* instance();
    static StTrsAnalogSignalGenerator* instance(StTpcGeometry*, StTpcSlowControl*, StTpcElectronics*, StTrsSector*);

    // charge generation
    void   setChargeDistribution(StDistribution);
  void   inducedChargeOnPad(StTrsWireHistogram*,Int_t sector);
    inline double signalOnPad(double, double, double, double, double, double);

    // sampling
    void   setElectronicSampler(StSignal);
    void   sampleAnalogSignal();
    inline double signalSampler(double, StTrsAnalogSignal&);

private:
    // charge generation
    double imageChargeIntegral(double, double, double, double, double, double);
    double endoChargeIntegral(double, double, double, double, double, double);
    //double gattiChargeIntegral(double, double, double, double, double, double);

    // sampling
    double deltaResponse(double, StTrsAnalogSignal&);
    double symmetricGaussianApproximateResponse(double, StTrsAnalogSignal&);
    double symmetricGaussianExactResponse(double, StTrsAnalogSignal&);
    double asymmetricGaussianApproximateResponse(double, StTrsAnalogSignal&);
    //    double asymmetricGaussianResponseWithUnrestoredBaseline(double, StTrsAnalogSignal&);
    double realShaperResponse(double, StTrsAnalogSignal&);
    double oneOverT(double, double);

protected:
    //StTrsSlowAnalogSignalGenerator();
    StTrsSlowAnalogSignalGenerator(StTpcGeometry*, StTpcSlowControl*, StTpcElectronics*, StTrsSector*);

private:
    static StTrsAnalogSignalGenerator* mInstance;

    double         mDriftVelocity;
    double         mTimeBinWidth;
    double         mTau;
    double         mTau1;
    double         mTau2;
    double         mSymGausApproxFactor;
    double         mAsymGausApproxFactor;
    double         mAsymGausUnRestFactor;
    
    StDistribution mChargeDistribution;
    StSignal       mSampler;
};
inline double StTrsSlowAnalogSignalGenerator::signalSampler(double t, StTrsAnalogSignal& sig)
{
    //
    // This is where the function for the Signal Sampling is selected
    // Add a function that returns the amplitude of a signal at
    // a time 't' given the position in time and amplitude of all
    // the other signals (contained in the StTrsAnalogSignal 'sig'
    // -- symmetricGaussianResponse
    // -- asymmetricGaussianResponse
    // -- endoResponse

    if(mSampler == (StTrsSlowAnalogSignalGenerator::undefined)) {
	cerr << "ERROR: no function selected" << endl;
	// this would be a good place to throw an exception
	exit(0);
    }
    
    switch(mSampler)
	{
	case symmetricGaussianApproximation:
	    return symmetricGaussianApproximateResponse(t, sig);
	    break;
	case delta:
	    return deltaResponse(t, sig);
	    break;
	case symmetricGaussianExact:
	    return symmetricGaussianExactResponse(t, sig);
	    break;
	case asymmetricGaussianApproximation:
	    return asymmetricGaussianApproximateResponse(t, sig);
	    break;
	case realShaper:
	    return realShaperResponse(t,sig);
	    break;
	    //case StSignal::asymmetricGaussianResponseWithUnRestoredBaseline:
	    //return asymmetricGaussianResponseWithUnRestoredBaseline(t, sig);
	    //break;
	default:
	    cerr << "Default Function Selected. ERROR!" << endl;
	    exit(0);
	    break;
	}
}

inline double StTrsSlowAnalogSignalGenerator::signalOnPad(double xo, double yo, double xl, double xu, double yl, double yu)
{
//     cout << "StTrsSlowAnalogSignalGenerator::signalOnPad()" << endl;
     switch(mChargeDistribution)
	{
	case endo:
// 	    cout << "********************Endo" << endl;
	    return endoChargeIntegral(xo,yo,xl,xu,yl,yu);
	    break;
 	case gatti:
//  	    cout << "********************GATTI" << endl;
// 	    return gattiChargeIntegral(xo,yo,xl,xu,yl,yu);
	    cout << "Gatti Distribution Not Implemented Yet!" << endl;
	    exit(0);
 	    break;
	case dipole:
// 	    cout << "********************DIPOLE" << endl;
	    return imageChargeIntegral(xo,yo,xl,xu,yl,yu);
	    break;
	default:
	    cerr << "Default Function Selected. ERROR!" << endl;
	    exit(0);
	    break;
	}
}
#endif

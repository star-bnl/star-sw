/***************************************************************************
 *
 * $Id: StTrsSlowAnalogSignalGenerator.hh,v 1.3 1999/01/18 10:20:57 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StTrsSlowAnalogSignalGenerator.hh,v $
 * Revision 1.3  1999/01/18 10:20:57  lasiuk
 * use integral to deposit total charge in time bin
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

#include <iostream.h>
#include "StTrsAnalogSignalGenerator.hh"
		   undefined};

public:
    ~StTrsSlowAnalogSignalGenerator();
    //StTrsSlowAnalogSignalGenerator(const StTrsSlowAnalogSignalGenerator&);
    //StTrsSlowAnalogSignalGenerator& operator=(const StTrsSlowAnalogSignalGenerator&);

    static StTrsAnalogSignalGenerator* instance();

    // charge generation
    void   setChargeDistribution(StDistribution);
    void   inducedChargeOnPad(StTrsWireHistogram*);

    // sampling
    void   setElectronicSampler(StSignal);
    void   sampleAnalogSignal();
    double signalSampler(double, StTrsAnalogSignal&);

private:
    // charge generation
    double imageChargeIntegral(double, double, double, double, double, double);
    double endoChargeIntegral(double, double, double, double, double, double);
    //double gattiChargeIntegral(double, double, double, double, double, double);

    // sampling
    double deltaResponse(double, StTrsAnalogSignal&);
    double symmetricGaussianApproximateResponse(double, StTrsAnalogSignal&);
    double shaperResponse(double, StTrsAnalogSignal&);
    double asymmetricGaussianApproximateResponse(double, StTrsAnalogSignal&);
    
    double realShaperResponse(double, StTrsAnalogSignal&);
    double oneOverT(double, double);

protected:
    //StTrsSlowAnalogSignalGenerator();
    StTrsSlowAnalogSignalGenerator(StTpcGeometry*, StTpcSlowControl*, StTpcElectronics*, StTrsSector*);


    StDistribution mChargeDistribution;
    StSignal       mSampler;
};
#endif

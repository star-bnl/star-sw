/***************************************************************************
 *
 * $Id: StTrsFastDigitalSignalGenerator.hh,v 1.1 1998/11/10 17:12:11 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: parameterization of the digital signal response
 *
 ***************************************************************************
 *
 * $Log: StTrsFastDigitalSignalGenerator.hh,v $
 * Revision 1.1  1998/11/10 17:12:11  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.1  1998/11/10 17:12:11  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/04 18:51:17  lasiuk
 * initialization in base class
 * incorporate electronics db
 * sector by reference
 *
 * Revision 1.1  1998/06/30 22:54:09  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_FAST_DIGITAL_SIGNAL_GENERATOR_HH
#define ST_TRS_FAST_DIGITAL_SIGNAL_GENERATOR_HH

#include <iostream.h>
#include "StTrsDigitalSignalGenerator.hh"

class StTrsFastDigitalSignalGenerator : public StTrsDigitalSignalGenerator {
public:
    ~StTrsFastDigitalSignalGenerator();
    //StTrsFastDigitalSignalGenerator(const StTrsFastDigitalSignalGenerator&);
    //StTrsFastDigitalSignalGenerator& operator=(const StTrsFastDigitalSignalGenerator&);

    static StTrsDigitalSignalGenerator* instance();
    static StTrsDigitalSignalGenerator* instance(StTpcElectronics*, StTrsSector*);
    
    void digitizeSignal()    ;
    void addWhiteNoise()     ;
    void addCorrelatedNoise();
    
protected:
    StTrsFastDigitalSignalGenerator(StTpcElectronics*, StTrsSector*);


private:
    static StTrsDigitalSignalGenerator* mInstance;

    double     mSimpleConversion;
};
#endif

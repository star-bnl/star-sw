/***************************************************************************
 *
 * $Id: StTrsFastDigitalSignalGenerator.hh,v 1.3 1999/02/04 18:35:17 lasiuk Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: parameterization of the digital signal response
 *
 ***************************************************************************
 *
 * $Log: StTrsFastDigitalSignalGenerator.hh,v $
 * Revision 1.3  1999/02/04 18:35:17  lasiuk
 * digital sector removed from constructor;
 * fillSector() added in base class
 *
 * Revision 1.2  1999/01/18 10:25:12  lasiuk
 * add conversion code for StTrsDigitalSector
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

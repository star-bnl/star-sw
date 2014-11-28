/***************************************************************************
 *
 * $Id: StTrsFastDigitalSignalGenerator.hh,v 1.1 1999/10/01 
 *      17:15:00 HUi Long Exp $
 *
 * Author: Hui Long
 ***************************************************************************
 *
 * Description: Head file for the StTrsFastDigitalSignalGenerator.cc.
 *              
 *
 ***************************************************************************
 *
 * $Log : v1.1 Hui Long$
 *
 **************************************************************************/
#ifndef ST_TRS_PARAMETERIZED_DIGITAL_SIGNAL_GENERATOR_HH
#define ST_TRS_PARAMETERIZED_DIGITAL_SIGNAL_GENERATOR_HH
#include <Stiostream.h>
#include "StTrsDigitalSignalGenerator.hh"
#include "StDaqLib/TPC/trans_table.hh" 
class StTrsFastDigitalSignalGenerator : public StTrsDigitalSignalGenerator {
public:
    ~StTrsFastDigitalSignalGenerator();
    //StTrsFastDigitalSignalGenerator(const StTrsFastDigitalSignalGenerator&);
    //StTrsFastDigitalSignalGenerator& operator=(const StTrsFastDigitalSignalGenerator&);

    static StTrsDigitalSignalGenerator* instance(StTpcElectronics* el=0, StTrsSector* se=0,double simpleConv=0);
    
    void digitizeSignal()    ;
    void addWhiteNoise()     ;
    void addCorrelatedNoise();
private :  
    unsigned char do10to8Translation(int ) const;  
public:
    StTrsFastDigitalSignalGenerator(StTpcElectronics*, StTrsSector*,double simpleConversion);

private:
    static StTrsDigitalSignalGenerator* mInstance;

    double     mSimpleConversion;
};
#endif









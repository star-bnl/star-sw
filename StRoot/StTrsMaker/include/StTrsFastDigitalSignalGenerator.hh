/***************************************************************************
 *
 *  StTrsFastDigitalSignalGenerator.hh,v 1.1 1999/10/01 
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
#include <iostream.h>
#include "StTrsDigitalSignalGenerator.hh"
#include "StDaqLib/TPC/trans_table.hh"
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
private :  
    unsigned char do10to8Translation(int ) const;  
protected:
    StTrsFastDigitalSignalGenerator(StTpcElectronics*, StTrsSector*);

private:
    static StTrsDigitalSignalGenerator* mInstance;

    double     mSimpleConversion;
};
#endif

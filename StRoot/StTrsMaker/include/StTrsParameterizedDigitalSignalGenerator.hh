/***************************************************************************
 *
 *  StTrsParameterizedDigitalSignalGenerator.hh,v 1.1 1999/10/01 
 *      17:15:00 HUi Long Exp $
 *
 * Author: Hui Long
 ***************************************************************************
 *
 * Description: Head file for the StTrsParameterizedDigitalSignalGenerator.cc.
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
class StTrsParameterizedDigitalSignalGenerator : public StTrsDigitalSignalGenerator {
public:
    ~StTrsParameterizedDigitalSignalGenerator();
    //StTrsParameterizedDigitalSignalGenerator(const StTrsParameterizedDigitalSignalGenerator&);
    //StTrsParameterizedDigitalSignalGenerator& operator=(const StTrsParameterizedDigitalSignalGenerator&);

    static StTrsDigitalSignalGenerator* instance();
    static StTrsDigitalSignalGenerator* instance(StTpcElectronics*, StTrsSector*);
    
    void digitizeSignal()    ;
    void addWhiteNoise()     ;
    void addCorrelatedNoise();
private :  
    unsigned char do10to8Translation(int ) const;  
protected:
    StTrsParameterizedDigitalSignalGenerator(StTpcElectronics*, StTrsSector*);

private:
    static StTrsDigitalSignalGenerator* mInstance;

    double     mSimpleConversion;
};
#endif

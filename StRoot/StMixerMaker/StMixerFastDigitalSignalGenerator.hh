/***************************************************************************
 *
 *  StMixerFastDigitalSignalGenerator.hh,v 1.1 1999/10/01 
 *      17:15:00 HUi Long Exp $
 *
 * Author: Patricia Fachini - a copy from StTrsFastDigitalSignalGenerator.hh
 *
 ***************************************************************************
 *
 * Description: Head file for the StMixerFastDigitalSignalGenerator.cc.
 *              
 *
 ***************************************************************************
 *
 * $Log : v1.1 Hui Long$
 *
 **************************************************************************/
#ifndef ST_MIXER_PARAMETERIZED_DIGITAL_SIGNAL_GENERATOR_HH
#define ST_MIXER_PARAMETERIZED_DIGITAL_SIGNAL_GENERATOR_HH
#include <iostream.h>
#include "StTrsMaker/include/StTrsDigitalSignalGenerator.hh"
//#include "StDaqLib/TPC/trans_table.hh"
class StMixerFastDigitalSignalGenerator : public StTrsDigitalSignalGenerator {
public:
    ~StMixerFastDigitalSignalGenerator();
    //StMixerFastDigitalSignalGenerator(const StMixerFastDigitalSignalGenerator&);
    //StMixerFastDigitalSignalGenerator& operator=(const StMixerFastDigitalSignalGenerator&);

    static StTrsDigitalSignalGenerator* instance();
    static StTrsDigitalSignalGenerator* instance(StTpcElectronics*, StTrsSector*);
    
    void digitizeSignal()    ;
    void addWhiteNoise()     ;
    void addCorrelatedNoise();
private :  
    //unsigned char do10to8Translation(int ) const;  
protected:
    StMixerFastDigitalSignalGenerator(StTpcElectronics*, StTrsSector*);
private:
    static StTrsDigitalSignalGenerator* mInstance;
};
#endif

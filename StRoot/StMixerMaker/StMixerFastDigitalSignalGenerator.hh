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
#include "StMixerDigitalSignalGenerator.hh"
//#include "StDaqLib/TPC/trans_table.hh"
class StMixerFastDigitalSignalGenerator : public StMixerDigitalSignalGenerator {
public:
    ~StMixerFastDigitalSignalGenerator();
    //StMixerFastDigitalSignalGenerator(const StMixerFastDigitalSignalGenerator&);
    //StMixerFastDigitalSignalGenerator& operator=(const StMixerFastDigitalSignalGenerator&);

    static StMixerDigitalSignalGenerator* instance();
    static StMixerDigitalSignalGenerator* instance(StTpcElectronics*, StMixerSector*);
    
    void digitizeSignal()    ;
private :  
    //unsigned char do10to8Translation(int ) const;  
protected:
    StMixerFastDigitalSignalGenerator(StTpcElectronics*, StMixerSector*);

private:
    static StMixerDigitalSignalGenerator* mInstance;
};
#endif

/***************************************************************************
 *
 * $Id: StMixerDigitalSignalGenerator.hh,v 1.1 2000/02/16 21:02:08 pfachini Exp $
 *
 * Author: Patricia Fachini - a copy from  StTrsDigitalSignalGenerator.hh
 *
 ***************************************************************************
 *
 * Description: Abstract class to define functionality of digital response 
 *
 ***************************************************************************
 *
 **************************************************************************/
#ifndef ST_MIXER_DIGITAL_SIGNAL_GENERATOR_HH
#define ST_MIXER_DIGITAL_SIGNAL_GENERATOR_HH
#include <vector>

#include "StMixerAnalogSignal.hh"
#include "StTrsMaker/include/StTpcElectronics.hh"
#include "StMixerSector.hh"
#include "StMixerDigitalSector.hh"

class StMixerDigitalSignalGenerator {
public:
    virtual ~StMixerDigitalSignalGenerator();

    virtual void digitizeSignal()     = 0;

    void         fillSector(StMixerDigitalSector*);
    
protected:
    //StMixerDigitalSignalGenerator();
    StMixerDigitalSignalGenerator(StTpcElectronics*, StMixerSector*);
	
protected:
    unsigned int        mNumberOfTimeBins;
    
    StTpcElectronics*     mElectronicsDb;
    StMixerSector*        mSector;
    StMixerSector*        mSectorRaw;
    StMixerSector*        mSectorTrs;
    StMixerDigitalSector* mDigitalSector;

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StMixerAnalogSignal>::iterator mTimeSequenceIteratorRaw;
    vector<StMixerAnalogSignal>::iterator mTimeSequenceIteratorTrs;
    vector<StMixerAnalogSignal>::iterator mTimeSequenceIterator;
#else
    vector<StMixerAnalogSignal, allocator<StMixerAnalogSignal> >::iterator mTimeSequenceIteratorRaw;
    vector<StMixerAnalogSignal, allocator<StMixerAnalogSignal> >::iterator mTimeSequenceIteratorTrs;
    vector<StMixerAnalogSignal, allocator<StMixerAnalogSignal> >::iterator mTimeSequenceIterator;
#endif
};

#endif

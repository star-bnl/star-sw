/***************************************************************************
 *
 * $Id: StMixerEmbedding.hh,v 1.2 2000/02/22 20:25:05 pfachini Exp $
 *
 * Author: Patricia Fachini
 *
 ***************************************************************************
 *
 * Description: Head file for the StMixerEmbedding.cc.
 *              
 *
 ***************************************************************************
 *
 * $Log : v1.1 Hui Long$
 *
 **************************************************************************/
#ifndef ST_MIXER_EMBEDDING_HH
#define ST_MIXER_EMBEDDING_HH
#include <vector>

#include "StMixerSector.hh"
#include "StMixerAnalogSignal.hh"
#include "StTrsMaker/include/StTpcElectronics.hh"
#include "StMixerDigitalSector.hh"

class StMixerEmbedding {
public:
    ~StMixerEmbedding();

    static StMixerEmbedding* instance();
    static StMixerEmbedding* instance(StTpcElectronics*, StMixerSector*, StMixerSector*, StMixerSector*);

    void doEmbedding();
protected:
    StMixerEmbedding(StTpcElectronics*, StMixerSector*, StMixerSector*, StMixerSector*);

#define mTimeBins 512
    unsigned int        mNumberOfTimeBins;
    float               conversion1,conversion2,accum[mTimeBins];
    int                 bin1,bin2;  

    StTpcElectronics*     mElectronicsDb;
    StMixerSector*        mSector;
    StMixerSector*        mSector1;
    StMixerSector*        mSector2;
    StMixerDigitalSector* mDigitalSector;

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StMixerAnalogSignal>::iterator mTimeSequenceIterator1;
    vector<StMixerAnalogSignal>::iterator mTimeSequenceIterator2;
    vector<StMixerAnalogSignal>::iterator mTimeSequenceIterator;
#else
    vector<StMixerAnalogSignal, allocator<StMixerAnalogSignal> >::iterator mTimeSequenceIterator1;
    vector<StMixerAnalogSignal, allocator<StMixerAnalogSignal> >::iterator mTimeSequenceIterator2;
    vector<StMixerAnalogSignal, allocator<StMixerAnalogSignal> >::iterator mTimeSequenceIterator;
#endif
private:
    static StMixerEmbedding* mInstance;
};

#endif

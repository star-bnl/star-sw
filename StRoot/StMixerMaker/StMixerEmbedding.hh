/***************************************************************************
 *
 * $Id: StMixerEmbedding.hh,v 1.3 2000/03/15 17:26:53 pfachini Exp $
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

#include "StTrsMaker/include/StTrsSector.hh"
#include "StTrsMaker/include/StTrsAnalogSignal.hh"
#include "StTrsMaker/include/StTpcElectronics.hh"
#include "StTrsMaker/include/StTrsDigitalSector.hh"

class StMixerEmbedding {
public:
    ~StMixerEmbedding();

    static StMixerEmbedding* instance();
    static StMixerEmbedding* instance(StTpcElectronics*, StTrsSector*, StTrsSector*, StTrsSector*);

    void doEmbedding();
protected:
    StMixerEmbedding(StTpcElectronics*, StTrsSector*, StTrsSector*, StTrsSector*);

#define mTimeBins 512
    unsigned int        mNumberOfTimeBins;
    float               conversion1,conversion2,accum[mTimeBins];
    int                 bin1,bin2;  

    StTpcElectronics*     mElectronicsDb;
    StTrsSector*        mSector;
    StTrsSector*        mSector1;
    StTrsSector*        mSector2;
    StTrsDigitalSector* mDigitalSector;

#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<StTrsAnalogSignal>::iterator mTimeSequenceIterator1;
    vector<StTrsAnalogSignal>::iterator mTimeSequenceIterator2;
    vector<StTrsAnalogSignal>::iterator mTimeSequenceIterator;
#else
    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> >::iterator mTimeSequenceIterator1;
    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> >::iterator mTimeSequenceIterator2;
    vector<StTrsAnalogSignal, allocator<StTrsAnalogSignal> >::iterator mTimeSequenceIterator;
#endif
private:
    static StMixerEmbedding* mInstance;
};

#endif

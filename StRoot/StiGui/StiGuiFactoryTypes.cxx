//StiGuiFactoryTypes.cxx
//M.L. Miller (Yale Software)
//09/01

//StiGui
#include "StiRootDrawableDetector.h"
#include "StiRootDrawableStiEvaluableTrack.h"
#include "StiGuiFactoryTypes.h"

//StiRDEvaluableTrack Factory

StiRDEvaluableTrackFactory::StiRDEvaluableTrackFactory(const string& newName,int original, int incremental, int maxInc)
    : StiObjectFactoryInterface<StiKalmanTrack>(newName, original, incremental, maxInc)
{
    initialize();
}

StiRDEvaluableTrackFactory::~StiRDEvaluableTrackFactory()
{
    // cout <<"StiRDEvaluableTrackFactory::~StiRDEvaluableTrackFactory()"<<endl;
}

void* StiRDEvaluableTrackFactory::makeNewObject() const
{
    return new StiRootDrawableStiEvaluableTrack();
}


//StiRDDetector Factory

StiRDDetectorFactory::StiRDDetectorFactory(const string& newName, int original, int incremental, int maxInc)
    : StiObjectFactoryInterface<StiDetector>(newName, original, incremental, maxInc)
{
    initialize();
}

StiRDDetectorFactory::~StiRDDetectorFactory()
{
}

void* StiRDDetectorFactory::makeNewObject() const
{
    return new StiRootDrawableDetector();
}


//StiGuiFactoryTypes.cxx
//M.L. Miller (Yale Software)
//09/01

//StiGui
#include "StiRootDrawableDetector.h"
#include "StiRootDrawableStiEvaluableTrack.h"
#include "StiRootDrawableKalmanTrack.h"
#include "StiGuiFactoryTypes.h"

//StiRDEvaluableTrack Factory

StiRDEvaluableTrackFactory::StiRDEvaluableTrackFactory(const string& newName,int original,
						       int incremental, int maxInc)
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

//StiRDKalmanTrack Factory

StiRDKalmanTrackFactory::StiRDKalmanTrackFactory(const string& newName,int original,
						 int incremental, int maxInc)
    : StiObjectFactoryInterface<StiKalmanTrack>(newName, original, incremental, maxInc)
{
    initialize();
}

StiRDKalmanTrackFactory::~StiRDKalmanTrackFactory()
{
    // cout <<"StiRDKalmanTrackFactory::~StiRDKalmanTrackFactory()"<<endl;
}

void* StiRDKalmanTrackFactory::makeNewObject() const
{
    return new StiRootDrawableKalmanTrack();
}


//StiRDDetector Factory

StiRDDetectorFactory::StiRDDetectorFactory(const string& newName, int original, int incremental, int maxInc)
    : StiObjectFactoryInterface<StiDetector>(newName, original, incremental, maxInc)
{
	cout << " StiRDDetectorFactory::StiRDDetectorFactory() executing" << endl;
	initialize();
}

StiRDDetectorFactory::~StiRDDetectorFactory()
{
}

void* StiRDDetectorFactory::makeNewObject() const
{
	//cout << "StiRDDetectorFactory::makeNewObject() called - making StiRootDrawableDetector()" << endl;
	return new StiRootDrawableDetector();
}


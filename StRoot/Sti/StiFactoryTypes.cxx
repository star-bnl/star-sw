//StiFactoryTypes.cxx
//M.L. Miller (Yale Software)
//09/01

#include <iostream>
using std::endl;
using std::cout;

#include <string>
using std::string;

//Sti
#include "StiHit.h"
#include "StiDetector.h"
#include "StiKalmanTrack.h"
#include "StiCompositeTreeNode.h"
#include "StiEvaluableTrack.h"
#include "StiKalmanTrackNode.h"

#include "StiFactoryTypes.h"

//StiHit Factory

StiHitFactory::StiHitFactory(const string& newName,int original, int incremental, int maxInc)
    : StiObjectFactoryInterface<StiHit>(newName, original, incremental, maxInc)
{
    initialize();
}

StiHitFactory::~StiHitFactory()
{
}

void* StiHitFactory::makeNewObject() const
{
    return new StiHit();
}

//StiKalmanTrack Factory

StiKalmanTrackFactory::StiKalmanTrackFactory(const string& newName,int original, int incremental, int maxInc)
    : StiObjectFactoryInterface<StiKalmanTrack>(newName, original, incremental, maxInc)
{
    initialize();
}

StiKalmanTrackFactory::~StiKalmanTrackFactory()
{
}

void* StiKalmanTrackFactory::makeNewObject() const
{
    return new StiKalmanTrack();
}

//StiEvaluableTrack Factory

StiEvaluableTrackFactory::StiEvaluableTrackFactory(const string& newName,int original, int incremental, int maxInc)
	: StiKalmanTrackFactory(newName, original, incremental,maxInc)
	//: StiObjectFactoryInterface<StiKalmanTrack>(newName, original, incremental,maxInc)
{
    initialize();
}

StiEvaluableTrackFactory::~StiEvaluableTrackFactory()
{
}

void* StiEvaluableTrackFactory::makeNewObject() const
{
    return new StiEvaluableTrack();
}

//StiDetector Factory

StiDetectorFactory::StiDetectorFactory(const string& newName,int original, int incremental, int maxInc)
    : StiObjectFactoryInterface<StiDetector>(newName, original, incremental, maxInc)
{
    initialize();
}

StiDetectorFactory::~StiDetectorFactory()
{
}

void* StiDetectorFactory::makeNewObject() const
{
	//cout << "StiDetectorFactory::makeNewObject() called - making StiDetector" << endl;
	return new StiDetector();
}

//StiKalmanTrackNode Factory

StiKalmanTrackNodeFactory::StiKalmanTrackNodeFactory(const string& newName,int original, int incremental, int maxInc)
    : StiObjectFactoryInterface<StiKalmanTrackNode>(newName, original,incremental, maxInc)
{
    initialize();
}

StiKalmanTrackNodeFactory::~StiKalmanTrackNodeFactory()
{
}

void* StiKalmanTrackNodeFactory::makeNewObject() const
{
    return new StiKalmanTrackNode();
}

//StiDetectorNode Factory
StiDetectorNodeFactory::StiDetectorNodeFactory(const string& newName,int original, int incremental, int maxInc)
    : StiObjectFactoryInterface<StiDetectorNode>(newName, original, incremental, maxInc)
{
    initialize();
}

StiDetectorNodeFactory::~StiDetectorNodeFactory()
{
}

void* StiDetectorNodeFactory::makeNewObject() const
{
    return new StiDetectorNode();
}


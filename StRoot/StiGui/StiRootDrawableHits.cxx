//StiRootDrawableHits.h
//M.L. Miller (Yale Software)
//07/01

//SCL
#include "StThreeVectorD.hh"

//StEvent
#include "StEventTypes.h"

//Sti
#include "Sti/StiHit.h"

//StiGui
#include "StiTPolyMarker3D.h"
#include "StiRootDrawableHits.h"

StiRootDrawableHits::StiRootDrawableHits() :
    mpoly( new StiTPolyMarker3D() ), marray(0), mcolor(4), mvisible(true) , mmarker(8), mPointsForRoot(0), mArraySize(1)
{
    marray = new double[0];
    //mpoly->SetPolyMarker(0, marray, mmarker);

    mpoly->SetMarkerStyle(mmarker);
    mpoly->SetMarkerSize(.5);
    mpoly->SetMarkerColor(mcolor);
    mpoly->ResetBit(kCanDelete);

    setName("Unnamed RootDrawableHits"); //Default name
    mPointsForRoot = new double[mArraySize];
}

StiRootDrawableHits::~StiRootDrawableHits()
{
    delete mPointsForRoot;
    mPointsForRoot=0;
}

void StiRootDrawableHits::setMarkerStyle(unsigned int val)
{
    mmarker = val;
    mpoly->SetMarkerStyle(mmarker);
}

void StiRootDrawableHits::setMarkerSize(double val)
{
    mpoly->SetMarkerSize(val);
}

void StiRootDrawableHits::fillHitsForDrawing()
{
    //mpoly->SetPolyMarker(0, marray, mmarker);
    mpoly->SetMarkerColor(mcolor);
    mpoly->ResetBit(kCanDelete);

    if (size()*3 > mArraySize) {
	cout <<"StiRootDrawableHits::fillHitsForDrawing() Reallocate Array"<<endl;
	//Deallocate array, increase to *at least* twice the size
	unsigned int maxSize = max(size()*3, 2*mArraySize); //Take bigger of two
	cout <<"Old Size:\t"<<mArraySize<<"\tN-Points:\t"<<3*size()<<"\tNew Size:\t"<<maxSize<<endl;
	mArraySize = maxSize;
	delete mPointsForRoot;
	mPointsForRoot = new double[mArraySize];
    }

    //Copy points to array
    int index=0;
    for (const_hit_vector::const_iterator it=begin(); it!=end(); ++it) {
	const StThreeVectorD& pos = (*it)->globalPosition();
	mPointsForRoot[index]=pos.x();
	mPointsForRoot[index+1]=pos.y();
	mPointsForRoot[index+2]=pos.z();
	index+=3;
    }
    
    mpoly->SetPolyMarker(size(), mPointsForRoot, mmarker);

    return;
}

void StiRootDrawableHits::draw()
{
    if (mvisible) {
	mpoly->Draw();
    }
}

void StiRootDrawableHits::update()
{
    return;
}

void StiRootDrawableHits::setColor(int val)
{
    mcolor=val;
    mpoly->SetMarkerColor(mcolor);
    return;
}

void StiRootDrawableHits::setVisibility(bool val)
{
    mvisible=val;
    return;
}


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
    mpoly( new StiTPolyMarker3D() ), mcolor(1), mvisible(true), mmarker(8)
{
    mpoly->SetMarkerStyle(mmarker);
    mpoly->SetMarkerSize(.5);
    mpoly->SetMarkerColor(mcolor);
    mpoly->ResetBit(kCanDelete);

    setName("Unnamed RootDrawableHits"); //Default name
}

StiRootDrawableHits::~StiRootDrawableHits()
{
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
    mpoly->SetMarkerColor(mcolor);
    mpoly->ResetBit(kCanDelete);

    mpoly->SetPolyMarker( size()/3, &(this->operator[](0)), mmarker);

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

void StiRootDrawableHits::add(double x, double y, double z)
{
  push_back(x);
  push_back(y);
  push_back(z);
}

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
    mpoly( new StiTPolyMarker3D() ), marray(0), mcolor(4), mvisible(true)
{
    marray = new double[0];

    mpoly->SetPolyMarker(0, marray, 8);

    mpoly->SetMarkerStyle(8);
    mpoly->SetMarkerSize(.5);
    mpoly->SetMarkerColor(mcolor);
    mpoly->ResetBit(kCanDelete);

    mname = "Unnamed RootDrawableHits";
}

StiRootDrawableHits::~StiRootDrawableHits()
{
}

void StiRootDrawableHits::fillHitsForDrawing()
{
    mpoly->SetPolyMarker(0, marray, 8);
    mpoly->SetMarkerColor(mcolor);
    mpoly->ResetBit(kCanDelete);

    for (const_hit_vector::const_iterator it=begin(); it!=end(); ++it) {
	const StThreeVectorD& pos = (*it)->stHit()->position();
	mpoly->SetNextPoint( pos.x(), pos.y(), pos.z() );
    }
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

const char* StiRootDrawableHits::name() const
{
    return mname;
}

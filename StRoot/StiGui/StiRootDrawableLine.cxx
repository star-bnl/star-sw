//StiRootDrawableLine.cxx
//M.L. Miller (Yale Software)
//07/01

//SCL
#include "StThreeVectorD.hh"

//StEvent
#include "StEventTypes.h"

//Sti
#include "Sti/StiHit.h"

//StiGui
#include "StiTPolyLine3D.h"
#include "StiRootDrawableLine.h"

StiRootDrawableLine::StiRootDrawableLine() :
    mline( new StiTPolyLine3D() ), marray(0), mcolor(2), mvisible(true)
{
    marray = new double[0];

    mline->SetPolyLine(0, marray);

    //mline->SetLineStyle(8);
    //mline->SetLineSize(.5);
    mline->SetLineColor(mcolor);
    mline->ResetBit(kCanDelete);

    setName("Unnamed RootDrawableLine");
}

StiRootDrawableLine::~StiRootDrawableLine()
{
}

void StiRootDrawableLine::setMarkerStyle(unsigned int val)
{
    return;
}

void StiRootDrawableLine::setMarkerSize(double val)
{
    //    mpoly->SetMarkerSize(10.);
}

void StiRootDrawableLine::fillHitsForDrawing()
{
    mline->SetPolyLine(0, marray);
    mline->SetLineColor(mcolor);
    mline->ResetBit(kCanDelete);

    for (const_hit_vector::const_iterator it=begin(); it!=end(); ++it) {
	const StThreeVectorD& pos = (*it)->stHit()->position();
	mline->SetNextPoint( pos.x(), pos.y(), pos.z() );
    }
    return;
}

void StiRootDrawableLine::draw()
{
    if (mvisible) {
	mline->Draw();
    }
}

void StiRootDrawableLine::update()
{
    return;
}

void StiRootDrawableLine::setColor(int val)
{
    mcolor=val;
    mline->SetLineColor(mcolor);
    return;
}

void StiRootDrawableLine::setVisibility(bool val)
{
    mvisible=val;
    return;
}


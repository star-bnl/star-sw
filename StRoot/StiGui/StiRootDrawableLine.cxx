//StiRootDrawableLine.cxx
//M.L. Miller (Yale Software)
//07/01

//SCL
#include "StThreeVectorD.hh"
#include "StThreeVector.hh"

//StEvent
#include "StEventTypes.h"

//Sti
#include "Sti/StiHit.h"

//StiGui
#include "StiTPolyLine3D.h"
#include "StiRootDrawableLine.h"

StiRootDrawableLine::StiRootDrawableLine() :
    mline( new StiTPolyLine3D() ), mcolor(2), mvisible(true)
{
    //mline->SetPolyLine(0, 0);

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
    //    mpoly->SetMarkerSize(val);
}

void StiRootDrawableLine::setLineWidth(double val)
{
    mline->SetLineStyle(static_cast<short>(val));
}

void StiRootDrawableLine::setLineStyle(unsigned int val)
{
    mline->SetLineStyle(val);
}

void StiRootDrawableLine::clearLine()
{
    mline->SetPolyLine(0);
    mline->ResetBit(kCanDelete);
}

void StiRootDrawableLine::fillHitsForDrawing()
{
    //mline->SetPolyLine(0);
    mline->SetLineColor(mcolor);
    mline->ResetBit(kCanDelete);

    mline->SetPolyLine(size()/3, &(this->operator[](0)));
    
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


//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 12 July 2007
//
// $Log: StPythiaEvent.cxx,v $
// Revision 1.1  2007/07/19 01:40:41  kocolosk
// use Pibero's StPythiaEvent class to supply mcAsymMaker results to user
//

#include "StPythiaEvent.h"

ClassImp(StPythiaEvent);

StPythiaEvent::StPythiaEvent()
{
    mParticles = new TClonesArray("TParticle");

    Clear();
}

StPythiaEvent::~StPythiaEvent()
{
    Clear();

    if (mParticles)   { delete mParticles;   mParticles   = 0; }
}

StPythiaEvent::StPythiaEvent(const StPythiaEvent& t) 
{
    mRunId      = t.mRunId;
    mEventId    = t.mEventId;
    mProcessId  = t.mProcessId;
    mVertex     = t.mVertex;
    mS          = t.mS;
    mT          = t.mT;
    mU          = t.mU;
    mPt         = t.mPt;
    mCosTheta   = t.mCosTheta;
    mX1         = t.mX1;
    mX2         = t.mX2;
    mPartonALL  = t.mPartonALL;
    
    for(int i=0; i<5; i++) {
        mDF1[i] = t.mDF1[i];
        mDF2[i] = t.mDF2[i];
    }
    
    for(int i=0; i<2; i++) {
        mF1[i]  = t.mF1[i];
        mF2[i]  = t.mF2[i];
    }
    
    for(int i=0; i<t.mParticles->GetEntries(); i++) {
        TParticle *p = (TParticle*)(t.mParticles->At(i));
        new ((*mParticles)[mParticles->GetEntriesFast()]) TParticle(*p);
    }
}

StPythiaEvent& StPythiaEvent::operator=(const StPythiaEvent& t) 
{
    if(this == &rhs) return *this;
    
    mRunId      = t.mRunId;
    mEventId    = t.mEventId;
    mProcessId  = t.mProcessId;
    mVertex     = t.mVertex;
    mS          = t.mS;
    mT          = t.mT;
    mU          = t.mU;
    mPt         = t.mPt;
    mCosTheta   = t.mCosTheta;
    mX1         = t.mX1;
    mX2         = t.mX2;
    mPartonALL  = t.mPartonALL;
    
    for(int i=0; i<5; i++) {
        mDF1[i] = t.mDF1[i];
        mDF2[i] = t.mDF2[i];
    }
    
    for(int i=0; i<2; i++) {
        mF1[i]  = t.mF1[i];
        mF2[i]  = t.mF2[i];
    }
    
    for(int i=0; i<t.mParticles->GetEntries(); i++) {
        TParticle *p = (TParticle*)(t.mParticles->At(i));
        new ((*mParticles)[mParticles->GetEntriesFast()]) TParticle(*p);
    }
    
    return *this;
}
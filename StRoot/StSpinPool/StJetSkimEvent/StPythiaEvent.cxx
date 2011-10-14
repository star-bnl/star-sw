//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 12 July 2007
//
// $Log: StPythiaEvent.cxx,v $
// Revision 1.1  2008/06/01 05:31:42  tai
// moved StPythiaEvent to StSpinPool/StJetSkimEvent
//
// Revision 1.5  2008/05/01 01:36:39  rfatemi
// check in D. Staszak modifications - additional grids
//
// Revision 1.4  2008/02/03 01:27:17  rfatemi
// Included Gehrmann-Stirling PDFs
//
// Revision 1.3  2007/11/01 02:48:10  rfatemi
// Dave Staszak update for additional GRSV grids
//
// Revision 1.2  2007/07/19 02:05:38  kocolosk
// fix two small bugs I missed in the last commit.
//
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
    
    for(int i=0; i<30; i++) {
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

StPythiaEvent& StPythiaEvent::operator=(const StPythiaEvent& rhs) 
{
    if(this == &rhs) return *this;
    
    mRunId      = rhs.mRunId;
    mEventId    = rhs.mEventId;
    mProcessId  = rhs.mProcessId;
    mVertex     = rhs.mVertex;
    mS          = rhs.mS;
    mT          = rhs.mT;
    mU          = rhs.mU;
    mPt         = rhs.mPt;
    mCosTheta   = rhs.mCosTheta;
    mX1         = rhs.mX1;
    mX2         = rhs.mX2;
    mPartonALL  = rhs.mPartonALL;
    
    for(int i=0; i<30; i++) {
        mDF1[i] = rhs.mDF1[i];
        mDF2[i] = rhs.mDF2[i];
    }
    
    for(int i=0; i<2; i++) {
        mF1[i]  = rhs.mF1[i];
        mF2[i]  = rhs.mF2[i];
    }
    
    for(int i=0; i<rhs.mParticles->GetEntries(); i++) {
        TParticle *p = (TParticle*)(rhs.mParticles->At(i));
        new ((*mParticles)[mParticles->GetEntriesFast()]) TParticle(*p);
    }
    
    return *this;
}

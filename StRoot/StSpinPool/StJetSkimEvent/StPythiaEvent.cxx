//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 12 July 2007
//
// $Log: StPythiaEvent.cxx,v $
// Revision 1.6  2012/12/10 21:52:46  pibero
// More simplifications...
//
// Revision 1.5  2012/01/18 18:11:36  pibero
// Added PYTHIA variables: MSTU(72), MSTU(73), and MSTP(111)
//
// Revision 1.4  2011/09/13 16:24:21  pibero
// Added DSSV2009 grid
//
// Revision 1.3  2010/10/04 19:18:29  pibero
// Fix copy constructor and assignment operator. Thanks, Alice!
//
// Revision 1.2  2009/12/08 15:14:24  pibero
// Added Pythia tune per Helen Caines request.
//
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

#include <algorithm>
#include "StPythiaEvent.h"

using std::copy;

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
    mTune       = t.mTune;
    mVertex     = t.mVertex;
    mS          = t.mS;
    mT          = t.mT;
    mU          = t.mU;
    mPt         = t.mPt;
    mCosTheta   = t.mCosTheta;
    mX1         = t.mX1;
    mX2         = t.mX2;
    mMstu72     = t.mMstu72;
    mMstu73     = t.mMstu73;
    mMstp111    = t.mMstp111;
    mPartonALL  = t.mPartonALL;

    copy(t.mDF1,t.mDF1+NPDF,mDF1);
    copy(t.mDF2,t.mDF2+NPDF,mDF2);
    
    copy(t.mF1,t.mF1+2,mF1);
    copy(t.mF2,t.mF2+2,mF2);
    
    mParticles = new TClonesArray("TParticle");

    for (int i = 0; i < t.mParticles->GetEntriesFast(); ++i) {
        TParticle* p = (TParticle*)t.mParticles->At(i);
	new ((*mParticles)[i]) TParticle(*p);
    }
}

StPythiaEvent& StPythiaEvent::operator=(const StPythiaEvent& rhs) 
{
    if(this == &rhs) return *this;
    
    mRunId      = rhs.mRunId;
    mEventId    = rhs.mEventId;
    mProcessId  = rhs.mProcessId;
    mTune       = rhs.mTune;
    mVertex     = rhs.mVertex;
    mS          = rhs.mS;
    mT          = rhs.mT;
    mU          = rhs.mU;
    mPt         = rhs.mPt;
    mCosTheta   = rhs.mCosTheta;
    mX1         = rhs.mX1;
    mX2         = rhs.mX2;
    mMstu72     = rhs.mMstu72;
    mMstu73     = rhs.mMstu73;
    mMstp111    = rhs.mMstp111;
    mPartonALL  = rhs.mPartonALL;
    
    copy(rhs.mDF1,rhs.mDF1+NPDF,mDF1);
    copy(rhs.mDF2,rhs.mDF2+NPDF,mDF2);
    
    copy(rhs.mF1,rhs.mF1+2,mF1);
    copy(rhs.mF2,rhs.mF2+2,mF2);
    
    mParticles->Clear();
    
    for (int i = 0; i < rhs.mParticles->GetEntriesFast(); ++i) {
        TParticle* p = (TParticle*)rhs.mParticles->At(i);
        new ((*mParticles)[i]) TParticle(*p);
    }
    
    return *this;
}

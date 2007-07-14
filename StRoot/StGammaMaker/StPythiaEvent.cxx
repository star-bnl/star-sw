//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 12 July 2007
//

#include "StPythiaEvent.h"

ClassImp(StPythiaEvent);

StPythiaEvent::StPythiaEvent()
{
  mRunId = 0;
  mEventId = 0;
  mProcessId = 0;
  mVertex.SetXYZ(0, 0, 0);
  mS = 0;
  mT = 0;
  mU = 0;
  mPt = 0;
  mCosTheta = 0;
  mX1 = 0;
  mX2 = 0;
  mALL = 0;
  mALL_LO = 0;
  mALL_NLO = 0;
  mALL_NLO_g0 = 0;
  mALL_NLO_gmax = 0;
  mALL_NLO_gmin = 0;
  mPartons = new TClonesArray("TParticle");
}

StPythiaEvent::~StPythiaEvent()
{
  Clear();
  if (mPartons) {
    delete mPartons;
    mPartons = 0;
  }
}

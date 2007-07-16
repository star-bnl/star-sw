//
// Pibero Djawotho <pibero@indiana.edu>
// Indiana University
// 12 July 2007
//

#include "StPythiaEvent.h"

ClassImp(StPythiaEvent);

StPythiaEvent::StPythiaEvent()
{
  mParticles = new TClonesArray("TParticle");
  mPion04Mom = new TClonesArray("TLorentzVector");
  mPrompt4Mom = new TClonesArray("TLorentzVector");
  mDecay4Mom = new TClonesArray("TLorentzVector");
  mFrag4Mom = new TClonesArray("TLorentzVector");
  mInitial4Mom = new TClonesArray("TLorentzVector");
  mFinal4Mom = new TClonesArray("TLorentzVector");

  Clear();
}

StPythiaEvent::~StPythiaEvent()
{
  Clear();

  if (mParticles)   { delete mParticles;   mParticles   = 0; }
  if (mPion04Mom)   { delete mPion04Mom;   mPion04Mom   = 0; }
  if (mPrompt4Mom)  { delete mPrompt4Mom;  mPrompt4Mom  = 0; }
  if (mDecay4Mom)   { delete mDecay4Mom;   mDecay4Mom   = 0; }
  if (mFrag4Mom)    { delete mFrag4Mom;    mFrag4Mom    = 0; }
  if (mInitial4Mom) { delete mInitial4Mom; mInitial4Mom = 0; }
  if (mFinal4Mom)   { delete mFinal4Mom;   mFinal4Mom   = 0; }
}

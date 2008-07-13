// $Id: StJetScratch.cxx,v 1.4 2008/07/13 05:36:39 tai Exp $
#include "StJetScratch.h"

#include "StJetTPCMuDst.h"
#include "StJetBEMCMuDst.h"
#include "StJetEEMCMuDst.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <TFile.h>
#include <TTree.h>

#include <iostream>

using namespace std;
using namespace StSpinJet;

ClassImp(StJetScratch)
  

StJetScratch::StJetScratch(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name)
  , _file(file)
  , _uDstMaker(uDstMaker)
{ }

Int_t StJetScratch::Init()
{
  return kStOk;
}

Int_t StJetScratch::Make()
{
  return kStOk;
}

Int_t StJetScratch::Finish()
{
  return kStOk;
}

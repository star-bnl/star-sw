// $Id: StJetScratch.cxx,v 1.6 2008/08/03 00:26:17 tai Exp $
#include "StJetScratch.h"

#include "StjTPCMuDst.h"
#include "StjBEMCMuDst.h"
#include "StjEEMCMuDst.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <TFile.h>
#include <TTree.h>

#include <iostream>

using namespace std;

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

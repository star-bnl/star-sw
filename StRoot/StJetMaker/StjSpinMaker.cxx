// $Id: StjSpinMaker.cxx,v 1.2 2008/08/02 19:22:27 tai Exp $
#include "StjSpinMaker.h"

#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>

#include <StSpinPool/StSpinDbMaker/StSpinDbMaker.h>

#include <TDirectory.h>

#include <iostream>

using namespace std;

ClassImp(StjSpinMaker)
  

StjSpinMaker::StjSpinMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker)
  : StMaker(name)
  , _file(file)
  , _uDstMaker(uDstMaker)
{ }

Int_t StjSpinMaker::Init()
{

  return kStOk;
}

Int_t StjSpinMaker::Make()
{
  int runNumber = _uDstMaker->muDst()->event()->runId();
  int eventId = _uDstMaker->muDst()->event()->eventId();

  int bx7 = _uDstMaker->muDst()->event()->l0Trigger().bunchCrossingId7bit( runNumber );
  int spin4 = _uDstMaker->muDst()->event()->l0Trigger().spinBits( runNumber );

  int bbcTimebin = _uDstMaker->muDst()->event()->bbcTriggerDetector().onlineTimeDifference()/32;

  double vertexZ = _uDstMaker->muDst()->event()->primaryVertexPosition().z();

  cout
    << runNumber << " "
    << eventId << " "
    << bx7 << " "
    << spin4 << " "
    << bbcTimebin << " "
    << vertexZ << " "
    << endl;

  return kStOk;
}

Int_t StjSpinMaker::Finish()
{
  return kStOk;
}

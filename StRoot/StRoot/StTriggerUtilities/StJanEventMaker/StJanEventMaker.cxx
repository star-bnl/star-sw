//
// Pibero Djawotho <pibero@iucf.indiana.edu>
// Indiana University
// Mar 1, 2006
//

// ROOT
#include "TSystem.h"

// STAR
#include "StEventTypes.h"
#include "StL2_2008EmulatorMaker.h"

// Local
#include "JanEvent.h"
#include "StJanEventMaker.h"

ClassImp(StJanEventMaker)


//==========================================
Int_t StJanEventMaker::Init()
{
  return StMaker::Init();
}

//==========================================
Int_t StJanEventMaker::InitRun(Int_t runNumber)
{
  const Char_t* filename = Form("R%d.eve.bin", runNumber);
  if (gSystem->Getenv("JOBID"))
    filename = gSystem->ConcatFileName(gSystem->Getenv("SCRATCH"),
				       Form("%s.%s", filename, gSystem->Getenv("JOBID")));

  if (mFile.is_open()) mFile.close();
  //  mFile.open(filename, ios_base::app);
  mFile.open(filename);// no append
  mEventCounter = 0;
  return StMaker::InitRun(runNumber);
}

//==========================================
Int_t StJanEventMaker::Make()
{
  StEvent* stEvent = (StEvent*)GetInputDS("StEvent");
  if (!stEvent) {
    gMessMgr->Warning("No StEvent");
    return kStWarn;
  }
  JanEvent janEvent;

  StL2_2008EmulatorMaker *L2EmuMk=(StL2_2008EmulatorMaker*)GetChain()->GetMaker("L2Emul2008"); // tmp, should have year dependent switch
  assert(L2EmuMk);
  
  unsigned short *btow=0, *etow=0;
  if(L2EmuMk->getBtowIn()) btow=L2EmuMk->getBtowBank();
  if(L2EmuMk->getEtowIn()) etow=L2EmuMk->getEtowBank();
  
  //  printf("i   BB=%d EE=%d \n",L2EmuMk->getBtowIn(),L2EmuMk->getEtowIn());
  // printf("a   BB=%p EE=%p \n",btow=L2EmuMk->getBtowBank(),btow=L2EmuMk->getEtowBank());

  // Char_t* trig=(Char_t*)L2EmuMk->mTrigData;
  fillJanEvent(triggerData(stEvent),btow,etow,janEvent);
  
  mFile << janEvent;
  return kStOk;
}

//==========================================
Int_t StJanEventMaker::Finish()
{
  mFile.close();
  return kStOk;
}


//==========================================
Char_t* StJanEventMaker::triggerData(StEvent* event)
{
  if (!event->triggerData()) {
    gMessMgr->Warning("No StTriggerData");
    return 0;
  }
#if 0
  const UShort_t JPSI_MB_BITS = 0x0802;
  if (trgData->EvtDesc.DSMInput & JPSI_MB_BITS == JPSI_MB_BITS) {
    cout << "jpsi-mb bits on" << endl;
  }
  else {
    cout << "jpsi-mb bits off" << endl;
  }
#endif
  return event->triggerData()->getTriggerStructure();
}


//==========================================
void StJanEventMaker::fillJanEvent(Char_t* trgData, UShort_t* bemcData,
				   UShort_t* eemcData, JanEvent& event)
{
  Char_t* header = Form("Form2,ieve=%d,run=%d,id=%d,%d,%d,%d",
			++mEventCounter, GetRunNumber(), GetEventNumber(),
			trgData != 0, bemcData != 0, eemcData != 0);
  event.setHeader(header);
  event.setTriggerData(trgData);
  event.setBemcData(bemcData);
  event.setEemcData(eemcData);
}

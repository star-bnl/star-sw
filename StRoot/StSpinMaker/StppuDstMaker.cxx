/***************************************************************************
 *
 * $Id: StppuDstMaker.cxx,v 1.1 2002/01/16 20:22:54 akio Exp $
 * 
 * Author: Akio Ogawa June 2001
 ***************************************************************************
 *
 * Description:  TTree uDst for spin-pp
 *
 ***************************************************************************
 *
 * $Log: StppuDstMaker.cxx,v $
 * Revision 1.1  2002/01/16 20:22:54  akio
 * First version
 *
 **************************************************************************/
#include <string.h>
#include <iostream.h>

#include "TFile.h"
#include "TTree.h"

#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StIOMaker/StIOMaker.h"

#include "StppuDstMaker.h"
#include "StppEvent.h"
#include "StppGeant.h"
#include "StTriggerDetectorCollection.h"
#include "StBbcTriggerDetector.h"
#include "StFpdCollection.h"
#include "St_trg_Maker/St_trg_Maker.h"

ClassImp(StppuDstMaker)

StppuDstMaker::StppuDstMaker(const Char_t *name) 
  : StMaker(name), mGoodCounter(0), mBadCounter(0){
  infoLevel = 1;
}

Int_t StppuDstMaker::Init() 
{
  // creating uDst file name
  TString uDstFileName("StppuDst.root");
  StIOMaker* pIOMaker = (StIOMaker*)GetMaker("IO");
  if(!pIOMaker){ pIOMaker = (StIOMaker*)GetMaker("inputStream"); }
  if(pIOMaker){
    uDstFileName = pIOMaker->GetFile() ;
    char* ccc = "/" ;
    Ssiz_t slashPosition = uDstFileName.Last(*ccc) ;
    if ( slashPosition != -1 &&
	 slashPosition < uDstFileName.Length() )
    uDstFileName.Remove(0,slashPosition+1);
  }
  uDstFileName.ReplaceAll(".dst.root",".uDst.root");
  uDstFileName.ReplaceAll(".event.root",".uDst.root");
  uDstFileName.ReplaceAll(".daq",".uDst.root");
  cout << "StppuDstMaker: uDst output file: " << uDstFileName << endl;
  
  //open udst file
  m_outfile = new TFile(uDstFileName,"recreate");
  m_outfile->SetFormat(1);
  m_outfile->SetCompressionLevel(1);

  //create udst & its branches
  ppuDst  = new TTree("uDst","ppSpinuDst",99);
  ppEvent = new StppEvent(); ppEvent->setInfoLevel(infoLevel);
  ppGeant = new StppGeant(); ppGeant->setInfoLevel(infoLevel);
  ppuDst->Branch ("Event","StppEvent",&ppEvent,64000,99);
  ppuDst->Branch ("Geant","StppGeant",&ppGeant,64000,99);
#ifdef _BBC_data_
  bbc     = new StBbcTriggerDetector();
  ppuDst->Branch ("Bbc","StBbcTriggerDetector",&bbc,64000,99);
#endif
#ifdef _FPD_data_
  fpd     = new StFpdCollection();
  ppuDst->Branch ("Fpd","StFpdCollection",&fpd,64000,99);
#endif

  return StMaker::Init();
}

Int_t StppuDstMaker::Make() {
  cout <<" Start StpuDstMaker :: "<< GetName() <<" mode="<<m_Mode<<endl;   

  // Get StEvent
  StEvent* event = (StEvent *)GetInputDS("StEvent");
  if(!event){
    mBadCounter++;
    return kStOK;        
  }

  // fill ppEvent 
  int res = ppEvent->fill(event);
  if(res<0){
    mBadCounter++;
    return kStOK;        
  }

  // Get geant info, if any, and fill geant branch
  TDataSet *geantBranch = GetInputDS("geantBranch");
  if(geantBranch){
    ppGeant->fill(geantBranch);
  }

  // Get FPD & BBC infos
#ifdef _BBC_data_
  bbc = &(event->triggerDetectorCollection()->bbc());
#endif
#ifdef _FPD_data_
  fpd = event->fpdCollection();
#endif

  //write out to uDst
  ppuDst->Fill();
    
  mGoodCounter++;
  ppEvent->clear();
  ppGeant->clear();
  return kStOk;
}

Int_t StppuDstMaker::Finish()
{
  m_outfile->Write();
  m_outfile->Close();
  cout << "=================================================================\n";
  cout << "StppuDstger statistics:\n";
  cout << "events with StppuDstger data: " << mGoodCounter << endl;
  cout << "events without StppuDstger data: " << mBadCounter << endl;
  cout << "=================================================================\n";    
  StMaker::Finish();
  return kStOK;
}




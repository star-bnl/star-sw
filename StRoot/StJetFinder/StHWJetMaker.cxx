/***************************************************************************
 *
 * $Id: StHWJetMaker.cxx,v 1.4 2003/07/24 22:06:47 thenry Exp $
 * 
 * Author: Thomas Henry April 2003
 ***************************************************************************
 *
 * Description:  Finds jets for fast off-line analysis
 *
 ***************************************************************************
 *
 * Revision 1.0  2003/04/23 thenry
 *
 **************************************************************************/
#include "TFile.h"
#include "TTree.h"
#include <math.h>

#include "StChain.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StIOMaker/StIOMaker.h"
#include "StEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/EMC/StEmcMicroEvent.h"
#include "StSpinMaker/StppCdfChargedConeJetAnalyzer.h"

#include "StHWJetMaker.h"
#include "StEmcPoint.h"
#include "StTpcFourPMaker.h"

ClassImp(StHWJetMaker)
  
StHWJetMaker::StHWJetMaker(const Char_t *name) 
  : StJetMaker(name, NULL, NULL, "/dev/null"){
  muDstMaker = NULL;
  TList* mkrlist = GetParentMaker()->GetMakeList();
  for(int i = 0; i < mkrlist->GetSize(); i++)
      if(dynamic_cast<StMuDstMaker*>(mkrlist->At(i)))
	{
	  muDstMaker = dynamic_cast<StMuDstMaker*>(mkrlist->At(i));
	  break;
	}

  if(muDstMaker == NULL)
    cout << "muDstMaker no found" << endl;
  StJetMaker::fourPMaker = new StTpcFourPMaker("FourPMaker", muDstMaker);
  if(StJetMaker::fourPMaker == NULL)
    {
      cout << "Error:  fourPMaker == NULL!" << endl;
    }
  SetSaveEventWithNoJets(false);
  SetStoreEMC(false);
  neverSave = true;

  StppCdfChargedConeJetAnalyzer* mcanalyzer = new StppCdfChargedConeJetAnalyzer
    (56, -1.6, 1.6, 120, -atan(1.0)*4.0, atan(1.0)*4.0);
  mcanalyzer->setConeR(0.7);
  mcanalyzer->setConeSeedEtMin(1.0);
  mcanalyzer->setConeAssocEtMin(0.2);
  mcanalyzer->setPerformMinimization(true);
  mcanalyzer->setAddMidpoints(true);
  mcanalyzer->setDoSplitMerge(true);
  mcanalyzer->setDoMidpointFix(true);
  mcanalyzer->setRequireStableMidpoints(true);
  mcanalyzer->setNhits(15);
  mcanalyzer->setDebug(false);
  mcanalyzer->setCutPtMin(0.2);
  mcanalyzer->setAbsEtaMax(1.6);
  mcanalyzer->setSeedEtMin(1.0);
  mcanalyzer->setAssocEtMin(.2);
  mcanalyzer->setEmcAccepted(false);
  mcanalyzer->setTpcAccepted(false);
  mcanalyzer->setFpdAccepted(false);
  mcanalyzer->setJetPtMin(2.0); // GeV/c2
  mcanalyzer->setJetEtaMax(100.0);
  mcanalyzer->setJetEtaMin(0);
  mcanalyzer->setJetNmin(0);
  addAnalyzer(mcanalyzer, "MkConeJets"); 
  dontGoOutaScope = mcanalyzer;
}

Int_t StHWJetMaker::Init()
{
  if(muDstMaker == NULL) return kStOk;
  //StJetMaker::fourPMaker->Init();

  Int_t retval = StJetMaker::Init();
  return retval;
}

Int_t StHWJetMaker::Make()
{
  if(muDstMaker == NULL) return kStOk;
  StJetMaker::fourPMaker->Make();

  cout << "Now calling StJetMaker::Make()" << endl;
  if(StJetMaker::fourPMaker == NULL)
    {
      cout << "Error:  fourPMaker == NULL!" << endl;
    }
  Int_t retval = StJetMaker::Make();
  return retval;
}

Int_t StHWJetMaker::Finish()
{
  if(muDstMaker == NULL) return kStOk;
  //fourPMaker->Finish();

  Int_t retval = StJetMaker::Finish();
  return retval;
}




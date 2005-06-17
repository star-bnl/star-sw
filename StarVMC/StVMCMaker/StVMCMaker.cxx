//*-- Author : Yuri Fisyak
// 
// $Id: StVMCMaker.cxx,v 1.3 2005/06/17 18:35:45 fisyak Exp $
// $Log: StVMCMaker.cxx,v $
// Revision 1.3  2005/06/17 18:35:45  fisyak
// Add flow diagram
//
// Revision 1.2  2005/06/09 20:14:40  fisyak
// Set Run number (=1 D)
//
// Revision 1.1  2005/05/24 22:58:08  fisyak
// The first version
//
//
/* Flow diagram:
   Load(); // shared libraries
   GetVMC(); // define gGeoManager
------------------
StVMCMaker::Init()
------------------

   StarVMCApplication *appl = new StarVMCApplication("StarVMC", "The STAR VMC application");
   Geant3TGeo* geant3 = new TGeant3TGeo("C++ Interface to Geant3"); // gMC
   StarMCPrimaryGenerator *generator = new StarMCHBPrimaryGenerator(fInputFile,m_DataSet); // a generator
   appl->SetPrimaryGenerator(generator);
   StarMCHits *hits = StarMCHits::instance(); 
   appl->SetStepping(hits);
   hits->SetHitHolder(m_DataSet); // set hit storage
   appl->InitMC();
   ->    gMC->SetStack(fStack);
   ->    gMC->Init();
   ->                  DefineParticles();
   ->                  appl->AddParticles();
   ->                  appl->ConstructGeometry();
   ->                  FinishGeometry();
   ->                  appl->InitGeometry();
   ->                        fMcHits->Init(); // hit description
   ->    gMC->BuildPhysics(); 
----------------
StVMCMaker::Make
----------------
   appl->RunMC(1);
   ->    gMC->ProcessRun(1)
   ->                  appl->BeginEvent();
   ->                  ProcessEvent();
   ->                    Gtrigi();
   ->                    Gtrigc();
   ->                    Gtrig();
   ->                      appl->GeneratePrimaries();             // gukine
   ->                      gtreveroot();
   ->                               appl->Field(xdouble,bdouble); // gufld
   ->                               appl->PreTrack();
   ->                               g3track();
   ->                                   appl->Stepping();         // gustep
   ->                                       hits->Step();
   ->                                         fill fHit
   ->                                         FillG2Table();
   ->                                            fCurrentDetector->GetChair()->Fill(fHit);
   ->                                                   G2TBook[Track]Hit();
   ->                                                   G2TFill[Track]Hit();
   ->                               appl->PostTrack();
   ->                  appl->FinishEvent();
   ->                      hits->FinishEvent(); // fill run,event,track tables
*/

#include <assert.h>
#include "TObjectSet.h"
#include "StVMCMaker.h"
#include "StChain.h"
#include "Stiostream.h"
#include "StarMagField.h"
#include "StarMCHits.h"
#include "StarMCSimplePrimaryGenerator.h"
#include "StarMCHBPrimaryGenerator.h"
#include "StMessMgr.h"

ClassImp(StVMCMaker);

StarVMCApplication* StVMCMaker::fgStarVMCApplication = 0;
TGeant3TGeo*        StVMCMaker::fgGeant3 = 0;
//_____________________________________________________________________________
Int_t StVMCMaker::Init(){
  if (m_Mode%100 != 1) { // Mixer mode == 1 - do not modify EvtHddr and MagF
    fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
    if (!fEvtHddr) {                            // Standalone run
      fEvtHddr = new StEvtHddr(m_ConstSet);
      SetOutput(fEvtHddr);	                //Declare this "EvtHddr" for output
    }
    fEvtHddr->SetRunNumber(fRunNo);
    fEvtHddr->SetEventNumber(0);
    fEvtHddr->SetEventType("VMC");
    fEvtHddr->SetProdDateTime();
  }
  TObjectSet *geom = (TObjectSet *) GetDataBase("VmcGeometry");
  assert(geom);
  if (fgStarVMCApplication) return kStOK;
  fgStarVMCApplication = new StarVMCApplication("StarVMC", "The STAR VMC application");
  if ( fgGeant3) return kStOK;
  fgGeant3 = new TGeant3TGeo("C++ Interface to Geant3");//, 1, 200000); 
  gMessMgr->Info() << "StVMCMaker::InitRun Geant3 has been created." << endm;
  if (m_Mode/100 != 0) {// switch off physics 
    gMessMgr->Info() << "StVMCMaker::InitRun switch off physics" << endm;
    fgGeant3->SetProcess("DCAY", 0);
    fgGeant3->SetProcess("ANNI", 0);
    fgGeant3->SetProcess("BREM", 0);
    fgGeant3->SetProcess("COMP", 0);
    fgGeant3->SetProcess("HADR", 0);
    fgGeant3->SetProcess("MUNU", 0);
    fgGeant3->SetProcess("PAIR", 0);
    fgGeant3->SetProcess("PFIS", 0);
    fgGeant3->SetProcess("PHOT", 0);
    fgGeant3->SetProcess("RAYL", 0);
    fgGeant3->SetProcess("LOSS", 4); // no fluctuations 
    //  fgGeant3->SetProcess("LOSS 1"); // with delta electron above dcute
    fgGeant3->SetProcess("DRAY", 0);
    fgGeant3->SetProcess("MULS", 0);
    fgGeant3->SetProcess("STRA", 0);
    fgGeant3->SetCut("CUTGAM",	1e-3  );
    fgGeant3->SetCut("CUTELE", 	1e-3  );
    fgGeant3->SetCut("CUTHAD", 	.001  );
    fgGeant3->SetCut("CUTNEU", 	.001  );
    fgGeant3->SetCut("CUTMUO", 	.001  );
    fgGeant3->SetCut("BCUTE", 	.001  );
    fgGeant3->SetCut("BCUTM", 	.001  );
    fgGeant3->SetCut("DCUTE", 	1e-3  );
    fgGeant3->SetCut("DCUTM", 	.001  );
    fgGeant3->SetCut("PPCUTM", 	.001  );
    fgGeant3->SetCut("TOFMAX", 	50.e-6);
  }
  gMessMgr->Info() << "StVMCMaker::InitRun SetMagField set as StarMagField" 
		   << " with Map: " << StarMagField::Instance()->GetMap()
		   << ",Factor: " << StarMagField::Instance()->GetFactor() 
		   << ",Rescale: " << StarMagField::Instance()->GetRescale() <<endm;
  fgStarVMCApplication->SetMagField(StarMagField::Instance());
  StarMCPrimaryGenerator *generator = 0;
  if (fInputFile != "") generator = new StarMCHBPrimaryGenerator(fInputFile,m_DataSet);
  //                                                             Ntrack Id Ptmin Ptmax Ymin Ymax Phimin Phimax Zmin Zmax
  //  else              generator = new StarMCSimplePrimaryGenerator( 1, 5,    1.,   1.,0.1, 0.1, 0.57,  0.57,  0.,   0., "G");
  else                  generator = new StarMCSimplePrimaryGenerator(80, 5,    1.,   1.,-4.,  4.,    0,  6.28,  0.,   0., "G");
  assert(generator);
  if (Debug()) generator->SetDebug(1);
  fgStarVMCApplication->SetPrimaryGenerator(generator);
  StarMCHits *hits = StarMCHits::instance();
  hits->SetHitHolder(m_DataSet);
  fgStarVMCApplication->SetStepping(hits);
  //  fgStarVMCApplication->SetStepping(new StMCSteppingHist("tgeom"));
  //  fgStarVMCApplication->SetStepping(new StMCStepping);
  // The "Init" method in the gMC object causes the geometry to be cosntructed
  fgStarVMCApplication->InitMC();
  if (Debug() > 1) {
    fgGeant3->SetDEBU(1,1,100);
    fgGeant3->SetSWIT(1,2);
    fgGeant3->SetSWIT(2,2);
  } else {
    fgGeant3->SetSWIT(4,0);
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StVMCMaker::Make(){
  fEventNo++;
  if (fEvtHddr) {
    fEvtHddr->SetRunNumber(fRunNo);
    fEvtHddr->SetEventNumber(fEventNo);
    fEvtHddr->SetEventType("VMC");
    fEvtHddr->SetProdDateTime();
    //    SetDateTime();
  }  
  TStopwatch sw;
  fgStarVMCApplication->RunMC(1);
  if (Debug())   sw.Print();
  return kStOK;
}
//_____________________________________________________________________________
void StVMCMaker::Clear(Option_t *option){
  StMaker::Clear();
}
//_____________________________________________________________________________
Int_t StVMCMaker::Finish(){
  //  StMCSteppingHist::Instance()->Finish();
  return StMaker::Finish();
}
//_____________________________________________________________________________
void StVMCMaker::SetDateTime(Int_t id, Int_t it) {
  if ( m_Mode%100 != 1 && fEvtHddr ) {
    fEvtHddr->SetDateTime(id,it);
  }  
}
//_____________________________________________________________________________
void StVMCMaker::Skip(Int_t nskip) {
}

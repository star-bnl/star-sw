//*-- Author : Yuri Fisyak
// 
// $Id: StVMCMaker.cxx,v 1.6 2013/11/26 20:56:45 fisyak Exp $
// $Log: StVMCMaker.cxx,v $
// Revision 1.6  2013/11/26 20:56:45  fisyak
// use StarMagField inherited from TVirtualMagField
//
// Revision 1.5  2011/02/11 16:15:27  fisyak
// more to Attributes
//
// Revision 1.4  2009/08/10 19:06:58  fisyak
// Add different fitted parameters types (Sti,SD,SC,Dca)
//
// Revision 1.3  2009/04/16 14:29:11  fisyak
// add StarVMCDetectorSet
//
// Revision 1.10  2009/04/15 21:46:41  fisyak
// Add StarVMCDetectorSet
//
// Revision 1.9  2009/02/03 15:55:44  fisyak
// synchronize with .DEV2
//
// Revision 1.2  2009/01/24 00:21:43  fisyak
// Fix debug flag
//
// Revision 1.1.1.1  2008/12/10 20:45:49  fisyak
// Merge with macos version
//
// Revision 1.8  2008/03/05 13:15:56  fisyak
// comply Skip signuture with base class
//
// Revision 1.7  2007/04/07 19:33:09  perev
// Check for input file added
//
// Revision 1.6  2007/01/09 04:53:20  potekhin
// New input modes
//
// Revision 1.4  2005/09/13 21:34:29  fisyak
// Move initialization from Init to InitRun, add conversion TGeoVolume to TVolume for StEventDisplayMaker and TofMatchers
//
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
StVMCMaker::InitRun()
----------------
   ->    StarVMCApplication::InitMC
   ->      TGeant3::Init
   ->        StarVMCApplication::InitGeometry
   ->          StarMCHits::Init
   ->            StarVMCDetectorSet::instance
   ->              StarVMCDetectorSet::StarVMCDetectorSet
   ->                StarVMCDetectorSet::Init
   ->                  StarVMCDetectorSet::MakeDetectorDescriptors
   ->                    StarVMCDetectorSet::LoopOverTgeo
----------------
StVMCMaker::Make
----------------
   StarVMCApplication::RunMC(1);
   ->    TGeant3::ProcessRun(1)
   ->                  StarVMCApplication::BeginEvent();
   ->                  TGeant3::ProcessEvent();
   ->                    TGeant3::Gtrigi();
   ->                    TGeant3::Gtrigc();
   ->                    TGeant3::Gtrig();
   ->                      StarVMCApplication::GeneratePrimaries();             // gukine
   ->                      gtreveroot();
   ->                        gutrack();
   ->                               StarVMCApplication::Field(xdouble,bdouble); // gufld
   ->                               StarVMCApplication::PreTrack();
   ->                          g3track();
   ->                            gustep();
   ->                                   StarVMCApplication::Stepping();         // gustep
   ->                                       StarMCHits::Step();
   ->                                         StarMCHits::FillG2Table();
   ->                                           StarVMCDetector::GetVolumeId();
   ->                                              StarVMCDetector::GetNumbv  
   ->                                            fCurrentDetector->GetChair()->Fill(fHit);
   ->                                                   G2TBook[Track]Hit();
   ->                                                   G2TFill[Track]Hit();
   ->                               StarVMCApplication::PostTrack();
   ->                  StarVMCApplication::FinishEvent();
   ->                      hits->FinishEvent(); // fill run,event,track tables
*/

#include <assert.h>
#include "TSystem.h"
#include "TGeometry.h"
#include "TGeoManager.h"
#include "TObjectSet.h"
#include "StVMCMaker.h"
#include "StChain.h"
#include "Stiostream.h"
#include "StarMagField.h"
#include "StarMCHits.h"
#include "StarMCSimplePrimaryGenerator.h"
#include "StarMCHBPrimaryGenerator.h"
#include "TGeoDrawHelper.h"
#include "StMessMgr.h"
#include "StarVMCDetectorSet.h"
ClassImp(StVMCMaker);

StarVMCApplication* StVMCMaker::fgStarVMCApplication = 0;
TGeant3TGeo*        StVMCMaker::fgGeant3 = 0;
//_____________________________________________________________________________
Int_t StVMCMaker::Init() {
  fgStarVMCApplication = new StarVMCApplication("StarVMC", "The STAR VMC application");
  fgGeant3 = new TGeant3TGeo("C++ Interface to Geant3");//, 1, 200000); 
  gMessMgr->Info() << "StVMCMaker::Init Geant3 has been created." << endm;
  if (! IAttr("VMCPassive")) {
    gMessMgr->Info() << "StVMCMaker::InitRun Active mode" << endm; 
    StarMCPrimaryGenerator *generator = fgStarVMCApplication->GetPrimaryGenerator();
    if (! generator) {
      if (fInputFile != "") generator = new StarMCHBPrimaryGenerator(fInputFile,m_DataSet);
      //                                                             Ntrack Id Ptmin Ptmax Ymin Ymax Phimin Phimax Zmin Zmax
      //  else              generator = new StarMCSimplePrimaryGenerator( 1, 5,    1.,   1.,0.1, 0.1, 0.57,  0.57,  0.,   0., "G");
      else                  generator = new StarMCSimplePrimaryGenerator(80, 6,    1.,   1.,-4.,  4.,    0,  6.28,  0.,   0., "G");
      fgStarVMCApplication->SetPrimaryGenerator(generator);
    }
    assert(generator);
    StarMCHits *hits = StarMCHits::instance();
    hits->SetHitHolder(m_DataSet);
    fgStarVMCApplication->SetStepping(hits);
  }
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StVMCMaker::InitRun  (Int_t runumber){
  if (! gGeoManager) {
    TObjectSet *geom = (TObjectSet *) GetDataBase("VmcGeometry/Geometry");
    if (! geom) {
      LOG_WARN << "StVMCMaker::InitRun: Can't get VMC geometry, try AgiGeometry" <<endm;
      geom = (TObjectSet *) GetDataBase("AgiGeometry/Geometry");
    }
    assert(geom);
  }
  if (!fVolume) {
#if 0
    TGeoDrawHelper Helper;
    fVolume = (TDataSet *) Helper.GetVolume();
#else
    fVolume = (TDataSet *) GetDataBase("StarDb/AgiGeometry/HALL");
#endif
  }
  if (fVolume) { 
#if 0 /* Don't remember why I need to remove it */
     if (gGeometry) {
        TList *listOfVolume = gGeometry->GetListOfNodes();

        // Remove hall from the list of ROOT nodes to make it free of ROOT control
        listOfVolume->Remove(fVolume);
        listOfVolume->Remove(fVolume);
     }
#endif 
     // Add "hall" into ".const" area of this maker
     ((StVMCMaker *)this)->AddConst(fVolume);
     if (Debug() > 1) fVolume->ls(3);
  }
  gMessMgr->Info() << "StVMCMaker::InitRun SetMagField set as StarMagField" 
		   << " with Map: " << StarMagField::Instance()->GetMap()
		   << ",Factor: " << StarMagField::Instance()->GetFactor() 
		   << ",Rescale: " << StarMagField::Instance()->GetRescale() <<endm;
  fgStarVMCApplication->SetMagField(StarMagField::Instance());
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,1)
  fgGeant3->SetMagField(StarMagField::Instance());
#endif
  if (IAttr("VMCPassive"))  {gMessMgr->Info() << "StVMCMaker::InitRun Passive   mode" << endm;} 
  else                      {gMessMgr->Info() << "StVMCMaker::InitRun Active    mode" << endm;
    if (IAttr("Embedding")) {gMessMgr->Info() << "StVMCMaker::InitRun Embedding mode" << endm;}
    else {
      gMessMgr->Info() << "StVMCMaker::InitRun Standalone run" << endm;
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
  }
  //  fgStarVMCApplication->SetStepping(new StMCSteppingHist("tgeom"));
  //  fgStarVMCApplication->SetStepping(new StMCStepping);
  // The "Init" method in the gMC object causes the geometry to be cosntructed
  if (! fInitRun) {
    fgStarVMCApplication->InitMC();
    fInitRun = 1;
  }
  if (Debug()) SetDebug(Debug());
  if (IAttr("phys_off")) {// switch off physics 
    gMessMgr->Info() << "StVMCMaker::InitRun switch off physics" << endm;
    gMC->SetProcess("DCAY", 0);
    gMC->SetProcess("ANNI", 0);
    gMC->SetProcess("BREM", 0);
    gMC->SetProcess("COMP", 0);
    gMC->SetProcess("HADR", 0);
    gMC->SetProcess("MUNU", 0);
    gMC->SetProcess("PAIR", 0);
    gMC->SetProcess("PFIS", 0);
    gMC->SetProcess("PHOT", 0);
    gMC->SetProcess("RAYL", 0);
    gMC->SetProcess("LOSS", 4); // no fluctuations 
    //  gMC->SetProcess("LOSS 1"); // with delta electron above dcute
    gMC->SetProcess("DRAY", 0);
    gMC->SetProcess("MULS", 0);
    gMC->SetProcess("STRA", 0);
    gMC->SetCut("CUTGAM",	1e-3  );
    gMC->SetCut("CUTELE", 	1e-3  );
    gMC->SetCut("CUTHAD", 	.001  );
    gMC->SetCut("CUTNEU", 	.001  );
    gMC->SetCut("CUTMUO", 	.001  );
    gMC->SetCut("BCUTE", 	.001  );
    gMC->SetCut("BCUTM", 	.001  );
    gMC->SetCut("DCUTE", 	1e-3  );
    gMC->SetCut("DCUTM", 	.001  );
    gMC->SetCut("PPCUTM", 	.001  );
    gMC->SetCut("TOFMAX", 	50.e-6);
  }
  if (! gGeoManager->IsClosed()) {
    gGeoManager->CloseGeometry();
  }
  return kStOK;
}
//_____________________________________________________________________________
Int_t StVMCMaker::Make(){
  if (! fInitRun) InitRun(fRunNo);
  fEventNo++;
  if (fEvtHddr) {
    fEvtHddr->SetRunNumber(fRunNo);
    fEvtHddr->SetEventNumber(fEventNo);
    fEvtHddr->SetEventType("VMC");
    fEvtHddr->SetProdDateTime();
    //    SetDateTime();
  }  
  if (! IAttr("VMCPassive")) {// Active  mode 
    TStopwatch sw;
    fgStarVMCApplication->RunMC(1);
    if (Debug())   sw.Print();
  }
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
  if ( ! IAttr("Embedding") && ! IAttr("VMCPassive") && fEvtHddr ) {
    fEvtHddr->SetDateTime(id,it);
  }  
 }
//_____________________________________________________________________________
Int_t StVMCMaker::Skip(Int_t nskip) {
  return kStOk;
}
//_____________________________________________________________________________
TDataSet  *StVMCMaker::FindDataSet (const char* logInput,const StMaker *uppMk,
                                        const StMaker *dowMk) const {
  TDataSet *ds = StMaker::FindDataSet(logInput,uppMk,dowMk);
  
  if (ds || strcmp(logInput,"HALL")) return ds;
  return fVolume;

}
//_____________________________________________________________________________
int StVMCMaker::SetInputFile(const Char_t *fileName)
{
  fInputFile = fileName;
  gSystem->ExpandPathName(fInputFile);
  if (!fInputFile.Contains(".")) {
    Error("SetInputFile","File %s has no extention",fInputFile.Data());
    return 1;
  }
  if (gSystem->AccessPathName(fInputFile,kReadPermission)) {
    Error("SetInputFile","File %s is not readable",fInputFile.Data());
    return 2;
  }
  return 0;
}
//________________________________________________________________________________
void StVMCMaker::SetDebug(Int_t l) {
  m_DebugLevel = l;
  if (fgGeant3) {
    fgGeant3->Gcflag()->idebug = Debug();
    if (Debug() > 0) {
      fgGeant3->SetDEBU(1,1,100);
      fgGeant3->SetSWIT(1,2);
      fgGeant3->SetSWIT(2,2);
    } else {
      fgGeant3->SetSWIT(4,0);
    }
  }
  StarMCPrimaryGenerator *generator = fgStarVMCApplication->GetPrimaryGenerator();
  if (generator) generator->SetDebug(Debug());
  StarMCHits *hits = StarMCHits::instance();
  if (hits) hits->SetDebug(Debug());
  if (fgStarVMCApplication) fgStarVMCApplication->SetDebug(Debug());
}

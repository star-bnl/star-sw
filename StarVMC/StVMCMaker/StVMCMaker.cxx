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
   ->                    StarStack::Reset
   ->                      StarStack::Clear
   ->                  TGeant3::ProcessEvent();
   ->                    TGeant3::Gtrigi();
   ->                    TGeant3::Gtrigc();
   ->                    TGeant3::Gtrig();
   ->                     g3trig
   ->                       TGeant3gu::gukine_()
   ->                          StarVMCApplication::GeneratePrimaries();  
   ->                            StarMCSimplePrimaryGenerator::GeneratePrimaries(origin)
   ->                              StarMCSimplePrimaryGenerator::GeneratePrimary()
   ->                                StarStack::PushTrack
   ->                       TGeant3gu::gutrev()
   ->                         gtreveroot();
   ->                           TGeant3::rxgtrak();
   ->                             TVirtualMC::GetMC()->GetStack()->PopNextTrack(mtrack) => StarMCStack::PopNextTrack
   ->                               StarStack::GetNextParticle()
   ->                                 StarStack::PopPrimaryForTracking
   ->                        TGeant3gu::gutrack_()
   ->                               StarVMCApplication::Field(xdouble,bdouble); // gufld
   ->                               StarVMCApplication::PreTrack();
   ->                          g3track();
   ->                            g3hadr()
   ->                              gdecay();
   ->                        
   ->                            TGeant3gu::gustep();
   ->                                   StarStack::PushTrack
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
#include "TInterpreter.h"
#include "TPythia6Decayer.h"
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
#include "TTreeIter.h"
ClassImp(StVMCMaker);

StarVMCApplication* StVMCMaker::fgStarVMCApplication = 0;
TGeant3TGeo*        StVMCMaker::fgGeant3 = 0;
StVMCMaker *        StVMCMaker::fgGeantMk = 0;
//_____________________________________________________________________________
Int_t StVMCMaker::Init() {
  fgStarVMCApplication = new StarVMCApplication("StarVMC", "The STAR VMC application");
  fgGeant3 = (TGeant3TGeo *) TVirtualMC::GetMC();
  if (! fgGeant3) {
    fgGeant3 = new TGeant3TGeo("TGeant3TGeo");
  }
  LOG_INFO << "Init Geant3 has been created." << endm;
  fgGeant3->SetExternalDecayer(TPythia6Decayer::Instance());
  if (IAttr("VMCAlignment")) fgStarVMCApplication->DoMisAlignment(kTRUE);
  if (! IAttr("VMCPassive")) {
    LOG_INFO << "InitRun Active mode" << endm; 
    TString CintF(SAttr("GeneratorFile"));
    if (CintF != "") {
      static const Char_t *path  = ".:./StarDb/Generators:$STAR/StarDb/Generators";
      Char_t *file = gSystem->Which(path,CintF,kReadPermission);
      if (! file) Fatal("StVMCMaker::Init","File %s has not been found in path %s",CintF.Data(),path);
      else        Warning("StVMCMaker::Init","File %s has been found as %s",CintF.Data(),file);
      TString command(Form(".L %s",file));
      TInterpreter::EErrorCode ee;
      gInterpreter->ProcessLine(command,&ee);
      assert(!ee);
      TDataSet *d = (TDataSet *) gInterpreter->Calc("CreateTable()",&ee);
      assert(!ee);
      AddConst(d);
#if 0 /* Don' do this beacuse root will try to unload shared libraries in the macro */
      command.ReplaceAll(".L ",".U ");
      gInterpreter->ProcessLine(command,&ee);
      assert(!ee);
#endif
    } 
    TString MuDstF(SAttr("MuDstFile"));
    if (MuDstF != "") {
      TFile *f = TFile::Open(MuDstF);
      if (! f) {
	LOG_ERROR << MuDstF.Data() << " file has not been found" << endm;
      }
      TTree *tree = (TTree *) f->Get("MuDst");
      if (! tree) {
	LOG_ERROR << "MuDst is not found in " << MuDstF.Data() << endm;
      } else {
	fMuDstIter = new TTreeIter();
	fMuDstIter->AddFile(MuDstF);
      }
      SafeDelete(f);
    }
    StarMCPrimaryGenerator *generator = StarMCPrimaryGenerator::Instance();
    if (! generator) {
      if (fInputFile != "") generator = new StarMCHBPrimaryGenerator(fInputFile,m_DataSet);
      //                                                             Ntrack Id Ptmin Ptmax Ymin Ymax Phimin Phimax Zmin Zmax
      //  else              generator = new StarMCSimplePrimaryGenerator( 1, 5,    1.,   1.,0.1, 0.1, 0.57,  0.57,  0.,   0., "G");
      else                  generator = new StarMCSimplePrimaryGenerator(80, 6,    1.,   1.,-4.,  4.,    0,  6.28,  0.,   0., "G");
    }
    assert(generator);
    StarMCHits *hits = StarMCHits::instance();
    hits->SetHitHolder(m_DataSet);
    fgStarVMCApplication->SetStepping(hits);
  }
  fRndmSaved = gRandom;
  fRndm = new TRandom3(IAttr("RunG"));
  LOG_INFO << "Init, Generator type: TRandom3 Seed: " << fRndm->GetSeed() << endm;
  return kStOK; // StMaker::Init();
}
//_____________________________________________________________________________
Int_t StVMCMaker::InitRun  (Int_t runumber){
  if (fInitRun) return kStOK;
  fRndmSaved = gRandom;
  gRandom =  fRndm; 
  fInitRun = 1;
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
  LOG_INFO << "StVMCMaker::InitRun SetMagField set as StarMagField" 
		   << " with Map: " << StarMagField::Instance()->GetMap()
		   << ",Factor: " << StarMagField::Instance()->GetFactor() 
		   << ",Rescale: " << StarMagField::Instance()->GetRescale() <<endm;
  fgStarVMCApplication->SetMagField(StarMagField::Instance());
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,1)
  fgGeant3->SetMagField(StarMagField::Instance());
#endif
  if (IAttr("VMCPassive"))  {LOG_INFO << "StVMCMaker::InitRun Passive   mode" << endm;} 
  else                      {LOG_INFO << "StVMCMaker::InitRun Active    mode" << endm;
    if (IAttr("Embedding")) {LOG_INFO << "StVMCMaker::InitRun Embedding mode" << endm;}
    else {
      LOG_INFO << "StVMCMaker::InitRun Standalone run" << endm;
      fEvtHddr = (StEvtHddr*)GetDataSet("EvtHddr");
      if (!fEvtHddr) {                            // Standalone run
	fEvtHddr = new StEvtHddr(m_ConstSet);
	SetOutput(fEvtHddr);	                //Declare this "EvtHddr" for output
	fEvtHddr->SetRunNumber(fRunNo);
	fEvtHddr->SetEventNumber(0);
	fEvtHddr->SetEventType("VMC");
	fEvtHddr->SetProdDateTime();
      } else {
	SetAttr("Don'tTouchTimeStamp",1);
      }
    }
  }
  //  fgStarVMCApplication->SetStepping(new StMCSteppingHist("tgeom"));
  //  fgStarVMCApplication->SetStepping(new StMCStepping);
  // The "Init" method in the gMC object causes the geometry to be cosntructed
  if (IAttr("phys_off") || IAttr("hadr_off")) {// switch off hadr physics 
    LOG_INFO << "StVMCMaker::InitRun switch off hadron" << endm;
    if (IAttr("phys_off")) {
      LOG_INFO << "StVMCMaker::InitRun switch off decays" << endm;
      TVirtualMC::GetMC()->SetProcess("DCAY", 0);
    } else {
      TVirtualMC::GetMC()->SetProcess("DCAY", 1);
    }
    TVirtualMC::GetMC()->SetProcess("ANNI", 0);
    TVirtualMC::GetMC()->SetProcess("BREM", 0);
    TVirtualMC::GetMC()->SetProcess("COMP", 0);
    TVirtualMC::GetMC()->SetProcess("HADR", 0);
    TVirtualMC::GetMC()->SetProcess("MUNU", 0);
    TVirtualMC::GetMC()->SetProcess("PAIR", 0);
    TVirtualMC::GetMC()->SetProcess("PFIS", 0);
    TVirtualMC::GetMC()->SetProcess("PHOT", 0);
    TVirtualMC::GetMC()->SetProcess("RAYL", 0);
    TVirtualMC::GetMC()->SetProcess("DRAY", 1);
    TVirtualMC::GetMC()->SetProcess("MULS", 1);
    TVirtualMC::GetMC()->SetProcess("STRA", 0);
    TVirtualMC::GetMC()->SetCut("CUTGAM",	1e-3  );
    TVirtualMC::GetMC()->SetCut("CUTELE", 	1e-3  );
    TVirtualMC::GetMC()->SetCut("CUTHAD", 	.001  );
    TVirtualMC::GetMC()->SetCut("CUTNEU", 	.001  );
    TVirtualMC::GetMC()->SetCut("CUTMUO", 	.001  );
    TVirtualMC::GetMC()->SetCut("BCUTE", 	.001  );
    TVirtualMC::GetMC()->SetCut("BCUTM", 	.001  );
    TVirtualMC::GetMC()->SetCut("DCUTE", 	1e-3  );
    TVirtualMC::GetMC()->SetCut("DCUTM", 	.001  );
    TVirtualMC::GetMC()->SetCut("PPCUTM", 	.001  );
    TVirtualMC::GetMC()->SetCut("TOFMAX", 	50.e-6);
  }
  fgStarVMCApplication->InitMC();
  if (! gGeoManager->IsClosed()) {
    gGeoManager->CloseGeometry();
  }
  gRandom = fRndmSaved;
  return kStOK;
}
//_____________________________________________________________________________
Int_t StVMCMaker::Make(){
  fRndmSaved = gRandom;
  gRandom =  fRndm; 
  if (! fInitRun) InitRun(fRunNo);
  fEventNo++;
  if (fEvtHddr && ! IAttr("Don'tTouchTimeStamp")) {
    fEvtHddr->SetRunNumber(fRunNo);
    fEvtHddr->SetEventNumber(fEventNo);
    fEvtHddr->SetEventType("VMC");
    fEvtHddr->SetProdDateTime();
    //    SetDateTime();
  }  
  if (! IAttr("VMCPassive")) {// Active  mode 
    //    TStopwatch sw;
    if (fMuDstIter) {
      Int_t ok = SetVertex();
      if (ok) return ok;
    }
    // Run Generators if any
    Int_t ok = StMaker::Make();
    if (ok) return ok;
    Bool_t runOk = fgStarVMCApplication->RunMC(1);
    if (! runOk) return kStEOF;
    //    if (Debug())   sw.Print();
  }
  gRandom = fRndmSaved;
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
  StarMCPrimaryGenerator *generator = StarMCPrimaryGenerator::Instance();
  if (generator) generator->SetDebug(Debug());
  StarMCHits *hits = StarMCHits::instance();
  if (hits) hits->SetDebug(Debug());
  //  if (fgStarVMCApplication) fgStarVMCApplication->SetDebug(Debug());
}
//________________________________________________________________________________
Int_t StVMCMaker::SetVertex() {
  // Skip event 
  // 1. if vertexZ is not in the required range
  // 2. if vr = sqrt{vx^2 + vy^2} is not in the required range
  static Double_t vzlow = -6, vzhigh = +6;
  static Double_t vrMax = 1.;
  static Bool_t mSkipMode = kTRUE;
  static Bool_t mVpdVzCutMode = kTRUE;
  static Double_t dzVpdMax = 3;
  StarMCPrimaryGenerator::Instance()->UnSetVertex();
  TVector3 V;
#if 0
  //  Int_t nFound = mTree->Draw("MuEvent.mEventSummary.mNumberOfGoodPrimaryTracks:primaryVertexFlag",
  Int_t nFound = fMuDstTree->Draw("MuEvent.mEventSummary.mNumberOfGoodPrimaryTracks",
				  Form("MuEvent.mEventInfo.mRunId==%i&&MuEvent.mEventInfo.mId==%i",
				       fEvtHddr->GetRunNumber(),fEvtHddr->GetEventNumber()),
				  "goff");
  if (nFound != 1) {
    LOG_ERROR << "Run/Event = " << fEvtHddr->GetRunNumber() << "/" << fEvtHddr->GetEventNumber() 
	      << " has been found in tag file" << nFound << " times" <<  endm;
    return kStErr;
  }
  const Int_t numberOfPrimaryTracks = (Int_t) fMuDstTree->GetV1()[0];
  LOG_INFO << "Run/Event = " << fEvtHddr->GetRunNumber()
	   << "/" << fEvtHddr->GetEventNumber() 
	   << " has been found with MuEvent.mEventSummary.mNumberOfGoodPrimaryTracks = " <<  numberOfPrimaryTracks 
	   << endm;
  //	   << " and primaryVertexFlag = " << fMuDstTree->GetV2()[0]  <<  endm; 
  if (numberOfPrimaryTracks <= 0) // || fMuDstTree->GetV2()[0] )
    {
      LOG_ERROR << "reject this event" << endm;
      return kStErr;
    }
  nFound = (Int_t) fMuDstTree->Draw("MuEvent.mEventSummary.mPrimaryVertexPos.mX1:"
				    "MuEvent.mEventSummary.mPrimaryVertexPos.mX2:"
				    "MuEvent.mEventSummary.mPrimaryVertexPos.mX3:"
				    "MuEvent.mEventInfo.mTriggerMask",
				    Form("MuEvent.mEventInfo.mRunId==%i&&MuEvent.mEventInfo.mId==%i",
					 fEvtHddr->GetRunNumber(),
					 fEvtHddr->GetEventNumber()),
				    "goff");
  TVector3 V(fMuDstTree->GetV1()[0],fMuDstTree->GetV2()[0],fMuDstTree->GetV3()[0]);
  if (mSkipMode == kTRUE){
    const Double_t vr = V.Perp();
    
    if (V.Z()<vzlow || V.Z()>vzhigh || vr>=vrMax ){
      LOG_INFO << " Event " << fEvtHddr->GetEventNumber()
        << " has tags with vertex at (" << V.X() << "," << V.Y() << "," << V.Z()
        << "), vr = " << vr
        << " - out of Vz or Vr range, skipping." << endm;
      return kStSKIP;
    }
    
    LOG_INFO << " Event " << fEvtHddr->GetEventNumber()
	     << " has tags with vertex at (" << V.X() << "," << V.Y() << "," << V.Z()
	     << "), vr = " << vr
	     << " - within requested Vz and Vr range !" << endm;
  }          
#if 0  
  // more skipping. cut on trigger id.
  if (mSkipMode == kTRUE){
    LOG_INFO << "StPrepEmbedMaker::Event " << fEvtHddr->GetEventNumber()
	     << " has Triggers: " << endm;
    for (Int_t iTrg=0 ; iTrg<mSettings->nTriggerId ; iTrg++){
      LOG_INFO << fMuDstTree->GetV4()[iTrg] << " ";
    }
    LOG_INFO << endm;
    
    Bool_t fired = kFALSE;
    for (Int_t iTrg=0 ; iTrg<mSettings->nTriggerId ; iTrg++){
      for (Int_t iReqTrg=0; iReqTrg<mSettings->NReqTrg ; iReqTrg++) {
        if (fMuDstTree->GetV4()[iTrg] == mSettings->ReqTrgId[iReqTrg]){
          LOG_INFO << "StPrepEmbedMaker::Requested trigger " << mSettings->ReqTrgId[iReqTrg] << " is fired!" << endm;
          fired = kTRUE;
        }
      }
    }
    
    if (!fired && mSettings->NReqTrg>0) {
      LOG_INFO << "StPrepEmbedMaker::No requested triggers are fired in this event, skipping." << endm;
      return kStSKIP;
    }
  }
#endif
  // more skipping. cut on VpdVz in btofheader
  Float_t vpdvz;
  if(mSkipMode == kTRUE && mVpdVzCutMode == kTRUE)  {
    nFound = (Int_t) fMuDstTree->Draw("MuEvent.mVpdVz",
				      Form("MuEvent.mEventInfo.mRunId==%i&&MuEvent.mEventInfo.mId==%i",
					   fEvtHddr->GetRunNumber(),
					   fEvtHddr->GetEventNumber()),"goff");
    if (nFound != 1) {
      LOG_ERROR << "Run/Event = " << fEvtHddr->GetRunNumber() << "/" << fEvtHddr->GetEventNumber()
		<< " has been found in moretags file " << nFound << " times" <<  endm;
      return kStErr;
    }
    vpdvz = fMuDstTree->GetV1()[0];
    LOG_INFO << vpdvz << endm;
    
    //cut on events
    if( TMath::Abs(vpdvz) < 1e-7 ) {
      LOG_INFO << " Event " << fEvtHddr->GetEventNumber()
	       << " has tags with vertex at (" << V.X() << "," << V.Y() << "," << V.Z()
	       << "), VpdVz = " << vpdvz
	       << " - VpdVz is too small (i.e. no BTOF in this run), skipping." << endm;
      return kStSKIP;
    }
    if( TMath::Abs(vpdvz) >= 100. ) {
      LOG_INFO << " Event " << fEvtHddr->GetEventNumber()
	       << " has tags with vertex at (" << V.X() << "," << V.Y() << "," << V.Z()
	       << "), VpdVz = " << vpdvz
	       << " - VpdVz is too large, skipping." << endm;
      return kStSKIP;
    }
    if( TMath::Abs(V.Z()-vpdvz) > dzVpdMax ) {
      LOG_INFO << " Event " << fEvtHddr->GetEventNumber()
	       << " has tags with vertex at (" << V.X() << "," << V.Y() << "," << V.Z()
	       << "), VpdVz = " << vpdvz
	       << " - out of |Vz-VpdVz| range, skipping." << endm;
      return kStSKIP;
    }
  }
#else
  static TTreeIter &muDstIter = *fMuDstIter;
  static const Int_t*&      RunId                = muDstIter("MuEvent.mEventInfo.mRunId");
  static const Int_t*&      Id                   = muDstIter("MuEvent.mEventInfo.mId");
  static const Float_t*&    X1                   = muDstIter("PrimaryVertices.mPosition.mX1");
  static const Float_t*&    X2                   = muDstIter("PrimaryVertices.mPosition.mX2");
  static const Float_t*&    X3                   = muDstIter("PrimaryVertices.mPosition.mX3");
  static const Int_t*&      NumberOfGoodPrimaryTracks = muDstIter("MuEvent.mEventSummary.mNumberOfGoodPrimaryTracks");
  //  static const UInt_t*&     TriggerMask          = muDstIter("MuEvent.mEventInfo.mTriggerMask");
  static const Float_t*&    VpdVz                = muDstIter("MuEvent.mVpdVz");
  do {
    if (! muDstIter.Next()) {return kStEOF;}
    if (RunId[0] != fEvtHddr->GetRunNumber() &&
	Id[0]    != fEvtHddr->GetEventNumber()) continue;
    LOG_INFO << "Run/Event = " << fEvtHddr->GetRunNumber() << "/" << fEvtHddr->GetEventNumber() 
	     << " has been found in MuDst file with " << NumberOfGoodPrimaryTracks[0] << " Number Of GoodPrimary Tracks" <<  endm;
    if (NumberOfGoodPrimaryTracks[0] <= 0) {
      LOG_ERROR << "reject this event" << endm;
      return kStErr;
    }
    V = TVector3(X1[0],X2[0],X3[0]);
    if (mSkipMode == kTRUE){
      const Double_t vr = V.Perp();
      
      if (V.Z()<vzlow || V.Z()>vzhigh || vr>=vrMax ){
	LOG_INFO << " Event " << fEvtHddr->GetEventNumber()
		 << " has tags with vertex at (" << V.X() << "," << V.Y() << "," << V.Z()
		 << "), vr = " << vr
		 << " - out of Vz or Vr range, skipping." << endm;
	return kStSKIP;
      }
      
      LOG_INFO << " Event " << fEvtHddr->GetEventNumber()
	       << " has tags with vertex at (" << V.X() << "," << V.Y() << "," << V.Z()
	       << "), vr = " << vr
	       << " - within requested Vz and Vr range !" << endm;
    }        
    if (mVpdVzCutMode) {
      Double_t vpdvz =  VpdVz[0];
      LOG_INFO << "VpdVz = " << vpdvz << endm;
      //cut on events
      if( TMath::Abs(vpdvz) < 1e-7 ) {
	LOG_INFO << " Event " << fEvtHddr->GetEventNumber()
		 << " has tags with vertex at (" << V.X() << "," << V.Y() << "," << V.Z()
		 << "), VpdVz = " << vpdvz
		 << " - VpdVz is too small (i.e. no BTOF in this run), skipping." << endm;
	return kStSKIP;
      }
      if( TMath::Abs(vpdvz) >= 100. ) {
	LOG_INFO << " Event " << fEvtHddr->GetEventNumber()
		 << " has tags with vertex at (" << V.X() << "," << V.Y() << "," << V.Z()
		 << "), VpdVz = " << vpdvz
		 << " - VpdVz is too large, skipping." << endm;
	return kStSKIP;
      }
      if( TMath::Abs(V.Z()-vpdvz) > dzVpdMax ) {
	LOG_INFO << " Event " << fEvtHddr->GetEventNumber()
		 << " has tags with vertex at (" << V.X() << "," << V.Y() << "," << V.Z()
		 << "), VpdVz = " << vpdvz
		 << " - out of |Vz-VpdVz| range, skipping." << endm;
	return kStSKIP;
      }
    }
    StarMCPrimaryGenerator::Instance()->SetVertex(V);
    return kStOK;
  } while(1);
#endif
  return kStFatal;
}
//________________________________________________________________________________

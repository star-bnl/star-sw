/***************************************************************************
 *
 * $Id: StHiMicroMaker.cxx,v 1.6 2015/08/29 00:00:42 perev Exp $                                      
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  This Maker creates the highpt uDST's from StEvent
 *               for highpt Analysis.
 *
 ***************************************************************************
 *
 * $Log: StHiMicroMaker.cxx,v $
 * Revision 1.6  2015/08/29 00:00:42  perev
 * ::isnan new gcc
 *
 * Revision 1.5  2003/04/30 20:37:32  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.4  2002/05/31 21:50:14  jklay
 * Fixed the way centrality is calculated, see README
 *
 * Revision 1.3  2002/04/03 00:37:41  jklay
 * Fixed some bugs, added new version of dcaz
 *
 * Revision 1.2  2002/04/02 23:35:14  jklay
 * Added L3RichTrigger information
 *
 * Revision 1.1  2002/04/02 20:00:41  jklay
 * Bums highpt uDST Maker
 *
 *
 **************************************************************************/
#include "StHiMicroMaker.h"

#include "TMath.h"
#include "TRandom.h"

#include "StHighptPool/StHiMicroEvent/StHiMicroEvent.h"
#include "Helper.h"

#include "StIOMaker/StIOMaker.h"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "SystemOfUnits.h" // tesla...
#include "StTpcDedxPidAlgorithm.h"
#include "StuProbabilityPidAlgorithm.h"
#include "StPhysicalHelixD.hh"
#include "StuRefMult.hh"
#define PR(x) cout << "##### StHiMicroMaker: " << (#x) << " = " << (x) << endl;


// increasing order
inline bool sortCmp(StTpcHit* p1, StTpcHit* p2){
  return p1->position().perp()<p2->position().perp();
}


//    
//  The constructor. Initialize you data members here.
//    
StHiMicroMaker::StHiMicroMaker(const Char_t *makerName)
  : StMaker(makerName), mNEvent(0), mNAcceptedEvent(0),
    mNAcceptedTrack(0),
    mVertexZCut(200), mHitLoop(0)
{
}

//
//  Usually ok to leave this as it is.
//
StHiMicroMaker::~StHiMicroMaker() 
{ 
/* noop */ 
}

//
//  Called once at the beginning.
//  This is a good place to book histos and tuples.
//
Int_t StHiMicroMaker::InitRun(int runID) {
  if (mDebug) gMessMgr->Info() << "StHiMicroMaker: InitRun()" << endm;
  PR(runID);

    mIOMaker = (StIOMaker*)GetMaker("IO");
    
    if(mIOMaker) mInFileName = strrchr(mIOMaker->GetFile(),'/')+1;

    if(mDebug) cout << "##Creating StHiMicroEvent..." << endl;
    mHiMicroEvent = new StHiMicroEvent;

    Int_t stat = openFile();
    
    return stat + StMaker::Init();

//  return kStOK;
}
//---------------------------
Int_t
StHiMicroMaker::Init()
{

    cout << "###StHiMicroMaker::Init():\n";

    const char* set = (mDebug) ? "ON" : "OFF";
    cout << "\tDebug is " << set << endl;

   //Pretty much moved everything that was here to InitRun(int)

    if(mSaveAllEvents) { cout << "\t<I> Saving event info for events without a primary vertex!!" << endl; }

    return StMaker::Init();

}

//
//  Called every event after Make(). Usually you do not
//  need to do anything here. Leave it as it is.
//
void
StHiMicroMaker::Clear(Option_t *opt)
{
    StMaker::Clear();
}

//
//  Called once at the end.
//
Int_t
StHiMicroMaker::Finish()
{

  cout << "###StHiMicroMaker::Finish()\n";
  cout << "\tTotal " << mNEvent << " events." << endl;
  cout << "\tProcessed " << mNAcceptedEvent << " events." << endl;
  cout << "\tTracks : " << mNAcceptedTrack << endl << endl;
  closeFile();

  return StMaker::Finish();
}

//
//  This method is called every event. That's the
//  right place to plug in your analysis. 
//
Int_t
StHiMicroMaker::Make()
{
  mNEvent++;
	
  //
  // it it's a new file, close the old one and open a new one
  //
  TString curFileName;
  if(mIOMaker) curFileName = strrchr(mIOMaker->GetFile(),'/')+1;
  if(mInFileName!=curFileName){
    if(mDebug) {
      cout << "##New file found : " << curFileName << endl;
      cout << "##Replacing " << mInFileName << endl;
    }
    closeFile();
    mInFileName = curFileName;
    openFile();
  }

  //
  //	Get pointer to StEvent
  //dd
  StEvent* event;
  event = (StEvent *) GetInputDS("StEvent");

  if (!event) return kStOK;        // if no event, we're done
  
  //
  // filter event - if no primary vertex, reject it
  //
  //31.May.2002 - If we keep events without the vertex, we can use vertexZ position
  //from ZDC timing to assess the vertex finding efficiency - but need to skip track filling and just fill event info
  if (accept(event)) {   

    mNAcceptedEvent++;  
    // 
    // fill tracks and hits
    //
    //30.May.2002 nGoodEta is no longer used, as there is a common defintion
    //from StEvent now...
    //    Int_t nGoodEta = fillTracks(event);
    //
    // fill StHiMicroEvent
    //
    //30.May.2002 nGoodEta is no longer used, as there is a common defintion
    //from StEvent now...
    fillEvent(event);
    //
    // fill the tree
    //
    mDSTTree->Fill();

  } else {

    if(mSaveAllEvents) {  //If we really want them all...
      fillEvent(event);
      mDSTTree->Fill();
    }
  }

  mHiMicroEvent->Clear(); // clears all the clonesarrays
  return kStOK;
}
//____________________

void
StHiMicroMaker::fillEvent(StEvent* stEvent)
{
  //Set it to some really big value, so it will be cut out by relevant analyses
//  const StThreeVectorF& prVtxPos(999.,999.,999.);

  if (stEvent->primaryVertex()) {
    mHiMicroEvent->SetVertexZ(stEvent->primaryVertex()->position().z());
    mHiMicroEvent->SetVertexX(stEvent->primaryVertex()->position().x());
    mHiMicroEvent->SetVertexY(stEvent->primaryVertex()->position().y());
    mHiMicroEvent->SetOriginMult(stEvent->primaryVertex()->numberOfDaughters()); 
  } else {
    mHiMicroEvent->SetVertexZ(999.);
    mHiMicroEvent->SetVertexX(999.);
    mHiMicroEvent->SetVertexY(999.);
    mHiMicroEvent->SetOriginMult(0); 
  }
    
  // beam stuff
  if(stEvent->runInfo()){
    mHiMicroEvent->SetCenterOfMassEnergy(stEvent->runInfo()->centerOfMassEnergy());
    mHiMicroEvent->SetMagneticField(stEvent->runInfo()->magneticField());
    mHiMicroEvent->SetBeamMassNumberEast(stEvent->runInfo()->beamMassNumber(east));
    mHiMicroEvent->SetBeamMassNumberWest(stEvent->runInfo()->beamMassNumber(west));
  }
  else{
    gMessMgr->Info() << "StHiMicroMaker: no Run Info, reverting to year 1 settings "
                     << endm;
    mHiMicroEvent->SetCenterOfMassEnergy(130);
    mHiMicroEvent->SetMagneticField(4.98);
    mHiMicroEvent->SetBeamMassNumberEast(197);
    mHiMicroEvent->SetBeamMassNumberWest(197);
  }

  mHiMicroEvent->SetNUncorrectedNegativePrimaries(uncorrectedNumberOfNegativePrimaries(*stEvent));
  mHiMicroEvent->SetNUncorrectedPrimaries(uncorrectedNumberOfPrimaries(*stEvent));

  //30.May.2002 - fixed this definition to match the rest of STAR
  //same as mNuncorrectedNumberOfPrimaries
  mHiMicroEvent->SetCentMult(mHiMicroEvent->NUncorrectedPrimaries());
  mHiMicroEvent->SetCentrality(mHiMicroEvent->NUncorrectedPrimaries());

  mHiMicroEvent->SetRunId((Int_t) stEvent->runId());
  mHiMicroEvent->SetEventId((Int_t) stEvent->id());

//Get the number of Global tracks.  Not sure if this is the best way, but it will work
//JLK

  StTrack *track;
  StSPtrVecTrackNode& theNodes = stEvent->trackNodes();
  long allcnt = 0; long goodcnt = 0; long goodFlagcnt = 0;
  long goodAcnt = 0; long goodBcnt = 0; long goodCcnt = 0;
  long goodDcnt = 0; long goodEcnt = 0;

  for (unsigned int i=0; i<theNodes.size(); i++) {
     allcnt += theNodes[i]->entries(global);   
     track = theNodes[i]->track(global);
//goodGlobal checks globals against evr criteria for vertex finding
     if (goodGlobal(track)) goodcnt++;
     if (goodGlobalA(track)) goodAcnt++;
     if (goodGlobalB(track)) goodBcnt++;
     if (goodGlobalC(track)) goodCcnt++;
     if (goodGlobalD(track)) goodDcnt++;
     if (goodGlobalE(track)) goodEcnt++;
     if (goodGlobalFlag(track)) goodFlagcnt++;
  }
  mHiMicroEvent->SetNAllGlobals(allcnt);
  mHiMicroEvent->SetNGoodGlobals(goodcnt);
  mHiMicroEvent->SetNFlagGlobals(goodFlagcnt);
  mHiMicroEvent->SetNGoodGlobalsA(goodAcnt);
  mHiMicroEvent->SetNGoodGlobalsB(goodBcnt);
  mHiMicroEvent->SetNGoodGlobalsC(goodCcnt);
  mHiMicroEvent->SetNGoodGlobalsD(goodDcnt);
  mHiMicroEvent->SetNGoodGlobalsE(goodEcnt);

  //** 01/28/02 - trigger word added
  StL0Trigger* pTrigger = stEvent->l0Trigger();
  if(pTrigger){
    mHiMicroEvent->SetL0TriggerWord(pTrigger->triggerWord());
  }
  StL3Trigger* pL3Trigger = stEvent->l3Trigger();
  if(pL3Trigger){
    mHiMicroEvent->SetL3UnbiasedTrigger(pL3Trigger->l3EventSummary()->unbiasedTrigger());
    mHiMicroEvent->SetL3RichTrigger(false);

    //Now check for L3 Rich Trigger
    const StPtrVecL3AlgorithmInfo& algoInfo = pL3Trigger->l3EventSummary()->algorithmsAcceptingEvent();
    for (UInt_t i = 0; i < algoInfo.size(); i++) {
       if(algoInfo[i]->id() == 4) mHiMicroEvent->SetL3RichTrigger(true);
    }
  }
  
  // taken from flow maker
  //
  Float_t ctb  = -1., zdce = -1, zdcw = -1, zdcVertexZ = 999.;

  StTriggerDetectorCollection *triggers 
    = stEvent->triggerDetectorCollection();
  if (triggers) {
    StCtbTriggerDetector &CTB = triggers->ctb();
    StZdcTriggerDetector &ZDC = triggers->zdc();
    // get CTB
    for (UInt_t slat=0; slat<CTB.numberOfSlats(); slat++) {
      for (UInt_t tray=0; tray<CTB.numberOfTrays();tray++) {
	ctb += CTB.mips(tray,slat,0);
      }
    }
    //get ZDCe and ZDCw        
    zdce = ZDC.adcSum(east);
    zdcw = ZDC.adcSum(west);
    zdcVertexZ = ZDC.vertexZ();
  } 
  
  mHiMicroEvent->SetCTB(ctb);
  mHiMicroEvent->SetZDCe(zdce);
  mHiMicroEvent->SetZDCw(zdcw);
  mHiMicroEvent->SetZDCVertexZ(zdcVertexZ);
  
  cout << "###StHiMicroMaker::fillEvent" << endl;
  cout << "\tvertex z : " << mHiMicroEvent->VertexZ() << endl;
  cout << "\tZDC vertex z : " << mHiMicroEvent->ZDCVertexZ() << endl;
  cout << "\tmultiplicity : " << mHiMicroEvent->OriginMult() << endl;
  cout << "\tuncorrected h- : " << mHiMicroEvent->NUncorrectedNegativePrimaries() << endl;	
  cout << "\tuncorrected Nch : " << mHiMicroEvent->NUncorrectedPrimaries() << endl;	
  cout << "\tNch centrality : " << mHiMicroEvent->Centrality() << endl;

}
//____________________
Int_t
StHiMicroMaker::fillTracks(StEvent* stEvent)
{

  //
  // centrality stuff
  //
  Int_t nGoodTrackEta(0);
  Int_t nHi(0);

  //
  // create the track and hit object to temporarily store the info.
  // these will then be added to the clonesarray
  //

  StHiMicroTrack* hiMicroTrack = new StHiMicroTrack;
  StHiMicroHit*   hiMicroHit   = new StHiMicroHit;

  //
  // pid stuff for contamination
  //
  //StuProbabilityPidAlgorithm uPid(*stEvent);
  StTpcDedxPidAlgorithm tpcDedxAlgo;


  //
  // loop over primary tracks
  //
  StSPtrVecPrimaryTrack& prTracks = stEvent->primaryVertex(0)->daughters();

  for(UInt_t i=0; i<prTracks.size(); i++){
    StPrimaryTrack* prTrack = prTracks[i];
    if(!prTrack) {
      cout << "No primary?" << endl;
      continue;
    }
    //
    // need the global info as well
    //
    StGlobalTrack* glTrack = 
      dynamic_cast<StGlobalTrack*>(prTracks[i]->node()->track(global));
    if(!glTrack) {
      cout << "Error! no global information?" << endl;
      continue;
    }

    //Okay, fill it but this is no longer used as we can get the
    //info from Manuel's uncorrectedNumberOfPrimaries(*stEvent)
    //30.May.2002
    if(acceptCentrality(prTrack)) nGoodTrackEta++;

    //
    // accept the track?
    //
    if(!accept(glTrack) && !accept(prTrack)) continue;

    nHi++;
    if(mDebug){
      cout << "Accepted track" << endl;
      dump(prTrack,glTrack);
    }
    // 
    // sort the hits
    //
    
    const StPtrVecHit& hhits = glTrack->detectorInfo()->hits(kTpcId);
    Float_t firstZ(0),lastZ(0);
    Short_t innerPadList(0);
    Int_t firstPadrow(0), lastPadrow(0), outerPadList(0);
    UInt_t firstSector(0), lastSector(0);
    Double_t crossAngle(999);

    vector<StTpcHit*> vec;
    int countHits=0;
    // fill the vector
    for(UInt_t i=0; i<hhits.size(); i++){
      StTpcHit* hit = (StTpcHit*)hhits[i];
      if(!hit) continue;
      countHits++;
      vec.push_back(hit);
    }
    sort(vec.begin(),vec.end(),sortCmp);
    if(vec.size()){
      firstZ=vec[0]->position().z(); lastZ=vec[vec.size()-1]->position().z();
      firstPadrow=vec[0]->padrow(); lastPadrow=vec[vec.size()-1]->padrow();

//JLK  Do bit packing here.  Basically set a mask for each bit to turn on or off according to the padrow numbers
//     in the vec

      Short_t inner = 0;
      Int_t outer = 0;
      Int_t SET_ON;   
      for (UInt_t i=0; i < vec.size(); i++) {
        //printf("vec[%d]->padrow()=%d",i,vec[i]->padrow());
        Int_t val = vec[i]->padrow();
        if (val < 14) { //Inner sector  
          SET_ON = 1 << val;  //Moves "1" over by val places
          inner = inner | SET_ON;
        } else { 
         SET_ON = (1 << (val-14));  //Moves "1" over by val places
         outer = outer | SET_ON;
        }
        innerPadList=inner; outerPadList=outer;
      }
      firstSector=vec[0]->sector(); lastSector=vec[vec.size()-1]->sector();
      //crossAngle=crossingAngle(glTrack->helix(),vec[0],stEvent->runInfo()->magneticField());
    }
    else{ 
      if(mDebug)
	cout << "\tno hits" << endl;
      //continue; 
    }

    //
    // alias to the momentum and helix
    //
    const StThreeVectorF& prMom = prTrack->geometry()->momentum();
    const StThreeVectorF& glMom = glTrack->geometry()->momentum();
    const StPhysicalHelixD& prHelix = prTrack->geometry()->helix();
    const StPhysicalHelixD& glHelix = glTrack->geometry()->helix();
    //
    //***** fill some track info *****
    //
        
    hiMicroTrack->SetCurvPr(prTrack->geometry()->curvature());
    hiMicroTrack->SetCurvGl(glTrack->geometry()->curvature());
    hiMicroTrack->SetPtPr(prMom.perp());
    hiMicroTrack->SetEtaPr(prMom.pseudoRapidity());
    hiMicroTrack->SetPhiPr(prMom.phi());
    hiMicroTrack->SetDipAnglePr(prTrack->geometry()->dipAngle());
    
    hiMicroTrack->SetPtGl(glMom.perp());  
    hiMicroTrack->SetEtaGl(glMom.pseudoRapidity());
    hiMicroTrack->SetPhiGl(glMom.phi()); 
    hiMicroTrack->SetDipAngleGl(glTrack->geometry()->dipAngle()); 
   
    //
    // find the sign of the dca	--Could just get this from StPhysicalHelix now - Jamie added it
    //
    Float_t dcaXYGl = computeXY(stEvent->primaryVertex()->position(),
				glTrack);    

    Float_t dcaXYPr = computeXY(stEvent->primaryVertex()->position(),
				prTrack);   

    hiMicroTrack->SetDcaGl(glHelix.distance(stEvent->primaryVertex(0)->position())); 
    hiMicroTrack->SetDcaPr(prHelix.distance(stEvent->primaryVertex(0)->position()));

    hiMicroTrack->SetDcaXYGl(dcaXYGl);
    hiMicroTrack->SetDcaXYPr(dcaXYPr);

    hiMicroTrack->SetDcaZGl(dcaz(glHelix,
				 stEvent->primaryVertex()->position(),glTrack));
    
    hiMicroTrack->SetChi2(prTrack->fitTraits().chi2());
    
    //
    // dedx
    //
    StDedxPidTraits* pid=0;
    StPtrVecTrackPidTraits traits = prTrack->pidTraits(kTpcId);
    
    for (UInt_t i = 0; i < traits.size(); i++) {
      pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
      if (pid && pid->method() == kTruncatedMeanId) break;
    }
    
    hiMicroTrack->SetDedx((pid) ? pid->mean() : 0);
    hiMicroTrack->SetDedxPts((pid) ? pid->numberOfPoints() : 0);

    hiMicroTrack->SetFitPts(glTrack->fitTraits().numberOfFitPoints(kTpcId));
    hiMicroTrack->SetAllPts(glTrack->detectorInfo()->numberOfPoints(kTpcId));
    hiMicroTrack->SetMaxPossPts(glTrack->numberOfPossiblePoints());
    hiMicroTrack->SetFlag(glTrack->flag());
    hiMicroTrack->SetCharge(glTrack->geometry()->charge());

    hiMicroTrack->SetFirstZ(firstZ);
    hiMicroTrack->SetLastZ(lastZ);
    hiMicroTrack->SetFirstPadrow(firstPadrow);
    hiMicroTrack->SetInnerPadList(innerPadList);
    hiMicroTrack->SetOuterPadList(outerPadList);
    hiMicroTrack->SetLastPadrow(lastPadrow);
    hiMicroTrack->SetFirstSector(firstSector);
    hiMicroTrack->SetLastSector(lastSector);

    hiMicroTrack->SetCrossingAngle(crossAngle);
    
    //
    // add the track!
    //

    mHiMicroEvent->AddTrack(hiMicroTrack);

    // ****** find some hit info ******
    //
       
    if (!mHitLoop) continue;
      
    const StPtrVecHit& hits = prTrack->detectorInfo()->hits(kTpcId);
    for(UInt_t iHit=0 ; iHit<hits.size(); iHit++){
      const StTpcHit* hit = dynamic_cast<StTpcHit*> (hits[iHit]);
      if(!hit){
        cout << "Not a tpc hit?" << endl;
        continue;
      }
      if (!hit->usedInFit()) continue;
      //
      // repeat some info in the hits
      //
      hiMicroHit->SetPtPr(hiMicroTrack->PtPr());
      hiMicroHit->SetPtGl(hiMicroTrack->PtGl());
      hiMicroHit->SetEta(hiMicroTrack->EtaPr());
      hiMicroHit->SetPhi(hiMicroTrack->PhiPr());
      hiMicroHit->SetFitPts(hiMicroTrack->FitPts());
      hiMicroHit->SetSDcaGl(hiMicroTrack->DcaGl());
      hiMicroHit->SetDipAngle(hiMicroTrack->DipAnglePr());
      hiMicroHit->SetExitZ(hiMicroTrack->LastZ());
      hiMicroHit->SetCharge(hiMicroTrack->Charge());
      //
      // general hit info
      //
      hiMicroHit->SetR(hit->position().perp());
      hiMicroHit->SetZ(hit->position().z());   
      hiMicroHit->SetPadRow( (Int_t) hit->padrow());
      hiMicroHit->SetSector( (Int_t) hit->sector());
      //
      // residuals
      //
      Double_t sGl   = glHelix.pathLength(hit->position());
      Double_t sPr   = prHelix.pathLength(hit->position());
        
      StThreeVectorF glHitPos(glHelix.at(sGl));
      StThreeVectorF prHitPos(prHelix.at(sPr));
      
      hiMicroHit->SetZResGl(hit->position().z() - glHitPos.z());
      hiMicroHit->SetZResPr(hit->position().z() - prHitPos.z());
      
      Float_t resXYGl = computeXY(hit->position(),glTrack);
      Float_t resXYPr = computeXY(hit->position(),prTrack);
      hiMicroHit->SetXYResGl(resXYGl);
      hiMicroHit->SetXYResPr(resXYPr);
      //
      // add the hit!
      //

      mHiMicroEvent->AddHit(hiMicroHit);
    } // hit loop

  } // track loop

  if(hiMicroTrack) delete hiMicroTrack;
  cout << "\t hi pt tracks : " << nHi << endl;
  mNAcceptedTrack += nHi;

  return nGoodTrackEta;
    
}
//____________________
void
StHiMicroMaker::dump(StTrack* prTrack,StTrack* glTrack)
{
  cout << "\tptGl=" 
       << glTrack->geometry()->momentum().perp() 
       << ",etaGl=" 
       << glTrack->geometry()->momentum().pseudoRapidity() 
       << ",fitpts="
       << glTrack->fitTraits().numberOfFitPoints(kTpcId)
       << ",allpts=" 
       <<  glTrack->detectorInfo()->numberOfPoints(kTpcId)<<endl
       << "\t\t ptPr=" <<prTrack->geometry()->momentum().perp()
       << ", etaPr="
       << glTrack->geometry()->momentum().pseudoRapidity() <<endl;
}
//____________________

Int_t
StHiMicroMaker::openFile()
{
  cout << "###StHiMicroMaker::openFile()" << endl;
  
  //
  // for the output root file, replace event.root with himicro.root
  //
  TString outFileName(mInFileName);

  //
  // assume the extension is of the form ( {\..*?\.root} (perl regexp)) 
  //

  char cTemp[100];
  strcpy(cTemp,outFileName.Data());
  
  cout << "We think the filename is " << outFileName.Data() << endl;
  TString replace;
  if(strstr(cTemp,"dst.root")) replace=".dst.root";
  else if(strstr(cTemp,"event.root")) replace=".event.root";
  else { cout << "unknown extension " << endl; exit(1); }
  
  //  outFileName.Replace(outFileName.Length(),
  //		      replace.Length(),".himicro.root");
  cout << replace << endl;

  outFileName.ReplaceAll(replace.Data(),".himicro.root");

  cout << outFileName << endl;

  outFileName.Prepend(mOutDir + "/");

  // use default compression 1
  mDSTFile = new TFile(outFileName.Data(),"RECREATE");

  if(!mDSTFile){ 
    gMessMgr->Error() 
      << "Cannot create = " << outFileName << endm;
    return kStErr;
  }
  //mDSTFile->SetFormat(1); 
  cout << "\toutfile = " << outFileName << endl;

  //
  // top level tree
  //
  if(mDebug) 
    cout << "##Creating the top level tree..." << endl;
  mDSTTree = new TTree("StHiMicroTree","StHiMicroTree");
  if(!mDSTTree){
    gMessMgr->Error() 
      << "Cannot create mDSTTree" << endm;
    return kStErr;
  }
  if(mDebug)cout << "##...done" << endl;
  //#if ROOT_VERSION_CODE >= ROOT_VERSION(3,01,05)
  //  mDSTTree->SetBranchStyle(0);
  //#endif 
  mDSTTree->SetAutoSave(10000000); // 10 MB
  
  // set the event branch
  Int_t bufSZ = 64000;
  mDSTTree->Branch("StHiMicroEvent","StHiMicroEvent",
		   &mHiMicroEvent, bufSZ,1);
  return kStOk;
}
//__________________________

Int_t
StHiMicroMaker::closeFile()
{
  cout << "###StHiMicroMaker::closeFile()" << endl;
  
  cout << "##Writing " << mInFileName << endl;

  if(mDSTFile && mDSTFile->IsOpen()){
    mDSTFile->Write();
    mDSTFile->Close();
  }

  cout << "##...done\n";
  //  SafeDelete(mMiniMcTree);

  return kStOk;
}
  

Float_t
StHiMicroMaker::computeXY(const StThreeVectorF& pos, const StTrack* track)
{
  //
  // find the distance between the center of the circle and pos.
  // if this greater than the radius of curvature, then call 
  // it negative.
  //
  double xCenter = track->geometry()->helix().xcenter();
  double yCenter = track->geometry()->helix().ycenter();
  double radius  = 1.0/track->geometry()->helix().curvature();
  
  double dPosCenter 
    = TMath::Sqrt( (pos.x()-xCenter) * (pos.x()-xCenter) +
		   (pos.y()-yCenter) * (pos.y()-yCenter));

  return (Float_t) radius - dPosCenter;
}


// 
// i like this one better. dip angle in s-z plane
// should be exact assuming a circle in x-y
// (redundant StTrack* for debugging)
double StHiMicroMaker::dcaz(const StPhysicalHelixD& helix, const StThreeVectorF& point,
            const StTrack* track)  
{  
  double z0       = helix.origin().z();
  double phi      = atan2(point.y()-helix.ycenter(),
                          point.x()-helix.xcenter());
  int    h        = helix.h(); // -sign(q*B) (+ := ccw looking down from +z)
  double dphi     = h*(phi-helix.phase());

  // half circle assumption
  dphi           = (fabs(dphi) < M_PI ) ? dphi :
                    ((dphi<0) ? 2*M_PI + dphi : 2*M_PI - dphi);
   
  double arclength= (1./helix.curvature()) * dphi;
  
  double dcaZ =  (point.z() - (z0 + arclength*tan(helix.dipAngle())));
  /*
  if(gRandom->Rndm(1)<0.1){
    cout << "--------" << endl;
    cout << "zpoint=" << point.z() << endl;
    if(track)
      cout << "pt=" << track->geometry()->momentum().perp()
           << ",fit pts=" << track->fitTraits().numberOfFitPoints(kTpcId)
           << endl;
  
    cout << ">>>zdca=" << dcaZ
         << ", dphi=" << dphi*180./M_PI
         << ", z0=" << z0 << ",arclength=" << arclength << endl
         << ", dip=" << helix.dipAngle()*180./M_PI
         << ", arc/tan(dip)="<< arclength/tan(helix.dipAngle()) << endl
         << ">>>dca.z= " << point.z()-helix.at(helix.pathLength(point)).z()
         << endl;
    cout << "--------" << endl;
  }
  */
  return dcaZ;
         
}

double 
StHiMicroMaker::dcaz(const StPhysicalHelixD& helix, const StThreeVectorF& point)
{
  pairD path = helix.pathLength(point.perp());
  
  const StThreeVectorD& pos1 = helix.at(path.first);
  const StThreeVectorD& pos2 = helix.at(path.second);
  const StThreeVectorD dis1 = point - pos1;
  const StThreeVectorD dis2 = point - pos2;
  
  double dcaZ = (dis1.mag() < dis2.mag()) ? dis1.z() : dis2.z();
  if(::isnan(dcaZ)) return 999;
  return dcaZ;
}


//____________________
//
// primary vertex exists 
//
bool StHiMicroMaker::accept(StEvent* stEvent)
{

  return (stEvent->primaryVertex());
}


//____________________
//
bool StHiMicroMaker::accept(StTrack* track)
{

  return (track && track->flag() > 0 &&
	  track->geometry()->momentum().perp()>=1.5);
}

//_____________________
//
bool StHiMicroMaker::acceptCentrality(StTrack *track)
{
  return (track && track->flag() > 0 && track->fitTraits().numberOfFitPoints(kTpcId) >= 10 &&
	  fabs(track->geometry()->momentum().pseudoRapidity())<.5);
//Bum's code had 0.75, but for the newest centrality numbers from Zhangbu (Mar-2002), they use 0.5

//JLK 30.May.2002 - just noticed a problem - need to cut on fitpts >= 10 to match Zhangbu's definition!!
}

//_____________________

bool StHiMicroMaker::goodGlobal(StTrack *track) 
{
  return (track && track->flag() > 0 && track->geometry()->charge() != 0 &&
	   track->fitTraits().numberOfFitPoints(kTpcId) >= 25);

}

bool StHiMicroMaker::goodGlobalA(StTrack *track) 
{
  return (track && track->flag() > 0 && track->geometry()->charge() != 0 &&
	   track->fitTraits().numberOfFitPoints(kTpcId) >= 10);

}

bool StHiMicroMaker::goodGlobalB(StTrack *track) 
{
  return (track && track->flag() > 0 && track->geometry()->charge() != 0 &&
	   track->fitTraits().numberOfFitPoints(kTpcId) >= 15);

}

bool StHiMicroMaker::goodGlobalC(StTrack *track) 
{
  return (track && track->flag() > 0 && track->geometry()->charge() != 0 &&
	   track->fitTraits().numberOfFitPoints(kTpcId) >= 20);

}

bool StHiMicroMaker::goodGlobalD(StTrack *track) 
{
  return (track && track->flag() > 0 && track->geometry()->charge() != 0 &&
	   track->fitTraits().numberOfFitPoints(kTpcId) >= 30);

}

bool StHiMicroMaker::goodGlobalE(StTrack *track) 
{
  return (track && track->geometry()->charge() != 0 &&
	   track->fitTraits().numberOfFitPoints(kTpcId) >= 25);

}

bool StHiMicroMaker::goodGlobalFlag(StTrack *track) 
{
  return (track && track->flag() > 0 );

}
//
ClassImp(StHiMicroMaker)

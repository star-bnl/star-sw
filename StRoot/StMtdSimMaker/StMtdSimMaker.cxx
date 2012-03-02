/***************************************************************************
 *
 * $Id: StMtdSimMaker.cxx,v 1.2 2012/03/02 02:18:34 perev Exp $
 *
 * Author: Frank Geurts
 ***************************************************************************
 *
 * Description: StMtdSimMaker class for Muon Telescope Simulations
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#include <Stiostream.h>
#include "StMtdSimMaker.h"

// SCL
#include <math.h>
#include "TRandom.h"
#include "SystemOfUnits.h"
#include "phys_constants.h"
#include "StThreeVectorD.hh"
#include "Random.h"
#include "RanluxEngine.h"
#include "RandGauss.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TFile.h"

// g2t tables and collections
#include "tables/St_g2t_mtd_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "StMcTrack.hh"

#include "StEventTypes.h"
//fg #include "StEvent/StMtdCollection.h"


static RanluxEngine engine;
static RandGauss ranGauss(engine);

ClassImp(StMtdSimMaker)

	//_____________________________________________________________________________
StMtdSimMaker::StMtdSimMaker(const char *name):StMaker(name) {
  mBookHisto=kTRUE;
  mHistFile="mtdsim.root";
  mWriteStEvent=kTRUE;
  Reset();
}

//_____________________________________________________________________________
StMtdSimMaker::~StMtdSimMaker() {
  // do nothing
}


//_____________________________________________________________________________
Int_t StMtdSimMaker::Init() {  
  if(mBookHisto) bookHistograms();
  return StMaker::Init();
}

//_____________________________________________________________________________
void StMtdSimMaker::Reset() {
  mGeantData = 0;
  mEvent  = 0;
  //mBTofCollection = 0;
  if (mWriteStEvent) delete mMtdCollection;
}
//_____________________________________________________________________________
Int_t StMtdSimMaker::InitRun(Int_t runnumber) {
  // do nothing
  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdSimMaker::FinishRun(Int_t runnumber) {
  // do nothing
  return kStOk;
}

//_____________________________________________________________________________
Int_t StMtdSimMaker::Finish() {
  if(mBookHisto){
    LOG_INFO << "StMtdSimMaker::Finish  writing mtdsim.root ...";
    TFile theFile(mHistFile.c_str(),"RECREATE","mtdsim");
    theFile.cd();
    writeHistograms();
    theFile.Close();
  }

  return kStOK;
}


//_____________________________________________________________________________
Int_t StMtdSimMaker::Make() {

  // Check to see that there are GEANT hits
  mGeantData = GetInputDS("geant"); // in bfc chain
  if(!mGeantData) { // when reading the geant.root file
    mGeantData = GetInputDS("geantBranch");
  }
  if(!mGeantData) {
    LOG_WARN << " No GEANT data loaded. Exit! " << endm;
    return kStWarn;
  }
  LOG_INFO << " Found GEANT data -- loading MTD hits... " << endm;

  // Look for MTD hits
  St_g2t_mtd_hit* g2t_mtd_hits = dynamic_cast<St_g2t_mtd_hit*>(mGeantData->Find("g2t_mtd_hit"));
  if(!g2t_mtd_hits){
    LOG_WARN << " No MTD hits in GEANT" << endm; 
    return kStWarn;}

  mNMtdHits  = g2t_mtd_hits->GetNRows();
  if(!mNMtdHits){
    LOG_WARN << " No MTD hits in GEANT" << endm; 
    return kStWarn;}

  LOG_INFO << " Found MTD hits: " << mNMtdHits << endm; 

  mEvent = (StEvent*)GetInputDS("StEvent");
  mMtdHitsFromGeant = g2t_mtd_hits->GetTable();

  FastCellResponse();

  return kStOK;
}

//_____________________________________________________________________________
int StMtdSimMaker::CalcCellId(Int_t volume_id, Float_t ylocal,
                              int &ibackleg,int &imodule,int &icell) 
{

// 		Decode GEANT volume_id
  Int_t ires    = volume_id/100;
  imodule = ires%100; 
  ibackleg =(ires/100)%100;

  // Construct cell ID from local Y coordinate
  icell = Int_t((ylocal + kMtdPadWidth * kNCell/2) / kMtdPadWidth) + 1;

  // Verify ranges
  if(ibackleg<0 || ibackleg>kNBackleg) return -3;
  if(imodule <0 || imodule >kNModule ) return -2;  
  if(icell  <=0 || icell   >kNCell   ) return -1;
  
  return icell + 100*(imodule+100*ibackleg);
}


//_____________________________________________________________________________
Int_t StMtdSimMaker::FastCellResponse() 
{
  std::map<int,StMtdHit*> myMap;
  std::map<int,StMtdHit*>::const_iterator myIter;
  
  for (int ihit=0;ihit<mNMtdHits;ihit++) {
    g2t_mtd_hit_st &ghit = mMtdHitsFromGeant[ihit];
    if(ghit.s_track<=0.0 || ghit.de <=0.0) continue;

    if(mBookHisto) {
      mDeGeant->Fill(ghit.de / keV);
      mTofGeant->Fill(ghit.tof / nanosecond);
    }

    Int_t icell, imodule, ibackleg;
    int cellId = CalcCellId(ghit.volume_id, ghit.x[1],ibackleg,imodule,icell);
    if (cellId<0) continue;
    StMtdHit *&sthit = myMap[cellId];
    if (!sthit) { sthit = new StMtdHit;}
    else        { if (sthit->tof()>ghit.tof) continue; }
    Float_t wt = 1.0;

//fg		Double_t tof= mtdHitsFromGeant->tof*1000./nanosecond + ranGauss.shoot()*mSimDb->timeres_tof()*1000./nanosecond;    //! 85ps per channel
//fg 		temporarily fix MTD resolution at 99ps
    Double_t tof= ghit.tof*1000./nanosecond + ranGauss.shoot()*99*1000./nanosecond;
    Double_t t0 = ghit.tof*1000./nanosecond;
    Double_t de = ghit.de * wt;
    Double_t pathL = ghit.s_track;
    Double_t q = 0.;
    
    sthit->setBackleg(ibackleg);
    sthit->setModule( imodule );
    sthit->setCell(   icell   );
    sthit->setLeadingEdgeTime(pair<double,double>(tof, tof));
    sthit->setTrailingEdgeTime(pair<double,double>(tof, tof));
    sthit->setAssociatedTrack(NULL);		//done in StMtdMatchMaker
    sthit->setIdTruth(ghit.track_p, 100);
  }

  mMtdCollection= new StMtdCollection();
  for (myIter=myMap.begin(); myIter!=myMap.end(); ++myIter) {
    mMtdCollection->addHit((*myIter).second);
  }
  if (mEvent) {
    mEvent->setMtdCollection(mMtdCollection);
    LOG_INFO << "... StMtdCollection Stored in StEvent! " << endm;
  }
}


Int_t StMtdSimMaker::bookHistograms()
{
  //only done if Histogram setting is turned on
  mBetaHist=new TH1F("mBetaHist","mBetaHist", 100, -2, 2);
  mPathLHist=new TH1F("mPathLHist","mPathLHist", 100, -2, 500);//cm's
  mTofHist=new TH1F("mTofHist","mTofHist", 1000, -10, 10000);
  mRecMass=new TH1F("mRecMass","mRecMass", 1000, -2, 4);

  mCellGeant  = new TH2F("CellGeant","CellGeant",192,0.,192.,120,1.,120.);
  mNCellGeant = new TH2F("NCellGeant","NCellGeant",192,0.,192.,120,1.,120.);   
  mDeGeant    = new TH1F("DeGeant","DeGeant",1000,0.,10.);      //! 10 keV
  mTofGeant   = new TH1F("TofGeant","TofGeant",1000,0.,20.);    //! 20 ns

  mCellSeen   = new TH2F("CellSeen","CellSeen",192,0.,192.,120,1.,120.);
  mNCellSeen  = new TH2F("NCellSeen","NCellSeen",192,0.,192.,120,1.,120.);
  mDeSeen     = new TH1F("DeSeen","DeSeen",1000,0.,10.);        //! 10 kev
  mT0Seen    = new TH1F("T0Seen","T0Seen",1000,0.,20.);      //! ns
  mTofSeen    = new TH1F("TofSeen","TofSeen",1000,0.,20.);      //! 20 ns

  mTofResSeen = new TH1F("TofResSeen","TofResSeen",1001,-500.,500.);//! ps

  mCellReco   = new TH2F("CellReco","CellReco",192,0.,192.,120,1.,120.);
  mNCellReco  = new TH2F("NCellReco","NCellReco",192,0.,192.,120,1.,120.);
  mADCReco    = new TH1F("ADCReco","ADCReco",4096,0.,4096.);
  mTDCReco    = new TH1F("TDCReco","TDCReco",4096,0.,4096.);
  mT0Reco   = new TH1F("T0Reco","T0Reco",1000,0.,20.);      //! ns
  mTofResReco = new TH1F("TofResReco","TofResReco",1000,-300.,300.);//ps
  mTACorr     = new TH2F("TACorr","TACorr",512,0.,4096.,512,0.,4096.);

  mModHist     = new TH1F("ModuleHist","ModuleHist",201,-100,100);
  return kStOk;

}

//_____________________________________________________________________________
Int_t StMtdSimMaker::writeHistograms()
{
  //only done if Histogram setting is turned on

  mBetaHist->Write();
  mPathLHist->Write();
  mTofHist->Write();
  mRecMass->Write();

  mCellGeant->Write();
  mNCellGeant->Write();
  mDeGeant->Write();   
  mTofGeant->Write();

  mCellSeen->Write();
  mNCellSeen->Write();
  mDeSeen->Write();
  mT0Seen->Write();
  mTofSeen->Write();
  mTofResSeen->Write();

  mCellReco->Write();
  mNCellReco->Write();
  mADCReco->Write();
  mTDCReco->Write();
  mT0Reco->Write();
  mTofResReco->Write();
  mTACorr->Write();

  mModHist->Write();
  return kStOk;
}

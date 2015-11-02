/***************************************************************************
 *
 * $Id: StMtdSimMaker.cxx,v 1.13 2015/11/02 18:19:46 marr Exp $
 *
 * Author: Frank Geurts
 *
 * Modified: Yi Yang (yiyang@bnl.gov) 
 * Date: 2014-05-06
 *
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
#include "tables/St_mtdGeant2BacklegIDMap_Table.h"
#include "StMcTrack.hh"

#include "StEventTypes.h"

static RanluxEngine engine;
static RandGauss ranGauss(engine);

Int_t geant2backlegIDMap[30]; 

ClassImp(StMtdSimMaker)

const float StMtdSimMaker::kMtdPadWidth = 3.8 + 0.6; 	//! Pad Width: 38mm padwidth + 6mm innerspacing

//_____________________________________________________________________________
StMtdSimMaker::StMtdSimMaker(const char *name):StMaker(name),
  mGeantData(0),
  mEvent(0),
  mMtdCollection(0),
  mNMtdHits(0),
  mMtdHitsFromGeant(0),

  mBetaHist(0),
  mPathLHist(0),
  mTofHist(0),
  mRecMass(0),

  mCellGeant(0),
  mVpdGeant(0),
  mNCellGeant(0),
  mNVpdGeant(0),
  mDeGeant(0),
  mTofGeant(0),

  mCellSeen(0),
  mVpdSeen(0), 
  mNCellSeen(0), 
  mNVpdSeen(0),
  mDeSeen(0),
  mT0Seen(0),
  mTofSeen(0), 
  mTofResSeen(0),
  mVpdResSeen(0),

  mCellReco(0),
  mVpdReco(0),
  mNCellReco(0), 
  mNVpdReco(0), 
  mTDCReco(0), 
  mADCReco(0),
  mT0Reco(0), 
  mTofResReco(0),
  mVpdResReco(0),
  mTACorr(0), 
  mModHist(0), 
  QABacklegChannel(0),

  /// TOFp histograms
  mdE(0),
  mdS(0),
  mNumberOfPhotoelectrons(0),
  mT(0), 
  mTime(0), 
  mTime1(0), 
  mPMlength(0),
  mAdc(0), 
  mTdc(0),

  starHall(0) {

  memset(mModuleChannel,0,5*24*sizeof(Int_t));

  mBookHisto=kTRUE;
  mWriteHisto=kFALSE;
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
  if (mWriteStEvent) delete mMtdCollection;
}
//_____________________________________________________________________________
Int_t StMtdSimMaker::InitRun(Int_t runnumber) {

    // Load geant2backlegID Map 
    // Extract MTD maps from database
    LOG_INFO << "Retrieving geant2backlegID table from database ..." << endm;
    TDataSet *dataset = GetDataBase("Geometry/mtd/mtdGeant2BacklegIDMap");
    St_mtdGeant2BacklegIDMap *mtdGeant2BacklegIDMap = static_cast<St_mtdGeant2BacklegIDMap*>(dataset->Find("mtdGeant2BacklegIDMap"));
    if ( !mtdGeant2BacklegIDMap ){
        LOG_ERROR << "No mtdTrayToTdigMap table found in database" << endm;
        return kStErr;
    }
    mtdGeant2BacklegIDMap_st *mGeant2BLTable = static_cast<mtdGeant2BacklegIDMap_st*>(mtdGeant2BacklegIDMap->GetTable());
    for ( Int_t i = 0; i < 30; i++ ){
        geant2backlegIDMap[i] = 0;
        geant2backlegIDMap[i] = (Int_t)mGeant2BLTable->geant2backlegID[i];
    }


  /// Channel group mapping for each module. Basic initialization below. Will move to database eventually.
  int channel(0);
  for (int module=0; module<5; module++){
    for (int cell=0; cell<24; cell ++){
      mModuleChannel[module][cell] = ++channel;
    }
  }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdSimMaker::FinishRun(Int_t runnumber) {
  // do nothing
  return kStOk;
}

//_____________________________________________________________________________
Int_t StMtdSimMaker::Finish() {
  if(mWriteHisto){
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

  /// Check to see that there are GEANT hits
  mGeantData = GetInputDS("geant"); // in bfc chain
  if(!mGeantData) { // when reading the geant.root file
    mGeantData = GetInputDS("geantBranch");
  }
  if(!mGeantData) {
    LOG_WARN << " No GEANT data loaded. Exit! " << endm;
    return kStWarn;
  }
  LOG_INFO << " Found GEANT data -- loading MTD hits... " << endm;

  /// Look for MTD hits
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

///This will calculate the cell ID as well as decode the module # and backleg # to store in an MTD Collection. Done from volume_id produced by GEANT.
int StMtdSimMaker::CalcCellId(Int_t volume_id, Float_t ylocal, int &ibackleg,int &imodule,int &icell) 
{
    ///Decode GEANT volume_id
    Int_t backlegTemp;
    Int_t ires    = volume_id/100;
    backlegTemp = ires/10;    /// This is the backleg number produced by GEANT, does not match value from geometry
    ibackleg = geant2backlegIDMap[backlegTemp - 1];  /// This is the corrected backleg number, will be stored in sthit       

    imodule = ires%10;        ///module number, i.e. where the module falls in a 5 tray backleg
    if ( ibackleg >= 12 && ibackleg <= 20 ) imodule = imodule + 1;

    /// Construct cell ID from local Y coordinate
    icell = Int_t((ylocal + kMtdPadWidth * kNCell/2) / kMtdPadWidth);
    // Get the correct cell ID
    if ( imodule > 3 ) icell = 11 - icell;

    /// Verify ranges
    if(ibackleg<0 || ibackleg>kNBackleg) return -3;
    if(imodule <0 || imodule >kNModule ) return -2;  
    if(icell  <0  || icell   >= kNCell ) return -1;

    return icell + 100*(imodule+100*ibackleg);
}


///This will define the maps to store the StMtdHits and will call the necessary functions to obtain the cell, module and backleg #s to
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

    LOG_DEBUG << "Volume ID: "<< ghit.volume_id << "  Cell ID: " << cellId << endm;
    LOG_DEBUG << " icell: " << icell << " imodule: " << imodule << " ibackleg: " << ibackleg << " ghit.tof: " << ghit.tof << endm;
    LOG_DEBUG << " track: " << ghit.track_p << endm;

    if (cellId<0) continue;
    StMtdHit *&sthit = myMap[cellId];
    if (!sthit) { sthit = new StMtdHit;}
    else        { if (sthit->tof()>ghit.tof) continue; }
    Float_t wt = 1.0;

    //fg temporarily fix MTD resolution at 99ps
    Double_t tof= ghit.tof/nanosecond + ranGauss.shoot()*(99.e-12)/nanosecond;
    Double_t de = ghit.de * wt;
   
    // Calculate MTD leading and tailing edge time
    // Assume the velocity = 56 ps/cm, module length = 87 cm
    double vel = 56.e-3; // ns/cm
    double leadingW = ghit.tof/nanosecond - ghit.x[2]*vel;
    double leadingE = ghit.tof/nanosecond + ghit.x[2]*vel;
    double trailingW = leadingW + 15;
    double trailingE = leadingE + 15;

    int channel1 = mModuleChannel[imodule - 1][icell];   /// uses the module and the cell id to find the first channel on RDO
    int channel2 = channel1 + 12;                        /// uses the module and the cell id to find the second channel on RDO

    LOG_INFO << "First hit for this cell, assigning channel 1: " << channel1 << " channel 2: " << channel2 << endm;

    //Fill Histograms here AJ////
    QABacklegChannel->Fill(ibackleg, channel1);
    QABacklegChannel->Fill(ibackleg, channel2);
    
    //mCellGeant->Fill(icell);
    mDeGeant->Fill(ghit.de);
    mTofGeant->Fill(ghit.tof);	
    
    //mCellSeen->Fill(cellId);
    mDeSeen->Fill(de);
    mTofSeen->Fill(tof);
    //////////////

    sthit->setBackleg(ibackleg);
    sthit->setModule(imodule);
    sthit->setCell(icell);
    sthit->setLeadingEdgeTime(pair<double,double>(leadingW, leadingE));
    sthit->setTrailingEdgeTime(pair<double,double>(trailingW, trailingE));
    sthit->setAssociatedTrack(NULL);		//done in StMtdMatchMaker
    sthit->setIdTruth(ghit.track_p, 100);

    LOG_INFO << "sthit " << sthit->tof() << " tof:" << tof << endm;
  }

  int nRealHits = 0;
  if (mEvent) 
    {
      mMtdCollection = mEvent->mtdCollection();
      if(!mMtdCollection)
	{
	  mMtdCollection= new StMtdCollection();
	  mEvent->setMtdCollection(mMtdCollection);
	}
      nRealHits = mMtdCollection->mtdHits().size();
    }
  else mMtdCollection= new StMtdCollection();

  int nMcHits = 0;
  for (myIter=myMap.begin(); myIter!=myMap.end(); ++myIter) {
    mMtdCollection->addHit((*myIter).second);
    nMcHits++;
  }
  if (mEvent) {
    LOG_INFO << "... " << nMcHits << " MC hits stored in StEvent with " << nRealHits << " real hits! " << endm;
  }

  return kStOK;
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
  QABacklegChannel = new TH2I("QABacklegChannel", "QABacklegChannel", 30, 1., 30., 120, 1., 120.); 
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
  QABacklegChannel->Write();
  return kStOk;
}

// *-- Author : Piotr A. Zolnierczuk
// $Id: EETowCompatMatchMaker.cxx,v 1.1.1.1 2003/12/18 18:00:54 zolnie Exp $

#include "TFile.h"
#include "TTree.h"
#include "TH1F.h"


#include "EETowCompatMatchMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"



#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"

#include "StEvent/StTrackTopologyMap.h"
#include "StEvent/StRunInfo.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include "StEEmcUtil/StEEmcSmd/StEEmcSmdGeom.h"

#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/StEEmcDbIndexItem1.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"


#include "EEmcTowerHit.h"


ClassImp(EETowCompatMatchMaker)


EETowCompatMatchMaker::EETowCompatMatchMaker(const char* self            , // this maker name
					   StMuDstMaker  *mumaker,
					   StEEmcDbMaker *dbmaker
) : StMaker(self),mMuDstMaker(mumaker),mEEmcDb(dbmaker) {

// get the main maker

  //if( (mMuDstMaker = (StMuDstMaker *)GetMaker(muDstMakerName)) == NULL )  
  if( mMuDstMaker == NULL )  
      Fatal("EETowCompatMatchMaker","invalid StMuDstMaker");
  
  // get the EEMC database
  //if( (mEEmcDb = (StEEmcDbMaker*)GetMaker(eemcDbMaker)) == NULL ) 
  if( mEEmcDb == NULL ) 
    Fatal("EETowCompatMatchMaker","invalid StEEmcDbMaker");
  
  // simple EEMC geometry description
  if( (mGeom = new EEmcGeomSimple()) == NULL) 
    Fatal("EETowCompatMatchMaker","cannot create EEmcGeomSimple class");

  mDebugLevel   = kWarning;

  mFileName  = TString(GetName());
  mFileName.ToLower();
  mFileName += ".root";
  mFile=NULL;
  mTree=NULL;

  mMatch=NULL;

  //InitTree();
}


EETowCompatMatchMaker::~EETowCompatMatchMaker(){
  // ReleaseTree(); 

  if(mTree !=NULL) delete mTree;
  if(mFile !=NULL) delete mFile;
  if(mMatch!=NULL) delete mMatch;
  if(mGeom !=NULL) delete mGeom;
}


//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t 
EETowCompatMatchMaker::Init(){
  PrintInfo();

  mMatch= new struct CompatTTM;  if(!mMatch) return kStErr;

  mFile = new TFile(mFileName, "RECREATE");   if(!mFile) return kStErr;
  mTree = new TTree("track","MuDST tracks");  if(!mTree) return kStErr;

  (void)mTree->Branch("ntracks" ,&(mMatch->numtracks),"numtracks/I");

  (void)mTree->Branch("sec"     , mMatch->sector,"sec[numtracks]/I");
  (void)mTree->Branch("ssec"    , mMatch->subsec,"ssec[numtracks]/I");
  (void)mTree->Branch("eta"     , mMatch->etabin,"eta[numtracks]/I");
  (void)mTree->Branch("adc"     , mMatch->adcval,"adc[numtracks]/F");

  (void)mTree->Branch("pt"      , mMatch->pt    ,"pt[numtracks]/F");
  (void)mTree->Branch("ptot"    , mMatch->ptot  ,"ptot[numtracks]/F");
  (void)mTree->Branch("nhits"   , mMatch->nhits ,"nhits[numtracks]/I");
  (void)mTree->Branch("length"  , mMatch->length,"length[numtracks]/F");
  (void)mTree->Branch("dedx"    , mMatch->dedx  , "dedx[numtracks]/F");

  (void)mTree->Branch("xvert"   , mMatch->xvert ,"xvert[numtracks]/F");
  (void)mTree->Branch("yvert"   , mMatch->yvert ,"xvert[numtracks]/F");
  (void)mTree->Branch("zvert"   , mMatch->zvert ,"xvert[numtracks]/F");

  (void)mTree->Branch("xsmd"    , mMatch->xsmd  , "xsmd[numtracks]/F");
  (void)mTree->Branch("ysmd"    , mMatch->ysmd  , "ysmd[numtracks]/F");
  (void)mTree->Branch("etasmd"  , mMatch->etasmd, "etasmd[numtracks]/F");
  (void)mTree->Branch("phismd"  , mMatch->phismd, "phismd[numtracks]/F");
  
  (void)mTree->Branch("detasmd" , mMatch->detasmd, "detasmd[numtracks]/F");
  (void)mTree->Branch("dphismd" , mMatch->dphismd, "dphismd[numtracks]/F");

  (void)mTree->Branch("detapres", mMatch->detapres, "detapres[numtracks]/F");
  (void)mTree->Branch("dphipres", mMatch->dphipres, "dphipres[numtracks]/F");

  (void)mTree->Branch("detapost", mMatch->detapost, "detapost[numtracks]/F");
  (void)mTree->Branch("dphipost", mMatch->dphipost, "dphipost[numtracks]/F");

  (void)mTree->Branch("ntrig"   ,&(mMatch->numtrig),"numtrig/I");
  (void)mTree->Branch("trigid"  , mMatch->trigid  ,"trigid[numtrig]/I");
  (void)mTree->Branch("daqbits" ,&(mMatch->daqbits),"daqbits/I");


  //TDirectory *dirHist = 
  mFile->mkdir("histos");
  mFile->cd("histos");
  // remove magic constants later
  hTrackNHits = new TH1F("hTrankNHits","hits/track"         ,100,  0.0,100  );
  hTrackLen   = new TH1F("hTrackLen"  ,"track length [cm]"  ,500,  0.0,500.0);
  hTrackPt    = new TH1F("hTrackPt"   ,"p_T   [GeV]"        ,500,  0.0,  5.0);
  hTrackPtot  = new TH1F("hTrackPtot" ,"p_tot [GeV]"        ,500,  0.0,  5.0);

  hTrackDCAX  = new TH1F("hTrackDCAX" , "x_vtxdca [cm]"     ,200,- 50.0, 50.0);
  hTrackDCAY  = new TH1F("hTrackDCAY" , "y_vtxdca [cm]"     ,200, -50.0, 50.0);
  hTrackDCAZ  = new TH1F("hTrackDCAZ" , "z_vtxdca [cm]"     ,200,  -5.0,  5.0);

  mFile->cd("");
  
  return StMaker::Init();
}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t 
EETowCompatMatchMaker::Make(){

  bzero(mMatch,sizeof(struct CompatTTM));
  int &nt =  mMatch->numtracks;

  StMuDst   *muDst  = mMuDstMaker->muDst();

  if(muDst==NULL) { 
    if(mDebugLevel>=kWarning) Warning("Make","%s aborted, muDST maker is missing",GetName());
    return kStErr;
  }

  if(mEEmcDb->valid()<=0) {
    if(mDebugLevel>=kWarning) Warning("Make","%s aborted, missing EEMC Db records",GetName());
    return kStErr;
  }

  // real work begins here
  TClonesArray      *tracks = muDst->primaryTracks();   // fetch primary tracks
  if (!tracks) { 
    if(mDebugLevel>=kInfo) Info("Make","no tracks for this event");
    return kStWarn;
  }

  StMuEmcCollection *emc    = muDst->emcCollection();   // fetch endcap data
  if (!emc) {
    if(mDebugLevel>=kInfo) Info("Make","no EMC data for this event");
    return kStWarn;
  }

  // select "good" tracks
  TList      goodTracks;
  TIter      nextTrack(tracks);
  StMuTrack *track  = NULL;

  while ( (track = (StMuTrack *)nextTrack()) ) {
    StThreeVectorF p  =track->p();
    StThreeVectorF dca=track->dca();
    
    hTrackNHits->Fill(track->nHitsFit());
    hTrackLen  ->Fill(track->lengthMeasured());
    hTrackPt   ->Fill(track->pt());
    hTrackPtot ->Fill(p.mag());
    
    hTrackDCAX ->Fill(dca.x());
    hTrackDCAY ->Fill(dca.y());
    hTrackDCAZ ->Fill(dca.z());


    if ( ! accept(track) ) continue;
    goodTracks.Add(track);

    mMatch->nhits[nt]  = track->nHitsFit();
    mMatch->pt[nt]     = track->pt();
    mMatch->ptot[nt]   = track->pt()*TMath::CosH(track->eta());    // for now
    mMatch->length[nt] = track->length();
    mMatch->dedx[nt]   = track->dEdx();
    nt++;  

  }

  // WARNING goodTracks leak????
  // the messageology in this (St/Root/...) enterprise awaits Ig-Nobel prize
  if(goodTracks.IsEmpty()) return kStOK; 

#if 0
  TCollection goodTowerHits;
  for (Int_t i=0; i< emc->getNEndcapTowerADC(); i++) {
    EEmcTowerHit *tower = NULL;
    if(! accept(emc,i,towerHit) ) continue;
    goodTowerHits.Add(towerHit);
    hTowerAdc[tower->sec][tower->sub][tower->eta]->Fill(tower->edep);
  }
#endif

  // access geometry info
  //float etaCenter     =mGeom->getEtaMean(ieta);
  //float phiCenter     =mGeom->getPhiMean(isec,isub);
  //TVector3 r= mGeom-> getTowerCenter(isec, isub,ieta);

  if(nt>0)  mTree->Fill();
  return kStOK;
}

//_____________________________________________________________________________
/// Clear 
void
EETowCompatMatchMaker::Clear(Option_t *option ) {
  //TString opt = option;
  //opt.ToLower();
  //if(opt.Contains("A")) { doSth() }  elseif (opt.Contains("B")) { doSthElse() };
  StMaker::Clear();
}

//_____________________________________________________________________________
/// Finish 
Int_t 
EETowCompatMatchMaker::Finish () {


  if(mFile)  { 
    mFile->Write();

    hTrackNHits->Write();
    hTrackLen->Write();
    hTrackPt->Write();
    hTrackPtot->Write();
    
    hTrackDCAX->Write();
    hTrackDCAY->Write();
    hTrackDCAZ->Write();
   
  }
  return kStOK;
}


// overloadable
Bool_t 
EETowCompatMatchMaker::accept(StMuTrack *track) {
  const double minTrackLength = 20.0;
  const double minTrackPt     =  0.5;
  const Int_t  minTrackHits   =  5;


  if( track->flag()     <= 0              ) return kFALSE;
  if(!track->topologyMap().trackTpcOnly() ) return kFALSE;
  if( track->nHitsFit() <  minTrackHits   ) return kFALSE;
  if( track->length()   <  minTrackLength ) return kFALSE;
  if( track->pt()       <  minTrackPt     ) return kFALSE;
  
  return kTRUE;
}



#if 0
Bool_t
EETowCompatMatchMaker::accept(StMuEmcCollection *emc, Int_t i, EEmcTowerHit *towerHit) 
{
  if (!emc) return kFALSE;

  // back to Fortran++
  // get endcap hit(s) and use dbase to subtract pedestal and apply gain
  int adc,s,ss,e;
  emc->getEndcapTowerADC(i,adc,s,ss,e); 
  if (adc<=0) return kFALSE;          // how about zero suppression :))

  towerHit = new EEmcTowerHit(s,ss,e,adc,float(adc));
  return kTRUE;
}
  
#endif


// $Log: EETowCompatMatchMaker.cxx,v $
// Revision 1.1.1.1  2003/12/18 18:00:54  zolnie
// Imported sources
//
// Revision 1.1.1.1  2003/12/15 22:48:47  zolnie
// Imported sources
//







#if 0


  edep = float(adc);
  switch(mAdcMode) {
  case kRawAdc:
    break;
  case kPedSub:
    edep -= dbi->ped;
    break;
  case kPedAndGain:
    edep -= dbi->ped;
    edep /= dbi->gain;
    break;
  default:
    break;
  }
  edep = adc - dbi->ped;
  if(mADCmodedbi->gain>0.0 && edep /= dbi->gain
     if(edep<0.0) continue;
     


  const StEEmcDbIndexItem1 *dbi = mEEmcDb->getT(s+1,ss+'A',e+1); // fortran scheiss .... 
  if(dbi==NULL) return kFALSE; 


// do the matching
TVector3 tc = mGeom->getTowerCenter(sec,sub,eta);
    TListIter next(&acceptedTracks);
    while( ( track = (StMuTrack *)next()) ) {
      StPhysicalHelixD helix = track->helix();
      StThreeVectorD   thit;
      if(helix.dipAngle()>1e-13) { 
	double s  = ( kEEmcZSMD - helix.origin().z() ) / sin( helix.dipAngle())  ;
	thit = helix.at(s);
	
      }
}
    
#endif

// *-- Author : Piotr A. Zolnierczuk, Indiana University Cyclotron Facility
// *-- Date   : 2003/12/08 
// $Id: EEmcTTMMaker.cxx,v 1.3 2004/01/06 22:42:55 zolnie Exp $
// see README for Info


#include <iostream>

#include "TFile.h"
#include "TTree.h"
#include "TH1F.h"


#include "EETowTrackMatchMaker.h"

#include "StChain.h"
#include "St_DataSetIter.h"

#include "StIOMaker/StIOMaker.h"

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

#if !defined(ST_NO_NAMESPACES)
using std::map;
using std::ostream;
#endif


ClassImp(EETowTrackMatchMaker)



const Int_t    EETowTrackMatchMaker::kDefMinTrackHits   =  5; 
const Double_t EETowTrackMatchMaker::kDefMinTrackLength = 20.0;
const Double_t EETowTrackMatchMaker::kDefMinTrackPt     =  0.5;



//_____________________________________________________________________________
EETowTrackMatchMaker::EETowTrackMatchMaker(
					   const char* self      , // this maker name
					   StMuDstMaker  *mumaker,
					   StEEmcDbMaker *dbmaker
					   ) 
  : StMaker(self),mMuDstMaker(mumaker),mEEmcDb(dbmaker) {

  if( mMuDstMaker == NULL )  
    Fatal("EETowTrackMatchMaker","invalid StMuDstMaker");
  
  if( mEEmcDb == NULL ) 
    Fatal("EETowTrackMatchMaker","invalid StEEmcDbMaker");
  
  // simple EEMC geometry description
  if( (mGeom = new EEmcGeomSimple()) == NULL) 
    Fatal("EETowTrackMatchMaker","cannot create EEmcGeomSimple class");

  //mDebugLevel   = kWarning;

  mFileName = TString(GetName());
  mFileName.ToLower();
  mFileName += ".root";
  mFile=NULL;
  mTree=NULL;

  mMatch  =NULL;


  // InitCuts()

  ResetZPositionsArray();
  AddZPosition("pres",kEEmcZPRE1+0.1);
  AddZPosition("post",kEEmcZPOST-0.1);
  AddZPosition("smd" ,kEEmcZSMD);

  // cuts
  mMinTrackHits   = kDefMinTrackHits;
  mMinTrackLength = kDefMinTrackLength;
  mMinTrackPt     = kDefMinTrackPt;
  
  //
  mPhiFac = 1.0;
  mEtaFac = 1.0;

  //
  ResetStats();

}

//_____________________________________________________________________________
EETowTrackMatchMaker::~EETowTrackMatchMaker(){
  if(mTree !=NULL) delete mTree;
  if(mFile !=NULL) delete mFile;
  if(mMatch!=NULL) delete mMatch;
  if(mGeom !=NULL) delete mGeom;
}


//_____________________________________________________________________________
Int_t 
EETowTrackMatchMaker::Init(){

  ResetStats();

  mMatch= new NTupleTTM_t;  if(!mMatch) return kStErr;

  mFile = new TFile(mFileName, "RECREATE");   if(!mFile) return kStErr;
  mTree = new TTree("track","MuDST tracks");  if(!mTree) return kStErr;

  (void)mTree->Branch("ntracks" ,&(mMatch->numtracks),"numtracks/I");

  (void)mTree->Branch("sec"     , mMatch->sector,"sec[numtracks]/I");
  (void)mTree->Branch("ssec"    , mMatch->subsec,"ssec[numtracks]/I");
  (void)mTree->Branch("eta"     , mMatch->etabin,"eta[numtracks]/I");
  (void)mTree->Branch("adc"     , mMatch->adc   ,"adc[numtracks]/F");
  (void)mTree->Branch("edep"    , mMatch->edep  ,"edep[numtracks]/F");

  (void)mTree->Branch("pt"      , mMatch->pt    ,"pt[numtracks]/F");
  (void)mTree->Branch("ptot"    , mMatch->ptot  ,"ptot[numtracks]/F");
  (void)mTree->Branch("nhits"   , mMatch->nhits ,"nhits[numtracks]/I");
  (void)mTree->Branch("length"  , mMatch->length,"length[numtracks]/F");
  (void)mTree->Branch("dedx"    , mMatch->dedx  ,"dedx[numtracks]/F");


  (void)mTree->Branch("numz"   ,&(mMatch->numz) , "numz/I");
  (void)mTree->Branch("zpos"   ,  mMatch->zpos  , "zpos[numz]/F");

  map<double,TString>::const_iterator zpos; 
  unsigned k=0; 
  for(zpos=mZ.begin(),k=0; zpos!=mZ.end() ; ++zpos, k++) {
    if(k>=kNTupleTTM_MaxZ) { 
      Warning("Init","too many z positions: %s %g will be used but not written",zpos->second.Data(),zpos->first);
      continue;
    }
    const TString deta = "deta";
    const TString dphi = "dphi";
    const TString ntra = "[numtracks]/F";
    TString bEtaName = deta + zpos->second;
    TString bEtaDef  = deta + zpos->second + ntra;
    TString bPhiName = dphi + zpos->second;
    TString bPhiDef  = dphi + zpos->second + ntra;

    //Info("Init","Adding branches %s/%s (test match at z=%g)",bEtaName.Data(),bPhiName.Data(),zpos->second);
    (void)mTree->Branch(bEtaName, mMatch->deta[k], bEtaDef);
    (void)mTree->Branch(bPhiName, mMatch->dphi[k], bPhiDef);
 
  }

  (void)mTree->Branch("ntrig"  ,&(mMatch->numtrig),"numtrig/I");
  (void)mTree->Branch("trigid"  , mMatch->trigid  ,"trigid[numtrig]/I");
  (void)mTree->Branch("daqbits",&(mMatch->daqbits),"daqbits/i");


  mFile->mkdir("histos");
  mFile->cd("histos");

  // remove magic constants later
  hTrackNHits = new TH1F("hTrankNHits","hits/track"         ,100,  0.0,100  );
  hTrackLen   = new TH1F("hTrackLen"  ,"track length [cm]"  ,500,  0.0,500.0);
  hTrackPt    = new TH1F("hTrackPt"   ,"p_T   [GeV]"        ,500,  0.0,  5.0);
  hTrackPtot  = new TH1F("hTrackPtot" ,"p_tot [GeV]"        ,500,  0.0,  5.0);

  hTrackDCA[0]  = new TH1F("hTrackDCAX" , "x_vtxdca [cm]"     ,200,- 50.0, 50.0);
  hTrackDCA[1]  = new TH1F("hTrackDCAY" , "y_vtxdca [cm]"     ,200, -50.0, 50.0);
  hTrackDCA[2]  = new TH1F("hTrackDCAZ" , "z_vtxdca [cm]"     ,200,  -5.0,  5.0);

  mFile->cd("");
  
  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t 
EETowTrackMatchMaker::Make(){
  mNEvents++;

  mMatch->Clear(); 
  int &ntrack =  mMatch->numtracks = 0; // an alias
  mMatch->numz = 3;

  map<double,TString>::const_iterator zpos=mZ.begin();
  for(unsigned int k=0; zpos!=mZ.end() && k< kNTupleTTM_MaxZ ; ++zpos,k++)  mMatch->zpos[k]=zpos->first;

  StMuDst   *muDst  = mMuDstMaker->muDst();   // get pointer to _the_ _data_

  // sanity checks
  if(muDst==NULL) { 
    Warning("Make","%s aborted, muDST maker data missing",GetName());
    return kStErr;
  }

  if(mEEmcDb->valid()<=0) {
    Warning("Make","%s aborted, missing EEMC Db records",GetName());
    return kStErr;
  }

  // real work begins here
  TClonesArray      *tracks = muDst->primaryTracks();   // fetch primary tracks
  if (!tracks) { 
    Info("Make","no tracks for this event");
    return kStWarn;
  }

  StMuEvent* muEvent = muDst->event();                     // fetch microEvent data
   if (!muEvent) {
     Info("Make","no MuEvent data for this event");
     return kStWarn;
  }


  StMuEmcCollection *emc    = muDst->emcCollection();   // fetch endcap data
  if (!emc) {
    Info("Make","no EMC data for this event");
    return kStWarn;
  }
  
  
  //StEventInfo             &evinfo = muEvent->eventInfo();           // event info
  StEventSummary          &evsumm = muEvent->eventSummary();        // event summary
  StL0Trigger             &l0trig = muEvent->l0Trigger();           // L0 trigger info 
  StMuTriggerIdCollection &evtrig = muEvent->triggerIdCollection(); // trigger Id's

  StThreeVectorF vertex = evsumm.primaryVertexPosition();


  // select "good" tracks
  TList      goodTracks;
  TIter      nextTrack(tracks);
  StMuTrack *track  = NULL;

  while ( (track = (StMuTrack *)nextTrack()) ) {
    StThreeVectorF p  =track->p();
    StThreeVectorF dca=track->dca();

    // control histograms
    hTrackNHits->Fill(track->nHitsFit());
    hTrackLen  ->Fill(track->lengthMeasured());
    hTrackPt   ->Fill(track->pt());
    hTrackPtot ->Fill(p.mag());
    
    hTrackDCA[0] ->Fill(dca.x());
    hTrackDCA[1] ->Fill(dca.y());
    hTrackDCA[2] ->Fill(dca.z());

    if ( ! AcceptTrack(track) ) continue; 
    goodTracks.Add(track);
  }

  // no good tracks
  if( goodTracks.IsEmpty() ) return kStOk;  // what the ... 

  // do the matching
  ntrack=0;
  for (Int_t i=0; i< emc->getNEndcapTowerADC(); i++) { // loop over EEMC hits
    // get endcap hit(s) and use dbase to subtract pedestal and apply gain
    int   adc,sec,sub,eta;  // back to Fortran++ 
    float adcped,edep;  
    emc->getEndcapTowerADC(i,adc,sec,sub,eta); 
    if (adc<=0) continue;          // how about zero suppression :))
    
    const StEEmcDbIndexItem1 *dbi = mEEmcDb->getT(sec+1,sub+'A',eta+1); // fortran scheiss .... 
    if(dbi==NULL) continue;

    adcped = float(adc) - dbi->ped; 
    edep   = (dbi->gain>0.0) ? adcped/dbi->gain : 0.0;
    
    
    TIter nextTrack(&goodTracks);
    while( (track=(StMuTrack *)nextTrack()) != NULL && ntrack<kNTupleTTM_MaxTracks ){
      TVector3 tc    = mGeom->getTowerCenter(sec,sub,eta);  // tower center
      double   phiHW = mGeom->getPhiHalfWidth();
      double   etaHW = mGeom->getEtaHalfWidth(eta);
      double   dphi=0.0, deta=0.0;
      //double   z=0.0;
      TVector3 r(0.0,0.0,0.0);
      
      bool matched=false;
      // REMOVE IT LATER

      zpos=mZ.begin();
      for(unsigned int k=0; zpos!=mZ.end() ; ++zpos,k++) { 
	double z = zpos->first;
	matched=false;
	if(!ExtrapolateToZ(track,z,r) ) break;   // track 'hit' at z
	dphi = tc.Phi()            - r.Phi()           ;
	deta = tc.PseudoRapidity() - r.PseudoRapidity();
	if( ! MatchTrack(dphi,deta,phiHW,etaHW) ) break   ; 
	if(k<kNTupleTTM_MaxZ) {
	  mMatch->deta[k][ntrack] = dphi;
	  mMatch->dphi[k][ntrack] = deta;
	}
	matched=true;
      }
      if(!matched) continue;

      mMatch->sector[ntrack]=sec;
      mMatch->subsec[ntrack]=sub;
      mMatch->etabin[ntrack]=eta;
      mMatch->adc   [ntrack]=adcped;
      mMatch->edep  [ntrack]=edep;
  
      mMatch->nhits[ntrack]  = track->nHitsFit();
      mMatch->pt[ntrack]     = track->pt();
      mMatch->ptot[ntrack]   = track->pt()*TMath::CosH(track->eta());    // for now
      mMatch->length[ntrack] = track->length();
      mMatch->dedx[ntrack]   = track->dEdx();

      // fill trigger info
      mMatch->daqbits = l0trig.triggerWord();
      const StTriggerId &trg =  evtrig.nominal();
      mMatch->numtrig = trg.triggerIds().size();
      for(int k=0; k<mMatch->numtrig; k++) mMatch->trigid[k]=trg.triggerIds()[k];

      ntrack++;       
    }
  }


  mNMatched += ntrack;
  if(0<ntrack && ntrack<kNTupleTTM_MaxTracks)  mTree->Fill();
  return kStOK;
}

//_____________________________________________________________________________
void
EETowTrackMatchMaker::Clear(Option_t *option ) {
  //TString opt = option;
  //opt.ToLower();
  //if(opt.Contains("A")) { doSth() }  elseif (opt.Contains("B")) { doSthElse() };
  StMaker::Clear();
}

//_____________________________________________________________________________
Int_t 
EETowTrackMatchMaker::Finish () {
  if(mFile)      mFile->Write();
  return kStOK;
}


//_____________________________________________________________________________
/// AcceptTrack() - accept track function
Bool_t 
EETowTrackMatchMaker::AcceptTrack(const StMuTrack *track) {
  if(! track->topologyMap().trackTpcOnly()  ) return kFALSE;
  if(  track->type()     != 1               ) return kFALSE;
  if(  track->flag()     <= 0               ) return kFALSE;
  if(  track->nHitsFit() <  mMinTrackHits   ) return kFALSE;
  if(  track->length()   <  mMinTrackLength ) return kFALSE;
  if(  track->pt()       <  mMinTrackPt     ) return kFALSE;
  return kTRUE;
}

//_____________________________________________________________________________
/// MatchTrack() - decide if a track matches a tower or not
Bool_t 
EETowTrackMatchMaker::MatchTrack(
				 const double dphi , // track hit to tower center distance
				 const double deta,  
				 const double phihw, // tower half-widths
				 const double etahw) 
{
  if( mPhiFac*phihw < fabs(dphi) )       return kFALSE; 
  if( mEtaFac*etahw < fabs(deta) )       return kFALSE; 
  return kTRUE;
}
				   

//_____________________________________________________________________________
Bool_t
EETowTrackMatchMaker::ExtrapolateToZ(const StMuTrack *track, const double   z, TVector3 &r)
{
  const double kMinDipAngle   = 1.0e-13;
  //const float kMinCurvature =  1e+00;

  StPhysicalHelixD   helix  = track->helix();
  double             dipAng = helix.dipAngle();
  double             z0     = helix.origin().z();
  if(dipAng<kMinDipAngle) return kFALSE; 
  double s  = ( z - z0 ) / sin(dipAng)  ;
  StThreeVectorD hit = helix.at(s);
  r.SetXYZ(hit.x(),hit.y(),hit.z());
  return   kTRUE;
}





//_____________________________________________________________________________
ostream& 
EETowTrackMatchMaker::Summary(ostream &out ) const
{
  out << "<EETowTrackMatchMaker::Summary>\n";
  out << " *** MakerName   : " << GetName() << "\n";

  out.setf(ios_base::fixed,ios_base::floatfield);
  out.precision(2);

  out << " *** Cuts Summary:\n";
  out << "     tracks are matched at the following depths:\n";
  map<double,TString>::const_iterator zpos; 
  int k=0;
  cout.precision(2);
  for(zpos=mZ.begin(); zpos!=mZ.end() ; ++zpos) 
    out << "      " << ++k << ". z=" << zpos->first << "   \"" << zpos->second << "\"\n";
  
  out << "     min. hits/track  required         " << mMinTrackHits   << "\n"; 
  out << "     min. track length required        " << mMinTrackLength << "\n";
  out << "     min. transverse momentum required " << mMinTrackPt     << "\n";

  out << "     max. track to tower center dist.  " << mPhiFac << " x tower half-width (phi)\n";
  out << "     max. track to tower center dist.  " << mEtaFac << " x tower half-width (eta)\n";

  if(mNEvents>0) { 
    out << " *** Statistics:\n";
    out << "     total # of events         " << mNEvents  << "\n";
    out << "     # of matched tracks       " << mNMatched << "\n";
    out << "     # of matched tracks/event " << float(mNMatched)/mNEvents << "\n";
  }
  out << "</EETowTrackMatchMaker::Summary>\n";
  out.setf(ios_base::fmtflags(0),ios_base::floatfield);
  return out;
}


ostream&  operator<<(ostream &out, const EETowTrackMatchMaker& ttm)  { 
  return ttm.Summary(out); 
};

// $Log: EEmcTTMMaker.cxx,v $
// Revision 1.3  2004/01/06 22:42:55  zolnie
// provide summary/statistics info
//
// Revision 1.2  2004/01/06 21:33:51  zolnie
// release
//
// Revision 1.1  2004/01/06 17:45:10  zolnie
// close to release
//








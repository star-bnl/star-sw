/// \author Piotr A. Zolnierczuk, Indiana University Cyclotron Facility
/// \date   2003/12/08 
// $Id: EEmcTTMMaker.cxx,v 1.13 2004/04/13 15:48:11 zolnie Exp $
// doxygen info here
/** 
    \mainpage TTM - an endcap Tower to Track Match maker

    \section intro Introduction
    This a MuDST based class to get tower calibration from matching TPC tracks
    Since cint in root/root4star does not allow to pass function pointers 
    (that would be ideal for user defineable AcceptTrack/MatchTrack)
    we're stuck with FORTRAN++


    \section algorithm Algorithm
    
    1. build a list of good TPC tracks/event (using AcceptTrack)
     - flag() > 0        (see StEvent manual for information)
     - type() == primary (see StEvent manual for information)
     - hits/track      >= mMinTrackHits   (user changeable via SetMinTrackHits  (Int_t    v)
     - track  length   >= mMinTrackLength (user changeable via SetMinTrackLength(Double_t v)
     - transverse momentum >= mMinTrackPt     (user changeable via SetMinTrackPt    (Double_t v)

     2. loop over all EEMC tower hits (with adc>ped)
     for each track check if it matches a tower at preselected positions (mZ)
         - extrapolate track to a mZ with ExtrapolateToZ() to get track_hit_position
         - match is established with MatchTrack()
	     - if the distance from track_hit_position to tower_center in phi/eta
             - is smaller than mPhiFac/mEtaFac x tower_half_width
         - if the match is found the data (see struct NTuple_TTM ) is written to mFileName file

    \section params Parameters
    user may change _almost_ everything
    - mMinTrackHits   (default kDefMinTrackHits  ) changeable via SetMinTrackHits  (Int_t    v)
    - mMinTrackLength (default kDefMinTrackLength) changeable via SetMinTrackLength(Double_t v)
    - mMinTrackPt     (default kDefMinTrackPt    ) changeable via SetMinTrackPt    (Double_t v)
    - mPhiFac/mEtaFac (default 1.0)  user changeable via  SetPhiFactor/SetEtaFactor(Double_t v)
    - mZ              (default   "pres" => kEEmcZPRE1+0.1,
	                         "post" => kEEmcZPOST-0.1,
	                         "smd"  => kEEmcSMD       ) 

	 changeable via ResetZPositions() and subsequent calls to AppendZPosition("name",value)

	 NOTE: "name" will define 2 new branches in TTree deta+name and dphi+name
                     that will hold distances in eta/phi for matched tracks for further cutting
                     to e.g. reduce fiducial volume         
     - mFileName       (default lowercase(MakerName)+".root") changeable with SetFileName


     \section example Example
     see 
      - macros/TTM/ttm.C   an example how to analyze MuDST data
      - macros/TTM/show.C  an example how to display MuDST data (track/towers)

      root -q -b './StRoot/StEEmcPool/macros/TTM/ttm.C("/star/2003/mudst/","","R4145010.root")'
     
      this will produce a simple tree file called R4145010.root

       \image html  snapshot.jpg "Screen Shot"
       \image html  eemc.gif     "Two track event"
       \image latex eemc.eps     "Two track event" width=10cm


     \section final Final Analysis 
     Final analysis is done with macros e.g. mipcalib.C 
     
     make -f StRoot/StEEmcPool/macros/TTM/Makefile
     
     ./mipcalib -f R4145010.root

     ./mipcalib -h will print all the options
     

     \todo    nothing on the todo list right now
     

     \bug     No known bugs at this moment

 */

#include <iostream>
#include <sstream>

#include "TList.h"
#include "TMap.h"
#include "TFile.h"
#include "TTree.h"
#include "TH1F.h"

#include "EEmcTTMMaker.h"

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

#define DEBUG_PRINTS 0
#define DEBUG        1

#if !defined(ST_NO_NAMESPACES)
using std::map;
using std::ostream;
using std::ostringstream;
#endif


ClassImp(EEmcTTMMaker);

ClassImp(EEmcTower);


const Int_t    EEmcTTMMaker::kDefMinTrackHits   =  5; 
const Double_t EEmcTTMMaker::kDefMinTrackLength = 20.0;
const Double_t EEmcTTMMaker::kDefMinTrackPt     =  0.5;



//_____________________________________________________________________________
//! the TTM constructor
/// \param self     this maker name (const char*)
/// \param mumaker a pointer to a StMuDstMaker 
/// \param dbmaker a pointer to a StEEmcDbMaker 
EEmcTTMMaker::EEmcTTMMaker(
					   const char* self      , // this maker name
					   StMuDstMaker  *mumaker,
					   StEEmcDbMaker *dbmaker
					   ) 
  : StMaker(self),mMuDstMaker(mumaker),mEEmcDb(dbmaker) {

  if( mMuDstMaker == NULL )  
    Fatal("EEmcTTMMaker","invalid StMuDstMaker");
  
  if( mEEmcDb == NULL ) 
    Fatal("EEmcTTMMaker","invalid StEEmcDbMaker");
  
  // simple EEMC geometry description
  if( (mGeom = new EEmcGeomSimple()) == NULL) 
    Fatal("EEmcTTMMaker","cannot create EEmcGeomSimple class");

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
  mTrackList = new TList;
  mTowerList = new TList;
  mMatchMap  = new TMap;

  //
  ResetStats();

}

//_____________________________________________________________________________
/// destructor - cleanup
EEmcTTMMaker::~EEmcTTMMaker(){
  if( mTree  !=NULL ) delete mTree;
  if( mFile  !=NULL ) delete mFile;
  if( mMatch !=NULL ) delete mMatch;
  if( mGeom  !=NULL ) delete mGeom;

  if( mMatchMap !=NULL ) delete mMatchMap;
  if( mTrackList!=NULL ) delete mTrackList;
  if( mTowerList!=NULL ) delete mTowerList;
}


//_____________________________________________________________________________
/// Init()
Int_t 
EEmcTTMMaker::Init(){

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
/// Make()
Int_t 
EEmcTTMMaker::Make(){
  mNEvents++;
  //
  mMatchMap->Clear();
  mTrackList->Clear() ;
  mTowerList->Delete();
  //
  mMatch->Clear()  ; 

  int &ntrack       = mMatch->numtracks = 0; // an alias
  mMatch->numz = 3;

  map<double,TString>::const_iterator zpos=mZ.begin();
  for(unsigned int k=0; zpos!=mZ.end() && k< kNTupleTTM_MaxZ ; ++zpos,k++)  mMatch->zpos[k]=zpos->first;

  StMuDst    *muDst  = mMuDstMaker->muDst();   // get pointer to _the_ _data_

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
  //
  StMuEvent* muEvent = muDst->event();                     // fetch microEvent data
  if (!muEvent) {
    Info("Make","no MuEvent data for this event");
    return kStWarn;
  }
  //
  StMuEmcCollection *emc    = muDst->emcCollection();   // fetch endcap data
  if (!emc) {
    Info("Make","no EMC data for this event");
    return kStWarn;
  }
  if(emc->getNEndcapTowerADC()<=0) {
    Info("Make","no EEMC tower data for this event");
    return kStWarn;
  }
  
  //StEventInfo             &evinfo = muEvent->eventInfo();           // event info
  StEventSummary          &evsumm = muEvent->eventSummary();        // event summary
  StL0Trigger             &l0trig = muEvent->l0Trigger();           // L0 trigger info 
  StMuTriggerIdCollection &evtrig = muEvent->triggerIdCollection(); // trigger Id's

  StThreeVectorF vertex = evsumm.primaryVertexPosition();


  // select "good" tracks
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
    mTrackList->Add(track);
  }

  // no good tracks
  if( mTrackList->IsEmpty() ) { 
#if     DEBUG 
    Info("Make","no good tracks for this event");
    //cerr << "TOTAL TRACKS FOR EVENT#" << evinfo.id() << endl;
    //cerr << " primary : " << muDst->primaryTracks()->GetEntries() << endl;
    //cerr << " global  : " << muDst->globalTracks()->GetEntries()  << endl;
    //cerr << " other   : " << muDst->otherTracks()->GetEntries()   << endl;
    //cerr << " l3      : " << muDst->l3Tracks()->GetEntries()      << endl;
#endif
    return kStOK  ;  // what the ...
  }

  // do the matching
  ntrack=0;
  int goodTowerHits = 0;
  for (Int_t i=0; i< emc->getNEndcapTowerADC(); i++) { // loop over EEMC hits
    // get endcap hit(s) and use dbase to subtract pedestal and apply gain
    int   adc,sec,sub,eta;  // back to Fortran++ 
    float adcped,edep;  
    emc->getEndcapTowerADC(i,adc,sec,sub,eta); 
    if (adc<=0) continue;          // how about zero suppression :))
    
    //const EEmcDbItem *dbi = mEEmcDb->getT(sec+1,sub+'A',eta+1); // fortran scheiss .... 
    const StEEmcDbIndexItem1 *dbi = mEEmcDb->getT(sec,sub+'@',eta); 
    // some idiot changed indexing scheme and function implementation
    // in the middle of the run 
    if(dbi==NULL) continue;
    // now because of that idiot I have to do this scheiss
    sec--;
    sub--;
    eta--;

    adcped = float(adc) - dbi->ped; 
    edep   = (dbi->gain>0.0) ? adcped/dbi->gain : 0.0;
    if(adcped<0.0) continue;
    goodTowerHits++;
    //cerr <<  sec+1 << "|" << char(sub+'A') << "|" << eta+1 << "\t=>\t" << adcped << endl;
    EEmcTower *eemcHit = new EEmcTower(sec,sub,eta,adcped);
    mTowerList->Add(eemcHit);
    
    TIter nextTrack(mTrackList);
    while( (track=(StMuTrack *)nextTrack()) != NULL && ntrack<kNTupleTTM_MaxTracks ){
      TVector3 tc    = mGeom->getTowerCenter(sec,sub,eta);  // tower center
      double   phiHW = mGeom->getPhiHalfWidth();
      double   etaHW = mGeom->getEtaHalfWidth(eta);
      double   dphi=0.0, deta=0.0;
      //double   z=0.0;
      TVector3 r(0.0,0.0,0.0);
      
      bool matched=false;

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


#if     DEBUG_PRINTS
      cerr << "<ExtrapolateToZ>\n";      
      cerr <<  sec+1 << "|" << char(sub+'A') << "|" << eta+1 << endl;
      cerr <<  track->helix() << endl;
      fprintf(stderr,"z=%7.3f phi=%7.3f eta=%5.3f r=(%7.3f,%7.3f,%7.3f)\n",
	     0.0,tc.Phi()/M_PI*180.0,tc.PseudoRapidity(),tc.x(),tc.y(),tc.z()); 

      zpos=mZ.begin();
      for(unsigned int k=0; zpos!=mZ.end() ; ++zpos,k++) { 
	double z = zpos->first;
	matched=false;
	if(!ExtrapolateToZ(track,z,r) ) break;   // track 'hit' at z
	dphi = tc.Phi()            - r.Phi()           ;
	deta = tc.PseudoRapidity() - r.PseudoRapidity();
	if( ! MatchTrack(dphi,deta,phiHW,etaHW) ) break   ; 
	fprintf(stderr,"z=%7.3f phi=%7.3f eta=%5.3f r=(%7.3f,%7.3f,%7.3f)  (%5.3f/%5.3f %5.3f/%5.3f)\n",
	       z,r.Phi()/M_PI*180.0,r.PseudoRapidity(),r.x(),r.y(),r.z(),dphi,phiHW,deta,etaHW); 
      }
      cerr << "</ExtrapolateToZ>\n";
#endif /* DEBUG_PRINTS */

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
      mMatchMap->Add(eemcHit,track);
      ntrack++;       
    }
  }

#if     DEBUG 
  if(goodTowerHits<=0) {
    Info("Make","no good EEMC tower data for this event");
    return kStWarn;
  }
#endif

  mNMatched += ntrack;
  if(0<ntrack && ntrack<kNTupleTTM_MaxTracks)  mTree->Fill();
  return kStOK;
}

//_____________________________________________________________________________
/// clear maker (does nothing at the moment)
void
EEmcTTMMaker::Clear(Option_t *option ) {
  //TString opt = option;
  //opt.ToLower();
  //if(opt.Contains("A")) { doSth() }  elseif (opt.Contains("B")) { doSthElse() };
  StMaker::Clear();
}

//_____________________________________________________________________________
/// finish the job, write TTree 
Int_t 
EEmcTTMMaker::Finish () {
  if(mFile)      mFile->Write();
  return kStOK;
}


//_____________________________________________________________________________
//! default criterion of track acceptance
//! \param track a pointer to a current StMuTrack
Bool_t 
EEmcTTMMaker::AcceptTrack(const StMuTrack *track) {
  if(! track->topologyMap().trackTpcOnly()  ) return kFALSE;
  if(  track->type()     != 1               ) return kFALSE;
  if(  track->flag()     <= 0               ) return kFALSE;
  if(  track->nHitsFit() <  mMinTrackHits   ) return kFALSE;
  if(  track->length()   <  mMinTrackLength ) return kFALSE;
  if(  track->pt()       <  mMinTrackPt     ) return kFALSE;
  return kTRUE;
}

//_____________________________________________________________________________
//! default criterion whether a track matches a tower or not
/// \param dphi  a distance from track hit to tower centre in phi
/// \param deta  a distance from track hit to tower centre in eta
/// \param phihw a tower half-width in phi
/// \param etahw a tower half-width in eta
/// \return Bool_t 
Bool_t 
EEmcTTMMaker::MatchTrack(
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
//! given track and position z return TVector3 with a 
/// \param track a const pointer to current track
/// \param z     a z (along the beam) position where 
/// \param r     a TVector (returned)
/// \return boolean indicating if track crosses a plane
Bool_t
EEmcTTMMaker::ExtrapolateToZ(const StMuTrack *track, const double   z, TVector3 &r)
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
/// prints matching cuts and statistics summary
/// \param out an ostream to print to 
/// \return ostream 
ostream& 
EEmcTTMMaker::Summary(ostream &out ) const
{
  out << "<EEmcTTMMaker:Summary>\n";
  out << " <Maker Name=\"" << GetName() << "\" />\n";

  out.setf(ios_base::fixed,ios_base::floatfield);
  out.precision(2);

  out << " <CutsSummary>\n";
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
  out << " </CutsSummary>\n";

  if(mNEvents>0) { 
    out << " <Statistics>\n";
    out << " *** Statistics:\n";
    out << "     total # of events         " << mNEvents  << "\n";
    out << "     # of matched tracks       " << mNMatched << "\n";
    out << "     # of matched tracks/event " << float(mNMatched)/mNEvents << "\n";
    out << " </Statistics>\n";
  }
  out << "</EEmcTTMMaker:Summary>\n";
  out.setf(ios_base::fmtflags(0),ios_base::floatfield);
  return out;
}

// ================================================================================================
ostream& 
EEmcTower::Out(ostream &out ) const
{
  out << "<EEmcTower";
  out << " SECTOR=\""    << int(sec+1)    << "\"" ;
  out << " SUBSECTOR=\"" << char(sub+'A') << "\"" ;
  out << " ETA=\""       << int(eta+1)    << "\"" ;
  out << " EDEP=\""      << edep          << "\"" ;
  out << "/>\n";
  return out;
}

// ================================================================================================
ostream& 
Out(ostream &out , const StMuTrack &t)
{
  out << "<StMuTrack";
  out << " ORIGIN=\""   << t.helix().origin() << "\"";
  out << " MOMENTUM=\"" << t.momentum()   << "\"";
  out << "/>\n";
  return out;
}

ostream& 
Out(ostream &out , const EEmcTower &t)
{
  return t.Out(out);
}



// ================================================================================================
ostream&  operator<<(ostream &out, const EEmcTTMMaker &ttm)  { 
  return ttm.Summary(out); 
};

ostream&  operator<<(ostream &out, const EEmcTower    &t  )  {
  return t.Out(out);
}

ostream&  operator<<(ostream &out, const StMuTrack    &t  )  {
  return Out(out,t);
}


// $Log: EEmcTTMMaker.cxx,v $
// Revision 1.13  2004/04/13 15:48:11  zolnie
// fixes for some idiot changed indexing scheme and implementation
// of some vital functions in the middle of the run
//
// Revision 1.12  2004/04/13 14:53:39  zolnie
// *** empty log message ***
//
// Revision 1.11  2004/04/12 16:19:59  balewski
// DB cleanup & update
//
// Revision 1.10  2004/01/27 20:38:42  zolnie
// more docs
//
// Revision 1.9  2004/01/27 16:26:15  zolnie
// polished doxygen documentation
//
// Revision 1.8  2004/01/26 22:54:15  zolnie
// after name cleanup
//
// Revision 1.7  2004/01/26 21:51:54  zolnie
// shorter names
//
// Revision 1.6  2004/01/26 21:08:32  zolnie
// working track/tower display (before big farewell cleanup)
//
// Revision 1.5  2004/01/19 22:07:50  zolnie
// toward track/tower display
//
// Revision 1.4  2004/01/14 22:59:02  zolnie
// use doxygen for documentation
//
// Revision 1.3  2004/01/06 22:42:55  zolnie
// provide summary/statistics info
//
// Revision 1.2  2004/01/06 21:33:51  zolnie
// release
//
// Revision 1.1  2004/01/06 17:45:10  zolnie
// close to release
//








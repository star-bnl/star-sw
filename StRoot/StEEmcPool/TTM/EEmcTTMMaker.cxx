/// \author Piotr A. Zolnierczuk, Indiana University Cyclotron Facility
/// \date   2003/12/08 
// $Id: EEmcTTMMaker.cxx,v 1.18 2004/05/04 18:28:55 zolnie Exp $
// doxygen info here
/** 
    \mainpage TTM - an endcap Tower to Track Match maker (FIXME not updated!!!)

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
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"

#include "EEmcTTMatch.h"

#define DEBUG        0

#if !defined(ST_NO_NAMESPACES)
using std::map;
using std::ostream;
using std::ostringstream;
#endif


ClassImp(EEmcTTMMaker);



const Int_t    EEmcTTMMaker::kDefMaxCTBsum      = 1000; 
const Int_t    EEmcTTMMaker::kDefMinTrackHits   =  5; 
const Double_t EEmcTTMMaker::kDefMinTrackLength = 20.0;
const Double_t EEmcTTMMaker::kDefMinTrackPt     =  0.1;

const Double_t EEmcTTMMaker::kDefMinTrackEta    =  0.0;
const Double_t EEmcTTMMaker::kDefMaxTrackEta    =  2.2;

const Double_t EEmcTTMMaker::kDefDeltaPhiCut    =  0.7;
const Double_t EEmcTTMMaker::kDefDeltaEtaCut    =  0.7;

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

  ResetZPositionsArray();
  AddZPosition("pres",kEEmcZPRE1+0.1);
  AddZPosition("post",kEEmcZPOST-0.1);
  AddZPosition("smd" ,kEEmcZSMD);

  // cuts
  mMaxCTBsum      = kDefMaxCTBsum;
  mMinTrackHits   = kDefMinTrackHits;
  mMinTrackLength = kDefMinTrackLength;
  mMinTrackPt     = kDefMinTrackPt;
  mMinTrackEta    = kDefMinTrackEta;
  mMaxTrackEta    = kDefMaxTrackEta;
  
  //
  mPhiFac         = kDefDeltaPhiCut;
  mEtaFac         = kDefDeltaEtaCut;

  // 
  mTrackList = new TList;
  mTowerList = new TList;
  mMatchList = new TList;

  //
  ResetStats();

}

//_____________________________________________________________________________
/// destructor - cleanup
EEmcTTMMaker::~EEmcTTMMaker(){
  if( mFile  !=NULL ) delete mFile;
  if( mGeom  !=NULL ) delete mGeom;

  if( mMatchList!=NULL ) delete mMatchList;
  if( mTrackList!=NULL ) delete mTrackList;
  if( mTowerList!=NULL ) delete mTowerList;
}


//_____________________________________________________________________________
/// Init()
Int_t 
EEmcTTMMaker::Init(){

  ResetStats();

  mFile = new TFile(mFileName, "RECREATE");   if(!mFile) return kStErr;
  mTree = new TTree("ttm","MuDST tracks");    if(!mTree) return kStErr;

  //(void)mTree

  mFile->mkdir("histos");
  mFile->cd("histos");

  // remove magic constants later
  hTrackNHits = new TH1F("hTrankNHits","hits/track"         ,100,  0.0,100  );
  hTrackLen   = new TH1F("hTrackLen"  ,"track length [cm]"  ,500,  0.0,500.0);
  hTrackPt    = new TH1F("hTrackPt"   ,"p_T   [GeV]"        ,500,  0.0,  5.0);
  hTrackPtot  = new TH1F("hTrackPtot" ,"p_tot [GeV]"        ,500,  0.0,  5.0);

  hTrackDCA[0] = new TH1F("hTrackDCAX" , "x_vtxdca [cm]"     ,200,- 50.0, 50.0);
  hTrackDCA[1] = new TH1F("hTrackDCAY" , "y_vtxdca [cm]"     ,200, -50.0, 50.0);
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
  mMatchList->Clear();
  // 
  mTrackList->Clear() ; // we do  not own this 
  mTowerList->Delete(); // we own that :) the beauty of C++

  //
  StMuDst    *muDst  = mMuDstMaker->muDst();   // get pointer to _the_ _data_

  // sanity checks
  if(muDst==NULL) { 
    Warning("Make","%s error, muDST maker data missing",GetName());
    return kStErr;
  }
  
  if(mEEmcDb->valid()<=0) {
    Warning("Make","%s: missing EEMC Db records",GetName());
    return kStErr;
  }

  // real work begins here
  StMuEvent* muEvent = muDst->event();                     // fetch microEvent data
  if (!muEvent) {
    Warning("Make","%s: no MuEvent data for this event",GetName());
    return kStErr;
  }
  // ignore event too many tracks
  if( muEvent->ctbMultiplicity() > mMaxCTBsum ) return kStOK;
  //
  TClonesArray      *tracks = muDst->primaryTracks();   // fetch primary tracks
  if (!tracks) { 
    Warning("Make","%s: no tracks for this event",GetName());
    return kStErr;
  }
  //
  StMuEmcCollection *emc    = muDst->emcCollection();   // fetch endcap data
  if (!emc) {
    Info("Make","%s: no EMC data for this event",GetName());
    return kStErr;
  }
  //
  if(emc->getNEndcapTowerADC()<=0) {
    Info("Make","%s: no EEMC tower data for this event",GetName());
    return kStErr;
  }
  
  //StEventInfo             &evinfo = muEvent->eventInfo();           // event info
  StEventSummary          &evsumm = muEvent->eventSummary();        // event summary
  //StL0Trigger             &l0trig = muEvent->l0Trigger();           // L0 trigger info 
  //StMuTriggerIdCollection &evtrig = muEvent->triggerIdCollection(); // trigger Id's

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
#endif
    return kStOK  ;  // what the ...
  }

  // do the matching
  int ntrack=0;
  int goodTowerHits = 0;
  for (Int_t i=0; i< emc->getNEndcapTowerADC(); i++) { // loop over EEMC hits
    // get endcap hit(s) and use dbase to subtract pedestal and apply gain
    int   adc,sec,sub,eta;  // back to Fortran++ 
    float adcped,edep;  
    // some idiot changed indexing scheme in the middle of the run 
    emc->getEndcapTowerADC(i,adc,sec,sub,eta); 
    if (adc<=0) continue;          // how about zero suppression :))
    
    //const EEmcDbItem *dbi = mEEmcDb->getT(sec+1,sub+'A',eta+1); // fortran scheiss .... 
    const EEmcDbItem *dbi = mEEmcDb->getT(sec,sub+'@',eta); 
    // some idiot changed indexing scheme in the middle of the run 
    if(dbi==NULL) continue;
    // now because of that idiot I have to do this scheiss
    sec--;  sub--;  eta--;

    adcped = float(adc) - dbi->ped; 
    edep   = (dbi->gain>0.0) ? adcped/dbi->gain : 0.0;
    if(adcped<0.0) continue;
    goodTowerHits++;
    //
    EEmcTower   *eemcHit   = new EEmcTower(sec,sub,eta,adcped);
    EEmcTTMatch *eemcMatch = new EEmcTTMatch();
    mTowerList->Add(eemcHit);
    eemcMatch->Add(eemcHit);

    TIter nextTrack(mTrackList);
    while( (track=(StMuTrack *)nextTrack()) != NULL ) {
      TVector3 tc    = mGeom->getTowerCenter(sec,sub,eta);  // tower center
      double   phiHW = mGeom->getPhiHalfWidth();
      double   etaHW = mGeom->getEtaHalfWidth(eta);
      double   dphi=0.0, deta=0.0;
      TVector3 r(0.0,0.0,0.0);

      // TODO maybe add tracks that touch the tower for better cleanup
      // of multiple tracks per tower
      bool   matched=false;
      map<double,TString>::const_iterator zpos=mZ.begin();
      for(unsigned int k=0; zpos!=mZ.end() ; ++zpos,k++) { 
	double z = zpos->first;
	matched=false;
	if( ! EEmcTTMatch::ExtrapolateToZ(track,z,r)   ) break; // track 'hit' at z
	dphi = tc.Phi()            - r.Phi()           ;
	deta = tc.PseudoRapidity() - r.PseudoRapidity();
	if( ! MatchTrack(dphi,deta,phiHW,etaHW) ) break; 
	matched=true;
      }
      if(!matched)                continue;
      eemcMatch->Add(track);
      ntrack++;       
    }
    if( eemcMatch->Matches() > 0 ) mMatchList->Add(eemcMatch); 
  }

  mNMatched += ntrack;
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
  if(  track->eta()      <  mMinTrackEta    ) return kFALSE;
  if(  track->eta()      >  mMaxTrackEta    ) return kFALSE;
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
ostream&  operator<<(ostream &out, const EEmcTTMMaker &ttm)  { 
  return ttm.Summary(out); 
};




// $Log: EEmcTTMMaker.cxx,v $
// Revision 1.18  2004/05/04 18:28:55  zolnie
// version after split
//
// Revision 1.17  2004/04/15 18:08:18  zolnie
// *** empty log message ***
//
// Revision 1.16  2004/04/14 16:20:25  zolnie
// added static method Run for faster analysis under root4star
//
// Revision 1.15  2004/04/13 17:26:09  zolnie
// more adaptation needed
//
// Revision 1.14  2004/04/13 16:34:17  zolnie
// *** empty log message ***
//
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








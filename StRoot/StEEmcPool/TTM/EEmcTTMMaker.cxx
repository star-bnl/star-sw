/// \author Piotr A. Zolnierczuk, Indiana University Cyclotron Facility
/// \date   2003/12/08 
// $Id: EEmcTTMMaker.cxx,v 1.30 2009/02/04 20:33:24 ogrebeny Exp $
// doxygen info here
/** 
 * \class  EEmcTTMMaker
 * \brief  EEMC tower to track matching
 *
 * This a MuDST based class to match TPC tracks to EEMC towers. Its main result is a list 
 * of towers with associated tracks (list of EEmcTTMatch objects)
 *
 * \author Piotr A. Zolnierczuk
 * $Date: 2009/02/04 20:33:24 $
 * $Revision: 1.30 $
 *
 * \section ttmakerremarks Remarks
 *
 * \bug  The matching algorithm assumes that z depths at which matching is performed 
 * are in fact inside the EEMC, i.e. it is only phi and eta that are checked at given z. 
 * So it is up to the user to ensure that z depths are really inside EEMC. 
 * And the defaults are.
 * \bug  Since cint in root/root4star does not allow to pass function pointers 
 * (that would be ideal for user defineable EEmcTTMMaker::AcceptTrack 
 * and EEmcTTMMaker::MatchTrack) we're stuck with FORTRAN++
 * 
 * \todo The maker does not produce any useful file output. Write it.
 */


#include <iostream>
#include <sstream>

#include "TList.h"
#include "TMap.h"
#include "TFile.h"
#include "TTree.h"
#include "TH1F.h"

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

#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"

#include "EEmcTower.h"
#include "EEmcTTMatch.h"
#include "EEmcTTMMaker.h"


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
EEmcTTMMaker::EEmcTTMMaker(
			   const char* self      , // this maker name
			   StMuDstMaker  *mumaker
			   ) 
  : StMaker(self),mMuDstMaker(mumaker),mEEmcDb(0),mGeom(EEmcGeomSimple::Instance()) {

  if( mMuDstMaker == NULL )  
    Fatal("EEmcTTMMaker","invalid StMuDstMaker");
  
  
  // simple EEMC geometry description
  //if( (mGeom = new EEmcGeomSimple()) == NULL) 
  //  Fatal("EEmcTTMMaker","cannot create EEmcGeomSimple class");

  //mDebugLevel   = kWarning;

  mFileName = TString(GetName());
  mFileName.ToLower();
  mFileName += ".ndst.root";
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
  mTowerList->SetOwner();
  mMatchList->SetOwner();
  //
  mEvInfo = NULL;
  mEvSumm = NULL;
  mEvTrig = NULL;
  //
  mTreeOut   = false;
  //
  ResetStats();

}

//_____________________________________________________________________________
/// destructor - cleanup
EEmcTTMMaker::~EEmcTTMMaker() {
  if( mTree  !=NULL ) delete mTree;
  if( mFile  !=NULL ) delete mFile;
  //if( mGeom  !=NULL ) delete mGeom;

  if( mMatchList!=NULL ) delete mMatchList;
  if( mTrackList!=NULL ) delete mTrackList;
  if( mTowerList!=NULL ) delete mTowerList;
}


//_____________________________________________________________________________
/// Init()
Int_t 
EEmcTTMMaker::Init() {
  mEEmcDb = (StEEmcDb*)this->GetDataSet("StEEmcDb");
  if(!mEEmcDb) 
    Fatal("EEmcTTMMaker","invalid StEEmcDbMaker");

  ResetStats();
  //
  mFile = new TFile(mFileName, "RECREATE");   if(!mFile) return kStErr;
  mTree = new TTree("ttm","TPC track to EEmc tower matches");    if(!mTree) return kStErr;
  //
  mTree->Branch("matches","TList",&mMatchList,32768,0);
  // three exta branches for (nano)dst
  mTree->Branch("info"    ,"StEventInfo"            ,&mEvInfo ,32768,0);
  mTree->Branch("summary" ,"StEventSummary"         ,&mEvSumm ,32768,0);
  mTree->Branch("trigger" ,"StMuTriggerIdCollection",&mEvTrig ,32768,0);
 
  // control histos
  mFile->mkdir("histos");
  mFile->cd("histos");

  // remove magic constants later
  // histList = new TList();
  hTrackNHits = new TH1F("hTrankNHits","hits/track"        ,100,  0.0,100  );
  hTrackLen   = new TH1F("hTrackLen"  ,"track length [cm]" ,500,  0.0,500.0);
  hTrackPt    = new TH1F("hTrackPt"   ,"p_T   [GeV]"       ,500,  0.0,  5.0);
  hTrackPtot  = new TH1F("hTrackPtot" ,"p_tot [GeV]"       ,500,  0.0,  5.0);

  hTrackDCA[0] = new TH1F("hTrackDCAX" , "x_vtxdca [cm]"   ,200,- 50.0, 50.0);
  hTrackDCA[1] = new TH1F("hTrackDCAY" , "y_vtxdca [cm]"   ,200, -50.0, 50.0);
  hTrackDCA[2] = new TH1F("hTrackDCAZ" , "z_vtxdca [cm]"   ,200,  -5.0,  5.0);

  mFile->cd("");

  
  
  return StMaker::Init();
}

//_____________________________________________________________________________
/// Make()
Int_t 
EEmcTTMMaker::Make(){
 // static int nDPhi=0;
  mNEvents++;
  //
  mTrackList->Clear(); 
  mTowerList->Clear(); 
  mMatchList->Clear();

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
  
  // tracks are now in a TObjArray -- JCW 08/24/05 
  TObjArray *tracks = muDst->primaryTracks(); // get primary tracks 
  if (!tracks) { 
    Warning("Make","%s: no tracks for this event",GetName());
    return kStErr;
  }
  //
  StMuEmcCollection *emc    = muDst->muEmcCollection();   // fetch endcap data
  if (!emc) {
    Info("Make","%s: no EMC data for this event",GetName());
    return kStErr;
  }
  //
  if(emc->getNEndcapTowerADC()<=0) {
    Info("Make","%s: no EEMC tower data for this event",GetName());
    return kStErr;
  }
  
  StEventInfo             &evinfo = muEvent->eventInfo();           // event info
  StEventSummary          &evsumm = muEvent->eventSummary();        // event summary
  StMuTriggerIdCollection &evtrig = muEvent->triggerIdCollection(); // trigger Id's

  mEvInfo = &evinfo;
  mEvSumm = &evsumm;
  mEvTrig = &evtrig;

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
  int ntrack        = 0;
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

    //
    EEmcTower   *eemcHit   = new EEmcTower(sec,sub,eta,adcped,edep);
    EEmcTTMatch *eemcMatch = new EEmcTTMatch();
    mTowerList->Add(eemcHit);
    eemcMatch->Add(eemcHit);

    double phi0  = mGeom.getPhiMean(sec,sub);  // fast inlines
    double eta0  = mGeom.getEtaMean(eta);
    double phiHW = mGeom.getPhiHalfWidth();
    double etaHW = mGeom.getEtaHalfWidth(eta);
    double dphi  = 0.0;
    double deta  = 0.0;

    TIter nextTrack(mTrackList);
    while( (track=(StMuTrack *)nextTrack()) != NULL ) {
      TVector3 r;
      // TODO maybe add tracks that touch the tower for better cleanup
      // of multiple tracks per tower
      bool   matched=false;
      map<double,TString>::const_iterator zpos=mZ.begin();
      for(unsigned int k=0; zpos!=mZ.end() ; ++zpos,k++) { 
	double z = zpos->first;
	matched=false;
	if( ! EEmcTTMatch::ExtrapolateToZ(track,z,r)   ) break; // track 'hit' at z
	// FIXME the matching algorithm implicitely assumes that zpos depths are inside EEMC 
	dphi = fmod(phi0 - r.Phi(),TMath::TwoPi()); // fixed , make sure that dphi is within [0,2pi] domain
	deta =     (eta0 - r.Eta()               ); 
	if( ! MatchTrack(dphi,deta,phiHW,etaHW) ) break; 
	matched=true;
      }
      if(!matched)                continue;
      eemcMatch->Add(track);
      ntrack++;       
    }
    if( eemcMatch->Matches() > 0 ) 
      mMatchList->Add(eemcMatch); 
    else 
      delete eemcMatch; // stop leaking 
  }
  if(mTreeOut && ntrack>0) mTree->Fill();
  mNMatched += ntrack;
  return kStOK;
}

//_____________________________________________________________________________
void
EEmcTTMMaker::Clear(Option_t *option ) {
  //TString opt = option;
  //opt.ToLower();
  //if(opt.Contains("A")) { doSth() }  elseif (opt.Contains("B")) { doSthElse() };
  //mMatchList->Clear();
  //mTrackList->Clear() ; // we do  not own this 
  //mTowerList->Clear(); // we own that :) the beauty of C++
  StMaker::Clear();
}

//_____________________________________________________________________________
/// finish the job, write TTree 
Int_t 
EEmcTTMMaker::Finish () {
  if(mTreeOut && mTree!=NULL) mFile->Write();
  return kStOK;
}


//_____________________________________________________________________________
/// default criterion of track acceptance
/// \param track a pointer to a current StMuTrack
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
/// default criterion whether a track matches a tower or not
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
  cout.precision(2);
  
  out << "     max CTB sum allowed                         " << mMaxCTBsum      << "\n\n";
  //
  out << "     min hits/track  required                    " << mMinTrackHits   << "\n"; 
  out << "     min track length required                   " << mMinTrackLength << "\n";
  out << "     min track transverse momentum required      " << mMinTrackPt     << "\n";
  out << "     min track pseudorapidity at origin required " << mMinTrackEta    << "\n";
  out << "     max track pseudorapidity at origin required " << mMaxTrackEta    << "\n\n";
  //
  out << "     tracks are matched at the following depths:\n";
  map<double,TString>::const_iterator zpos; 
  int k=0;
  for(zpos=mZ.begin(); zpos!=mZ.end() ; ++zpos) 
    out << "      " << ++k << ". z=" << zpos->first << "   \"" << zpos->second << "\"\n";
  //
  out << "     max track to tower center dist.  " << mPhiFac << " x tower half-width (phi)\n";
  out << "     max track to tower center dist.  " << mEtaFac << " x tower half-width (eta)\n";
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
}




// $Log: EEmcTTMMaker.cxx,v $
// Revision 1.30  2009/02/04 20:33:24  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.29  2007/07/12 19:27:23  fisyak
// Add includes for TMath for ROOT 5.16
//
// Revision 1.28  2005/08/24 14:17:12  jwebb
// Primary tracks now in a TObjArray.  Code has been updated, and should
// "just work".
//
// Revision 1.27  2004/10/21 13:31:36  balewski
// to match new name of emcCollection in muDst
//
// Revision 1.26  2004/07/08 00:45:21  balewski
// cleanup
//
// Revision 1.25  2004/06/03 21:02:28  zolnie
// fixed subtle bug: when e.g. dphi = +180.(tower center) - -180.0(track)
//  the match would be rejected - in practice it never happen
//
// Revision 1.24  2004/05/10 23:02:47  zolnie
// EEmcTTMMaker produces now  nanoDST
//
// Revision 1.23  2004/05/07 22:02:56  zolnie
// fixed a nasty memory leak in EEmcTTMMaker
//
// Revision 1.22  2004/05/06 16:02:49  zolnie
// more docs
//
// Revision 1.21  2004/05/05 23:00:51  zolnie
// more docs
//
// Revision 1.20  2004/05/05 22:04:16  zolnie
// forgor about EEmcTower
//
// Revision 1.19  2004/05/05 21:37:37  zolnie
// ver 2.0 released
//
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








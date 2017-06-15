#include "StTruthTestMaker.h"
///////////////////////////////////////////////////////////////////////////////////////////////////////////////

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"

#include "TDataSetIter.h"
#include "tables/St_g2t_track_Table.h"

#include "TString.h"

#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"

#include <map>
using namespace std;

// Few histograms in global scope
#include "TH1.h"
#include "TH1F.h"
#include "TH2F.h"

#include "TVector2.h"

// Few cuts also
const Float_t invCut = 0.12000   * 3.0;
const Float_t etaCut = 0.01200   * 3.0;
const Float_t phiCut = 0.01900   * 3.0;
const Float_t absEtaCut = 1.0;

#define Res(x) TMath::Abs( x##R - x##T )

#define MUDST_CUT qaTruth < 95 || track -> nHits() < 10

// ------------------------------------------------------------------------------------------ Init //
Int_t StTruthTestMaker::Init()
{
  hMatchedEta = new TH2F("hMatchedEta",";#eta thrown;#eta reco",201,-1.005,1.005,201,-1.005,1.005);
  hMatchedPhi = new TH2F("hMatchedPhi",";#phi thrown;#phi reco",201,-1.005*TMath::Pi(),1.005*TMath::Pi(),201,-1.005*TMath::Pi(),1.005*TMath::Pi());
  hMatchedPt  = new TH2F("hMatchedPt", ";p_{T} thrown;p_{T} reco",200,0.,10.,200,0.,10.);
  hMatchedInv = new TH2F("hMatchedInv",";1/p_{T} thrown;1/p_{T} reco",200,0.,10.,200,0.,10.);
  hMatchedPID = new TH1F("hMatchedPID","PID of matched particles",2112*2+1.0,-2112.5,+2112.5);
#if  ROOT_VERSION_CODE < ROOT_VERSION(6,0,0)
  hMatchedPID->SetBit(TH1::kCanRebin);
#endif
  hMatchedQA  = new TH1F("hMatchedQA", "QA truth of matched tracks",101,-0.5,100.5);

  hMatchedEtaRes = new TH1F("hMatchedEtaRes",";#eta resolution",201,-0.2*1.005,0.2*1.005);
  hMatchedPhiRes = new TH1F("hMatchedPhiRes",";#phi resolution",201,-0.2*1.005,0.2*1.005);
  hMatchedPtRes  = new TH1F("hMatchedPtRes", ";p_{T} resolution",201,-1.005,1.005);
  hMatchedInvRes = new TH1F("hMatchedInvRes",";1/p_{T} resolution",201,-1.005,1.005);

  hNumMismatched = new TH1F("hNumMismatched","Number of mismatched tracks / event;N mismatched",11,-0.5,10.5); 
  hPerMismatched = new TH1F("hPerMismatched","Percentage of mismatched globals / event;N mismatched/total",21,-0.025,1.025);
  hPidMismatched = new TH1F("hPidMismatched","PID of mismatched globals",2112*2+1.0,-2112.5,+2112.5); 
#if  ROOT_VERSION_CODE < ROOT_VERSION(6,0,0)
  hPidMismatched->SetBit(TH1::kCanRebin);
#endif
  TH1           *hList[]={hMatchedEta,  hMatchedPhi,  hMatchedPt,  hMatchedPID,hMatchedEtaRes,   hMatchedPhiRes,   hMatchedPtRes,  hNumMismatched,hPerMismatched,       hPidMismatched,  hMatchedInv,  hMatchedInvRes,   hMatchedQA};
  //  const Char_t *xtitle[]={"#eta thrown","#phi thrown","pt thrown","pid",       "#eta resolution","#phi resolution","pt resolution","N mismatched","fraction mismatched","pid mismatched","1/pt thrown","1/pt resolution","QA truth"};
  //  const Char_t *ytitle[]={"#eta reco",  "#phi reco",  "pt reco",  "",          "",               "",               "",             "",            "",                   "",              "1/pt reco",  "",               "" };

  for ( UInt_t i=0;i<sizeof(hList)/sizeof(TH1*);i++ ) { 
    //    hList[i]->GetXaxis()->SetTitle(xtitle[i]);
    //    hList[i]->GetYaxis()->SetTitle(ytitle[i]);
    AddHist( hList[i] ); 
    //    cout << "Added histogram " << hList[i]->GetName() << endl;
  }


  
  TString name=GetName();
  if ( name=="testGeant" ) mDoGeant = true;
  else                     mDoGeant = false;
  
  return kStOK;
}
// ------------------------------------------------------------------------------------------ Init //

Int_t StTruthTestMaker::Make()
{
  
  if ( mDoGeant ) MakeGeant();
  else            MakeRecord();
  return kStOK;
  
}

Int_t StTruthTestMaker::MakeGeant()
{

  static Bool_t first = true;

  /*
   *****************************************************************

   Obtain the MuDst and map all tracks to their ID truth value

   ***************************************************************** 
   */

  map< Int_t, StMuTrack * > muTrackMap;

  StMuDst   *muDst = (StMuDst *)GetInputDS("MuDst");                                   assert(muDst);
  StMuEvent *muEvent = muDst -> event();                                               assert(muEvent);

  { Int_t ntracks = muDst -> globalTracks() -> GetEntries();
    for ( Int_t itrack = 0; itrack < ntracks; itrack++ )
      {
	
	StMuTrack *track = muDst -> globalTracks(itrack);

	Int_t idTruth = track->idTruth();
	Int_t qaTruth = track->qaTruth();
//	Int_t id      = track->id();

	if ( MUDST_CUT ) continue;
	
	if ( idTruth ) muTrackMap[idTruth] = track;
	//      	cout << Form( "Mu Track %03i has idtruth=%03i w/ qa=%03i/100", id, idTruth, qaTruth ) << endl;
	
      }
  }

  /*
   *************************************************************************

   Obtain the geant dataset and loop over all tracks.  Map g2t_track to the
   primary key

   *************************************************************************
   */

  TDataSet     *geant = GetDataSet("geant");                                             assert(geant);
  TDataSetIter  geantIter(geant);
  St_g2t_track *g2t_trackTable = (St_g2t_track *)geantIter("g2t_track");
  g2t_track_st *trackTable = (g2t_track_st *)g2t_trackTable->GetTable();

  if ( first ) { // old fortran habits die hard
    g2t_trackTable->Print();
    first = false;
  }

  cout << "-- G2T Table Event Record ----------------------------------------" << endl;

  Int_t count  = 0;
  Int_t missed = 0;
  Int_t matched = 0;
 
  { Int_t ntracks = g2t_trackTable->GetNRows(); 
    for ( Int_t itrack = 0; itrack<ntracks; itrack++ )
      {

	Int_t key = trackTable[itrack].id;
	Int_t pid = trackTable[itrack].eg_pid;
	Int_t lab = trackTable[itrack].eg_label;

	// Skip MC track if it's a geant track
	if ( !lab ) continue;

	// Retrieve corresponding MuDst track, skip if there is none
	StMuTrack *muTrack = muTrackMap[ key ];
	if ( !muTrack ) continue;
	count++;

//	Int_t id   = muTrack->id();
//	Int_t type = muTrack->type();
//	Int_t flag = muTrack->flag();
	Int_t qaTruth = muTrack->qaTruth();
	Int_t idTruth = muTrack->idTruth(); assert(idTruth==key);
	//	cout << Form(" + matched to Mu Track with id=%03i type=%03i flag=%03i qa=%03i",id,type,flag,qaTruth) << endl;

	// Get MuTrack kinematics
	Float_t etaR = muTrack->eta();
	Float_t phiR = muTrack->phi();
	Float_t ptR  = muTrack->pt();
	Float_t invR = (ptR>0)? 1.0/ptR : 999;

	// Get g2tTrack kinematics
	Float_t etaT = trackTable[itrack].eta;
	Float_t ptT  = trackTable[itrack].pt;
	Float_t px   = trackTable[itrack].p[0];
	Float_t py   = trackTable[itrack].p[1];
	Float_t pz   = trackTable[itrack].p[2];
	Float_t invT = (ptT>0)? 1.0/ptT : 999;
	TVector2 pT(px,py);
	Float_t phiT = TVector2::Phi_mpi_pi( pT.Phi() );

	cout << Form("g2tTrack idtruth=%03i pid=%i px=%6.3f py=%6.3f pz=%7.3f",key,pid,px,py,pz) << endl;

	hMatchedEta -> Fill( etaT, etaR );
	hMatchedPhi -> Fill( phiT, phiR );
	hMatchedPt  -> Fill( ptT,  ptR  );
	hMatchedInv -> Fill( invT, invR );
	hMatchedQA  -> Fill( qaTruth );

	Float_t etaD = (etaR - etaT);
	Float_t phiD = TVector2::Phi_mpi_pi(phiR - phiT);
	Float_t ptD  = ptR - ptT;
	Float_t invD = invR - invT;

	hMatchedEtaRes -> Fill( etaD );
	hMatchedPhiRes -> Fill( phiD );
	hMatchedPtRes  -> Fill( ptD  );
	hMatchedInvRes -> Fill( invD );

	if ( TMath::Abs( etaD ) > etaCut     &&
	     TMath::Abs( phiD ) > phiCut     &&
	     TMath::Abs( invD ) > invCut ) goto MISMATCH;

	matched++;

	continue;

      MISMATCH:

	missed++;
	
	continue;

      }
  }


  hNumMismatched -> Fill( missed );
  if ( count > 0 ) { 
    hPerMismatched -> Fill( float(missed / count) );
  };

  return kStOK;
}

Int_t StTruthTestMaker::MakeRecord()
{

  static Bool_t first = true;

  /*
   *****************************************************************

   Obtain the MuDst and map all tracks to their ID truth value

   ***************************************************************** 
   */

  map< Int_t, StMuTrack * > muTrackMap;

  StMuDst   *muDst = (StMuDst *)GetInputDS("MuDst");                                   assert(muDst);
  StMuEvent *muEvent = muDst -> event();                                               assert(muEvent);

  { Int_t ntracks = muDst -> globalTracks() -> GetEntries();
    for ( Int_t itrack = 0; itrack < ntracks; itrack++ )
      {
	
	StMuTrack *track = muDst -> globalTracks(itrack);

	Int_t idTruth = track->idTruth();
	Int_t qaTruth = track->qaTruth();
//	Int_t id      = track->id();
	
	if ( MUDST_CUT ) continue;

	if ( idTruth ) muTrackMap[idTruth] = track;
	//	cout << Form( "Mu Track %03i has idtruth=%03i w/ qa=%03i/100", id, idTruth, qaTruth ) << endl;
	
      }
  }

  
  /*
   *****************************************************************

   Obtain the generator event record

   ***************************************************************** 
   */
  TObjectSet *oset = (TObjectSet *)GetData("GenEvent",".event");
  StarGenEvent *event = (StarGenEvent *)oset->GetObject();
  if ( first ) { // old fortran habits die har
    event->Print();
    first = false;
  }

  Int_t count  = 0;
  Int_t missed = 0;
  Int_t matched = 0;


  // Loop over all event generator tracks and print those with ID truth info
  // (e.g. the tracks which were fed out to geant for simulation)
  TIter Next( event -> IterAll() );
  //  TIter Next( event -> IterStacked() );
  StarGenParticle *particle = 0;
  cout << "-- Event Generator Record ----------------------------------------" << endl;
  while ( (particle=(StarGenParticle *)Next()) )
    {
      Int_t idtruth = particle->GetStack();

      if ( particle->GetPrimaryKey() != 0 ) particle->Print();
      

      StMuTrack *muTrack = muTrackMap[idtruth];
      if ( !muTrack ) continue;
      count++;

      //      particle->Print();
      
//    Int_t id   = muTrack->id();
//    Int_t type = muTrack->type();
//    Int_t flag = muTrack->flag();
      Int_t qaTruth = muTrack->qaTruth();
//    Int_t idTruth = muTrack->idTruth(); // assert(idTruth==key);

      // Get MuTrack kinematics
      Float_t etaR = muTrack->eta();
      Float_t phiR = muTrack->phi();
      Float_t ptR  = muTrack->pt();
      Float_t invR = (ptR>0)? 1.0/ptR : 999;

      // Get generator kinematics
      Float_t etaT = particle->momentum().Eta();
      Float_t ptT  = particle->pt();
      Float_t px   = particle->GetPx();
      Float_t py   = particle->GetPy();
      Float_t pz   = particle->GetPz();
      Float_t invT = (ptT>0)? 1.0/ptT : 999;
      TVector2 pT(px,py);
      Float_t phiT = TVector2::Phi_mpi_pi( pT.Phi() );
      Int_t   pid  = particle->GetId();

      cout << Form("evtTrack idtruth=%03i pid=%i px=%6.3f py=%6.3f pz=%7.3f",idtruth   ,pid,px,py,pz) << endl;

      hMatchedEta -> Fill( etaT, etaR );
      hMatchedPhi -> Fill( phiT, phiR );
      hMatchedPt  -> Fill( ptT,  ptR  );
      hMatchedInv -> Fill( invT, invR );
      hMatchedQA  -> Fill( qaTruth );

      Float_t etaD = (etaR - etaT);
      Float_t phiD = TVector2::Phi_mpi_pi(phiR - phiT);
      Float_t ptD  = ptR - ptT;
      Float_t invD = invR - invT;

      hMatchedEtaRes -> Fill( etaD );
      hMatchedPhiRes -> Fill( phiD );
      hMatchedPtRes  -> Fill( ptD  );
      hMatchedInvRes -> Fill( invD );

      if ( TMath::Abs( etaD ) > etaCut     &&
	   TMath::Abs( phiD ) > phiCut     &&
	   TMath::Abs( invD ) > invCut ) goto MISMATCH;
      
      matched++;
      
      continue;
      
    MISMATCH:
      
      missed++;
      
      continue;     
  
    }
  
  hNumMismatched -> Fill( missed );
  if ( count > 0 ) { 
    hPerMismatched -> Fill( float(missed / count) );
  }


  return kStOK;
}


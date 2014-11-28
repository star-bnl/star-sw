#include "StEEmcPi0Maker.h"
#include "StMessMgr.h"

#include "StEEmcPair.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"

#include "StEvent/StEvent.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StEvent/StTriggerId.h"

#include "StarClassLibrary/StThreeVectorF.hh"

#include "TH1F.h"
#include "TH2F.h"

#include "TFile.h"
#include "TTree.h"
#include "StEEmcMixEvent.h"

ClassImp(StEEmcPi0Maker);

//34567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
//       1         2         3         4         5         6         7       *

// ---------------------------------------------------------------------------
StEEmcPi0Maker::StEEmcPi0Maker( const Char_t *name,
				StEEmcA2EMaker *a2e,
				StEEmcGenericClusterMaker *cl,
				StEEmcGenericPointMaker   *pt ) : StMaker(name)
{
  mEEanalysis=a2e; assert(a2e);
  mEEclusters=cl;  assert(cl);
  mEEpoints=pt;    assert(pt);
  mCheckTrigger=false;
  setFile(0);
  mTree=0;
}

void   StEEmcPi0Maker::setFile( TFile *f ){ mFile=f; }
TTree *StEEmcPi0Maker::tree(){ return mTree; }

// ---------------------------------------------------------------------------
Int_t StEEmcPi0Maker::Init()
{
  hMass   = new TH2F("hMass","Diphoton invariant mass; M [GeV]",120,0.,1.2,20,0.,20.);
  hMass_cut = new TH2F("hMass_cut","Diphoton invariant mass; M [GeV]",120,0.,1.2,20,0.,20.);
  hMass_split = new TH2F("hMass_split","Mass for events w/ a split smd cluster",120,0.,1.2,20,0.,20.);

  hPT     = new TH1F("hPT","Diphoton p_{T}; p_{T} [GeV]",20,0.,20.);
  hPT_cut = new TH1F("hPT_cut","Diphoton p_{T}; p_{T} [GeV]",20,0.,20.);
  hXF     = new TH1F("hXF","x_{F}=#frac{p_{L}}{#sqrt{s}/2}",50,0.,1.);
  hEnergy = new TH1F("hEnergy","Diphoton energy; E [GeV]",50,0.,50.);
  hEta    = new TH1F("hEta","Diphoton #eta",24,0.8,2.2);
  hPhi    = new TH1F("hPhi","Diphoton #phi",30,-3.142,3.142);
  hZvertex= new TH1F("hZvertex","z vertex",150,-150.,150.);
  hZgg    = new TH1F("hZgg","energy sharing",50,0.,1.);
  hZgg_cut    = new TH1F("hZgg_cut","energy sharing",50,0.,1.);

  hEChi2  = new TH1F("hEChi2","#sum_{points} #frac{(Eu-Ev)^{2}}{Nmip}",50,0.,10.);
  hE1Chi2  = new TH1F("hE1Chi2","point1 #frac{(Eu-Ev)^{2}}{Nmip}",50,0.,10.);
  hE2Chi2  = new TH1F("hE2Chi2","point2 #frac{(Eu-Ev)^{2}}{Nmip}",50,0.,10.);

  hEChi2_low  = new TH1F("hEChi2_low","#sum_{points} #frac{(Eu-Ev)^{2}}{Nmip}",50,0.,10.);
  hE1Chi2_low  = new TH1F("hE1Chi2_low","point1 #frac{(Eu-Ev)^{2}}{Nmip}",50,0.,10.);
  hE2Chi2_low  = new TH1F("hE2Chi2_low","point2 #frac{(Eu-Ev)^{2}}{Nmip}",50,0.,10.);

  hEChi2_hi  = new TH1F("hEChi2_hi","#sum_{points} #frac{(Eu-Ev)^{2}}{Nmip}",50,0.,10.);
  hE1Chi2_hi  = new TH1F("hE1Chi2_hi","point1 #frac{(Eu-Ev)^{2}}{Nmip}",50,0.,10.);
  hE2Chi2_hi  = new TH1F("hE2Chi2_hi","point2 #frac{(Eu-Ev)^{2}}{Nmip}",50,0.,10.);

  hRatio = new TH1F("hRatio","E_{smd} / E_{towers}",20,0.,0.05);
  hRatio_low = new TH1F("hRatio_low","E_{smd} / E_{towers}",20,0.,0.05);
  hRatio_hi = new TH1F("hRatio_hi","E_{smd} / E_{towers}",20,0.,0.05);
  
  hEvents = new TH1F("hEvents","event counter",1,0.,1.); hEvents->SetBit(TH1::kCanRebin);

  //hdEds   = new TH2F("hdEds","smd energy derivative; #frac{1}{E}#frac{dE}{ds}; rel. strip",30,-2.0,1.0,10,-5.,5.);

  if ( mFile ) 
    {
      mTree=new TTree("mPi0Tree","EEmc pi0 events");
      mPi0Event=new StEEmcMixEvent();
      mTree->Branch("pi0event",&mPi0Event,32000,99);
      mTree->SetDirectory( mFile );
    }

  return StMaker::Init();
}

// ---------------------------------------------------------------------------
Int_t StEEmcPi0Maker::Make()
{

  StMuDstMaker *mudst=(StMuDstMaker*)GetMaker("MuDst");
  assert(mudst);

  hEvents->Fill("event",1.0);

  if ( !checkTrigger() ) return kStOK;
  hEvents->Fill("trigger",1.0);
  
  StThreeVectorF pv=mudst->muDst()->event()->primaryVertexPosition();
  TVector3 vertex( pv.x(), pv.y(), pv.z() );

  if ( pv.z() < -150. || pv.z() > 150.0 || pv.z()==0.0 ) return kStOK;
  hEvents->Fill("vertex",1.0);
  
  // get the points from the point maker
  StEEmcPointVec_t points = mEEpoints->points();

  if ( points.size() < 2 ) return kStOK;
  hEvents->Fill("two+ points",1.0);

  mPi0Event->setEvent( mudst->muDst()->event() );

  // loop over all pairs of points
  for ( UInt_t ipoint=0;ipoint<points.size();ipoint++ )
    {
      StEEmcPoint p1 = points[ipoint];


      for ( UInt_t jpoint=0;jpoint<points.size();jpoint++ )
	{
	  if (ipoint>=jpoint) continue;
	  StEEmcPoint p2=points[jpoint];


	  Int_t phibin1 = p1.tower(0).phibin();
	  Int_t phibin2 = p2.tower(0).phibin();

	  //Int_t etabin1 = p1.tower(0).etabin();
	  //Int_t etabin2 = p2.tower(0).etabin();

	  //Int_t deta = TMath::Abs(etabin1-etabin2);
	  Int_t dphi = phibin1-phibin2;
	  if ( dphi < 0 ) dphi+=60;
	  if ( dphi > 30 ) dphi=60-dphi;

	  if ( dphi > 10 ) continue; /** limit distance between points to +/- 10 towers **/


	  StEEmcPair pair(p1,p2,vertex,vertex);

	  
	  /***********************************************
	   **
	   ** Fill the TTre and the list of pi0 pairs
	   **
	   ***********************************************/

	  mPairs.push_back( pair );
	  mPi0Event->addPair( pair );

	  //Int_t index1 = p1.tower(0).index();
	  //Int_t index2 = p2.tower(0).index();
	  
	  StEEmcClusterVec_t tower_clusters; // tower clusters associated w/ pair (should be 1 or 2)
	  StEEmcCluster c1 = p1.clusters(0)[0];
	  StEEmcCluster c2 = p2.clusters(0)[0];
	  tower_clusters.push_back( c1 );
	  if ( c1.key() != c2.key() ) {
	    tower_clusters.push_back(c2);
	  }

	  mPi0Event->mNumberOfTowerClusters.push_back( (Int_t)tower_clusters.size() );

	  Int_t ntracks = 0;
	  Int_t npoints = 0;
	  for ( UInt_t i=0;i<tower_clusters.size();i++ )
	    {
	      ntracks += mEEclusters->numberOfTracks( tower_clusters[i] );
	      npoints += mEEpoints->numberOfPoints( tower_clusters[i] );
	    }
	  mPi0Event->mNumberOfTracks.push_back( ntracks );
	  mPi0Event->mNumberOfPoints.push_back( npoints );



	  /***********************************************/
	  



	  /// the rest is gravy


	  hEvents->Fill("pair",1.0);

	  if ( pair.momentum().Perp() < 3.0 ) continue;

	  hEvents->Fill("pair pt>3",1.0);


	  //	  printf("------------------- retrieve tower clusters from points -------------------\n");
	  StEEmcCluster cluster1=p1.clusters(0).at(0); //mEEpoints->cluster( p1 );
	  StEEmcCluster cluster2=p2.clusters(0).at(0); //mEEpoints->cluster( p2 );

	  // require both points to match same tower cluster
	  //555  if ( cluster1.key() != cluster2.key() ) continue;

	  hEvents->Fill("pair in cl",1.0);
	  // require 2 and only 2 smd clusters matched to tower cluster
	  // in each view

	  if ( cluster1.numberOfMatchingClusters(4) > 2 ||
	       cluster1.numberOfMatchingClusters(5) > 2 ) continue;

	  StEEmcSmdCluster u1=p1.cluster(0);
	  StEEmcSmdCluster u2=p2.cluster(0);
	  StEEmcSmdCluster v1=p1.cluster(1);
	  StEEmcSmdCluster v2=p2.cluster(1);

	  Bool_t didSplit = false;
	  if ( u1.key() == u2.key() || v1.key() == v2.key() ) didSplit=true;


	  //	  printf("cluster1 key=%i E=%5.2f cluster2 key=%i E=%5.2f\n",
	  //		 cluster1.key(),cluster1.energy(),cluster2.key(),cluster2.energy());

	  Float_t esum_smd = u1.energy()+u2.energy()+v1.energy()+v2.energy(); // [GeV]
	  Float_t esum_tow = cluster1.energy();
	  if ( cluster1.key() != cluster2.key() ) esum_tow += cluster2.energy();
	    
	  

	  hEvents->Fill("pair ncl",1.0);

	  hMass->Fill( pair.mass(), pair.pt() );
	  if( didSplit ) hMass_split->Fill( pair.mass(), pair.pt() );

	  Float_t ediff1 = 1000.0*(u1.energy()-v1.energy());
	  Float_t ediff2 = 1000.0*(u2.energy()-v2.energy());
	  Float_t nmip1  = 1000.0*(u1.energy()+v1.energy())/1.3;
	  Float_t nmip2  = 1000.0*(u2.energy()+v2.energy())/1.3;
	  if ( nmip1>0. && nmip2>0. ) 
	    {
	      Float_t e1chi2 = ediff1*ediff1/nmip1;
	      Float_t e2chi2 = ediff2*ediff2/nmip2;
	      Float_t echi2  = e1chi2+e2chi2;

	      if ( e1chi2 < 4.0 && e2chi2 < 4.0 ) {
		hMass_cut -> Fill( pair.mass(), pair.pt() );
		if ( pair.mass()>0.1 && pair.mass()<0.18 ) hPT_cut->Fill( pair.pt() );
		if ( pair.mass()>0.1 && pair.mass()<0.18 ) hZgg_cut->Fill( pair.zgg() );
	      }

	      if ( pair.mass() < 0.1 ) {	    
		hEChi2_low->Fill(echi2);
		hE1Chi2_low->Fill(e1chi2);
		hE2Chi2_low->Fill(e2chi2);
		hRatio_low -> Fill( esum_smd/esum_tow );		
	      }
	      if ( pair.mass() > 0.2 ) {	    
		hEChi2_hi->Fill(echi2);
		hE1Chi2_hi->Fill(e1chi2);
		hE2Chi2_hi->Fill(e2chi2);
		hRatio_hi -> Fill( esum_smd/esum_tow );
	      }
	    }


	  if ( pair.mass() > 0.1 && pair.mass() < 0.18 ) {

	    hPT->Fill( pair.momentum().Perp() );
	    hXF->Fill( pair.momentum().Z()/100.0 );
	    hEnergy->Fill( pair.energy() );
	    hEta->Fill( pair.momentum().Eta() );
	    hPhi->Fill( pair.momentum().Phi() );
	    hZvertex->Fill( pair.vertex().Z() );
	    hZgg->Fill( pair.zgg() );

	    if ( nmip1==0. || nmip2==0. ) continue;
            Float_t e1chi2 = ediff1*ediff1/nmip1;
	    Float_t e2chi2 = ediff2*ediff2/nmip2;
	    Float_t echi2  = e1chi2+e2chi2;
 
	    hEChi2->Fill(echi2);
	    hE1Chi2->Fill(e1chi2);
	    hE2Chi2->Fill(e2chi2);
	    hRatio -> Fill( esum_smd/esum_tow );
	    printf("esum_smd/esum_tow = %5.3f\n", esum_smd/esum_tow );
	    
	  }

	}
    }

  if ( mTree )
    mTree->Fill();
  
  return kStOK;
}



// ---------------------------------------------------------------------------
void StEEmcPi0Maker::Clear(Option_t *opts)
{
  mPairs.clear();
  mPi0Event->Clear();
}



// ----------------------------------------------------------------------------                                                     
void  StEEmcPi0Maker::setCheckTrigger(Bool_t t){ mCheckTrigger=t; }
void  StEEmcPi0Maker::addTrigger(Int_t t){ mTriggerList.push_back(t); }

Bool_t StEEmcPi0Maker::checkTrigger()
{

  /// If user didn't specify a trigger ID, always return true                                                                       
  if ( mTriggerList.size() == 0 ) return 1;

  StTriggerId nominal;

  /// Get Trigger from MuDst if available, fallback to StEvent if not                                                              $
  StMuDstMaker *mumk = (StMuDstMaker*)GetMaker("MuDst");
  StEvent *event = (StEvent*)GetInputDS("StEvent");

  if ( mumk )
    {
      nominal = mumk->muDst()->event()->triggerIdCollection().nominal();
      goto CHECK_TRIGGER;
    }

  /// Get Trigger from StEvent if available                                                                                        $

  if ( event )
    {
      nominal=*event->triggerIdCollection()->nominal();
      goto CHECK_TRIGGER;
    }

  /// Bail out here because we don't have anything to do!                                                                          $
  goto NO_DATA;


 CHECK_TRIGGER:

  for ( UInt_t ii=0;ii<mTriggerList.size();ii++ )
    {
      if ( nominal.isTrigger( mTriggerList[ii] ) ) return mTriggerList[ii];
    }
  return 0;

 NO_DATA:
  assert(2+2==5); // noooo data                                                                                                    $
  return 0;

}

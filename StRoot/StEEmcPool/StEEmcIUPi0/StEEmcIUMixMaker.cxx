/**
 *
 * \class StEEmcIUMixMaker
 * \brief A class for mixing pi0 candidates
 *
 * This class takes as input points found using StEEmcIUPointMaker or
 * a derivative class.  We then loop over all possible pairs of 
 * points to find pi0 (eta) --> gamma gamma candidates.  
 *
 * Points from
 * each event are stored in a E_T ranked pool, which we then mix
 * to form combinatoric background candidates.  In the future we
 * may create a separate maker for mixed events.
 *
 * The code allows two cuts:
 *
 * <ol>
 *   <li>Trigger list -- the user may specify a list of triggers to process</li>
 *   <li>Sector limits -- by default, points are mixed if in the same sector.  The user may specify other sector-wise limits.</li>
 * </ol>
 *
 */
#include <iostream>
#include "StEEmcIUMixMaker.h"
#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcIUPointMaker.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StEvent/StTriggerId.h"

#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include "TRandom.h"
#include "TH1F.h"
#include "TH2F.h"
#include "StMcOutputMaker.h"

#define SIMPLE_MC

ClassImp(StEEmcIUMixMaker);

// ----------------------------------------------------------------------------
StEEmcIUMixMaker::StEEmcIUMixMaker(const Char_t *name,Int_t s):StMaker(name)
{
  mRandom=new TRandom();
  mPoolSize=s; 

  mEEmcTow = new EEmcGeomSimple();

  ///
  /// Defaults
  ///
  minET(1.0);     // minimum pair E_T [GeV]
  maxZ(0.5);      // maximum pair zgg [GeV]
  minEpoint(0.8); // minimum point energy [GeV]
  range(0.08,0.18); 
  mTrigMode=0;

  /// initialize background pool
  StEEmcIUPointVec_t points;
  for ( Int_t i=0; i<40; i++ ) mPool.push_back(points);

  mFixedVertex=TVector3(-999.,-999.,-999.);
  mSigmaVertex=-999.;

  mMixLimits = 0;

}

// ----------------------------------------------------------------------------
Int_t StEEmcIUMixMaker::Init()
{
  mEEpoints=(StEEmcIUPointMaker *)GetMaker(mPointMakerName); 
  assert(mEEpoints);
  mEEanalysis=(StEEmcA2EMaker *)GetMaker(mAnalysisName);   
  assert(mEEanalysis);
  mMuDstMaker=(StMuDstMaker   *)GetMaker(mMuDstMakerName); 
  assert(mMuDstMaker);
  

  /// initialize histograms
  book();

  return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcIUMixMaker::Make()
{

  //StMcOutputMaker *mkDc=(StMcOutputMaker *)GetMaker("mcRead");
  //assert(mkDc);

  /// Check trigger, run QA, etc...
  if ( !accept( mMuDstMaker -> muDst() -> event() ) ) 
    return kStOK;

  mH1[1]->Fill("accepted",1.0);

  /// Get the list of points
  mPoints=mEEpoints->points();
    
  /// Not enough points in event
  if ( mPoints.size() <= 1 ) return kStOK;
  mH1[1]->Fill( "2+ points", 1.0);

  /// Construct all pairs of real events
  mixReal();  
    
  /// Construct combinatoric background
  mixBackground();

  /// fill histograms
  fill();

  /// Fill the event pool with points from
  /// current event.
  fillPool();

  return kStOK;
}

// ----------------------------------------------------------------------------
void StEEmcIUMixMaker::Clear( Option_t *opts )
{
  mCandidates.clear();
  mBackground.clear();
  mPoints.clear();

  return;
}

// ----------------------------------------------------------------------------
void StEEmcIUMixMaker::mixReal()
{

  if ( !mPoints.size() ) return; 

  Float_t emax=0.;
  Int_t   imax=0;
  Int_t   count=0;
 
  StMuEvent *event = mMuDstMaker -> muDst() -> event();
  if ( !event ) return ; 
  StThreeVectorF v=event->primaryVertexPosition();

  TVector3 vertex = TVector3(v.x(),v.y(),v.z());

  /// Verify that we have a valid (or user specified) event vertex
  if ( mFixedVertex.Z()<-500. ) {
    if ( v.z()==0. && v.x()==0. && v.y()==0. ) return; 
  }
  else {
    //vertex= TVector3(0.,0.,genzz);
    vertex=mFixedVertex;
    if ( mSigmaVertex > 0. ) vertex[2]=( mFixedVertex.Z() + mRandom->Gaus(0.,mSigmaVertex));
  }
  //printf("2vertex z=%f zz=%f\n",v.z(),vertex.z()); 
 
  /// mix all pairs of points, avoiding self-pairs


  for ( UInt_t ipoint=0; ipoint<mPoints.size()-1; ipoint++ ){
    for ( UInt_t jpoint=ipoint+1; jpoint<mPoints.size(); jpoint++ )
      {

	StEEmcIUPoint point1=mPoints[ipoint];
	StEEmcIUPoint point2=mPoints[jpoint];

	/// Require points to be in specified sectors
	Bool_t go1 = false;
	Bool_t go2 = false;
	for ( UInt_t isec=0;isec<mSectorList.size();isec++ )
	  {
	    go1 |= point1.sector() == mSectorList[isec];
	    go2 |= point2.sector() == mSectorList[isec];
	  }
	if ( !(go1&&go2) ) 
	  {
	    numberofFailpoint.push_back(1);
	    continue;
	  }


	/// same sector
	if ( point1.sector() != point2.sector() )
	  {
	    numberofFailpoint.push_back(1);
	    continue;
	  }

	count++;

	mCandidates.push_back ( StEEmcIUPair( point1, point2, vertex, vertex ) );


	if ( mCandidates.back().energy() > emax ) { 
	  emax = mCandidates.back().energy();
	  imax=count;
	}

      }
  }
}

void StEEmcIUMixMaker::mixBackground()
{

  if ( !mPoints.size() ) return ; 

  /// get the high-tower in the eemc
  StEEmcTower high_tower = mEEanalysis->hightower();

  Int_t bin=(Int_t)(high_tower.adc()/100);
  if ( bin<0 ) bin=0;
  if ( bin>40 ) bin=40;

  /// get a reference to the list of points with the
  /// indexed by trigger
  StEEmcIUPointVec_t &points = mPool[bin];

  StMuEvent *event = mMuDstMaker -> muDst() -> event();
  if ( !event ) return; 
  StThreeVectorF v=event->primaryVertexPosition();
  TVector3 vertex = TVector3(v.x(),v.y(),v.z());

  if ( mFixedVertex.Z()<-500. ) {
    if ( v.z()==0. && v.x()==0. && v.y()==0. ) return; 
  }
  else {
    vertex=mFixedVertex;
    if ( mSigmaVertex > 0. ) vertex[2]=( mFixedVertex.Z() + mRandom->Gaus(0.,mSigmaVertex)); 
  }


  /// loop over all points in current event
  for ( UInt_t i=0; i<mPoints.size(); i++ ) {

    StEEmcIUPoint point1=mPoints[i];

    /// loop over all points in ranked pool
    for ( UInt_t j=0; j<points.size(); j++ ) {

      StEEmcIUPoint point2=points[j];

      /// Require points to be in specified sectors
      Bool_t go1 = false;
      Bool_t go2 = false;
      for ( UInt_t isec=0;isec<mSectorList.size();isec++ )
	  {
	    go1 |= point1.sector() == mSectorList[isec];
	    go2 |= point2.sector() == mSectorList[isec];
	  }
      if ( !(go1&&go2) ) continue;

      /// same sector
      if ( point1.sector() != point2.sector() ) continue;

      /// add to background
      mBackground.push_back( StEEmcIUPair( point1, point2, vertex, vertex ) );

      

    }// pool

  }// current

}



// ----------------------------------------------------------------------------
Bool_t StEEmcIUMixMaker::accept( StEEmcIUPoint &p1, StEEmcIUPoint &p2 )
{

  Int_t s1=p1.sector();
  Int_t s2=p2.sector();

  Int_t ss1=TMath::Max(s1,s2);
  Int_t ss2=TMath::Min(s1,s2);

  Int_t d1=ss1-ss2;
  Int_t d2=ss2+12-ss1;

  if ( d1 > mMixLimits && d2 > mMixLimits ) return false;

  return true;

}
// ----------------------------------------------------------------------------
Bool_t StEEmcIUMixMaker::accept( StMuEvent *event )
{
  if ( !event ) return false;
  StMuTriggerIdCollection tic = event -> triggerIdCollection();
  StTriggerId l1trig = tic.l1();

  /// no triggers in list, assume wide open 
  if ( mTriggerList.size() <= 0 ) {
      mH1[0]->Fill("no selection",1.0); 
      return true; 
  }

  Int_t go=0;
  std::vector<Int_t>::iterator iter=mTriggerList.begin();
  while ( iter != mTriggerList.end() ) {
    go = l1trig.isTrigger( (*iter) );
    if ( go ) {
      go = (*iter);
      break;   
    }
    iter++;
  }
  TString name=go;
  mH1[0]->Fill(name,1.0);
  return (go!=0);
}


// ----------------------------------------------------------------------------
void StEEmcIUMixMaker::book()
{
  
    // 1D QA histograms
  mH1.push_back(new TH1F("triggers","Number of triggers fired",1,0.,1.));
  mH1[0]->SetBit(TH1::kCanRebin);
  mH1.push_back(new TH1F("status","Events processed up to...",1,0.,1.));
  mH1[1]->SetBit(TH1::kCanRebin);

  // 2D QA histograms
  mH2.push_back(new TH2F("uvha","<u> vs <v> for higher-energy gamma",288,0.,288.,288,0.,288.)); 
  mH2.push_back(new TH2F("uvla","<u> vs <v> for lower-energy gamma",288,0.,288.,288,0.,288.)); 
  mH2.push_back(new TH2F("uvhc","<u> vs <v> for higher-energy gamma, mass cut",288,0.,288.,288,0.,288.)); 
  mH2.push_back(new TH2F("uvlc","<u> vs <v> for lower-energy gamma, mass cut",288,0.,288.,288,0.,288.)); 


  mH1real.push_back(new TH1F("massR","Invariant mass of photon pairs",360,0.,3.6) );
  mH1real.push_back(new TH1F("energyR","Energy of photon pairs",200,0.,40.));
  mH1real.push_back(new TH1F("zggR","Energy sharing of photon pairs",50,0.,1.));
  mH1real.push_back(new TH1F("phiggR","Opening angle of photon pairs",100,0.,0.1));
  mH1real.push_back(new TH1F("ptR","p_{T} of photon pairs",100,0.,10.));
  mH1real.push_back(new TH1F("zvertexR","Z_{vertex} [cm]",100,-100.,100.));
  
  mH1mix.push_back(new TH1F("massM","Invariant mass of photon pairs",360,0.,3.6) );
  mH1mix.push_back(new TH1F("energyM","Energy of photon pairs",200,0.,40.));
  mH1mix.push_back(new TH1F("zggM","Energy sharing of photon pairs",50,0.,1.));
  mH1mix.push_back(new TH1F("phiggM","Opening angle of photon pairs",100,0.,0.1));
  mH1mix.push_back(new TH1F("ptM","p_{T} of photon pairs",100,0.,10.));
  mH1mix.push_back(new TH1F("zvertexM","Z_{vertex} [cm]",100,-100.,100.));
  
}

// ----------------------------------------------------------------------------
void StEEmcIUMixMaker::fill()
{

    StEEmcIUPairVec_t::iterator ipair=mCandidates.begin(); 
    while ( ipair != mCandidates.end() ) { 
      fill( mH1real, (*ipair ) ); 
      fillQA( mH2, (*ipair ) );       
      ipair++; 
    } 

    ipair=mBackground.begin();
    while ( ipair != mBackground.end() ) {
	fill( mH1mix, (*ipair) );
	ipair++;
    } 
  
}

void StEEmcIUMixMaker::fillQA( std::vector<TH2F *> &h, StEEmcIUPair pair ) 
{

    StEEmcIUPoint p1=pair.point(0);
    StEEmcIUPoint p2=pair.point(1); 
    StEEmcIUSmdCluster uh,ul,vh,vl; 
    if ( p1.energy() > p2.energy() ) {
	uh=p1.cluster(0); vh=p1.cluster(1); 
	ul=p2.cluster(0); vl=p2.cluster(1);
    }
    else {
	ul=p1.cluster(0); vl=p1.cluster(1); 
	uh=p2.cluster(0); vh=p2.cluster(1);
    } 
    mH2[0]->Fill( uh.mean(), vh.mean() );
    mH2[1]->Fill( ul.mean(), vl.mean() ); 
    if ( pair.mass() > mMinMass && pair.mass() < mMaxMass ) { 
	mH2[2]->Fill( uh.mean(), vh.mean() );
	mH2[3]->Fill( ul.mean(), vl.mean() );
    } 

} 

void StEEmcIUMixMaker::fill( std::vector<TH1F*> &h, StEEmcIUPair pair )
{

    h[0]->Fill( pair.mass() );
    if ( pair.mass() > mMinMass && pair.mass() < mMaxMass ) { 
	h[1]->Fill( pair.energy() );
       	h[2]->Fill( pair.zgg() );
	h[3]->Fill( pair.phigg() );
	h[4]->Fill( pair.vertex().Z() );
    }

} 


// ----------------------------------------------------------------------------
void StEEmcIUMixMaker::fillPool()
{

  if ( !mPoints.size() ) return;

  /// get the high-tower in the eemc
  StEEmcTower high_tower = mEEanalysis->hightower();

  Int_t bin=(Int_t)(high_tower.adc()/100);
  if ( bin<0 ) bin=0;
  if ( bin>40 ) bin=40;

  /// get a reference to the list of points with the
  /// indexed by trigger
  StEEmcIUPointVec_t &points = mPool[bin];

  /// reverse the list of old points
  std::reverse(points.begin(),points.end());

  /// add points from the current event
  for ( UInt_t i=0; i<mPoints.size(); i++ ) {

    TVector3 d=mPoints[i].position();
    
    /// make sure point is beneath a valid tower
    Int_t sec,sub,eta;
    if ( !mEEmcTow->getTower(d,sec,sub,eta) ) continue;

    /// high-tower trigger mode, reject if it's the high tower
    if ( mTrigMode == 1 ) {
      if ( sec==high_tower.sector() && 
	   sub==high_tower.subsector() && 
	   eta==high_tower.etabin() ) continue;
    }
    
    /// push point into vector
    points.push_back( mPoints[i] );

  }

  /// reverse yet again
  std::reverse(points.begin(),points.end());

  /// and truncate to the maximum pool size
  if ( points.size() > (UInt_t)mPoolSize ) points.resize(mPoolSize);

}

// ----------------------------------------------------------------------------

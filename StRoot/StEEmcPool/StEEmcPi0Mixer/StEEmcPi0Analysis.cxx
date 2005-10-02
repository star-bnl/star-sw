/**
 
\class StEEmcPi0Analysis
\brief A maker for creating pi0 histograms

    The StEEmcPi0Analysis takes as input the list of pi0 candiates 
provided by StEEmcMixMaker, filters out pairs depending on user-specified
cuts, then spin sorts the events into a number of histograms.  These
histograms are stored in TDirectory's underneath the .hist dataset. 
The user may specify cuts in the SpinCuts object (accessible through
a call to cuts().) 

*/


#include "StEEmcPi0Analysis.h"
#include "StEEmcMixMaker.h"
#include "StEEmcPool/StEEmcPointMaker/StEEmcPointMaker.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h" 
#include <stdio.h>
#include <iostream>
#include "SpinCuts.h" 
#include "StEEmcPool/StEEmcPointMaker/EEmcSectorFit.h" 
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcPool/StRFEmcTrigger/StRFEmcTrigMaker.h"
#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h" 

ClassImp(StEEmcPi0Analysis);

// ----------------------------------------------------------------------------
StEEmcPi0Analysis::StEEmcPi0Analysis(const Char_t *name):StMaker(name)
{
    mCuts=new SpinCuts(); 
    mTrigSim = 0;
    mTrigSimThreshold = 0;
    mEEgeom=new EEmcGeomSimple();
}

// ----------------------------------------------------------------------------
Int_t StEEmcPi0Analysis::Init()
{

  // book histograms for pi0 analysis
  mHistograms[ kAny  ] = new SpinHistos("Any", "Pi0 spectra, unsorted");
  // book histograms for pi0 analysis
  mBackgrounds[ kAny  ] = new SpinHistos("AnyB", "combinatoric spectra, unsorted");

  hEventCounter=new TH1F("hEventCounter","Event counts",10,0.,10.);
  hEventCounter -> GetXaxis() -> SetBinLabel(1,"raw");
  hEventCounter -> GetXaxis() -> SetBinLabel(2,"minb");
  hEventCounter -> GetXaxis() -> SetBinLabel(3,"hw trg");
  hEventCounter -> GetXaxis() -> SetBinLabel(4,"sw trg");
  hEventCounter -> GetXaxis() -> SetBinLabel(5,"sw trg[3,6]");
  hEventCounter -> GetXaxis() -> SetBinLabel(6,"vertex");
  hPairCounter=new TH1F("hPairCounter","Pair counts",10,0.,10.);
 
  return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcPi0Analysis::Make()
{

  /// Check trigger 
  if ( !accept( mMuDst->muDst() ) ) return kStOK; 

  hPairCounter -> Fill( kEvent ); 

  /// Loop over all candidate pairs
  for ( Int_t i=0;i<mEEmixer->numberOfCandidates();i++ )
    {

      StEEmcPair pair = mEEmixer->candidate(i);
      hPairCounter -> Fill( kPair ); 
      if ( !accept( pair ) ) continue; 
      pair.print();
      mHistograms[ kAny ] -> Fill( pair );

    }
 
  /// Repeat for backgrounds
  for ( Int_t i=0;i<mEEmixer->numberOfMixedCandidates();i++ )
    {

      StEEmcPair pair = mEEmixer->mixedCandidate(i);
      if ( !accept( pair, false ) ) continue;
      mBackgrounds[ kAny ] -> Fill( pair );

    }


  return kStOK;
}

// ----------------------------------------------------------------------------
void StEEmcPi0Analysis::mixer(const Char_t *name)
{
  mEEmixer=(StEEmcMixMaker *)GetMaker(name);
  assert(mEEmixer);// please specify a valid mix maker
}

// ----------------------------------------------------------------------------
void StEEmcPi0Analysis::points(const Char_t *name)
{
  mEEpoints=(StEEmcPointMaker *)GetMaker(name);
  assert(mEEpoints);
}

// ----------------------------------------------------------------------------
void StEEmcPi0Analysis::mudst(const Char_t *name)
{
    mMuDst=(StMuDstMaker*)GetMaker(name);
    assert(mMuDst); 
}
// ----------------------------------------------------------------------------
void StEEmcPi0Analysis::analysis(const Char_t *name)
{
    mEEanalysis=(StEEmcA2EMaker*)GetMaker(name);
    assert(mEEanalysis); 
} 
// ----------------------------------------------------------------------------
Bool_t StEEmcPi0Analysis::twoBodyCut( StEEmcPair &pair )
{

  static const Int_t   maxPerCluster = 2;
  static const Float_t minTowerFrac = 0.;
  
  /// Obtain the 6-18 towers which contribute energy to this pair

  Bool_t towers[720]; for (Int_t i=0;i<720;i++ ) towers[i]=false; 
  StEEmcTower t1 = pair.point(0).tower(0);
  StEEmcTower t2 = pair.point(1).tower(0);

  Float_t Etowers=0.;
  Etowers += t1.energy();


  towers[ t1.index() ] = true;
  towers[ t2.index() ] = true;

  for ( Int_t i=0;i<t1.numberOfNeighbors();i++ ) {
    StEEmcTower mytow=t1.neighbor(i);
    towers[ mytow.index() ] = true;    
    Etowers += mytow.energy();
  }
  for ( Int_t i=0;i<t2.numberOfNeighbors();i++ ) {
    StEEmcTower mytow=t2.neighbor(i);
    if ( !towers[mytow.index()] ) Etowers += mytow.energy();
    towers[ mytow.index() ] = true;
  }



  /// NOTE: we could consider adding the next-nearest neighbors as well
  
  /// Loop over all points in the endcap and count the number which
  /// match one of the specified towers

  Int_t count = 0;
  for ( Int_t i=0;i<mEEpoints->numberOfPoints();i++ )
    {
      StEEmcPoint p=mEEpoints->point(i);
      StEEmcTower t=p.tower(0);      
      if ( towers[ t.index() ] ) count++;
    }

  Float_t Epoints = pair.energy();

  return ( count <= maxPerCluster && Epoints > minTowerFrac * Etowers );

}

// ----------------------------------------------------------------------------
// mudst based cuts
Bool_t StEEmcPi0Analysis::accept( StMuDst *mudst )
{

    // verify that the trigger is in the trigger list 
    StMuEvent *event=mudst->event();
    assert(event);

    Bool_t good_trigger = false; 

    // -------------------------------------------------------
    //
    // Trigger from real data
    //

    StMuTriggerIdCollection tic = event -> triggerIdCollection();
    StTriggerId l1trig = tic.l1();
    // if no triggers are specified, always return true 
    if ( mTriggerList.size() <=0 ) {
	good_trigger = true; 
    } 

    Int_t go=0;
    std::vector<Int_t>::iterator iter=mTriggerList.begin();
    while ( iter != mTriggerList.end() ) {
      	go = l1trig.isTrigger( (*iter) );
	good_trigger |= go; 	
    	iter++;
    }


    // -------------------------------------------------------
    //
    // Trigger from sim data
    //
    if ( mTrigSim ) 
      {
	good_trigger = mTrigSim -> getEEMCtrigHT( mTrigSimThreshold );
	good_trigger &= mTrigSim -> getBBCtrig();
      }


    if ( l1trig.isTrigger( mMinBias ) )
      hEventCounter -> Fill( kMinBias );

    // must have a valid trigger to proceed 
    if ( good_trigger ) 
    {
	hEventCounter -> Fill( kTrig );
    } 
    else 
    {        
        return false; 
    }

    // loop over all hit towers and verify that one tower
    // exceeds the software threshold in mCuts
    StEEmcTower ht;
    Bool_t sw36=0; 
    for ( Int_t i=0;i<mEEanalysis->numberOfHitTowers(0);i++ )
    {
	StEEmcTower t=mEEanalysis->hittower(i,0);
	if ( !t.fail() && t.et() > ht.et() ) {
	    ht=t; 
	    if ( ht.et()>=mCuts->tower_et_cut &&
		 ht.sector()>=2&&ht.sector()<=5 ) sw36=true; 
	}
    } 
    if ( ht.et() < mCuts->tower_et_cut ) {
	good_trigger = false; 
	return false; 
    } 
    else 
    {
	hEventCounter -> Fill( kSoftTrig );
    } 
    if ( sw36 ) hEventCounter -> Fill( kSoftTrig36 ); 
    
    
    StThreeVectorF vert=event->primaryVertexPosition();
    if ( vert.z() < mCuts->z_vertex_min) {
      // std::cout << "No vertex" << std::endl;
      return false;
    }
    if ( vert.z() > mCuts->z_vertex_max ) {
      // std::cout << "No vertex" << std::endl;
      return false; 
    }
    
    hEventCounter -> Fill( kVertex ); 


    return true; 

} 

Bool_t StEEmcPi0Analysis::accept( StEEmcPair pair, Bool_t fill )
{

    /// Pair must be isolated, i.e. only two points sharing tower energy 
    if ( !twoBodyCut(pair) ) return false; 
    if (fill) hPairCounter -> Fill( kTwoBody ); 

    /// One gamma must be associated with a tower above trigger threshold
    StEEmcPoint point1=pair.point(0);
    StEEmcPoint point2=pair.point(1); 
    StEEmcTower tower1=pair.point(0).tower(0);
    StEEmcTower tower2=pair.point(1).tower(0);

    if ( tower1.adc() < mCuts->adc_cut &&
	 tower2.adc() < mCuts->adc_cut ) return false; 

    if ( fill) hPairCounter -> Fill( kAdcTow ); 

    /*
    if ( tower1.et()  < mCuts->tower_et_cut &&
	 tower2.et()  < mCuts->tower_et_cut ) return false;
    */
    TVector3 d1 = mEEgeom -> getTowerCenter( (UInt_t)tower1.sector(), (UInt_t)tower1.subsector(), (UInt_t)tower1.etabin() );
    TVector3 d2 = mEEgeom -> getTowerCenter( (UInt_t)tower2.sector(), (UInt_t)tower2.subsector(), (UInt_t)tower2.etabin() );
    d1=d1.Unit();
    d2=d2.Unit();
    Float_t et1 = (tower1.energy()*d1).Perp();
    Float_t et2 = (tower2.energy()*d2).Perp();
    
    if ( et1 < mCuts->tower_et_cut &&
	 et2 < mCuts->tower_et_cut ) return false;
  

    /*
    /// test code for a level 2 trigger
    Float_t et_sum1 = tower1.et();
    Float_t et_sum2 = tower2.et();
    for ( Int_t ii=0;ii<tower1.numberOfNeighbors();ii++ )
      {
	et_sum1 += tower1.neighbor(ii).et();
      }
    for ( Int_t ii=0;ii<tower2.numberOfNeighbors();ii++ )
      {
	et_sum2 += tower2.neighbor(ii).et();
      }

    Bool_t go = false;
    if ( et1 > 3.0 || et2 > 3.0 ) go = true;
    if ( et1 > 2.0 && et_sum1 > 4.0 ) go = true;
    if ( et2 > 2.0 && et_sum2 > 4.0 ) go = true;
    if ( !go ) return false;
    */ 

    if ( fill ) hPairCounter -> Fill( kEtTow ); 

    /// Pair must be w/in eta range
    Float_t eta=pair.momentum().Eta();
    if ( eta < mCuts->eta_min || eta > mCuts->eta_max ) return false;
    if ( fill ) hPairCounter->Fill( kKinematics ); 


    /// Larger energy gamma must be w/in specified fiducial cut


    
    return true;

} 


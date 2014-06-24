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
#include "TTree.h" 
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
    mSpinSort = true;
    
}

// ----------------------------------------------------------------------------
Int_t StEEmcPi0Analysis::Init()
{

  // book histograms for pi0 analysis
  mHistograms[ kAny  ] = new SpinHistos("Any", "Pi0 spectra, unsorted");
  // book histograms for pi0 analysis
  mBackgrounds[ kAny  ] = new SpinHistos("AnyB", "combinatoric spectra, unsorted");

  mHistograms[1]=new SpinHistos("PP","Pi0 spectra, PP, spin4=dec5");
  mHistograms[2]=new SpinHistos("PN","Pi0 spectra, PN, spin4=dec6");
  mHistograms[3]=new SpinHistos("NP","Pi0 spectra, NP, spin4=dec9");
  mHistograms[4]=new SpinHistos("NN","Pi0 spectra, NN, spin4=dec10");

  mHistograms[5]=new SpinHistos("Upper","Pi0 spectra, upper sectors 1-2, 8-12");
  mHistograms[6]=new SpinHistos("Lower","Pi0 spectra, lower sectors 3-7");

  hEventCounter=new TH1F("hEventCounter","Event counts",10,0.,10.);
  hEventCounter -> GetXaxis() -> SetBinLabel(1,"raw");
  hEventCounter -> GetXaxis() -> SetBinLabel(2,"minb");
  hEventCounter -> GetXaxis() -> SetBinLabel(3,"hw trg");
  hEventCounter -> GetXaxis() -> SetBinLabel(5,"sw trg");
  hEventCounter -> GetXaxis() -> SetBinLabel(5,"sw trg[3,6]");
  hEventCounter -> GetXaxis() -> SetBinLabel(6,"vertex");
  hPairCounter  = new TH1F("hPairCounter","Pair counts",10,0.,10.);
  hFillPatternI = new TH1F("hFillPatternI","Intended fill pattern:bunch X-ing @ STAR IP:n runs",120,0.,120.);
  hSpin4        = new TH1F("hSpin4","Spin 4:spin4 state",11,0.,11.);
  hSpin4mb      = new TH1F("hSpin4mb","Spin 4:spin4 state",11,0.,11.);
  hBx7          = new TH1F("hBx7","7-bit bunch crossing:bx7",120,0.,120.);
  hBx48         = new TH1F("hBx48","48-bit bunch crossing:bx48",120,0.,120.);
  hBx7diffBx48  = new TH2F("hBx7diffBx48","bx1=(bx7-off7)%120, bx2=(bx48-off48)%120;bx7;bx1-bx2",120,0.,120.,21,-10.5,10.5);
  hBxStar       = new TH1F("hBxStar","Beam x-ing at STAR IP;star bunch x-ing",120,0.,120.); 
  hBxStarPi0    = new TH1F("hBxStarPi0","Beam x-ing at STAR IP with pi0 detected;star bunch x-ing",120,0.,120.); 

  hMassBx=new TH2F("hMassBx","Beam x-ing vs Mass;M [GeV];STAR bunch xing",120,0.,1.2,120,0.,120.); 
  hZvertBx=new TH2F("hZvertBx","Beam x-ing vs Z vertex [0.1,0.18]",150,-150.,150.,120,0.,120.); 
  hZggBx=new TH2F("hZggBx","Beam x-ing vs Zgg",100,0.,1.,120,0.,120.);
  hEtaBx=new TH2F("hEtaBx","Beam x-ing vs eta",120,0.8,2.2,120,0.,120.); 
  mRealEvent=new StEEmcMixEvent();
  mMixedEvent=new StEEmcMixEvent(); 
  mRealTree=new TTree("mRealTree","Real Events");
  mRealTree->Branch("MixEvent","StEEmcMixEvent",&mRealEvent);
  mMixedTree=new TTree("mMixedTree","Mixed Events");
  mMixedTree->Branch("MixEvent","StEEmcMixEvent",&mMixedEvent); 

  AddObj( mRealTree, ".hist" );
  AddObj( mMixedTree, ".hist" ); 

  return StMaker::Init(); 
}
// ----------------------------------------------------------------------------
Int_t StEEmcPi0Analysis::InitRun(Int_t run)
{
  
  mSpinDb->print(0); // 0=short, 1=huge
  Info("InitRun","run number = %d",run);
  const Int_t *spin8bits=mSpinDb->getSpin8bits();
  for(int bx=0;bx<120;bx++){
    Bool_t isFilled=(spin8bits[bx] & 0x11)==0x11;
    if(isFilled) hFillPatternI->Fill(bx);
  }

  mRunNumber = run;
  
  return StMaker::InitRun(run);

}
// ----------------------------------------------------------------------------
Int_t StEEmcPi0Analysis::Make()
{

  
  /// Determine spin state.
  Int_t spin4 = -1;
  Int_t bxStar = -1; 
  if ( mSpinDb -> isValid() && mSpinDb -> isPolDirLong() ) 
    spin4=getSpinState( mMuDst->muDst(), bxStar );

  hBxStar -> Fill( bxStar ); 

  mRealEvent -> setEvent( mMuDst->muDst()->event() ); 
  mRealEvent -> setSpin4( spin4 ); 
  mMixedEvent -> setEvent( mMuDst->muDst()->event() ); 
  mMixedEvent -> setSpin4( spin4 ); 

  for ( Int_t ii=0;ii<720;ii++ ) {
    StEEmcTower tower=mEEanalysis->tower(ii);
    if ( tower.fail() ) continue;
    mRealEvent->mADC[ii]=(60./4096.)*tower.adc();///meh
    mRealEvent->mStat[ii]=tower.stat();
  }

  /// Check trigger 
  if ( !accept( mMuDst->muDst() ) ) goto END_OF_MAKE;
  hPairCounter -> Fill( kEvent ); 

  /// map spin decimal bits to histograms
  static Int_t mymap[]={0,0,0,0,0,1,2,0,0,3,4};

  /// event energy sums
  mRealEvent -> mTotalEnergyT = mEEanalysis -> energy(0); 
  mRealEvent -> mTotalEnergyP = mEEanalysis -> energy(1); 
  mRealEvent -> mTotalEnergyQ = mEEanalysis -> energy(2); 
  mRealEvent -> mTotalEnergyR = mEEanalysis -> energy(3); 
  mRealEvent -> mTotalEnergyU = mEEanalysis -> energy(4); 
  mRealEvent -> mTotalEnergyV = mEEanalysis -> energy(5); 

  /// Loop over all candidate pairs
  for ( Int_t i=0;i<mEEmixer->numberOfCandidates();i++ )
    {

      StEEmcPair pair = mEEmixer->candidate(i);
      hPairCounter -> Fill( kPair ); 
      if ( !accept( pair ) ) continue; 

      mRealEvent -> addPair( pair ); 

      std::cout << "spin4=" << spin4 << " ";
      pair.print();
      mHistograms[ kAny ] -> Fill( pair );

      StEEmcPoint p1=pair.point(0);
      StEEmcPoint p2=pair.point(1);
      p1=(p1.energy()>p2.energy())?p1:p2;
      if ( p1.sector() >= 2 && p1.sector() < 7 ) 
	mHistograms[5]->Fill( pair );
      else
	mHistograms[6]->Fill( pair );

      mRealEvent->mNumberU[i] = mEEanalysis->numberOfHitStrips( p1.sector(), 0 );
      mRealEvent->mNumberV[i] = mEEanalysis->numberOfHitStrips( p1.sector(), 1 );

#if 1

      // count up the number of active towers which can form a 
      // "megacluster"
      //jb -not used Float_t mSeedEnergy = 0.8;
      Float_t mMinEnergy  = 0.1;
      Bool_t used[720];
      for ( Int_t j=0;j<720;j++ ) used[j]=false;
      StEEmcTower tow=p1.tower(0);
      StEEmcCluster c;
      if (!tow.fail() )
	{
	  c.add(tow);
	  used[tow.index()]=true;
	  StEEmcTowerVec_t hits=mEEanalysis->towers(0);
	  for ( UInt_t j=0;j<hits.size();j++ )
	    {
	      tow=hits[j];
	      if ( used[ tow.index() ] ) continue; // next hit
	      if ( tow.energy() < mMinEnergy ) continue;
	      if ( c.isNeighbor( tow ) ) {
		c.add(tow);
		used[tow.index()]=true;
	      }
	    }
	}
      mRealEvent->mNumberT[i]=c.numberOfTowers();

#endif 
#if 1
      // repeat for postshower
      mMinEnergy=0.1/1000.0;
      for ( Int_t j=0;j<720;j++ ) used[j]=false;
      Int_t ind=p1.tower(0).index();
      tow=mEEanalysis->tower(ind,3);      
      StEEmcCluster b;
       if (!tow.fail() )
	{
	  b.add(tow);
	  used[tow.index()]=true;
	  StEEmcTowerVec_t hits=mEEanalysis->towers(3);
	  for ( UInt_t j=0;j<hits.size();j++ )
	    {
	      tow=hits[j];
	      if ( used[ tow.index() ] ) continue; // next hit
	      if ( tow.energy() < mMinEnergy ) continue;
	      if ( b.isNeighbor( tow ) ) {
		b.add(tow);
		used[tow.index()]=true;
	      }
	    }
	}
      mRealEvent->mNumberR[i]=b.numberOfTowers();
#endif




      /// If spin sorting has been disabled
      if ( !mSpinSort ) {
	std::cout << "Problem detected, spin sorting disabled" << std::endl;
	continue;
      }
      
      spin4=getSpinState( mMuDst->muDst(),bxStar );
      if ( spin4>=0 )
      if ( mymap[spin4] ) {
	mHistograms[ mymap[spin4] ] -> Fill( pair );
      }

      hMassBx->Fill(pair.mass(),bxStar); 
      if ( pair.mass()>0.1 && pair.mass() < 0.18 ) {
	  hBxStarPi0->Fill(bxStar); 
	  hZvertBx->Fill(pair.vertex().Z(),bxStar);
	  hZggBx->Fill(pair.zgg(),bxStar);
	  hEtaBx->Fill(pair.momentum().Eta(),bxStar); 
      }
      

    }
 
  /// Repeat for backgrounds
  for ( Int_t i=0;i<mEEmixer->numberOfMixedCandidates();i++ )
    {

      StEEmcPair pair = mEEmixer->mixedCandidate(i);
      if ( !accept( pair, false ) ) continue;
      mBackgrounds[ kAny ] -> Fill( pair );
      mMixedEvent -> addPair( pair ); 

    }

 END_OF_MAKE:
  mRealTree->Fill();
  mMixedTree->Fill(); 
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
void StEEmcPi0Analysis::spin(const Char_t *name) 
{
  mSpinDb=(StSpinDbMaker*)GetMaker(name);
  /// no assert, null pointer expected if running on MC
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

// ----------------------------------------------------------------------------
Int_t StEEmcPi0Analysis::getSpinState( StMuDst *mudst, Int_t &bxStar )
{

  StMuEvent   *event = mudst -> event();
  StL0Trigger *trig  =&(event->l0Trigger());

  StMuTriggerIdCollection tic = event -> triggerIdCollection();
  StTriggerId l1trig = tic.l1();


  Int_t bx48 = trig->bunchCrossingId();
  Int_t bx7  = trig->bunchCrossingId7bit( mRunNumber );  

  //bxStar = mSpinDb->BXstarUsingBX48(bx48);
  bxStar = mSpinDb->BXyellowUsingBX48(bx48); 

  mRealEvent->bx48 = bx48;
  mRealEvent->bx7  = bx7;
  mRealEvent->bxStar = bxStar; 

  mMixedEvent->bx48=bx48;
  mMixedEvent->bx7 =bx7;
  mMixedEvent->bxStar=bxStar; 

  //////////////////////////////////////////////////////////////////////
  /// HARDCODED KLUDGE 
  if ( bx7 == 0 || bx7 == 119 ) return -1; // kick 0 and 119 out of mix
  if ( bx7 == 14 || bx7 == 54 ) return -1; 
  //////////////////////////////////////////////////////////////////////

  hBx7->Fill( bx7 );
  hBx48->Fill( bx48 );
  hBx7diffBx48->Fill( bx7, mSpinDb->offsetBX48minusBX7(bx48,bx7) );

  //$$$::cout << "bx7=" << bx7 << " bx48=" << bx48 << std::endl;

  if ( mSpinDb -> isMaskedUsingBX48(bx48) ) return -1;  // return an error flag
  if ( mSpinDb->offsetBX48minusBX7(bx48,bx7)!=0 ) std::cout << "BUNCH CROSSINGS INCONSISTENT" << std::endl;

  //  assert(mSpinDb->offsetBX48minusBX7(bx48,bx7)==0); // scaler boards were not in sync
  //  mSpinSort = (mSpinDb->offsetBX48minusBX7(bx48,bx7)==0);
  // disable spin sorting and clear histograms
  if ( mSpinDb->offsetBX48minusBX7(bx48,bx7)!=0 )
    {
      mSpinSort = false; 
      for ( Int_t ii=1;ii<=4;ii++ ) mHistograms[ii]->Clear();
    }


  Int_t spin4 = mSpinDb->spin4usingBX48(bx48);
  hSpin4->Fill(spin4);

  if ( l1trig.isTrigger(96011) ) hSpin4mb -> Fill(spin4); 

  
  return spin4;

}
// ----------------------------------------------------------------------------
void StEEmcPi0Analysis::Clear(Option_t *opts)
{
    mRealEvent -> Clear();
    mMixedEvent -> Clear(); 
} 

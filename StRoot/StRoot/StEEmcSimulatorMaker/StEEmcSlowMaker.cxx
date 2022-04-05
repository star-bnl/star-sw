// *-- Author : Hal Spinka
// 
// $Id: StEEmcSlowMaker.cxx,v 2.11 2010/09/07 22:24:52 stevens4 Exp $

#include <TFile.h>
#include <TH2.h>
#include <TRandom.h>
#include <StMessMgr.h>

#include "StEventTypes.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include "StEEmcUtil/database/EEmcDbItem.h"
#include "StEEmcUtil/database/StEEmcDb.h"

#include "StEEmcFastMaker.h"
#include "StEEmcSlowMaker.h"

ClassImp(StEEmcSlowMaker)

//________________________________________________
StEEmcSlowMaker::StEEmcSlowMaker(const Char_t *name, const Char_t*)
: StMaker(name) {
   mMip2ene = getMipdEdx()*0.7; // This is the SMD thickness of 7 mm
                            // times the minimum ionizing energy loss of
                            // 1.998 MeV/cm from the PDG book
   mSig1pe = 0.85;          // from info from S. Vigdor on MAPMT test results
   //set different thicknesses for pre and post layers (found in geometry debug by Jason et. al.)
   mPmip2ene[0] = getMipdEdx()*0.475; // The pre-shower tiles are only 4.75 mm thick.
   mPmip2ene[1] = getMipdEdx()*0.475; // The pre-shower tiles are only 4.75 mm thick.
   mPmip2ene[2] = getMipdEdx()*0.5;   // The post-shower tiles are only 5 mm thick.
   mPmip2pe = 2.6*1.5;      // 2.6 mip/tower scint * 1.5 light yield
                            // in pre- and post-shower elements
   // loop to init  mMip2pe[] - this will eventually need to be
   // replaced by a table of measured values
   for (Int_t i = 0;i < MaxSmdStrips;i++) {
     // This is currently a trapezoidal-parameterization of
     // the light curve measurements
     mMip2pe[i] = avgNumPePerMip(i);
   }

  mEeDb=0;
  mNInpEve=0; 
  memset(mHist,0,sizeof(mHist));

  /// By default, enable all three simulator subsystems
  mEnableTower=true;
  mEnableSMD=true;
  mEnablePrePost=true;

  /// By default, we add a pedestal offset           
  mAddPed   = true;                                  
  /// By default, we smear the pedestal              
  mSmearPed = true;                                  
  /// By default, all channels are kept
  mDropBad  = false;
  /// By default, overwrite ADC in muDst
  mOverwrite = true;

  mIsEmbeddingMode = false;
  /// By default, source is MuDst
  mSource = kMuDst;

  /// By default, truncate ped smearing at 3 sigma   
  mTruncatePedSmear = 3;
  
  /// By default, make tower energy sum corrections due to the
  /// relative light yields of the pre1, pre2 and post layers
  setDoLightYield(true);

  /// Set benchmark values for the relative light yields from
  /// measurements.  We multiply these measurements by a factor of
  /// 4.0/4.75 and 4.0/5.0 (the thickness of a normal layer
  /// divided by the thickness of the preshower and postshower
  /// layers, respectively), since the GEANT model already knows about the
  /// thickness of the layers.
  setRelativeLightYield( (4.0/4.75) * 1.68, (4.0/4.75) * 1.68, (4.0/5.0)* 0.94);

  /// Copy and verify configuration of the fast simulator
  mSamplingFraction = StEEmcFastMaker::getSamplingFraction();
  mSamplingFractionUser = mSamplingFraction;

  /// Copy fast simulator gains
  for (Int_t i = 0;i < kEEmcNumEtas;i++) {
      mTowerGains[i] = StEEmcFastMaker::getTowerGains()[i] * mSamplingFraction;
  }
  mPrepostGains = StEEmcFastMaker::getPreshowerGain();
  mSmdGains     = StEEmcFastMaker::getSmdGain();
  mMaxAdc       = StEEmcFastMaker::getMaxAdc();
  
  if ( (mPrepostGains-23000.0)>1. ||
       (mSmdGains-23000.0)>1.     ||
       (mTowerGains[0]-18.9654)>0.01 )
    {
      LOG_WARN << "--------------------------------------------" << endm;
      LOG_WARN << "You changed the gains in the fast simulator." << endm;
      LOG_WARN << "You are on your own." << endm;
      LOG_WARN << "                   -- the eemc software team" << endm;
      LOG_WARN << endm;
      LOG_WARN << "mSmdGains=" << mSmdGains << endm;
      LOG_WARN << "mPrepostGains="<<mPrepostGains<<endm;
      for ( Int_t ii=0;ii<12;ii++ )
	LOG_WARN << "mTowerGains[etabin="<<ii<<"]="<<mTowerGains[ii]<<endm;
      //assert(2+2==5);
    }

  // initialize tower gain factors to 1
  for (Int_t sec = 0;sec < kEEmcNumSectors;sec++) {
    for (Int_t sub = 0;sub < kEEmcNumSubSectors;sub++) {
      for (Int_t eta = 0;eta < kEEmcNumEtas;eta++) {
	  mTowerGainFact[sec][sub][eta] = 1.0;
      }
    }
  }

  // initialize SMD gain factors to 1
  for (Int_t sec = 0;sec < kEEmcNumSectors;sec++) {
    for (Int_t uv = 0;uv < kEEmcNumSmdUVs;uv++) {
      for (Int_t strip = 0;strip < kEEmcNumStrips;strip++) {
	  mSmdGainFact[sec][uv][strip] = 1.0;
      }
    }
  }

  // flag set if used in BFC to change defaults in Init()
  mIsBFC = GetParentChain()->InheritsFrom("StBFChain");         
}

//________________________________________________
StEEmcSlowMaker::~StEEmcSlowMaker() {
}

//________________________________________________
Float_t StEEmcSlowMaker::avgNumPePerMip(Int_t stripID) {                                                                                                           
//                                                                                                                                                          
// A parameterization of the average number of photoelectrons                                                                                               
// per mip for a given SMD strip.  See elog 457.                                                                                                            
//                                                                                                                                                          
  Float_t ya=0,yb=0,xa=1,xb=2;                                                                                                                                 
  if ( stripID<1) {                                                                                                                                          
    ;                                                                                                                                                        
  } else if (stripID<20) {                                                                                                                                   
    xa=1; ya=2.;                                                                                                                                             
    xb=20; yb=4.;                                                                                                                                            
  } else if (stripID<250) {                                                                                                                                  
    xa=20; ya=4.;                                                                                                                                            
    xb=250; yb=6.;                                                                                                                                           
  } else {                                                                                                                                                   
    xa=250; ya=6.;                                                                                                                                           
    xb=290; yb=9.;                                                                                                                                           
  }                                                                                                                                                          
  const Float_t y = ya + (yb - ya) / (xb - xa) * (stripID - xa);
  return y;
}                                                                                                                                                            
 
//________________________________________________
Int_t StEEmcSlowMaker::Init() {
  LOG_INFO << "mIsEmbeddingMode=" << mIsEmbeddingMode << ", mIsBFC=" << mIsBFC << endm;          

  if (mIsEmbeddingMode) {
    setDropBad(1);   // force bad channels to be dropped in db
    setAddPed(0);    // disable pedestal addition
    setSmearPed(0);  // disable pedestal smearing
    setOverwrite(1); // overwrites StEvent values
    setSource("StEvent");    
  } else if (mIsBFC) { //set defaults for BFC with no embedding 
    setAddPed(1);    // add pedestals                 
    setSmearPed(1);  // smear pedestals                 
    setDropBad(1);   // force bad channels to be dropped in db
    setOverwrite(1); // overwrite ADC values
    setSource("StEvent");             
  } else { //set defaults for running in analysis chain 
    if (mSource == kMuDst) disableTower(); //don't change tower in analysis chain since only ADC is stored in MuDst  
  }                                   
    
  //print out full configuration in log file  
  LOG_INFO << "mAddPed=" << mAddPed << ", mSmearPed=" << mSmearPed << ", mDropBad=" << mDropBad << ", mOverwrite=" << mOverwrite 
    << ", mEnableTower=" << mEnableTower << ", mEnableSMD=" << mEnableSMD << ", mEnablePrePost=" << mEnablePrePost << endm;

  mEeDb = (StEEmcDb*)GetDataSet("StEEmcDb");
  if (!mEeDb) {
    LOG_ERROR << "Cannot find EEMC database StEEmcDb" << endm;
  }
  InitHisto();
  if ( mSmearPed && !mAddPed) {
    LOG_WARN << "detected mSmearPed=true && mAddPed=false\nwill not work due to ETOW hits storage container in muDst not accepting negative ADC values." << endm; 
    //assert(0);
  }
  if ( mSmearPed ) {
    LOG_INFO << "detected mSmearPed,  (be sure peds>N*sig are loaded in DB!), muDst can't store negative ADC for towers, the NUadc will be forced to be non-negative" << endm; 
  }
  return StMaker::Init();
}

//________________________________________________
void StEEmcSlowMaker::InitHisto() {
  Char_t tt1[100], tt2[500];

  sprintf(tt1,"mm");
  sprintf(tt2,"freq vs. sector ID; sector ID");
  mHist[12]=new TH1F(tt1,tt2,20,-0.5,19.5);
  
  sprintf(tt1,"PreADC");
  sprintf(tt2,"Revised ADC for Pre/Post Scint.");
  mHist[0]=new TH1F(tt1,tt2,100,0,200.);
  
  sprintf(tt1,"Pre2ADC");
  sprintf(tt2,"Initial ADC for Pre/Post Scint.");
  mHist[1]=new TH1F(tt1,tt2,100,0,200.);
  
  sprintf(tt1,"SMDADC");
  sprintf(tt2,"Revised ADC for SMD Strips");
  mHist[2]=new TH1F(tt1,tt2,100,0,200.);

  sprintf(tt1,"SMD2ADC");
  sprintf(tt2,"Initial ADC for SMD Strips");
  mHist[3]=new TH1F(tt1,tt2,100,0,200.);

  mHist[4]=new TH1F("hADCtow","Initial ADC for towers",512+32,-32.,512.);
  mHist[5]=new TH1F("hADCtow2","Finial ADC fot towers",512+32,-32.,512.);

  //..................
  sprintf(tt1,"adc");
  sprintf(tt2," Adc vs strip ID; strip ID; ADC ");
  mHist[20]=new TH2F(tt1,tt2,30,0,300,100,0,200.);

  sprintf(tt1,"ADCvsstr");
  sprintf(tt2," Adc vs, strip ID; strip ID; ADC ");
  mHist[19]=new TH2F(tt1,tt2,60,0,300,100,0,200.);

  // add histos to the list (if provided)
  for (Int_t i = 0; i < maxHist; ++i) {
      if (mHist[i]) this->AddHist(mHist[i]);
  }
}

//________________________________________________
Int_t StEEmcSlowMaker::Make() {
  Int_t result = StMaker::Make();
  mNInpEve++;
  LOG_DEBUG << "iEve " << mNInpEve << ", mSource = " << mSource << endm;
  
  switch (mSource) {
  case kMuDst:
    /// Access to muDst .......................
    {
      if (!GetInputDS("MuDst")) {
	LOG_DEBUG<<"::Make() MuDst missing"<<endm;
	return kStWarn;
      }

      StMuEmcCollection* emc = StMuDst::muEmcCollection();
      if (!emc) {
	LOG_DEBUG<<"::Make() StMuEmcCollection missing"<<endm;
	return kStWarn;
      }

      /// Run slow simulator on towers
      if (mEnableTower && (result == kStOk)) result = MakeTower(emc);

      /// Run slow simulator on pre/postshower
      if (mEnablePrePost && (result == kStOk)) result = MakePrePost(emc);

      /// Run slow simulator on smd
      if (mEnableSMD && (result == kStOk)) result = MakeSMD(emc);
    }
    break;

  case kStEvent:
    /// Acces to StEvent, automatic detection if in Embedding or BFC mode .....................
    {

      StEmcCollection *emc =0; 
      if(mIsEmbeddingMode) {
	StEEmcFastMaker *fast = (StEEmcFastMaker*)GetMakerInheritsFrom("StEEmcFastMaker");
	if(fast==0) {
	  LOG_WARN << GetName() << "::Make()  no EEmcFastSim in the chain, ignore Endcap"<< endm;
	  return kStWarn;
	}
	emc = fast->GetLocalEmcCollection();
      } else { // it is not Embedding mode
	
	StEvent* event = (StEvent*)GetInputDS("StEvent");
	if (!event) {
	  LOG_WARN << GetName() << "::Make()  no StEvent"<< endm;
	  return kStWarn;
	}
	emc = event->emcCollection();
      }
      // now emc-collection should be accessible one way or another
      
      if (!emc) {
	LOG_WARN << GetName() << "::Make()  no emcCollection()" << endm;
	return kStWarn;
      }

      /// Run slow simulator on towers
      if (mEnableTower && (result == kStOk)) result = MakeTower(emc);

      /// Run slow simulator on pre/postshower
      if (mEnablePrePost && (result == kStOk)) result = MakePrePost(emc);

      /// Run slow simulator on smd
      if (mEnableSMD && (result == kStOk)) result = MakeSMD(emc);
    }
    break;

  default:
    LOG_ERROR<< "Unknown source type " << mSource << " for this event" << endm;
    break;
  }
  return result;
}

//________________________________________________
//
// Some notes on differences between using StEvent and StMuDst
//
//    Roundoff errors
// 
//    Since the MuDst only stores ADC values for the towers, it will be 
//    subject to roundoff errors.  This is most noticable for towers with
//    small ADC response.  Keep in mind that (int)2.7 rounds down to 2.
//
//    To partially offset this, we will always assume a starting ADC value
//    of ADC+0.5 for the MuDst.
//

Int_t StEEmcSlowMaker::MakeTower(StMuEmcCollection *emc) {

  /**************************************************************
   *
   * Performs pedestal addition and smearing, computes ADC based
   * on GEANT energy deposit and database gains, and corrects
   * tower ADC for the relative brightness of the pre- and post-
   * shower layers.
   *
   **************************************************************
   */

  /// Arrays to store ADC corrections for  each tower
  Float_t prepost_adc[kEEmcNumSectors][kEEmcNumSubSectors][kEEmcNumEtas];
  memset( prepost_adc, 0, sizeof(prepost_adc) );
 
  /// loop over pre and post shower hits to calculate the
  /// adc corrrections to the tower due to the relative
  /// light yields in the pre/postshower
  if (mDoLightYield) for (Int_t i = 0;i < emc->getNEndcapPrsHits();i++) {
      /// muDst ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post
      Int_t sec,sub,eta,pre;
      StMuEmcHit *hit=emc->getEndcapPrsHit(i,sec,sub,eta,pre);
      // range check on returned values
      if (!( sec >= 1 && sec <= 12) || !(sub >= 1 && sub <= 5) || !(eta >= 1 && eta <= 12) || !(pre >= 1 && pre <= 3)) {
	LOG_ERROR << "Indexing errors detected: EPRS hit " << i << ", sec = " << sec << ", sub = " << sub << ", eta = " << eta << ", pre = " << pre << endm;
	setZeroAdc(emc);
	return kStErr;
      }
      /// for pre/postshower, geant energy deposit gets propagated
      /// from StEvent into the MuDst as StMuEmcHit::energy().
      const Float_t edeposit = hit ? hit->getEnergy() : 0;

      // divide by sampling fraction, multiply by _tower_ gain
      // and brightness factor, and add to the adc sum for 
      // this tower.  Since GEANT already accounts for the 
      // difference in thickness between pre/post and normal
      // layers, the factor of 0.8 is introduced to prevent us
      // from double correcting.
      const EEmcDbItem *tower = mEeDb ? mEeDb->getTile(sec,sub-1+'A', eta, 'T') : 0;
      if (!tower) {
	LOG_ERROR << "Cannot find DB entry for ETOW: sec = " << sec << ", sub = " << sub << ", eta = " << eta << endm;
	continue;
      }
      Float_t myadc = edeposit / mSamplingFractionUser * tower->gain;
      myadc *= ( mRelativeLightYield[pre-1] - 1.0 );

      prepost_adc[sec-1][sub-1][eta-1] += myadc;
  }

  /// now loop over the tower hits to apply the correction.  note
  /// that we are using standard "STAR" indexing scheme which 
  /// counts from 1
  for (Int_t i = 0;i < emc->getNEndcapTowerADC();i++) {
      Int_t sec,sub,eta,adc;
      /// Get the ADC value stored for this tower
      emc->getEndcapTowerADC(i,adc,sec,sub,eta);
      // range check on returned values
      if (!(sec >= 1 && sec <= 12) || !(sub >= 1 && sub <= 5) || !(eta >= 1 && eta <= 12)) {
	LOG_ERROR << "Indexing errors detected: ETOW hit " << i << ", sec = " << sec << ", sub = " << sub << ", eta = " << eta << endm;
	setZeroAdc(emc);
	return kStErr;
      }
      
      const Int_t old = adc;
      if (mHist[4]) mHist[4]->Fill( old );

      /// Get the database gains
      const EEmcDbItem *tower = mEeDb ? mEeDb->getTile(sec,sub-1+'A', eta, 'T') : 0;
      if (!tower) {
	LOG_ERROR << "Cannot find DB entry for ETOW: sec = " << sec << ", sub = " << sub << ", eta = " << eta << endm;
	continue;
      }

      if (mDropBad && (tower->fail || !checkDBped(tower))) {
	//zero out values if tower has fail bit set or ped is bad        
	adc = 0;
      } else {
        /// Offset ADC to middle of the bin to minimize roundoff errrors
        Float_t myadc=(Float_t)adc + 0.5;
        /// adc value originally computed using fast simulator gains.  we
        /// change to gains specified by the database.
        myadc *= tower->gain / mTowerGains[eta-1] * mSamplingFraction / mSamplingFractionUser;
        /// Add in the pre/post brightness correction
        myadc += prepost_adc[sec-1][sub-1][eta-1];
    	/// Adjust for spread in tower gains (represents an uncertainty in measured gains)
        myadc *= ( mTowerGainFact[sec-1][sub-1][eta-1] );
        Float_t ped = tower->ped;
        /// Add pedestal offset
        if (mAddPed) {
	    if ( mSmearPed ) ped += getPedSmear( tower->sigPed );   
	    myadc += ped;
        }
        /// overwrite adc with new value (integerized) 
        adc = (Int_t)myadc;
        if (mHist[5]) mHist[5]->Fill(adc);
        if (mDropBad && (tower->gain <= 0) && mAddPed) {
	    //still add ped to tower if gain is bad since trigger emulator expects peds
	    adc = ped;
        }
        /// Check for zero values and saturation
        if (adc < 0) adc = 0;
        if (adc > mMaxAdc) adc = mMaxAdc;
      }
      if (mOverwrite) {
        if (old) {
	    LOG_DEBUG << "overwriting tower=" << tower->name << " old adc=" << old << " new adc=" << adc << endm;
	}
        /// Yes, the following is correct.  We setTowerAdc(i+1,...), and it 
        /// really does correspond to getEndcapTowerAdc(i+0,...).  It 
	/// would be nice if someone would add one line of documentation
	/// in StMuEmcCollection, rather than force people to trace this
	/// down in the code...
	emc->setTowerADC( i+1, adc, eemc );
      }
  }
  return kStOk;
}

//________________________________________________
Int_t StEEmcSlowMaker::MakePrePost(StMuEmcCollection *emc) {
  for (Int_t i = 0;i < emc->getNEndcapPrsHits();i++) {
    Int_t pre,sec,eta,sub;
    /// muDst ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post
    StMuEmcHit *hit = emc->getEndcapPrsHit(i,sec,sub,eta,pre);
    if (!hit) continue;
    
    // range check on returned values
    if (!( sec >= 1 && sec <= 12) || !(sub >= 1 && sub <= 5) || !(eta >= 1 && eta <= 12) || !(pre >= 1 && pre <= 3)) {
      LOG_ERROR << "Indexing errors detected: EPRS hit " << i << ", sec = " << sec << ", sub = " << sub << ", eta = " << eta << ", pre = " << pre << endm;
      setZeroAdc(emc);
      return kStErr;
    }

    /// tmp, for fasted analysis use only hits from sectors init in DB
    if (mEeDb && (sec < mEeDb->getFirstSector() || sec > mEeDb->getLastSector())) continue;
     
    /// Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x = mEeDb ? mEeDb->getTile(sec,sub-1+'A', eta, pre-1+'P') : 0; 
    if (!x) {
	LOG_ERROR << "Cannot find DB entry for EPRS: sec = " << sec << ", sub = " << sub << ", eta = " << eta << ", pre = " << pre << endm;
	continue;
    }

    Int_t NUadc;
    // Zero out values if tower is marked as bad
    if (mDropBad && (x->fail || !checkDBped(x) || (x->gain <= 0))) {
	NUadc=0;
    } else {
	const Float_t adc = hit->getAdc();
        const Float_t energy = hit->getEnergy();

	Float_t newadc = energy * x->gain;
	if (adc > 0) {
    	    if (mHist[1]) mHist[1]->Fill(adc);
      
    	    /// Addition of Poisson fluctuations and Gaussian 
    	    /// 1 p.e. resolution
    	    const Float_t mipval = energy / mPmip2ene[pre-1];
    	    const Float_t avgpe  = mipval * mPmip2pe;
    	    const Float_t Npe    = gRandom->Poisson(avgpe);
      
    	    if (Npe > 0) {
    		/// Determine number of photoelectrons
    		const Float_t sigmape   = sqrt(Npe) * mSig1pe;
    		Float_t smearedpe = gRandom->Gaus(Npe,sigmape);
		if (smearedpe < 0) smearedpe = 0;
    		/// Determine new ADC value
    		newadc = smearedpe * mMip2ene*(x->gain) / mPmip2pe;
    	    }
	} // non-zero ADC on input

	/// Lookup pedestal in database (possibly zero)
	Float_t ped = (mAddPed) ? x->ped : 0;
	/// Smear the pedestal
	if ( mSmearPed ) ped += getPedSmear( x->sigPed ); 
       
	/// add signal & pedestal back
	NUadc = (Int_t)(newadc + 0.5 + ped );
    
	/// Check for zero values and saturation
	if (NUadc < 0) NUadc = 0;
	if (NUadc > mMaxAdc) NUadc = mMaxAdc;
	if (adc > 0 && mHist[0]) mHist[0]->Fill(NUadc); //??
    }

    ///
    /// If we've made it here, overwrite the muDst
    ///
    if (mOverwrite) hit->setAdc(NUadc);
  }
  return kStOk;
}

//________________________________________________
Int_t StEEmcSlowMaker::MakeSMD(StMuEmcCollection *emc) {
  Int_t iuv = 0;
  for (Char_t uv = 'U';uv <= 'V';uv++) {
    iuv++;
    Int_t sec,strip;
    for (Int_t i = 0;i < emc->getNEndcapSmdHits(uv);i++) {
      StMuEmcHit *hit=emc->getEndcapSmdHit(uv,i,sec,strip);
      // range check on returned values
      if (!(strip-1 >= 0) || !(strip <= 288) || !(iuv-1 >= 0 && iuv-1 < 2) || !(sec > 0 && sec <= MaxSectors)) {
	LOG_ERROR << "Bad index for ESMD: sec = " << sec << ", iuv = " << iuv << ", strip = " << strip << endm;
	setZeroAdc(emc); 
	return kStErr;
      }
      
      // tmp, for fasted analysis use only hits from sectors init in DB
      if (mEeDb && (sec < mEeDb->getFirstSector() || sec > mEeDb->getLastSector())) continue;
      const EEmcDbItem *x = mEeDb ? mEeDb->getByStrip(sec,uv,strip) : 0;
      if (!x) {
	LOG_ERROR << "Cannot find DB entry for ESMD: sec = " << sec << ", uv = " << uv << ", strip = " << strip << endm;
	continue;
      }

      Int_t NUadc;
      // Zero out values if tower is marked as bad
      if (mDropBad && (x->fail || !checkDBped(x) || (x->gain <= 0))) {
	  NUadc=0;
      } else {
        const Float_t adc = hit->getAdc();
        const Float_t energy = hit->getEnergy();
        Float_t newadc = energy * x->gain;
      
        if (adc > 0) {
    	    if (mHist[12]) mHist[12]->Fill(x->sec);
    	    if (mHist[20]) mHist[20]->Fill(x->strip,adc); 
    	    if (mHist[3]) mHist[3]->Fill(adc);

    	    // Addition of Poisson fluctuations and 
    	    // Gaussian 1 p.e. resolution
    	    const Float_t mipval = energy / mMip2ene;
    	    const Float_t avgpe  = mipval * mMip2pe[strip];
    	    const Float_t Npe    = gRandom->Poisson(avgpe);
    	    if (Npe > 0) {
        	/// Determine number of photoelectrons
        	const Float_t sigmape   = sqrt(Npe) * mSig1pe;
        	Float_t smearedpe = gRandom->Gaus(Npe, sigmape);
		if (smearedpe < 0) smearedpe = 0;
        	/// Determine new ADC value
        	newadc = smearedpe * mMip2ene * (x->gain) / mMip2pe[strip];
    	    }
        }
      
        // Add in factor from gain smearing
        newadc *= mSmdGainFact[sec-1][iuv-1][strip-1];

        // Lookup pedestal in database (possibly zero)
        Float_t ped = (mAddPed) ? x->ped : 0;
        /// Smear the pedestal
        if ( mSmearPed ) ped += getPedSmear( x->sigPed );
            
        /// add signal & pedestal back
        NUadc = (Int_t)(newadc + 0.5 + ped );
      
        /// Check for zero values and saturation
        if (NUadc < 0) NUadc = 0;
        if (NUadc > mMaxAdc) NUadc = mMaxAdc;
        if (adc > 0 && mHist[19]) mHist[19]->Fill(x->strip,NUadc);
        if (adc > 0 && mHist[2]) mHist[2]->Fill(NUadc);
      }
      
      ///
      /// If we've made it here, overwrite the muDst
      ///
      if (mOverwrite) hit->setAdc(NUadc);
    }// loop over 1 plane
  } // loop over U,V
  return kStOk;
}

//________________________________________________
void StEEmcSlowMaker::setSource(const Char_t* name) {
  if (strcmp(name, "MuDst") == 0) {
    mSource = kMuDst;
  } else if (strcmp(name, "StEvent") == 0) {
    mSource = kStEvent;
  } else {
    LOG_WARN<<"::setSource()"<<"Source must be \"MuDst\" or \"StEvent\""<<endm;
  }
}

//________________________________________________
Int_t StEEmcSlowMaker::MakeTower(StEmcCollection* emc) {
  StEmcDetector *det = emc->detector(kEndcapEmcTowerId);
  if (!det) {
    LOG_DEBUG<<"No kEndcapEmcTowerId"<<endm;
    return kStOk;
  } 
  StEmcDetector *prepost = emc->detector(kEndcapEmcPreShowerId);
  if (!prepost) {
    LOG_WARN<<"No kEndcapEmcPreShowerId, towers are fast simu only."<<endm;
    return kStOk; 
  } 

  /// Arrays to store ADC corrections for  each tower
  Float_t prepost_adc[kEEmcNumSectors][kEEmcNumSubSectors][kEEmcNumEtas];
  memset( prepost_adc, 0, sizeof(prepost_adc) );

  for (UInt_t sec = 1;sec <= det->numberOfModules();sec++) {
      StSPtrVecEmcRawHit &prepost_hits = prepost->module(sec)->hits();
      if (mDoLightYield) for (UInt_t ihit = 0;ihit < prepost_hits.size();ihit++) {
          StEmcRawHit *hit = prepost_hits[ihit];
          const UInt_t sub = (hit->sub()-1)%5+1;
          const UInt_t eta = hit->eta();
          const UInt_t pre = (hit->sub()-1)/5+1;
          // range check on returned values
	  if (!(sec >= 1 && sec <= 12) || !(sub >= 1 && sub <= 5) || !(eta >= 1 && eta <= 12) || !(pre >= 1 && pre <= 3)) {
	    LOG_ERROR << "Indexing errors detected for EPRS: sec = " << sec << ", sub = " << sub << ", eta = " << eta << ", pre = " << pre << endm;
    	    setZeroAdc(emc); 
	    return kStErr;
	  }
	  	  
	  // energy deposit in GeV
          const Float_t edeposit = hit->energy();
          // divide by sampling fraction, multiply by _tower_ gain
          // and brightness factor, and add to the adc sum for 
          // this tower.  Since GEANT already accounts for the 
	  // difference in thickness between pre/post and normal
	  // layers, the factor of 0.8 is introduced to prevent us
	  // from double correcting.
          const EEmcDbItem *tower = mEeDb ? mEeDb->getTile(sec,sub-1+'A', eta, 'T') : 0;
          if (!tower) {
	    LOG_ERROR << "Cannot find DB entry for ETOW: sec = " << sec << ", sub = " << sub << ", eta = " << eta << endm;
	    continue;
          }
          Float_t myadc = edeposit / mSamplingFraction * tower->gain;
          myadc *= ( mRelativeLightYield[pre-1] - 1.0 );
          prepost_adc[sec-1][sub-1][eta-1] += myadc;
      }
      Int_t nTchange = 0; // counts # of towers with ADC (int) values changed
      StSPtrVecEmcRawHit &tower_hits = det->module(sec)->hits();
      for (UInt_t ihit = 0;ihit < tower_hits.size();ihit++) {
          StEmcRawHit *hit = tower_hits[ihit];
          const UInt_t sub = hit->sub();
          const UInt_t eta = hit->eta();
          // range check on returned values
	  if (!(sec >= 1 && sec <= 12) || !(sub >= 1 && sub <= 5) || !(eta >= 1 && eta <= 12)) {
	    LOG_ERROR << "Indexing errors detected for ETOW: sec = " << sec << ", sub = " << sub << ", eta = " << eta << endm;
    	    setZeroAdc(emc); 
	    return kStErr;
	  }

          // get DB entry for this tower
          const EEmcDbItem *tower = mEeDb ? mEeDb->getTile(sec,sub-1+'A', eta, 'T') : 0;
          if (!tower) {
	    LOG_ERROR << "Cannot find DB entry for ETOW: sec = " << sec << ", sub = " << sub << ", eta = " << eta << endm;
	    continue;
          }
	  Int_t adc;
          if (mDropBad && (tower->fail || !checkDBped(tower))) {
	    //zero out values if tower has fail bit set or ped is bad
	    adc = 0;
	  } else {
            if (mHist[4]) mHist[4]->Fill( hit->adc() );
            // energy deposit in GeV
            const Float_t edeposit = hit->energy();
	    // compute adc using gains stored in the databse
            Float_t myadc = edeposit / mSamplingFraction * tower->gain;
            // add in brightness correction for the pre and postshower layers
            myadc += prepost_adc[sec-1][sub-1][eta-1];
	    /// Adjust for spread in tower gains (represents an uncertainty in measured gains)
	    myadc *= ( mTowerGainFact[sec-1][sub-1][eta-1] );
	    /// Add pedestal offset
            Float_t ped = tower->ped;
            if (mAddPed) {
		if ( mSmearPed ) ped += getPedSmear( tower->sigPed );
		myadc += ped;
            }
            adc = (Int_t)myadc;
            // Zero out values if tower is marked as bad
            if (mDropBad && (tower->gain <= 0) && mAddPed) {
		//still add ped to tower if gain is bad since trigger emulator expects peds
		adc = ped;
	    }
            /// Check for zero values and saturation
            if (adc < 0) adc = 0;
            if (adc > mMaxAdc) adc = mMaxAdc;
	  }
          if (mHist[5]) mHist[5]->Fill(adc);
	  if (mOverwrite) {
	    if ((Int_t)hit->adc() != adc) nTchange++;
            LOG_DEBUG <<"overwriting tower=" << tower->name << " old adc=" << hit->adc() << " new adc=" << adc << endm;
	    hit->setAdc( adc );
	  }
      }
      LOG_DEBUG << " Etow changed " << nTchange << " ADC values for sector=" << sec << endm;
  }// loop over sectors
  return kStOk;
}

//________________________________________________
Int_t StEEmcSlowMaker::MakePrePost(StEmcCollection* emc) {
  StEmcDetector* det = emc->detector(kEndcapEmcPreShowerId);
  if (!det) {
    LOG_WARN << "No kEndcapEmcPreShowerId" << endm;
    return kStOk;
  }

  for (UInt_t sector =1; sector <= det->numberOfModules(); ++sector) {
    StSPtrVecEmcRawHit& hits = det->module(sector)->hits();
    for(UInt_t i = 0; i < hits.size(); ++i) {
      StEmcRawHit* hit = hits[i];
      Char_t sub = 'A'+(hit->sub()-1)%5;
      
      // Layer: 'P'=preshower1, 'Q'=preshower2, 'R'=postshower
      Char_t layer = 'P'+(hit->sub()-1)/5;
      Int_t layerP = (hit->sub()-1)/5;

      const UInt_t isub = (hit->sub()-1)%5+1;
      const UInt_t ieta = hit->eta();
      const UInt_t ipre = (hit->sub()-1)/5+1;
      // range check on returned values
      if (!(sector >= 1 && sector <= 12) || !(isub >= 1 && isub <= 5) || !(ieta >= 1 && ieta <= 12) || !(ipre >= 1 && ipre <= 3)) {
	LOG_ERROR << "Indexing errors detected for EPRS: sec = " << sector << ", sub = " << isub << ", eta = " << ieta << ", pre = " << ipre << endm;
	setZeroAdc(emc); 
	return kStErr;
      }
      
      // Database ranges: sector=1-12, sub=A-E, eta=1-12, type=T,P-R; Slow method
      const EEmcDbItem* x = mEeDb ? mEeDb->getTile(sector, sub, hit->eta(), layer) : 0;
      if (!x) {
        LOG_ERROR << "Cannot find DB entry for EPRS: sec = " << sector << ", sub = " << sub << ", eta = " << hit->eta() << ", pre = " << layer << endm;
	continue;
      }

      Int_t NUadc;
      // Zero out values if tower is marked as bad
      if (mDropBad && (x->fail || !checkDBped(x) || (x->gain <= 0))) {
	  NUadc=0;
      } else {
        Float_t adc = hit->adc();
        Float_t energy = hit->energy();
        Float_t newadc = energy * x->gain;
        if(adc > 0) {
	    if (mHist[1]) mHist[1]->Fill(adc);

	    /// Addition of Poisson fluctuations and Gaussian 
	    /// 1 p.e. resolution
	    const Float_t mipval = energy / mPmip2ene[layerP];
	    const Float_t avgpe  = mipval * mPmip2pe;
	    const Float_t Npe    = gRandom->Poisson(avgpe);

	    if (Npe > 0) {
		/// Determine number of photoelectrons
		const Float_t sigmape   = sqrt(Npe) * mSig1pe;
		Float_t smearedpe = gRandom->Gaus(Npe, sigmape);
		if (smearedpe < 0) smearedpe = 0;
		/// Determine new ADC value
		newadc = smearedpe * mMip2ene * x->gain / mPmip2pe;
	    }
        } // non-zero ADC on input

        /// Lookup pedestal in database (possibly zero)
        Float_t ped = mAddPed ? x->ped : 0;

        /// Smear the pedestal
        if ( mSmearPed ) ped += getPedSmear( x->sigPed );
      
        /// add signal & pedestal back
        NUadc = Int_t(newadc + 0.5 + ped);

        /// Check for zero values and saturation
        if ( NUadc < 0 ) NUadc = 0;
        if ( NUadc > mMaxAdc ) NUadc = mMaxAdc;
        if (adc > 0 && mHist[0]) mHist[0]->Fill(NUadc); //??
      }

      ///
      /// If we've made it here, overwrite the muDst
      ///
      if (mOverwrite) hit->setAdc(NUadc);
    }
  }
  return kStOk;
}

//________________________________________________
Int_t StEEmcSlowMaker::MakeSMD(StEmcCollection* emc) {
  Int_t iuv = 0;
  for (Char_t plane = 'U'; plane <= 'V'; ++plane) {
    iuv++;
    StEmcDetector* det = 0;
    switch (plane) {
    case 'U':
      det = emc->detector(kEndcapSmdUStripId);
      if (!det) {
	LOG_DEBUG << "No kEndcapSmdUStripId" << endm;
	continue;
      }
      break;
    case 'V':
      det = emc->detector(kEndcapSmdVStripId);
      if (!det) {
	LOG_DEBUG << "No kEndcapSmdVStripId" << endm;
	continue;
      }
      break;
    }

    if (!det) continue;

    for (UInt_t sector = 1; sector <= det->numberOfModules();++sector) {
      StSPtrVecEmcRawHit& hits = det->module(sector)->hits();
      for (UInt_t i = 0; i < hits.size(); ++i) {
	StEmcRawHit* hit = hits[i];
	Int_t strip = hit->eta();
	
	// range check on returned values
	if (!(strip-1 >= 0) || !(strip <= 288) || !(iuv-1 >= 0 && iuv-1 < 2) || !(sector > 0 && sector <= MaxSectors)) {
	  LOG_ERROR << "Bad index for ESMD: sec = " << sector << ", iuv = " << iuv << ", strip = " << strip << endm;
	  setZeroAdc(emc); 
	  return kStErr;
	}

	// Database ranges: sector=1-12, plane=U-V, strip=1-288
	const EEmcDbItem* x = mEeDb ? mEeDb->getByStrip(sector, plane, strip) : 0;
	if (!x) {
	  LOG_ERROR << "Cannot find DB entry for ESMD: sec = " << sector << ", uv = " << iuv-1+'U' << ", strip = " << strip << endm;
	  continue;
	}

	Int_t NUadc;
	// Zero out values if tower is marked as bad
	if (mDropBad && (x->fail || !checkDBped(x) || (x->gain <= 0))) {
	    NUadc = 0;
	} else {
	    const Float_t adc    = hit->adc();
	    const Float_t energy = hit->energy();
	    Float_t newadc = energy * x->gain;
	    if (adc > 0) {
		if (mHist[12]) mHist[12]->Fill(x->sec);
		if (mHist[2]) mHist[20]->Fill(x->strip, adc); 
		if (mHist[3]) mHist[3]->Fill(adc);
		// Addition of Poisson fluctuations and 
		// Gaussian 1 p.e. resolution
		const Float_t mipval = energy / mMip2ene;
		const Float_t avgpe  = mipval * mMip2pe[strip];
		const Float_t Npe    = gRandom->Poisson(avgpe);
		if (Npe > 0) {
		    /// Determine number of photoelectrons
		    const Float_t sigmape   = sqrt(Npe) * mSig1pe;
		    Float_t smearedpe = gRandom->Gaus(Npe, sigmape);
		    if (smearedpe < 0) smearedpe = 0;
		    /// Determine new ADC value
		    newadc = smearedpe * mMip2ene * x->gain / mMip2pe[strip];
		}
	    }
	    // Add in factor from gain smearing
	    newadc *= mSmdGainFact[sector-1][iuv-1][strip-1];
	    /// Lookup pedestal in database (possibly zero)
	    Float_t ped = mAddPed ? x->ped : 0;
	    /// Smear the pedestal
	    if ( mSmearPed ) ped += getPedSmear( x->sigPed );
	    /// Add signal & pedestal back
	    NUadc = Int_t(newadc + 0.5 + ped);
	    /// Check for zero values and saturation
	    if ( NUadc < 0 ) NUadc = 0;
	    if ( NUadc > mMaxAdc ) NUadc = mMaxAdc;
	    if (adc > 0 && mHist[19]) mHist[19]->Fill(x->strip, NUadc);
	    if (adc > 0 && mHist[2]) mHist[2]->Fill(NUadc);
	}

	///
	/// If we've made it here, overwrite the muDst
	///
	if (mOverwrite) hit->setAdc(NUadc);
      } // Loop over 1 plane
    }
  }
  return kStOk;
}

//________________________________________________
void StEEmcSlowMaker::setTowerGainSpread(Float_t s, Float_t mean) {
  LOG_INFO << "setTowerGainSpread(): gain spread: " << s << "; gain mean value: " << mean << endm;
  // initialize tower gain factors to mean +/- spread
  for ( Int_t sec=0;sec<kEEmcNumSectors;sec++ ) {
    for ( Int_t sub=0;sub<kEEmcNumSubSectors;sub++ ) {
      for ( Int_t eta=0;eta<kEEmcNumEtas;eta++ ) {
	  Float_t f = -1.0E9;
	  while (f <= -mean || f >= 1.0) f = gRandom->Gaus(0, s);
	  mTowerGainFact[sec][sub][eta] = mean + f;
      }
    }
  }
}

//________________________________________________
void StEEmcSlowMaker::setSmdGainSpread(Float_t s) {
  for (Int_t strip = 0;strip < kEEmcNumStrips;strip++) {
      setSmdGainSpread(s, strip);
  }
}

//________________________________________________
void StEEmcSlowMaker::setSmdGainSpread(Float_t s, Int_t strip) {
  for (Int_t sec = 0;sec < kEEmcNumSectors;sec++) {
    for (Int_t uv = 0;uv < kEEmcNumSmdUVs;uv++) {
      setSmdGainSpread(s, sec, uv, strip);
    }
  }
}

//________________________________________________
void StEEmcSlowMaker::setSmdGainSpread(Float_t s, Int_t sec, Int_t uv, Int_t strip) {
  const Float_t mean = 1.0;
  Float_t f = -1.0E9;
  while (f <= -mean || f >= 1.0) f = gRandom->Gaus(0, s);
  mSmdGainFact[sec][uv][strip] = mean + f;
}

//________________________________________________
Bool_t StEEmcSlowMaker::checkDBped(const EEmcDbItem *x) {
  //check that channels ped and pedSigma are all >= 0
  return (x && (x->ped >= 0) && (x->sigPed >= 0));
}

//________________________________________________
Float_t StEEmcSlowMaker::getPedSmear(Float_t sigPed) {
  //get pedestal smearing from gaussian distribution truncated at N*sigma
  Float_t smear = 0;
  if (mTruncatePedSmear > 0) {
    do {
	smear = gRandom->Gaus(0, 1.0);
    } while (fabs(smear) > mTruncatePedSmear);
  }
  return smear * sigPed;
}

//________________________________________________
void StEEmcSlowMaker::setZeroAdc(StEmcCollection* emc) {
  StDetectorId detId[4]={kEndcapEmcTowerId,kEndcapEmcPreShowerId,kEndcapSmdUStripId,kEndcapSmdVStripId};
  StEmcDetector *det=0;

  for(int i=0; i<4; i++) {
    det = emc->detector(detId[i]);
    if (!det) continue;
    for (UInt_t sec = 1;sec <= det->numberOfModules();sec++) {
      StSPtrVecEmcRawHit &det_hits = det->module(sec)->hits();
      for (UInt_t ihit = 0;ihit < det_hits.size();ihit++) {
	StEmcRawHit *hit = det_hits[ihit];
	hit->setAdc(0);
      }
    }
  }
}

//________________________________________________
void StEEmcSlowMaker::setZeroAdc(StMuEmcCollection *emc) {
  
  for (Int_t i = 0;i < emc->getNEndcapTowerADC();i++) 
    emc->setTowerADC( i+1, 0, eemc );

  for (Int_t i = 0;i < emc->getNEndcapPrsHits();i++) {
    Int_t pre,sec,eta,sub;
    StMuEmcHit *hit = emc->getEndcapPrsHit(i,sec,sub,eta,pre);
    if (hit) hit->setAdc(0);
  }

  for (Char_t uv = 'U';uv <= 'V';uv++) {
    Int_t sec,strip;
    for (Int_t i = 0;i < emc->getNEndcapSmdHits(uv);i++) {
      StMuEmcHit *hit=emc->getEndcapSmdHit(uv,i,sec,strip);
      if (hit) hit->setAdc(0);
    }
  }
}

//________________________________________________
Float_t StEEmcSlowMaker::getMipdEdx() {
  // Return MIP dE/dx = 1.998 MeV/cm from the PDG book
  // used to simulate SMD, Pre, Post ADC response
  return 0.001998;
}

// $Log: StEEmcSlowMaker.cxx,v $
// Revision 2.11  2010/09/07 22:24:52  stevens4
// give access to MIP dE/dx to other makers
//
// Revision 2.10  2010/08/03 02:20:40  stevens4
// final update from peer review
//
// Revision 2.9  2010/07/29 16:12:03  ogrebeny
// Update after the peer review
//
// Revision 2.8  2010/05/03 20:47:22  ogrebeny
// Some code cleanup
//
// Revision 2.7  2010/02/12 23:02:38  ogrebeny
// By the request of the photon group, added an option to shift EEMC gains in the slow simulator.
//
// Revision 2.6  2009/11/23 23:44:32  ogrebeny
// At Pibero's request, for the embedding infrastructure.
//
// Revision 2.5  2009/02/05 20:06:53  ogrebeny
// Changed StEEmcDbMaker -> StEEmcDb
//
// Revision 2.4  2008/04/15 00:15:52  jwebb
// Fixed bug in sector 12V.
//
// Revision 2.3  2008/04/11 14:37:17  jwebb
// Added options to disable operation of individual slow simulaor subsystems.
//
// Revision 2.2  2007/11/28 16:17:30  jwebb
// Added the following features:
//
// 1. User may specify the sampling fraction.
//
// 2. Tower and SMD gain spreads.
//
// Revision 2.1  2007/01/24 21:07:03  balewski
// 1) no cout or printf, only new Logger
// 2) EndcapMixer:
//    - no assert()
//    - locks out on first fatal error til the end of the job
//
// Revision 2.0  2007/01/13 00:03:04  jwebb
// Upgrade of the slow simulator.  The following changes have been made:
//
// 1. Towers will always be masked out when a "fail" bit is set in the database.
//    Previously this only happened if pedestals were being added, smeared.
//
// 2. Tower, preshower and postshower ADC values will be simulated using the
//    GEANT energy loss stored in StEmcHit and StMuEmcHit.  Previously, ADC
//    values from the fast simulator were used and energy loss recovered
//    using gains, resulting in roundoff errors.   Note that towers still use
//    the old path for MuDst-based analysis.
//
// 3. Tower simulation now accounts for the different light yields provided
//    by the brighter scintillator and two-fiber readout in the preshower
//    and postshower layers.  Previously, only the difference in thickness
//    was accounted for by the GEANT simulation.
//
// Revision 1.5  2006/12/12 20:29:14  balewski
// added hooks for Endcap embedding
//
// Revision 1.4  2006/08/07 18:50:11  balewski
// added capabilty to run on StEvent, use se-method, see macros/ for example
//
// Revision 1.3  2005/09/23 17:23:01  balewski
// now peds are added also to ADC of zero
// fixed bug in ETOW ped smearing
//
// Revision 1.2  2005/09/23 01:30:10  jwebb
// Tower peds now added  if option is set.
//
// Revision 1.1  2004/12/15 17:02:56  balewski
// try 2
//

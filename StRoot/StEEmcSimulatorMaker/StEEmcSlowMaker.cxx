// *-- Author : Hal Spinka
// 
// $Id: StEEmcSlowMaker.cxx,v 2.5 2009/02/05 20:06:53 ogrebeny Exp $

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
//________________________________________________
StEEmcSlowMaker::StEEmcSlowMaker( const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  eeDb=0;
  mHList=0;
  nInpEve=0; 
  memset(hA,0,sizeof(hA));

  /// By default, enable all three simulator subsystems
  mEnableTower=true;
  mEnableSMD=true;
  mEnablePrePost=true;

  /// By default, we do not add a pedestal offset
  mAddPed   = false;
  /// By default, we do not smear the pedestal
  mSmearPed = false;
  /// By default, all channels are kept
  mDropBad  = false;
  /// By default, overwrite ADC in muDst
  mOverwrite = true;

  mIsEmbeddingMode=false;
  /// By default, source is MuDst
  mSource = kMuDst;

  /// By default, make tower energy sum corrections due to the
  /// relative light yields of the pre1, pre2 and post layers
  setDoLightYield(true);

  /// Set benchmark values for the relative light yields from
  /// measurements.  We multiply these measurements by a factor of 
  /// 4.0/5.0 (the thickness of a normal layer divided by the 
  /// thickness of the special layers), since the GEANT model
  /// already knows about the thickness of the layers.
  setRelativeLightYield( (4.0/5.0) * 1.68, (4.0/5.0) * 1.68, (4.0/5.0)* 0.94);

  /// Copy and verify configuration of the fast simulator
  mSamplingFraction = StEEmcFastMaker::getSamplingFraction();
  mSamplingFractionUser = mSamplingFraction;

  /// Copy fast simulator gains
  for ( Int_t ii=0;ii<kEEmcNumEtas;ii++ )
    {
      mTowerGains[ii] = StEEmcFastMaker::getTowerGains()[ii] * mSamplingFraction;
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
      assert(2+2==5);
    }

  // initialize tower gain factors to 1
  for ( Int_t sec=0;sec<kEEmcNumSectors;sec++ )
    for ( Int_t sub=0;sub<kEEmcNumSubSectors;sub++ )
      for ( Int_t eta=0;eta<kEEmcNumEtas;eta++ )
	{
	  mTowerGainFact[sec][sub][eta]=1.0;
	}

  // initialize SMD gain factors to 1
  for ( Int_t sec=0;sec<kEEmcNumSectors;sec++ )
    for ( Int_t uv=0;uv<kEEmcNumSmdUVs;uv++ )
      for ( Int_t strip=0;strip<kEEmcNumStrips;strip++ )
	{
	  mSmdGainFact[sec][uv][strip]=1.0;
	}

}


//___________________ _____________________________
//________________________________________________
StEEmcSlowMaker::~StEEmcSlowMaker(){
}

 
//________________________________________________
//________________________________________________
Int_t StEEmcSlowMaker::Init(){
 LOG_INFO<<Form("%s::Init(), mIsEmbeddingMode=%d",GetName(),mIsEmbeddingMode)<<endm;

  if(mIsEmbeddingMode) {
    setDropBad(1);   // force bad channels to be dropped in db
    setAddPed(0);    // disable pedestal addition
    setSmearPed(0);  // diabale pedestal smearing
    setOverwrite(1); // overwrites StEvent values
    setSource("StEvent");    
  }


  eeDb=(StEEmcDb*)GetDataSet("StEEmcDb");
  assert( eeDb);
  if ( mHList ) InitHisto();
  if ( mSmearPed && !mAddPed) {
       LOG_ERROR<<"::Init() detected mSmearPed=true && mAddPed=false \n will not work due to ETOW hits storage container in muDst not accepting negative ADC values, ABORT" << endm; 
       exit(1);
  }
  if ( mSmearPed )
    LOG_INFO<<"::Init() detected mSmearPed,  (be sure peds>>N*sig are loaded in DB!), muDst can't store negative ADC for towers, the NUadc will be forced to be non-negative" << endm; 
  
  return StMaker::Init();
}

//________________________________________________
//________________________________________________
void StEEmcSlowMaker::InitHisto(){

  char tt1[100], tt2[500];
  int sectID=5;

  sprintf(tt1,"mm");
  sprintf(tt2,"freq vs. sector ID; sector ID");
  hA[12]=new TH1F(tt1,tt2,20,-0.5,19.5);
  
  sprintf(tt1,"PreADC");
  sprintf(tt2,"Revised ADC for Pre/Post Scint.");
  hA[0]=new TH1F(tt1,tt2,100,0,200.);
  
  sprintf(tt1,"Pre2ADC");
  sprintf(tt2,"Initial ADC for Pre/Post Scint.");
  hA[1]=new TH1F(tt1,tt2,100,0,200.);
  
  sprintf(tt1,"SMDADC");
  sprintf(tt2,"Revised ADC for SMD Strips");
  hA[2]=new TH1F(tt1,tt2,100,0,200.);

  sprintf(tt1,"SMD2ADC");
  sprintf(tt2,"Initial ADC for SMD Strips");
  hA[3]=new TH1F(tt1,tt2,100,0,200.);

  hA[4]=new TH1F("hADCtow","Initial ADC for towers",512+32,-32.,512.);
  hA[5]=new TH1F("hADCtow2","Finial ADC fot towers",512+32,-32.,512.);

  //..................
  sprintf(tt1,"adc%02d",sectID);
  sprintf(tt2," Adc vs, strip ID, sector=%02d; strip ID; ADC ",sectID);
  hA[20]=new TH2F(tt1,tt2,30,0,300,100,0,200.);

  sprintf(tt1,"ADCvsstr");
  sprintf(tt2," Adc vs, strip ID; strip ID; ADC ");
  hA[19]=new TH2F(tt1,tt2,60,0,300,100,0,200.);

  // add histos to the list (if provided)
  if (mHList) {
    for (int i = 0; i < mxH; ++i) {
      if (hA[i] == 0) continue;
      mHList->Add(hA[i]);
    }
  }
}

//________________________________________________
//________________________________________________
Int_t StEEmcSlowMaker::InitRun(int runNo){
  if(runNo==0) {
    LOG_WARN<<"::InitRun("<<runNo<<") ??? changed to 555, it s OK for M-C - perhaps, JB"<<endm;
    runNo=555;
  }
  return kStOK;
}



//________________________________________________
//________________________________________________
Int_t StEEmcSlowMaker::Finish(){
  return kStOK;
}


//________________________________________________
//________________________________________________
Int_t StEEmcSlowMaker::Make(){

  nInpEve++;
  LOG_DEBUG << GetName() << "::Make() is called , iEve " << nInpEve << " mSource="<<mSource<<endm;
  
  mKSigma = eeDb->getKsigOverPed();

  switch (mSource) {
  case kMuDst:
    /// Access to muDst .......................
    {
      if (!GetInputDS("MuDst")) {
	LOG_DEBUG<<"::Make() MuDst missing"<<endm;
	return kStOk;
      }

      StMuEmcCollection* emc = StMuDst::muEmcCollection();
      if (!emc) {
	LOG_DEBUG<<"::Make() StMuEmcCollection missing"<<endm;
	return kStOk;
      }

      /// Run slow simulator on towers
      if ( mEnableTower) MakeTower(emc);

      /// Run slow simulator on pre/postshower
      if ( mEnablePrePost ) MakePrePost(emc);

      /// Run slow simulator on smd
      if ( mEnableSMD ) MakeSMD(emc);
    }
    break;

  case kStEvent:
    /// Acces to StEvent, automatic detection if in Embedding mode .....................
    {

      StEmcCollection *emc =0; 
      if(mIsEmbeddingMode) {
	StEEmcFastMaker *fast = (StEEmcFastMaker*)GetMaker("EEmcFastSim");
	if(fast==0) {
	  LOG_WARN << GetName() << "::Make()  no EEmcFastSim in the chain, ignore Endcap"<< endm;
	  return kStOk;
	}
	emc = fast->GetLocalEmcCollection();
      } else { // it is not Embedding mode
	
	StEvent* event = (StEvent*)GetInputDS("StEvent");
	if (!event) {
	  LOG_WARN << GetName() << "::Make()  no StEvent"<< endm;
	  return kStOk;
	}
	emc = event->emcCollection();
      }
      // now emc-collection should be accessible one way or another
      
      if (!emc) {
	LOG_WARN << GetName() << "::Make()  no emcCollection()" << endm;
	return kStOk;
      }

      /// Run slow simulator on towers
      if ( mEnableTower ) MakeTower(emc);

      /// Run slow simulator on pre/postshower
      if ( mEnablePrePost ) MakePrePost(emc);

      /// Run slow simulator on smd
      if ( mEnableSMD) MakeSMD(emc);
    }
    break;

  default:
    LOG_ERROR<< "Unknown source type " << mSource << " for this event" << endm;
    break;
  }

  return  kStOK;

}
// ----------------------------------------------------------------------------

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

void StEEmcSlowMaker::MakeTower( StMuEmcCollection *emc )
{

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
  if ( mDoLightYield )
  for ( Int_t i = 0; i < emc->getNEndcapPrsHits(); i++ ) 
    {

      /// muDst ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post
      Int_t sec,sub,eta,pre;
      StMuEmcHit *hit=emc->getEndcapPrsHit(i,sec,sub,eta,pre);
      // range check on returned values
      assert( sec >= 1 && sec <= 12 ); // Indexing errors detected
      assert( sub >= 1 && sub <= 5  ); // Indexing errors detected
      assert( eta >= 1 && eta <= 12 ); // Indexing errors detected
      assert( pre >= 1 && pre <= 3  ); // Indexing errors detected

      /// for pre/postshower, geant energy deposit gets propagated
      /// from StEvent into the MuDst as StMuEmcHit::energy().
      Float_t edeposit = hit->getEnergy();

      // divide by sampling fraction, multiply by _tower_ gain
      // and brightness factor, and add to the adc sum for 
      // this tower.  Since GEANT already accounts for the 
      // difference in thickness between pre/post and normal
      // layers, the factor of 0.8 is introduced to prevent us
      // from double correcting.

      const EEmcDbItem *tower=eeDb-> getTile(sec,sub-1+'A', eta, 'T');
      Float_t myadc = edeposit / mSamplingFractionUser * tower->gain;
      myadc *= ( mRelativeLightYield[pre-1] - 1.0 );

      prepost_adc[sec-1][sub-1][eta-1] += myadc;

    }

  /// now loop over the tower hits to apply the correction.  note
  /// that we are using standard "STAR" indexing scheme which 
  /// counts from 1
  for (Int_t i=0; i < emc->getNEndcapTowerADC(); i++)
    {

      Int_t sec,sub,eta,adc,old;

      /// Get the ADC value stored for this tower
      emc->getEndcapTowerADC(i,adc,sec,sub,eta);
      assert( sec >= 1 && sec <= 12 ); // Indexing errors detected
      assert( sub >= 1 && sub <= 5  ); // Indexing errors detected
      assert( eta >= 1 && eta <= 12 ); // Indexing errors detected
      old=adc;
      if ( mHList ) hA[4]->Fill( old );

      /// Get the database gains
      const EEmcDbItem *tower=eeDb-> getTile(sec,sub-1+'A', eta, 'T');

      /// Offset ADC to middle of the bin to minimize roundoff errrors
      Float_t myadc=(Float_t)adc + 0.5;
      /// adc value originally computed using fast simulator gains.  we
      /// change to gains specified by the database.
      myadc *= tower->gain / mTowerGains[eta-1] * mSamplingFraction / mSamplingFractionUser;

      /// Add in the pre/post brightness correction
      myadc += prepost_adc[sec-1][sub-1][eta-1];

      /// Adjust for spread in tower gains (represents an uncertainty in measured gains)
      myadc *= ( mTowerGainFact[sec-1][sub-1][eta-1] );

      /// Add pedestal offset
      if ( mAddPed )
        {
          Float_t ped = tower->ped;
          if ( mSmearPed )
            {
              ped += gRandom -> Gaus( 0, tower->sigPed );
            }
          myadc += ped;
        }

      /// overwrite adc with new value (integerized) 
      adc = (Int_t)myadc;
      if ( mHList ) hA[5]->Fill(adc);

      /// Check for zero values and saturation
      if ( adc < 0 )
        adc = 0;
      if ( adc > mMaxAdc )
        adc = mMaxAdc;

      // Zero out values if tower is marked as bad
      if ( mDropBad && tower->fail )
        {
          adc = 0;
        }

      if (mOverwrite) {
        if(old)LOG_DEBUG <<"overwriting tower=" << tower->name << " old adc=" << old << " new adc=" << adc << endm;
        /// Yes, the following is correct.  We setTowerAdc(i+1,...), and it 
        /// really does correspond to getEndcapTowerAdc(i+0,...).  It 
	/// would be nice if someone would add one line of documentation
	/// in StMuEmcCollection, rather than force people to trace this
	/// down in the code...
	emc->setTowerADC( i+1, adc, eemc );
      }

    }


}

// ----------------------------------------------------------------------------
void StEEmcSlowMaker::MakePrePost( StMuEmcCollection *emc ){

  for ( Int_t i = 0; i < emc->getNEndcapPrsHits(); i++ ) {

    Int_t pre,sec,eta,sub;

    /// muDst ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post
    StMuEmcHit *hit=emc->getEndcapPrsHit(i,sec,sub,eta,pre);
    
    /// tmp, for fasted analysis use only hits from sectors init in DB
    if (sec<eeDb->getFirstSector() || sec>eeDb->getLastSector()) continue;
     
    /// Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x=eeDb-> getTile(sec,sub-1+'A', eta, pre-1+'P'); 
    if(x==0) continue;
  
    /// Drop broken channels
    if(mDropBad) if(x->fail ) continue; 
      
    Float_t adc   = hit -> getAdc();
    Float_t energy = hit->getEnergy();

    Float_t newadc = energy * x->gain;
    if(adc>0) {
      if ( mHList )  hA[1]->Fill(adc);
      
      /// Addition of Poisson fluctuations and Gaussian 
      /// 1 p.e. resolution
      Float_t mipval = energy / Pmip2ene;
      Float_t avgpe  = mipval * Pmip2pe;
      Float_t Npe    = gRandom->Poisson(avgpe);
      
      if (Npe>0) {
        /// Determine number of photoelectrons
        Float_t sigmape   = sqrt(Npe) * sig1pe;
        Float_t smearedpe = gRandom -> Gaus(Npe,sigmape);
        /// Determine new ADC value
        newadc= smearedpe * mip2ene*(x->gain)/Pmip2pe;
      }
    }// non-zero ADC on input

    /// Lookup pedestal in database (possibly zero)
    Float_t ped    = (mAddPed)?x -> ped:0;
    /// Smear the pedestal
    if ( mSmearPed ) ped = gRandom -> Gaus(ped,x->sigPed);
    
    /// add signal & pedestal back
    Int_t   NUadc     = (Int_t)(newadc + 0.5 + ped );
    
    if (adc>0 &&  mHList )   hA[0]->Fill(NUadc); //??

    ///
    /// If we've made it here, overwrite the muDst
    ///
    if ( mOverwrite ) hit -> setAdc( NUadc );
    
  }

}

// ----------------------------------------------------------------------------
void StEEmcSlowMaker::MakeSMD( StMuEmcCollection *emc ) {
  
  Int_t iuv = 0;
  Char_t uv='U';
  for ( uv='U'; uv <= 'V'; uv++ ) {
    iuv++;
    
    Int_t sec,strip;
    for (Int_t i = 0; i < emc->getNEndcapSmdHits(uv); i++ ) {
      
      StMuEmcHit *hit=emc->getEndcapSmdHit(uv,i,sec,strip);
      assert(sec>0 && sec<=MaxSectors);// total corruption of muDst
      
      // tmp, for fasted analysis use only hits from sectors init in DB
      if(sec<eeDb->getFirstSector() || sec>eeDb->getLastSector()) continue;
      
      const EEmcDbItem *x=eeDb->getByStrip(sec,uv,strip);
      assert(x); // it should never happened for muDst

      /// Drop broken channels
      if(mDropBad)  if(x->fail ) continue;
      
      Float_t adc    = hit->getAdc();
      Float_t energy = hit->getEnergy();
      Float_t newadc = energy * x->gain;
      
      if(adc>0) {

        if ( mHList ) { 
          hA[12]->Fill(x->sec);
          if(x->sec==5) hA[20]->Fill(x->strip,adc); 
          hA[3]->Fill(adc);
        }

        // Addition of Poisson fluctuations and 
        // Gaussian 1 p.e. resolution
        Float_t mipval = energy / mip2ene;
        Float_t avgpe  = mipval * mip2pe[strip];
        Float_t Npe    = gRandom->Poisson(avgpe);

        if (Npe>0) {
          
          /// Determine number of photoelectrons
          Float_t sigmape   = sqrt(Npe) * sig1pe;
          Float_t smearedpe = gRandom -> Gaus(Npe,sigmape);
          
          /// Determine new ADC value
          newadc    = smearedpe * mip2ene * (x->gain) / mip2pe[strip];
        }
      }
      

      /// Add in factor from gain smearing
      assert(strip-1>=0); // or die strips are counted from zero already
      assert(sec-1>=0); // or die sectors are counted from zero already
      assert(iuv-1>=0&&iuv-1<2); 
      newadc *= mSmdGainFact[sec-1][iuv-1][strip-1];


      /// Lookup pedestal in database (possibly zero)
      Float_t ped    = (mAddPed)?x -> ped:0;
      /// Smear the pedestal
      if ( mSmearPed ) ped = gRandom -> Gaus(ped,x->sigPed);
      
      /// add signal & pedestal back
      Int_t   NUadc     = (Int_t)(newadc + 0.5 + ped );
      
      if (adc>0 && mHList ) {
        hA[19]->Fill(x->strip,NUadc);
        hA[2]->Fill(NUadc);
      }
      
      ///
      /// If we've made it here, overwrite the muDst
      ///
      if ( mOverwrite ) hit -> setAdc( NUadc );
      
    }// loop over 1 plane
  } // loop over U,V
 
}

void StEEmcSlowMaker::setSource(const Char_t* name)
{
  if (strcmp(name, "MuDst") == 0)
    mSource = kMuDst;
  else if (strcmp(name, "StEvent") == 0)
    mSource = kStEvent;
  else {
    LOG_WARN<<"::setSource()"<<"Source must be \"MuDst\" or \"StEvent\""<<endm;
  }
}

// ----------------------------------------------------------------------------
void StEEmcSlowMaker::MakeTower(StEmcCollection* emc)
{

  StEmcDetector *det = emc->detector(kEndcapEmcTowerId);
  if (!det) {
    LOG_DEBUG<<"No kEndcapEmcTowerId"<<endm;
    return;
  } 

  StEmcDetector *prepost = emc->detector(kEndcapEmcPreShowerId);
  if (!prepost) {
    LOG_WARN<<"No kEndcapEmcPreShowerId, towers are fast simu only."<<endm;
    return; 
  } 

  /// Arrays to store ADC corrections for  each tower
  Float_t prepost_adc[kEEmcNumSectors][kEEmcNumSubSectors][kEEmcNumEtas];
  memset( prepost_adc, 0, sizeof(prepost_adc) );


  for ( UInt_t sec = 1; sec <= det->numberOfModules(); sec++ )
    {

      StSPtrVecEmcRawHit &prepost_hits = prepost->module(sec)->hits();

      if ( mDoLightYield )
      for ( UInt_t ihit=0;ihit<prepost_hits.size();ihit++ )
        {
          StEmcRawHit *hit = prepost_hits[ihit];
          UInt_t sub = (hit->sub()-1)%5+1;
          UInt_t eta = hit->eta();
          UInt_t pre = (hit->sub()-1)/5+1;
          
          assert( sec >= 1 && sec <= 12 ); // Indexing errors detected
          assert( sub >= 1 && sub <= 5  ); // Indexing errors detected
          assert( eta >= 1 && eta <= 12 ); // Indexing errors detected
          assert( pre >= 1 && pre <= 3  ); // Indexing errors detected

          // energy deposit in GeV
          Float_t edeposit = hit->energy();

          // divide by sampling fraction, multiply by _tower_ gain
          // and brightness factor, and add to the adc sum for 
          // this tower.  Since GEANT already accounts for the 
	  // difference in thickness between pre/post and normal
	  // layers, the factor of 0.8 is introduced to prevent us
	  // from double correcting.

          const EEmcDbItem *tower=eeDb-> getTile(sec,sub-1+'A', eta, 'T');
          Float_t myadc = edeposit / mSamplingFraction * tower->gain;
          myadc *= ( mRelativeLightYield[pre-1] - 1.0 );

          prepost_adc[sec-1][sub-1][eta-1] += myadc;

        }

      int nTchange=0; // counts # of towers with ADC (int) values changed
      StSPtrVecEmcRawHit &tower_hits = det->module(sec)->hits();
      for ( UInt_t ihit=0;ihit<tower_hits.size();ihit++ )
        {

          StEmcRawHit *hit = tower_hits[ihit];
          UInt_t sub = hit->sub();
          UInt_t eta = hit->eta();

          assert( sec >= 1 && sec <= 12 ); // Indexing errors detected
          assert( sub >= 1 && sub <= 5  ); // Indexing errors detected
          assert( eta >= 1 && eta <= 12 ); // Indexing errors detected

          // get DB entry for this tower
          const EEmcDbItem *tower=eeDb-> getTile(sec,sub-1+'A', eta, 'T');
          assert(tower);
          if ( mHList ) hA[4]->Fill( hit->adc() );

          // energy deposit in GeV
          Float_t edeposit = hit->energy();
	  // compute adc using gains stored in the databse
          Float_t myadc = edeposit / mSamplingFraction * tower->gain;

          // add in brightness correction for the pre and postshower layers
          myadc += prepost_adc[sec-1][sub-1][eta-1];

          /// Add pedestal offset
          if ( mAddPed )
            {
              Float_t ped = tower->ped;
              if ( mSmearPed )
                {
                  ped += gRandom -> Gaus( 0, tower->sigPed );
                }
              myadc += ped;
            }

          Int_t adc = (Int_t)myadc;

          /// Check for zero values and saturation
          if ( adc < 0 )
            adc = 0;
          if ( adc > mMaxAdc )
            adc = mMaxAdc;

          // Zero out values if tower is marked as bad
          if ( mDropBad && tower->fail )
            {
              adc = 0;
            }

          if ( mHList ) hA[5]->Fill(adc);

          if (mOverwrite) {
	    if( (int)hit->adc() !=adc) nTchange++;

            LOG_DEBUG <<"overwriting tower=" << tower->name << " old adc=" << hit->adc() << " new adc=" << adc << endm;
            hit->setAdc( adc );
          }

        }

      LOG_INFO <<Form(" Etow changed %d ADC values for sector=%d",nTchange, sec )<<endm;
    }// loop over sectors

}

// ----------------------------------------------------------------------------
void StEEmcSlowMaker::MakePrePost(StEmcCollection* emc)
{
  StEmcDetector* det = emc->detector(kEndcapEmcPreShowerId);
  if (!det) {
    LOG_WARN<<Form("No kEndcapEmcPreShowerId");
    return;
  }

  for (UInt_t sector =1; sector <= det->numberOfModules(); ++sector) {
    StSPtrVecEmcRawHit& hits = det->module(sector)->hits();
    for(UInt_t i = 0; i < hits.size(); ++i) {
      StEmcRawHit* hit = hits[i];
      Char_t sub = 'A'+(hit->sub()-1)%5;

      // Layer: 'P'=preshower1, 'Q'=preshower2, 'R'=postshower
      Char_t layer = 'P'+(hit->sub()-1)/5;

      // Database ranges: sector=1-12, sub=A-E, eta=1-12, type=T,P-R; Slow method
      const EEmcDbItem* x = eeDb->getTile(sector, sub, hit->eta(), layer);
      if (!x) continue;

      // Drop broken channels
      if (mDropBad && x->fail) continue;
      Float_t adc = hit->adc();
      Float_t energy = hit->energy();
      Float_t newadc = energy * x->gain;
      if(adc > 0) {
	if (mHList) hA[1]->Fill(adc);

	/// Addition of Poisson fluctuations and Gaussian 
	/// 1 p.e. resolution
	Float_t mipval = energy / Pmip2ene;
	Float_t avgpe  = mipval * Pmip2pe;
	Float_t Npe    = gRandom->Poisson(avgpe);

	if (Npe > 0) {
	  /// Determine number of photoelectrons
	  Float_t sigmape   = sqrt(Npe) * sig1pe;
	  Float_t smearedpe = gRandom->Gaus(Npe, sigmape);

	  /// Determine new ADC value
	  newadc = smearedpe * mip2ene * x->gain / Pmip2pe;
	}
      } // non-zero ADC on input

      /// Lookup pedestal in database (possibly zero)
      Float_t ped = mAddPed ? x->ped : 0;

      /// Smear the pedestal
      if (mSmearPed) ped = gRandom->Gaus(ped, x->sigPed);

      /// add signal & pedestal back
      Int_t NUadc = Int_t(newadc + 0.5 + ped);

      if (adc > 0 && mHList) hA[0]->Fill(NUadc); //??

      ///
      /// If we've made it here, overwrite the muDst
      ///
      if (mOverwrite) hit->setAdc(NUadc);
    }
  }
}

// ----------------------------------------------------------------------------
void StEEmcSlowMaker::MakeSMD(StEmcCollection* emc)
{
  for (Char_t plane = 'U'; plane <= 'V'; ++plane) {
    StEmcDetector* det = 0;
    switch (plane) {
    case 'U':
      det = emc->detector(kEndcapSmdUStripId);
      if (!det) {
	gMessMgr->Debug("No kEndcapSmdUStripId");
	continue;
      }
      break;
    case 'V':
      det = emc->detector(kEndcapSmdVStripId);
      if (!det) {
	gMessMgr->Debug("No kEndcapSmdVStripId");
	continue;
      }
      break;
    }

    assert(det);

    for (UInt_t sector = 1; sector <= det->numberOfModules();++sector) {
      StSPtrVecEmcRawHit& hits = det->module(sector)->hits();
      for (UInt_t i = 0; i < hits.size(); ++i) {
	StEmcRawHit* hit = hits[i];
	Int_t strip = hit->eta();
	// Database ranges: sector=1-12, plane=U-V, strip=1-288
	const EEmcDbItem* x = eeDb->getByStrip(sector, plane, strip);
	if (!x) continue;

	/// Drop broken channels
	if (mDropBad && x->fail) continue;

	Float_t adc    = hit->adc();
	Float_t energy = hit->energy();
	Float_t newadc = energy * x->gain;

	if(adc > 0) {

	  if (mHList) { 
	    hA[12]->Fill(x->sec);
	    if (x->sec == 5) hA[20]->Fill(x->strip, adc); 
	    hA[3]->Fill(adc);
	  }

	  // Addition of Poisson fluctuations and 
	  // Gaussian 1 p.e. resolution
	  Float_t mipval = energy / mip2ene;
	  Float_t avgpe  = mipval * mip2pe[strip];
	  Float_t Npe    = gRandom->Poisson(avgpe);

	  if (Npe > 0) {

	    /// Determine number of photoelectrons
	    Float_t sigmape   = sqrt(Npe) * sig1pe;
	    Float_t smearedpe = gRandom->Gaus(Npe, sigmape);

	    /// Determine new ADC value
	    newadc = smearedpe * mip2ene * x->gain / mip2pe[strip];
	  }
	}

	/// Lookup pedestal in database (possibly zero)
	Float_t ped = mAddPed ? x->ped : 0;

	/// Smear the pedestal
	if (mSmearPed) ped = gRandom->Gaus(ped, x->sigPed);

	/// Add signal & pedestal back
	Int_t NUadc = Int_t(newadc + 0.5 + ped);

	if (adc > 0 && mHList) {
	  hA[19]->Fill(x->strip, NUadc);
	  hA[2]->Fill(NUadc);
	}

	///
	/// If we've made it here, overwrite the muDst
	///
	if (mOverwrite) hit->setAdc(NUadc);

      } // Loop over 1 plane
    }
  }
}



// ----------------------------------------------------------------------------
void StEEmcSlowMaker::setSmearPed( Bool_t s ){ mSmearPed = s; }
void StEEmcSlowMaker::setAddPed( Bool_t a ) { mAddPed = a; }
void StEEmcSlowMaker::setDropBad( Bool_t d ) { mDropBad = d; }
void StEEmcSlowMaker::setOverwrite( Bool_t o ) { mOverwrite = o; }

void StEEmcSlowMaker::setMipElossSmd(Float_t e){ mip2ene=e; }
void StEEmcSlowMaker::setMipElossPre(Float_t e){ Pmip2ene=e; }
void StEEmcSlowMaker::setSinglePeResolution(Float_t s){ sig1pe=s; }
void StEEmcSlowMaker::setNpePerMipSmd( Int_t strip, Float_t npe ) { mip2pe[strip] = npe; }
void StEEmcSlowMaker::setNpePerMipSmd( Float_t npe ) { for ( Int_t i = 0; i < 288; i++ ) mip2pe[i] = npe; }
void StEEmcSlowMaker::setNpePerMipPre( Float_t npe ) { Pmip2pe = npe; }

void StEEmcSlowMaker::setRelativeLightYield( Float_t pre1, Float_t pre2, Float_t post ){ mRelativeLightYield[kPre1]=pre1; mRelativeLightYield[kPre2]=pre2; mRelativeLightYield[kPost]=post; }

void StEEmcSlowMaker::setEmbeddingMode(Bool_t x){ mIsEmbeddingMode=x;} 

// ----------------------------------------------------------------------------
void StEEmcSlowMaker::setTowerGainSpread(Float_t s)
{

  // initialize tower gain factors to 1
  for ( Int_t sec=0;sec<kEEmcNumSectors;sec++ )
    for ( Int_t sub=0;sub<kEEmcNumSubSectors;sub++ )
      for ( Int_t eta=0;eta<kEEmcNumEtas;eta++ )
	{
	  //	  mTowerGainFact[sec][sub][eta]=1.0;

	  Float_t f = -1.0E9;
	  while ( f <= -1. || f >= 1.0 )
	    f = gRandom->Gaus(0., s);

	  mTowerGainFact[sec][sub][eta] = 1.0 + f;

	}

}

// ----------------------------------------------------------------------------
void StEEmcSlowMaker::setSmdGainSpread( Float_t s )
{
  for ( Int_t strip=0;strip<kEEmcNumStrips;strip++ )
    {
      setSmdGainSpread(s,strip);
    }
}

void StEEmcSlowMaker::setSmdGainSpread( Float_t s, Int_t strip )
{
  for ( Int_t sec=0;sec<kEEmcNumSectors;sec++ )
    for ( Int_t uv=0;uv<kEEmcNumSmdUVs;uv++ )
      setSmdGainSpread(s, sec,uv, strip );
}

void StEEmcSlowMaker::setSmdGainSpread( Float_t s, Int_t sec, Int_t uv, Int_t strip )
{

  Float_t f = -1.0E9;
  while ( f <= -1. || f >= 1.0 )
    f = gRandom->Gaus(0., s);

  mSmdGainFact[sec][uv][strip]= 1.0 + f;
  
}


// $Log: StEEmcSlowMaker.cxx,v $
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

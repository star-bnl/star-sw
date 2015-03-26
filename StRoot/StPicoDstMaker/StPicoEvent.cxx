#include "StPicoEvent.h"
#include "StPicoConstants.h"
#include "StEventTypes.h"
#include "StTree.h"
#include "StuRefMult.hh"
#include "TVector2.h"
#include "StMuDSTMaker/COMMON/StMuDst.h" 
#include "StMuDSTMaker/COMMON/StMuTrack.h" 
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMtdHeader.h"
#include "StBTofHeader.h"
#include "StMessMgr.h"
#include "StPicoUtilities.h"

ClassImp(StPicoEvent)

StPicoEvent::StPicoEvent()
{}

//StPicoEvent::StPicoEvent(StMuEvent* ev, StBTofHeader *header, float *Q)
StPicoEvent::StPicoEvent(const StMuDst& muDst, const Float_t* Q)
{
  StMuEvent* ev = muDst.event() ;
  StBTofHeader* header = muDst.btofHeader() ;

  mRunId = ev->runNumber();
  mEventId = ev->eventNumber();
  mFillId = ev->runInfo().beamFillNumber(blue);
  mBField=ev->magneticField();

  mPrimaryVertex = ev->primaryVertexPosition();
  mPrimaryVertexError = ev->primaryVertexErrors();
  if( mPrimaryVertex.x()==mPrimaryVertex.y()&&mPrimaryVertex.y()==mPrimaryVertex.z() ){
    mPrimaryVertex.set(-999.,-999.,-999.);
    mPrimaryVertexError.set(0.,0.,0);
  }

  unsigned int triggerId = 0;
  for(int i=0;i<nTrigger;i++) {
    if(ev->triggerIdCollection().nominal().isTrigger(Pico::mTriggerId[i])) triggerId |= (1 << i);
  }
  mTriggerWord = (UInt_t)triggerId;

  triggerId = 0;
  for(int i=0;i<nTriggerMtd;i++) {
    if(ev->triggerIdCollection().nominal().isTrigger(Pico::mTriggerIdMtd[i])) triggerId |= (1 << i);
  }
  mTriggerWordMtd = (UInt_t)triggerId;

  mRefMultFtpcEast = (UShort_t)(ev->refMultFtpcEast());
  mRefMultFtpcWest = (UShort_t)(ev->refMultFtpcWest());
  mRefMultNeg = (UShort_t)(ev->refMultNeg());
  mRefMultPos = (UShort_t)(ev->refMultPos());
  mRefMult2NegEast = (UShort_t)StPicoUtilities::refMult2(0, 0, muDst);
  mRefMult2PosEast = (UShort_t)StPicoUtilities::refMult2(1, 0, muDst);
  mRefMult2NegWest = (UShort_t)StPicoUtilities::refMult2(0, 1, muDst);
  mRefMult2PosWest = (UShort_t)StPicoUtilities::refMult2(1, 1, muDst);
  mRefMultHalfNegEast = (UShort_t)StPicoUtilities::refMultHalf(0, 0, muDst);
  mRefMultHalfPosEast = (UShort_t)StPicoUtilities::refMultHalf(1, 0, muDst);
  mRefMultHalfNegWest = (UShort_t)StPicoUtilities::refMultHalf(0, 1, muDst);
  mRefMultHalfPosWest = (UShort_t)StPicoUtilities::refMultHalf(1, 1, muDst);
  mGRefMult = (UShort_t)ev->grefmult();
  mNHitsHFT[0] = (UShort_t)ev->numberOfPxlInnerHits();
  mNHitsHFT[1] = (UShort_t)ev->numberOfPxlOuterHits();
  mNHitsHFT[2] = (UShort_t)ev->numberOfIstHits();
  mNHitsHFT[3] = (UShort_t)ev->numberOfSsdHits();


  LOG_DEBUG << "Tmp: Neg: RefMult(org) = " << mRefMultNeg << "  (new) = " << StPicoUtilities::refMult(0, muDst)
    << " refmult2 = " << mRefMult2NegEast+ mRefMult2PosEast+ mRefMult2NegWest+ mRefMult2PosWest
    << " refmulthalf(<0) = " << mRefMultHalfNegEast+ mRefMultHalfPosEast
    << " refmulthalf(>0) = " << mRefMultHalfNegWest+ mRefMultHalfPosWest
    << endm;

  mNVpdHitsEast = 0;
  mNVpdHitsWest = 0;
  mNT0 = 0;
  mVzVpd = Pico::SHORTMAX;
  if(header) {
    mNVpdHitsEast = (UChar_t)(header->numberOfVpdHits(east));
    mNVpdHitsWest = (UChar_t)(header->numberOfVpdHits(west));
    mNT0 = (UShort_t)(header->nTzero());
    mVzVpd = (fabs(header->vpdVz()*100.)>Pico::SHORTMAX) ? Pico::SHORTMAX : (UShort_t)(TMath::Nint(header->vpdVz()*100.));
  }

  mbTofTrayMultiplicity = ev->btofTrayMultiplicity() ;
  mNumberOfGlobalTracks = muDst.numberOfGlobalTracks() ;

  StMuPrimaryVertex* pv = muDst.primaryVertex() ;
  if(pv){
    mRanking = pv->ranking() ;
    mNBEMCMatch = pv->nBEMCMatch() ;
    mNBTOFMatch = pv->nBTOFMatch() ;
  }
  else{
    mRanking = -999.;
    mNBEMCMatch = 0;
    mNBTOFMatch = 0;
  }


    //Nov.10, 2008, Na
    StZdcTriggerDetector &ZDC = ev->zdcTriggerDetector();
    mZdcSumAdcEast = (UShort_t)ZDC.adcSum(east);
    mZdcSumAdcWest = (UShort_t)ZDC.adcSum(west);   
    for (int strip=1;strip<9;strip++) {
        if (ZDC.zdcSmd(east,1,strip)) 
          mZdcSmdEastHorizontal[strip-1] = (UShort_t)ZDC.zdcSmd(east,1,strip);
        if (ZDC.zdcSmd(east,0,strip))  
          mZdcSmdEastVertical[strip-1] = (UShort_t)ZDC.zdcSmd(east,0,strip);
        if (ZDC.zdcSmd(west,1,strip)) 
          mZdcSmdWestHorizontal[strip-1] = (UShort_t)ZDC.zdcSmd(west,1,strip);
        if (ZDC.zdcSmd(west,0,strip))  
          mZdcSmdWestVertical[strip-1] = (UShort_t)ZDC.zdcSmd(west,0,strip);
    }

    StVpdTriggerDetector &VPD = ev->vpdTriggerDetector();

    for(int i=0; i<16; i++){
      //event.VPD[i]= 1.0*theVPD.adc(i);
      if(i>=0 && i<8) {
        mVpd[i]=(UShort_t)VPD.ADC(east,8-i);
        mVpd[i+8]=(UShort_t)VPD.TDC(east,8-i);
        mVpd[i+32]=(UShort_t)VPD.ADC(west,8-i);
        mVpd[i+40]=(UShort_t)VPD.TDC(west,8-i);
      }
      if(i>=8 && i<16) {
        mVpd[i+8]=(UShort_t)VPD.ADC(east,32-(i+8));
        mVpd[i+16]=(UShort_t)VPD.TDC(east,32-(i+8));
        mVpd[i+40]=(UShort_t)VPD.ADC(west,32-(i+8));
        mVpd[i+48]=(UShort_t)VPD.TDC(west,32-(i+8));
        //cout<<"VPD-------  "<<VPD.ADC(east,32-(i+8))<<endl;
      }
    }

    mZDCx = (UInt_t)ev->runInfo().zdcCoincidenceRate();
    mBBCx = (UInt_t)ev->runInfo().bbcCoincidenceRate();

    mBackgroundRate = ev->runInfo().backgroundRate();

    mBbcBlueBackgroundRate = ev->runInfo().bbcBlueBackgroundRate();
    mBbcYellowBackgroundRate = ev->runInfo().bbcYellowBackgroundRate();

    mBbcEastRate = ev->runInfo().bbcEastRate();
    mBbcWestRate = ev->runInfo().bbcWestRate();

    mZdcEastRate = ev->runInfo().zdcEastRate();
    mZdcWestRate = ev->runInfo().zdcWestRate();

    mSpaceCharge = ev->runInfo().spaceCharge();

    // BBC ADC (Hiroshi)
    StBbcTriggerDetector bbc = ev->bbcTriggerDetector() ;
    for(UInt_t i=0;i<bbc.numberOfPMTs();i++){
      const UInt_t eastWest = (i<24) ? 0 : 1 ; // East:0-23, West:24-47
      const UInt_t pmtId    = i%24 ;           // pmtId:0-23

      if( eastWest == 0 ) mBbcAdcEast[pmtId] = bbc.adc(i) ;
      else                mBbcAdcWest[pmtId] = bbc.adc(i) ;
    }

#if 0
    mQx_ran_1 = Q[0];
    mQy_ran_1 = Q[1];
    mQx_ran_2 = Q[2];
    mQy_ran_2 = Q[3];
    
    mQx = mQx_ran_1 + mQx_ran_2;
    mQy = mQy_ran_1 + mQy_ran_2;
    
    mQx_chg_pos = Q[4];
    mQy_chg_pos = Q[5];
    mQx_chg_neg = Q[6];
    mQy_chg_neg = Q[7];
    
    mQx_eta_pos = Q[8];
    mQy_eta_pos = Q[9];
    mQx_eta_neg = Q[10];
    mQy_eta_neg = Q[11];
#endif

    for(int i=0;i<4;i++) {
      setHT_Th(i,0);
      setJP_Th(i,0);
    }
}

StPicoEvent::~StPicoEvent()
{ }

int StPicoEvent::year() const
{
  return mRunId/1000000 - 1 + 2000;
}

int StPicoEvent::day() const
{
  return (mRunId%1000000)/1000;
}

float StPicoEvent::energy() const
{
  if(year()<2010) return 0.0; // not applicable for data before year 2010
  if(year()==2010) {
    if(day()<=77) return 200.0;
    else if(day()<=98) return 62.4;
    else if(day()<=112) return 39.;
    else if(day()<=147) return 7.7;
    else return 11.5;
  } else if(year()==2011) {
    if(day()>=112&&day()<=122) return 19.6;
    if(day()>=172&&day()<=179) return 27.;
    if(day()>=123&&day()<=171) return 200.;
  } else if(year()==2013) {
    return 510.;
  } else if(year()==2014) {
    if(day()>=046&&day()<=070) return 14.5;
    if(day()>=077) return 200.;
  }
  return 0.0;
}

bool StPicoEvent::isMinBias() const  // continue to be updated
{
  if(year()<2010) return kFALSE;  // not applicable for data before year 2010
  if(year()==2010) {
    if(fabs(energy()-200.)<1.e-4) {
      return kTRUE;      // 200 GeV, minbias stored in a separated output, always true
    } else if(fabs(energy()-62.4)<1.e-4) {
      return ( mTriggerWord & 0x7 );
    } else if(fabs(energy()-39.)<1.e-4) {
      return ( mTriggerWord & 0x1 );
    } else if(fabs(energy()-11.5)<1.e-4) {
      return ( mTriggerWord & 0x3 );
    } else if(fabs(energy()-7.7)<1.e-4) {
      return ( mTriggerWord & 0x3 );
    }
  } else if(year()==2011) {
    if(fabs(energy()-19.6)<1.e-4) {
      return ( mTriggerWord & 0x7 );
    } else if(fabs(energy()-27.)<1.e-4) {
      return ( mTriggerWord & 0x1 );
    } else if(fabs(energy()-200.)<1.e-4) {
      return ( mTriggerWord>>2 & 0x1f );  // return vpd-minbias-protected
//      return kTRUE;     // 200 GeV, only minbias
    }
  } else if(year()==2014) {
    if(fabs(energy()-14.5)<1.e-4) {
      return ( (mTriggerWord>>2 & 0x1) || (mTriggerWord>>5 & 0x1) );
    } else if(fabs(energy()-200.)<1.e-4) {
      return ( mTriggerWord & 0x1f );
    }
  }

  return kFALSE;
}

bool StPicoEvent::isMBSlow() const  // continue to be updated
{
  if(year()<2010) return kFALSE;  // not applicable for data before year 2010
  if(year()==2010) {
    if(fabs(energy()-200.)<1.e-4) {
      return kFALSE;     // no mbslow data produced yet
    } else if(fabs(energy()-62.4)<1.e-4) { 
      return ( mTriggerWord>>3 & 0x1 );    
    } else if(fabs(energy()-39.)<1.e-4) {
      return ( mTriggerWord>>1 & 0x1 );
    } else if(fabs(energy()-11.5)<1.e-4) {
      return ( mTriggerWord>>2 & 0x3 );
    } else if(fabs(energy()-7.7)<1.e-4) {
      return ( mTriggerWord>>2 & 0x1 );
    }
  } else if(year()==2011) {
    if(fabs(energy()-19.6)<1.e-4) {
      return ( mTriggerWord>>3 & 0x7 );
    } else if(fabs(energy()-27.)<1.e-4) {
      return ( mTriggerWord>>1 & 0x1 );  
    } else if(fabs(energy()-200.)<1.e-4) {
      return kFALSE;
    }
  }
  return kFALSE;
}

bool StPicoEvent::isCentral() const  // continue to be updated
{
  if(year()<2010) return kFALSE;  // not applicable for data before year 2010
  if(year()==2010) { 
    if(fabs(energy()-200.)<1.e-4) {
      return kTRUE;      // 200 GeV, central stored in a separated output, always true
    } else if(fabs(energy()-62.4)<1.e-4) { 
      return ( mTriggerWord>>4 & 0x1 );    
    }
  } else if(year()==2014) {
    if(fabs(energy()-200.)<1.e-4) {
      return ( mTriggerWord>>18 & 0x1 );
    }
  }
  return kFALSE;
}

bool StPicoEvent::isHT() const    // continue to be updated
{
  if(year()<2010) return kFALSE;  // not applicable for data before year 2010
  if(year()==2010) {
    if(fabs(energy()-200.)<1.e-4) {
      return kFALSE;      // 200 GeV, no HT data so far
    } else if(fabs(energy()-62.4)<1.e-4) { 
      return ( mTriggerWord>>5 & 0xF );    
    } else if(fabs(energy()-39.)<1.e-4) {
      return ( mTriggerWord>>2 & 0x1 );
    } else if(fabs(energy()-7.7)<1.e-4) {
      return ( mTriggerWord>>3 & 0x1 );
    }
  } else if(year()==2011) {
    if(fabs(energy()-19.6)<1.e-4) {
      return ( mTriggerWord>>6 & 0x1 );
    } else if(fabs(energy()-27.)<1.e-4) {
      return ( mTriggerWord>>2 & 0x1 );  
    } else if(fabs(energy()-200.)<1.-4) {
      return kFALSE;
    }
  } else if(year()==2014) {
    if(fabs(energy()-200.)<1.e-4) { 
      return ( mTriggerWord>>19 & 0xf );
    }
  }
  return kFALSE;
}
    
bool StPicoEvent::isHT11() const    // continue to be updated
{ 
  if(!isHT()) return kFALSE;
  if(year()==2010) { 
    if(fabs(energy()-62.4)<1.e-4) {
      return ( mTriggerWord>>5 & 0x7 );
    } 
  }
  return kTRUE;  // default HT trigger ht-11
} 
    
bool StPicoEvent::isHT15() const    // continue to be updated
{ 
  if(!isHT()) return kFALSE;
  if(year()==2010) { 
    if(fabs(energy()-62.4)<1.e-4) {
      return ( mTriggerWord>>8 & 0x1 );
    } 
  } else if(year()==2014) {
    if(fabs(energy()-200.)<1.e-4) { 
      return ( mTriggerWord>>19 & 0x3 );
    }
  }              
  return kFALSE;
} 
bool StPicoEvent::isHT18() const    // continue to be updated
{
  if(!isHT()) return kFALSE;
  if(year()==2014) {
    if(fabs(energy()-200.)<1.e-4) {
      return ( mTriggerWord>>21 & 0x3 );
    }
  }
  return kFALSE;
}

bool StPicoEvent::isMtdTrig() const    // continue to be updated
{ 
  if(isDiMuon() || isSingleMuon() || isEMuon())
    return kTRUE;
  else
    return kFALSE;
}

bool StPicoEvent::isDiMuon() const    // continue to be updated
{ 
  if(year()==2014)
    { 
      for(Int_t i=0; i<8; i++)
	{
	  if(mTriggerWordMtd & (1<<i)) 
	    return kTRUE;
	}
    }
  else if(year()==2013)
    { 
      for(Int_t i=0; i<2; i++)
	{
	  if(mTriggerWordMtd & (1<<i)) 
	    return kTRUE;
	}
    }
  return kFALSE;
}

bool StPicoEvent::isDiMuonHFT() const    // continue to be updated
{ 
  if(year()==2014)
    { 
      for(Int_t i=5; i<8; i++)
	{
	  if(mTriggerWordMtd & (1<<i)) 
	    return kTRUE;
	}
    }
  return kFALSE;
} 

bool StPicoEvent::isSingleMuon() const    // continue to be updated
{ 
  if(year()==2014)
    { 
      for(Int_t i=13; i<18; i++)
	{
	  if(mTriggerWordMtd & (1<<i)) 
	    return kTRUE;
	}
    }
  else if(year()==2013)
    { 
      for(Int_t i=5; i<7; i++)
	{
	  if(mTriggerWordMtd & (1<<i)) 
	    return kTRUE;
	}
    }
  return kFALSE;
}

bool StPicoEvent::isEMuon() const    // continue to be updated
{ 
  if(year()==2014)
    { 
      for(Int_t i=8; i<13; i++)
	{
	  if(mTriggerWordMtd & (1<<i)) 
	    return kTRUE;
	}
    }
  else if(year()==2013)
    { 
      for(Int_t i=2; i<5; i++)
	{
	  if(mTriggerWordMtd & (1<<i)) 
	    return kTRUE;
	}
    }
  return kFALSE;
}

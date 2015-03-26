#include <iostream>
#include <bitset>

#include "StTriggerData.h"
#include "StMessMgr.h"

#include "StPicoMtdTrigger.h"

ClassImp(StPicoMtdTrigger)

//----------------------------------------------------------------------------------
StPicoMtdTrigger::StPicoMtdTrigger()
{
  memset(mQTtacSum,0,sizeof(mQTtacSum));
  memset(mMT101Tac,0,sizeof(mMT101Tac));
  memset(mMT101Id,0,sizeof(mMT101Id));
  mTF201TriggerBit = 0;
}

//----------------------------------------------------------------------------------
StPicoMtdTrigger::StPicoMtdTrigger(const StTriggerData *trigger)
{
  // QT information
  UShort_t mtdQTtac[4][16];
  for(Int_t i=0; i<32; i++)
    {
      if((i/4)%2==1)
        {
          mtdQTtac[0][i-i/4*2-2] = trigger->mtdAtAddress(i,0);
          mtdQTtac[1][i-i/4*2-2] = trigger->mtdgemAtAddress(i,0);
          mtdQTtac[2][i-i/4*2-2] = trigger->mtd3AtAddress(i,0);
          mtdQTtac[3][i-i/4*2-2] = trigger->mtd4AtAddress(i,0);
        }
    }

  for(Int_t im=0; im<4; im++)
    {
      for(Int_t i=0; i<16; i++)
        {
          if(i%2==1) continue;
	  mQTtacSum[im][i/2] = 0;

          Int_t j2 = mtdQTtac[im][i];
          Int_t j3 = mtdQTtac[im][i+1];

          if(j2<mtd_qt_tac_min || j2>mtd_qt_tac_max || 
             j3<mtd_qt_tac_min || j3>mtd_qt_tac_max ||
             TMath::Abs(j2-j3)>mtd_qt_tac_diff_range_abs) continue;

	  mQTtacSum[im][i/2] = j2 + j3;
	}
    }

  // MT101
  for(Int_t i = 0; i < 4; i++)
    {
      mMT101Tac[i][0] = (trigger->mtdDsmAtCh(3*i,0)) + ((trigger->mtdDsmAtCh(3*i+1,0)&0x3)<<8);
      mMT101Id[i][0]  = (trigger->mtdDsmAtCh(3*i+1,0)&0xc)>>2;
      mMT101Tac[i][1] = (trigger->mtdDsmAtCh(3*i+1,0)>>4) + ((trigger->mtdDsmAtCh(3*i+2,0)&0x3f)<<4);
      mMT101Id[i][1]  = (trigger->mtdDsmAtCh(3*i+2,0)&0xc0)>>6;
    }

  // TF201
  mTF201TriggerBit = 0;
  UShort_t  decision = trigger->dsmTF201Ch(0);
  for(Int_t i=0; i<8; i++)
    {
      mTF201TriggerBit |= ((decision>>(i+4))&0x1)<<i;
    }
  LOG_DEBUG << "input  = " << (std::bitset<16>) decision << endm;  
  LOG_DEBUG << "output = " << (std::bitset<8>)  mTF201TriggerBit << endm;
}

//----------------------------------------------------------------------------------
StPicoMtdTrigger::~StPicoMtdTrigger()
{
}


//----------------------------------------------------------------------------------
void StPicoMtdTrigger::getMaximumQTtac(const Int_t qt, Int_t& pos1, Int_t& pos2)
{
  pos1 = 0;
  pos2 = 0;

  UShort_t max1 = 0, max2 = 0;

  for(Int_t i=0; i<8; i++)
    {
      if(max1 < mQTtacSum[qt-1][i])
	{
	  max2 = max1;
	  pos2 = pos1;
	  max1 = mQTtacSum[qt-1][i];
	  pos1 = i+1;
	}
      else if(max2 < mQTtacSum[qt-1][i])
	{
	  max2 = mQTtacSum[qt-1][i];
	  pos2 = i+1;
	}
    }
}

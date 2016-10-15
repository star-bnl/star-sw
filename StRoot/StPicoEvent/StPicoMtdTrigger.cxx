#include <bitset>

#include "StEvent/StTriggerData.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuMtdHeader.h"
#include "St_base/StMessMgr.h"
#include "StEvent/StEnumerations.h"

#include "StPicoEvent/StPicoMtdTrigger.h"


//----------------------------------------------------------------------------------
StPicoMtdTrigger::StPicoMtdTrigger():
  mVpdTacSum(0), mTHUBtime{},
  mQTtacSum{}, mMT101Tac{}, mMT101Id{}, mTF201TriggerBit(0),
  mShouldHaveRejectEvent(-1)
{
}

//----------------------------------------------------------------------------------
StPicoMtdTrigger::StPicoMtdTrigger(const StMuDst& muDst, const int QTtoModule[8][8],
                                   const int QTSlewBinEdge[8][16][8], const int QTSlewCorr[8][16][8]): StPicoMtdTrigger()
{
  // Header information
  StMuMtdHeader* muMtdHeader = muDst.mtdHeader();
  if (muMtdHeader)
  {
    mTHUBtime[0] = 25 * (muMtdHeader->triggerTime(1) & 0xfff);
    mTHUBtime[1] = 25 * (muMtdHeader->triggerTime(2) & 0xfff);
    mShouldHaveRejectEvent = (Char_t)(muMtdHeader->shouldHaveRejectEvent());
  }

  // RHIC year
  const int runnumber = muDst.event()->runNumber();
  int year = runnumber / 1e6 + 1999;
  if ((runnumber % 1000) / 1000 > 334) year += 1;

  // Trigger data
  UShort_t mtd_qt_tac_min = 100;
  if (runnumber >= 16045067) mtd_qt_tac_min = 80;
  UShort_t mtd_qt_tac_diff_range_abs = 600;
  if (year == 2015) mtd_qt_tac_diff_range_abs = 1023;

  StTriggerData* trigger = const_cast<StTriggerData*>(muDst.event()->triggerData());
  if (trigger)
  {
    // VPD tac sum
    mVpdTacSum = trigger->vpdEarliestTDCHighThr(east) + trigger->vpdEarliestTDCHighThr(west);

    // QT information
    UShort_t mtdQTadc[kNQTboard][16];
    UShort_t mtdQTtac[kNQTboard][16];
    memset(mtdQTadc, 0, sizeof(mtdQTadc));
    memset(mtdQTtac, 0, sizeof(mtdQTtac));

    for (Int_t i = 0; i < 32; i++)
    {
      Int_t type = (i / 4) % 2;
      if (year == 2016)
      {
        for (int im = 0; im < kNQTboard; im++)
        {
          if (type == 0) mtdQTadc[im][i - i / 4 * 2] = trigger->mtdQtAtCh(im + 1, i, 0);
          else        mtdQTtac[im][i - i / 4 * 2 - 2] = trigger->mtdQtAtCh(im + 1, i, 0);
        }
      }
      else
      {
        if (type == 1)
        {
          mtdQTtac[0][i - i / 4 * 2 - 2] = trigger->mtdAtAddress(i, 0);
          mtdQTtac[1][i - i / 4 * 2 - 2] = trigger->mtdgemAtAddress(i, 0);
          mtdQTtac[2][i - i / 4 * 2 - 2] = trigger->mtd3AtAddress(i, 0);
          mtdQTtac[3][i - i / 4 * 2 - 2] = trigger->mtd4AtAddress(i, 0);
        }
        else
        {
          mtdQTadc[0][i - i / 4 * 2] = trigger->mtdAtAddress(i, 0);
          mtdQTadc[1][i - i / 4 * 2] = trigger->mtdgemAtAddress(i, 0);
          mtdQTadc[2][i - i / 4 * 2] = trigger->mtd3AtAddress(i, 0);
          mtdQTadc[3][i - i / 4 * 2] = trigger->mtd4AtAddress(i, 0);
        }
      }
    }

    Int_t j[2], a[2];
    for (Int_t im = 0; im < kNQTboard; im++)
    {
      for (int i = 0; i < 8; i++)
      {
        if (year == 2016 && i % 2 == 0) // moniter channel only used for Run16
	{
	  mQTtacSum[im][i] = 0;
	  continue; 
	}

        // Apply slewing correction
        for (int k = 0; k < 2; k++)
        {
          j[k] = mtdQTtac[im][i * 2 + k];
          a[k] = mtdQTadc[im][i * 2 + k];

          int slew_bin = -1;
          if (a[k] > 0 && a[k] <= QTSlewBinEdge[im][i * 2 + k][0]) slew_bin = 0;
          else
          {
            for (int l = 1; l < 8; l++)
            {
              if (a[k] > QTSlewBinEdge[im][i * 2 + k][l - 1] && a[k] <= QTSlewBinEdge[im][i * 2 + k][l])
              {
                slew_bin = l;
                break;
              }
            }
          }
          if (slew_bin >= 0)
            j[k] += QTSlewCorr[im][i * 2 + k][slew_bin];
        }

        if (j[0] <= mtd_qt_tac_min || j[0] >= mtd_qt_tac_max ||
            j[1] <= mtd_qt_tac_min || j[1] >= mtd_qt_tac_max ||
            TMath::Abs(j[0] - j[1]) >= mtd_qt_tac_diff_range_abs) // no "equal" in online algorithm
        {
          mQTtacSum[im][i] = 0;
          continue;
        }

	// Apply position correction 	
	int module = QTtoModule[im][i];
 	if(module<0)
 	{
	  mQTtacSum[im][i] = 0;
	  continue;
	}
 	mQTtacSum[im][i] = UShort_t( j[0] + j[1] + abs(module-3)*1./8 * (j[0]-j[1]) );
      }
    }

    // MT101
    for (Int_t i = 0; i < kNQTboard; i++)
    {
      int idx = 0;
      if(year==2016) idx = i / 2 * 3 + i % 2 * 16;
      else           idx = i * 3;
      mMT101Tac[i][0] = (trigger->mtdDsmAtCh(idx, 0)) + ((trigger->mtdDsmAtCh(idx + 1, 0) & 0x3) << 8);
      mMT101Id[i][0]  = (trigger->mtdDsmAtCh(idx + 1, 0) & 0xc) >> 2;
      mMT101Tac[i][1] = (trigger->mtdDsmAtCh(idx + 1, 0) >> 4) + ((trigger->mtdDsmAtCh(idx + 2, 0) & 0x3f) << 4);
      mMT101Id[i][1]  = (trigger->mtdDsmAtCh(idx + 2, 0) & 0xc0) >> 6;
    }

    // TF201
    UInt_t decision = trigger->dsmTF201Ch(0);
    UInt_t decision2 = 0;
    if (year == 2016) decision2 = trigger->dsmTF201Ch(6);
    mTF201TriggerBit = 0;

    for (Int_t i = 0; i < 4; i++)
    {
      for (Int_t j = 0; j < 2; j++)
      {
        if(year==2016)
        {
          int qt = i * 2;
          mTF201TriggerBit |= ((decision >> (i * 2 + j + 4)) & 0x1) << (qt * 2 + j);
          qt = i * 2 + 1;
          mTF201TriggerBit |= ((decision2 >> (i * 2 + j + 4)) & 0x1) << (qt * 2 + j);
        }
	else
        {
          int qt = i;
          mTF201TriggerBit |= ((decision >> (i * 2 + j + 4)) & 0x1) << (qt * 2 + j);
        }
      }
    }

    LOG_DEBUG << "input1 = " << (std::bitset<16>) decision << "\n"
              << "input2 = " << (std::bitset<16>) decision2 << "\n"
              << "output = " << (std::bitset<16>) mTF201TriggerBit
              << endm;
  }
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

  if (qt < 1 || qt > kNQTboard)
  {
    LOG_ERROR << "Wrong qt board number: " << qt << endm;
    return;
  }

  UShort_t max1 = 0, max2 = 0;
  for (Int_t i = 0; i < 8; i++)
  {
    if (max1 < mQTtacSum[qt - 1][i])
    {
      max2 = max1;
      pos2 = pos1;
      max1 = mQTtacSum[qt - 1][i];
      pos1 = i + 1;
    }
    else if (max2 < mQTtacSum[qt - 1][i])
    {
      max2 = mQTtacSum[qt - 1][i];
      pos2 = i + 1;
    }
  }
}

//
// StPicoMtdTrigger stores trigger information related to MTD
//

// C++ headers
#include <bitset>
#include <math.h>

// PicoDst headers
#include "StPicoMessMgr.h"
#include "StPicoMtdTrigger.h"

ClassImp(StPicoMtdTrigger)

//_________________
StPicoMtdTrigger::StPicoMtdTrigger() : TObject(),
  mVpdTacSum(0), mTHUBtime{},
  mQTtacSum{}, mMT101Tac{}, mMT101Id{}, mTF201TriggerBit(0),
  mShouldHaveRejectEvent(-1) {
    /* empty */
}

//_________________
void StPicoMtdTrigger::setQTtacSum(Int_t runnumber, UShort_t mtdQTadc[8][16],
				   UShort_t mtdQTtac[8][16], const int QTtoModule[8][8],
				   const Int_t QTSlewBinEdge[8][16][8],
				   const Int_t QTSlewCorr[8][16][8]) {
  
  // Obtain RHIC run year
  int year = runnumber / 1e6 + 1999;
  // Oct. 1st (approx. 273rd day) is the start of a new running year
  if ((runnumber % 1000000) / 1000 >= 273) year += 1;

  // QT Tac cuts
  UShort_t mtd_qt_tac_min = 100;
  if (runnumber >= 16045067) mtd_qt_tac_min = 80;
  if (runnumber >= 18070005) mtd_qt_tac_min = 200; // change due to new boards
  UShort_t mtd_qt_tac_diff_range_abs = 600;
  if (year == 2015) mtd_qt_tac_diff_range_abs = 1023;

  Int_t j[2] = {0, 0};
  Int_t a[2] = {0, 0};
  for (Int_t im = 0; im < kNQTboard; im++) {
    for (Int_t i = 0; i < 8; i++) {
      if (year == 2016 && i % 2 == 0) { // moniter channel only used for Run16
	mQTtacSum[im][i] = 0;
	continue;
      }

      // Apply slewing correction
      for (Int_t k=0; k<2; k++) {
	j[k] = mtdQTtac[im][i*2+k];
	a[k] = mtdQTadc[im][i*2+k];
	
	Int_t slew_bin = -1;
	if (a[k]>0 && a[k]<=QTSlewBinEdge[im][i*2+k][0]) {
	  slew_bin = 0;
	}
	else {
	  for (Int_t l=1; l<8; l++) {
	    if (a[k] > QTSlewBinEdge[im][i*2+k][l-1] && a[k]<=QTSlewBinEdge[im][i*2+k][l]) {
	      slew_bin = l;
	      break;
	    }
	  } //for (int l=1; l<8; l++)
	} //else
	if (slew_bin >= 0) {
	  j[k] += QTSlewCorr[im][i * 2 + k][slew_bin];
	}
      } //for (int k = 0; k < 2; k++)
      
      if (j[0] <= mtd_qt_tac_min || j[0] >= mtd_qt_tac_max ||
	  j[1] <= mtd_qt_tac_min || j[1] >= mtd_qt_tac_max ||
	  ::abs(j[0] - j[1]) >= mtd_qt_tac_diff_range_abs) { // no "equal" in online algorithm
	mQTtacSum[im][i] = 0;
	continue;
      }

      // Apply position correction
      Int_t module = QTtoModule[im][i];
      if(module<0) {
	mQTtacSum[im][i] = 0;
	continue;
      }
      mQTtacSum[im][i] = UShort_t( j[0] + j[1] + abs(module-3)*1./8 * (j[0]-j[1]) );
    } //for (int i = 0; i < 8; i++)
  } //for (Int_t im = 0; im < kNQTboard; im++)
}

//_________________
void StPicoMtdTrigger::setMT101(UShort_t mt101Tac[8][2], UShort_t mt101Id[8][2]) {
  // MT101
  for (Int_t i = 0; i < kNQTboard; i++) {
    for(Int_t j = 0; j < 2; j++) {
      mMT101Tac[i][j] = mt101Tac[i][j];
      mMT101Id[i][j] = mt101Id[i][j];
    }
  }
}

//_________________
void StPicoMtdTrigger::setTF201TriggerBit(Int_t year, UInt_t dsmBit1, UInt_t dsmBit2)
{
  // TF201
  UInt_t decision = dsmBit1;
  UInt_t decision2 = dsmBit2;
  mTF201TriggerBit = 0;
  
  for (Int_t i = 0; i < 4; i++) {
    for (Int_t j = 0; j < 2; j++) {
      if (year == 2016) {
	int qt = i * 2;
	mTF201TriggerBit |= ((decision >> (i * 2 + j + 4)) & 0x1) << (qt * 2 + j);
	qt = i * 2 + 1;
	mTF201TriggerBit |= ((decision2 >> (i * 2 + j + 4)) & 0x1) << (qt * 2 + j);
      }
      else {
	int qt = i;
	mTF201TriggerBit |= ((decision >> (i * 2 + j + 4)) & 0x1) << (qt * 2 + j);
      }
    } //for (Int_t j = 0; j < 2; j++)
  } //for (Int_t i = 0; i < 4; i++)
  
  LOG_DEBUG << "input1 = " << (std::bitset<16>) decision << "\n"
	    << "input2 = " << (std::bitset<16>) decision2 << "\n"
	    << "output = " << (std::bitset<16>) mTF201TriggerBit
	    << endm;
}

//_________________
StPicoMtdTrigger::StPicoMtdTrigger(const StPicoMtdTrigger &trigger) : TObject() {
  mVpdTacSum = trigger.mVpdTacSum;
  for(Int_t iIter=0; iIter<2; iIter++) {
    mTHUBtime[iIter] = trigger.mTHUBtime[iIter];
  }

  for(UShort_t iQTboard=0; iQTboard<kNQTboard; iQTboard++) {

    for(UShort_t iIter=0; iIter<8; iIter++) {
      mQTtacSum[iQTboard][iIter] = trigger.mQTtacSum[iQTboard][iIter];
    } //for(UShort_t iIter=0; iIter<8; iIter++)

    for(UShort_t iIter=0; iIter<2; iIter++) {
      mMT101Tac[iQTboard][iIter] = trigger.mMT101Tac[iQTboard][iIter];
      mMT101Id[iQTboard][iIter] = trigger.mMT101Id[iQTboard][iIter];
    } //for(UShort_t iIter=0; iIter<2; iIter++)
  } //for(UShort_t iQTboard=0; iQTBoard<kNQTboard; iQTboard++)

  mTF201TriggerBit = trigger.mTF201TriggerBit;
  mShouldHaveRejectEvent = trigger.mShouldHaveRejectEvent;
}

//_________________
StPicoMtdTrigger::~StPicoMtdTrigger() {
  /* empty */
}

//_________________
void StPicoMtdTrigger::getMaximumQTtac(const Int_t qt, Int_t& pos1, Int_t& pos2) {

  pos1 = 0;
  pos2 = 0;

  if (qt < 1 || qt > kNQTboard) {
    LOG_ERROR << "Wrong qt board number: " << qt << endm;
    return;
  }

  UShort_t max1 = 0, max2 = 0;
  for (Int_t i = 0; i < 8; i++) {
    if (max1 < mQTtacSum[qt - 1][i]) {
      max2 = max1;
      pos2 = pos1;
      max1 = mQTtacSum[qt - 1][i];
      pos1 = i + 1;
    }
    else if (max2 < mQTtacSum[qt - 1][i]) {
      max2 = mQTtacSum[qt - 1][i];
      pos2 = i + 1;
    }
  } //for (Int_t i = 0; i < 8; i++)
}

//_________________
void StPicoMtdTrigger::Print(const Char_t *option __attribute__((unused)) ) const {
  LOG_INFO << " VPD TAC sum: " << mVpdTacSum
	   << " THUB time ( " << mTHUBtime[0] << " , " << mTHUBtime[1] << " )" << endm;
  LOG_INFO << "TCU trigger bit: " << mTF201TriggerBit
	   << " Should have reject event: " << mShouldHaveRejectEvent
	   << endm;
}

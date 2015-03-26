#ifndef StPicoMtdTrigger_hh
#define StPicoMtdTrigger_hh

class StTriggerData;
#include "TObject.h"

class StPicoMtdTrigger : public TObject {
 public:
  StPicoMtdTrigger();
  StPicoMtdTrigger(const StTriggerData *trigger);
  ~StPicoMtdTrigger();

  // qt: 1-4, pos: 1-8
  UShort_t   getQTtacSum(const Int_t qt, const Int_t pos)   { return mQTtacSum[qt-1][pos-1]; }
  UShort_t   getMT101Tac(const Int_t qt, const Int_t index) { return mMT101Tac[qt-1][index]; }
  UShort_t   getMT101Id(const Int_t qt, const Int_t index)  { return mMT101Id[qt-1][index];  }
  UChar_t    getTF201TriggerBit()                           { return mTF201TriggerBit;       }

  void       getMaximumQTtac(const Int_t qt, Int_t& pos1, Int_t& pos2);

 protected:
  static const UShort_t mtd_qt_tac_max = 4095;
  static const UShort_t mtd_qt_tac_min = 100; 
  static const UShort_t mtd_qt_tac_diff_range_abs = 600; 


 private:
  UShort_t      mQTtacSum[4][8]; // tacSum in 4 QT boards
  UShort_t      mMT101Tac[4][2]; // two largest tacSum from each QT board
  UChar_t       mMT101Id[4][2];  // id of largest tacSum -> position
  UChar_t       mTF201TriggerBit; // final trigger bit to TCU

  ClassDef(StPicoMtdTrigger,1);
};
#endif

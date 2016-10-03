#ifndef StPicoEmcTrigger_h
#define StPicoEmcTrigger_h

#include "TObject.h"

class StPicoEmcTrigger : public TObject
{
public:
  StPicoEmcTrigger();
  StPicoEmcTrigger(int flag, int id, int adc);
  virtual ~StPicoEmcTrigger();

  virtual void Print(const Char_t* option = "") const;  ///< Print trigger info

  UInt_t   flag() const;
  Int_t   id() const;
  Int_t   adc() const;

  bool isHT0() const;
  bool isHT1() const;
  bool isHT2() const;
  bool isHT3() const;

  bool isJP0() const;
  bool isJP1() const;
  bool isJP2() const;

protected:
  UChar_t mFlag;   // 0x1: ht0, 0x2: ht1, 0x4: ht2; 0x8: ht3
                   // 0x10: jp0, 0x20: jp1, 0x40: jp2
  UShort_t mId;    // soft id.  bjp: 1-18, ht: 1-4800
  UShort_t mAdc;   // adc

  ClassDef(StPicoEmcTrigger, 1)
};
inline UInt_t StPicoEmcTrigger::flag() const { return (UInt_t)mFlag; }
inline Int_t StPicoEmcTrigger::id() const { return (Int_t)mId; }
inline Int_t StPicoEmcTrigger::adc() const { return (Int_t)mAdc; }

inline bool StPicoEmcTrigger::isHT0() const { return mFlag & 0x1;}
inline bool StPicoEmcTrigger::isHT1() const { return mFlag & 0x2;}
inline bool StPicoEmcTrigger::isHT2() const { return mFlag & 0x4;}
inline bool StPicoEmcTrigger::isHT3() const { return mFlag & 0x8;}

inline bool StPicoEmcTrigger::isJP0() const { return mFlag & 0x10;}
inline bool StPicoEmcTrigger::isJP1() const { return mFlag & 0x20;}
inline bool StPicoEmcTrigger::isJP2() const { return mFlag & 0x40;}
#endif

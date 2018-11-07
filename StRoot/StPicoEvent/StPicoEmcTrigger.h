#ifndef StPicoEmcTrigger_h
#define StPicoEmcTrigger_h

/// ROOT headers
#include "TObject.h"

//_________________
class StPicoEmcTrigger : public TObject {

 public:
  /// Default constructor
  StPicoEmcTrigger();
  /// Constructor that takes values
  StPicoEmcTrigger(Int_t flag, Int_t id, Int_t adc);
  /// Copy constructor
  StPicoEmcTrigger(const StPicoEmcTrigger &trigger);
  /// Destructor
  virtual ~StPicoEmcTrigger();
  /// Print EMC trigger information
  virtual void Print(const Char_t* option = "") const;  ///< Print trigger info

  /**
   * Getters
   */

  /// Return flag:  0x1: ht0, 0x2: ht1, 0x4: ht2; 0x8: ht3
  ///               0x10: jp0, 0x20: jp1, 0x40: jp2
  UInt_t  flag() const;
  /// Return ID: bjp: 1-18, ht: 1-4800
  Int_t   id() const;
  /// Return ADC
  Int_t   adc() const;

  /// Check if the trigger is HT0
  bool isHT0() const;
  /// Check if the trigger is HT1
  bool isHT1() const;
  /// Check if the trigger is HT2
  bool isHT2() const;
  /// Check if the trigger is HT3
  bool isHT3() const;

  /// Check if the trigger is JP0
  bool isJP0() const;
  /// Check if the trigger is JP1
  bool isJP1() const;
  /// Check if the trigger is JP2
  bool isJP2() const;

  /**
   * Setters
   */
  
  /// Set trigger flag
  void setFlag(Int_t flag);
  /// Set ID
  void setId(Int_t id);
  /// Set ADC
  void setAdc(Int_t adc);

 protected:
  /// Flag encdoes next triggers: 0x1: ht0, 0x2: ht1, 0x4: ht2; 0x8: ht3
  ///                             0x10: jp0, 0x20: jp1, 0x40: jp2
  UChar_t mFlag;
  /// SoftId.  bjp: 1-18, ht: 1-4800
  UShort_t mId;
  /// ADC
  UShort_t mAdc;

  ClassDef(StPicoEmcTrigger, 1)
};

/**
 * Getters
 */
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

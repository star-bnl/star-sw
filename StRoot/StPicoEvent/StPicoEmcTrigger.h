/**
 * \class StPicoEmcTrigger
 * \brief Holds EMC trigger information
 *
 * Holds EMC trigger information
 */

#ifndef StPicoEmcTrigger_h
#define StPicoEmcTrigger_h

// C++ headers
#include <vector>

// ROOT headers
#include "TObject.h"

//_________________
class StPicoEmcTrigger : public TObject {

 public:
  /// Default constructor
  StPicoEmcTrigger();
  /// Constructor that takes values
  StPicoEmcTrigger(Int_t flag, Int_t id, Int_t adc);
  /// Constructor that takes values with SMD
  StPicoEmcTrigger(Int_t flag, Int_t id, Int_t adc, std::vector<unsigned short> smdE,
		   std::vector<unsigned short> smdP);
  /// Copy constructor
  StPicoEmcTrigger(const StPicoEmcTrigger &trigger);
  /// Destructor
  virtual ~StPicoEmcTrigger();
  /// Print EMC trigger information
  virtual void Print(const Char_t* option = "") const;

  //
  // Getters
  //

  /// Return flag:  0x1: ht0, 0x2: ht1, 0x4: ht2; 0x8: ht3
  ///               0x10: jp0, 0x20: jp1, 0x40: jp2
  UInt_t  flag() const              { return (UInt_t)mFlag; }
  /// Return ID: bjp: 1-18, ht: 1-4800
  Int_t   id() const                { return (Int_t)mId; }
  /// Return ADC
  Int_t   adc() const               { return (Int_t)mAdc; }
  /// Return number of associated BEmc SMD Eta hits
  Int_t numberOfSmdEHits() const    { return (Int_t)mSmdE.size(); }
  /// Return number of associated BEmc SMD Phi hits
  Int_t numberOfSmdPHits() const    { return (Int_t)mSmdP.size(); }
  /// Return i-th BEmc SMDE hit index. -1 = no entries, -2 = out of range
  Int_t smdEIndex(Int_t i) const;
  /// Return i-th BEmc SMDP hit index. -1 = no entries, -2 = out of range
  Int_t smdPIndex(Int_t i) const;

  /// Check if the trigger is HT0
  bool isHT0() const                { return mFlag & 0x1; }
  /// Check if the trigger is HT1
  bool isHT1() const                { return mFlag & 0x2; }
  /// Check if the trigger is HT2
  bool isHT2() const                { return mFlag & 0x4; }
  /// Check if the trigger is HT3
  bool isHT3() const                { return mFlag & 0x8; }

  /// Check if the trigger is JP0
  bool isJP0() const                { return mFlag & 0x10; }
  /// Check if the trigger is JP1
  bool isJP1() const                { return mFlag & 0x20; }
  /// Check if the trigger is JP2
  bool isJP2() const                { return mFlag & 0x40; }

  //
  // Setters
  //

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
  /// Vector of associated BEmc SMD Eta hit indices
  std::vector<unsigned short> mSmdE;
  /// Vector of associated BEmc SMD Phi hit indices
  std::vector<unsigned short> mSmdP;

  ClassDef(StPicoEmcTrigger, 2)
};

#endif

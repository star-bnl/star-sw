/**
 * \class StPicoMtdTrigger
 * \brief Class storing MTD trigger information including VPD, QT, MT101, TF201
 *
 * The class stores trigger information related to the Muon Telescope Detector (MTD)
 */

#ifndef StPicoMtdTrigger_h
#define StPicoMtdTrigger_h

// ROOT headers
#include "TObject.h"

//_________________
class StPicoMtdTrigger : public TObject {

 public:
  /// Default constructor
  StPicoMtdTrigger();
  /// Copy constructor
  StPicoMtdTrigger(const StPicoMtdTrigger &trigger);
  /// Destructor
  virtual ~StPicoMtdTrigger();
  /// Print MTD trigger information
  virtual void Print(const Char_t *option = "") const;

  //
  // Getters
  //
  
  /// VPD tag sum
  UShort_t getVpdTacSum() const     { return mVpdTacSum; }
  /// VPD tag sum
  UShort_t vpdTacSum() const        { return getVpdTacSum(); }
  ///  Return trigger time from the two THUBs
  ///  \param thub 2 for backlegs 1-15 and 1 for 16-30
  UInt_t getTHUBtime(const Int_t thub) const { return mTHUBtime[thub - 1]; }
  ///  Return trigger time from the two THUBs
  ///  \param thub 2 for backlegs 1-15 and 1 for 16-30
  UInt_t thubTime(const Int_t thub) const    { return getTHUBtime(thub); }
  /// Return the TACsum for a given position in a QT
  /// \param qt QT board nubmer running from 1-8
  /// \param pos position index in each QT board running from 1-8
  UShort_t getQTtacSum(const Int_t qt, const Int_t pos) const { return mQTtacSum[qt - 1][pos - 1]; }
  /// Return the TACsum for a given position in a QT
  /// \param qt QT board nubmer running from 1-8
  /// \param pos position index in each QT board running from 1-8
  UShort_t qtTacSum(const Int_t qt, const Int_t pos) const    { return getQTtacSum(qt,pos); }
  /// Return the largest and second-largest TACsum for a given QT
  /// stored in MT101
  /// \param qt QT board number running from 1-8
  /// \param index 0 - largest TACsum; 1 - second-largest TACsum
  UShort_t getMT101Tac(const Int_t qt, const Int_t index) const { return mMT101Tac[qt - 1][index]; }
  /// Return the largest and second-largest TACsum for a given QT
  /// stored in MT101
  /// \param qt QT board number running from 1-8
  /// \param index 0 - largest TACsum; 1 - second-largest TACsum
  UShort_t mt101Tac(const Int_t qt, const Int_t index) const  { return getMT101Tac(qt,index); }
  /// /brief Return the Id of largest and second-largest TACsum for a given QT.
  /// stored in MT101.
  /// It runs bewteen 0-3, and was intended to be used for Daq10k.
  /// It is not used in analysis.
  /// \param qt QT number running from 1-8
  /// \param index 0 - largest TACsum; 1 - second-largest TACsum
  UShort_t getMT101Id(const Int_t qt, const Int_t index) const { return mMT101Id[qt - 1][index]; }
  /// /brief Return the Id of largest and second-largest TACsum for a given QT.
  /// stored in MT101.
  /// It runs bewteen 0-3, and was intended to be used for Daq10k.
  /// It is not used in analysis.
  /// \param qt QT number running from 1-8
  /// \param index 0 - largest TACsum; 1 - second-largest TACsum
  UShort_t mt101Id(const Int_t qt, const Int_t index) const { return getMT101Id(qt,index); }
  /// Return TCU bit used in online trigger decision.
  /// Its format is modified from the one in MuDst
  UInt_t getTF201TriggerBit() const                         { return mTF201TriggerBit; }
  /// Return TCU bit used in online trigger decision.
  /// Its format is modified from the one in MuDst
  UInt_t tf201TriggerBit() const                            { return getTF201TriggerBit(); }
  /// Return the position of the two largest TACsum for a given QT board
  /// \param qt QT board number running from 1-8
  /// \param pos1 position of the largest TACsum. Runs from 1-8
  /// \pram  pos2 position of the second-largest TACsum. Runs from 1-8
  void getMaximumQTtac(const Int_t qt, Int_t& pos1, Int_t& pos2);
  Char_t shouldHaveRejectEvent()                            { return mShouldHaveRejectEvent; }

  //
  // Getters
  //

  /// Set VPD TAC sum
  void setVpdTacSum(UShort_t tacSum)            { mVpdTacSum = (UShort_t)tacSum; }
  /// Set VPD TAC sum
  void setVpdTacSum(UShort_t tdcHighThrEast, UShort_t tdcHighThrWest)
  { mVpdTacSum = (UShort_t)tdcHighThrEast + (UShort_t)tdcHighThrWest; }
  /// Set THUB time
  void setTHUBtime(Int_t thubID, UInt_t word)   { mTHUBtime[thubID] = (UInt_t)word;}
  /// Set should have reject event (retrieve from mtdHeader)
  void setShouldHaveRejectEvent(Int_t val)      { mShouldHaveRejectEvent = (Char_t)val; }
  /// Set should have reject event (retrieve from mtdHeader)
  void setShouldHaveRejectEvent(Char_t val)     { mShouldHaveRejectEvent = val; }
  /// Set TAC sum (j2+j3) for each position in each QT board
  void setQTtacSum(Int_t runnumber, UShort_t mtdQTadc[8][16], UShort_t mtdQTtac[8][16],
		   const Int_t QTtoModule[8][8], const Int_t QTSlewBinEdge[8][16][8],
		   const Int_t QTSlewCorr[8][16][8]);
  /// Set two largest TACsum for all QT boards
  void setMT101(UShort_t mt101Tac[8][2], UShort_t mt101Id[8][2]);
  /// Set trigger bit in TCU that used for online trigger
  void setTF201TriggerBit(Int_t year, UInt_t dsmBit1, UInt_t dsmBit2);
    
 protected:
  static const UShort_t mtd_qt_tac_max = 4095;
  static const UShort_t kNQTboard = 8;

 private:

  /// VPD: TACsum (east+west) for the event
  UShort_t      mVpdTacSum;
  /// Trigger time from the two THUBs
  UInt_t        mTHUBtime[2];
  /// MTD: TACsum (j2+j3) of each position in each QT board
  UShort_t      mQTtacSum[kNQTboard][8];
  /// Two largest TACsum's stored in MT101 for each QT board 
  UShort_t      mMT101Tac[kNQTboard][2];
  /// Id of two largest TACsum's for each QT board
  UChar_t       mMT101Id[kNQTboard][2];
  /// Trigger bit in TCU used for online trigger decision.
  /// Modified from the original format in MuDst.
  UInt_t        mTF201TriggerBit;        
  /// Indication of event status during filtering process in production
  ///     0 - events not triggered by di-muon
  ///     1 - events should have been rejected if only triggered by di-muon
  ///     2 - events passing filtering cuts
  Char_t        mShouldHaveRejectEvent;  

  ClassDef(StPicoMtdTrigger, 1);
};

#endif

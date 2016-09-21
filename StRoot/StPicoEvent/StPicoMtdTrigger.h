/*! 
 *  \class StPicoMtdTrigger
 *  \brief Class storing MTD trigger information
 *  including VPD, QT, MT101, TF201
 *
 */

#ifndef StPicoMtdTrigger_h
#define StPicoMtdTrigger_h

class StMuDst;
#include "TObject.h"

class StPicoMtdTrigger : public TObject
{
public:
  StPicoMtdTrigger();

  /*!
   *  \brief Default constructor
   *  \param muDst pointer to MuDst object
   *  \param QTtoModule map between QT channels and MTD modules
   *  \param QTSlewBinEdge bin edges of online slewing correction for QT
   *  \param QTSlewCorr online slewing correction for QT
   *
   */
  StPicoMtdTrigger(const StMuDst& muDst, const int QTtoModule[8][8],
                   const int QTSlewBinEdge[8][16][8], const int QTSlewCorr[8][16][8]);

  virtual ~StPicoMtdTrigger();

  // VPD tag sum
  UShort_t   getVpdTacSum();

  /*!
   *
   *  Return trigger time from the two THUBs
   *  \param thub 2 for backlegs 1-15 and 1 for 16-30
   */
  UInt_t     getTHUBtime(const Int_t thub);

  /*!
   *
   *  Return the TACsum for a given position in a QT
   *  \param qt QT board nubmer running from 1-8
   *  \param pos position index in each QT board running from 1-8
   */
  UShort_t   getQTtacSum(const Int_t qt, const Int_t pos);

  /*!
   *
   *  Return the largest and second-largest TACsum for a given QT
   *  stored in MT101
   *  \param qt QT board number running from 1-8
   *  \param index 0 - largest TACsum; 1 - second-largest TACsum
   */
  UShort_t   getMT101Tac(const Int_t qt, const Int_t index);

  /*!
   *
   *  /brief Return the Id of largest and second-largest TACsum for a given QT.
   *  stored in MT101.
   *
   *  It runs bewteen 0-3, and was intended to be used for Daq10k.
   *  It is not used in analysis.
   *
   *  \param qt QT number running from 1-8
   *  \param index 0 - largest TACsum; 1 - second-largest TACsum
   */
  UShort_t   getMT101Id(const Int_t qt, const Int_t index);

  /*!
   *
   *  Return TCU bit used in online trigger decision.
   *  Its format is modified from the one in MuDst
   */
  UInt_t     getTF201TriggerBit();

  /*!
   *
   *  Return the position of the two largest TACsum for a given QT board
   *  \param qt QT board number running from 1-8
   *  \param pos1 position of the largest TACsum. Runs from 1-8
   *  \pram  pos2 position of the second-largest TACsum. Runs from 1-8
   */
  void       getMaximumQTtac(const Int_t qt, Int_t& pos1, Int_t& pos2);

  Char_t     shouldHaveRejectEvent();

protected:
  static const UShort_t mtd_qt_tac_max = 4095;
  static const UShort_t kNQTboard = 8;


private:
  UShort_t      mVpdTacSum;              ///< VPD: TACsum (east+west) for the event
  UInt_t        mTHUBtime[2];            ///< Trigger time from the two THUBs
  UShort_t      mQTtacSum[kNQTboard][8]; ///< MTD: TACsum (j2+j3) of each position in each QT board
  UShort_t      mMT101Tac[kNQTboard][2]; ///< Two largest TACsum's stored in MT101 for each QT board 
  UChar_t       mMT101Id[kNQTboard][2];  ///< Id of two largest TACsum's for each QT board
  UInt_t        mTF201TriggerBit;        
  ///< Trigger bit in TCU used for online trigger decision.
  ///< Modified from the original format in MuDst.
  Char_t        mShouldHaveRejectEvent;  
  ///< Indication of event status during filtering process in production
  ///< 0 - events not triggered by di-muon
  ///< 1 - events should have been rejected if only triggered by di-muon
  ///< 2 - events passing filtering cuts

  ClassDef(StPicoMtdTrigger, 1);
};
inline UShort_t StPicoMtdTrigger::getVpdTacSum() { return mVpdTacSum; }
inline UInt_t   StPicoMtdTrigger::getTHUBtime(const Int_t thub) { return mTHUBtime[thub - 1]; }
inline UShort_t StPicoMtdTrigger::getQTtacSum(const Int_t qt, const Int_t pos) { return mQTtacSum[qt - 1][pos - 1]; }
inline UShort_t StPicoMtdTrigger::getMT101Tac(const Int_t qt, const Int_t index) { return mMT101Tac[qt - 1][index]; }
inline UShort_t StPicoMtdTrigger::getMT101Id(const Int_t qt, const Int_t index) { return mMT101Id[qt - 1][index]; }
inline UInt_t   StPicoMtdTrigger::getTF201TriggerBit() { return mTF201TriggerBit; }
inline Char_t   StPicoMtdTrigger::shouldHaveRejectEvent() { return mShouldHaveRejectEvent; }
#endif

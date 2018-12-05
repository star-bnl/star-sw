#ifndef STMTDTRIGUTIL_HH
#define STMTDTRIGUTIL_HH

/***************************************************************************
 *
 * $Id: StMtdTrigUtil.h,v 1.2 2018/12/03 16:56:09 marr Exp $ 
 * StMtdTrigUtil: this class reads in the MTD trigger information and 
 * determines the trigger unit that fire the MTD trigger
 * Author: Rongrong Ma
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/

#include "StMaker.h"
#include "StMtdUtil/StMtdConstants.h"
class StEvent;
class StMuDst;
class StPicoDst;
class StTriggerData;

class StMtdTrigUtil: public StMaker {
 public:
  StMtdTrigUtil(const Char_t *name = "MtdTriggerDecision");
  ~StMtdTrigUtil();

  Int_t    Init();
  Int_t    InitRun(const Int_t runNumber);
  Int_t    Make();

  void     setPosCorrToQTtac(Bool_t corr)                 { mPosCorrToQTtac = corr;        }
  Int_t    getVpdTacSum()                                 { return mVpdTacSum;             }
  Int_t    getTHUBtime(const Int_t thub)                  { return mTHUBtime[thub-1];      }
  Int_t    getQt(const int backleg, const int module)     { return mModuleToQT[backleg-1][module-1]; } 
  Int_t    getQtPos(const int backleg, const int module)  { return mModuleToQTPos[backleg-1][module-1]; }
  Int_t    getQtTacSum(const Int_t qt, const Int_t pos)   { return mQtTacSum[qt-1][pos-1]; }
  Int_t    getQtTacSumHighestTwo(const Int_t qt, const Int_t index)   { return mQtTacSumHighestTwo[qt-1][index]; }
  Int_t    getQtPosHighestTwo(const Int_t qt, const Int_t index)      { return mQtPosHighestTwo[qt-1][index]; }
  Int_t    getQtPosTrig(const Int_t qt, const Int_t index){ return mQtPosTrig[qt-1][index]; }
  Int_t    getMT101Tac(const Int_t qt, const Int_t index) { return mMT101Tac[qt-1][index]; }
  Int_t    getMT101Id(const Int_t qt, const Int_t index)  { return mMT101Id[qt-1][index];  }
  Int_t    getTF201TriggerBit()                           { return mTF201TriggerBit;       }

  Bool_t   isQtFireTrigger(const Int_t qt, const Int_t pos);
  Bool_t   isQtHighestTwo(const int qt, const int pos);

  Int_t    getHitTimeInQT(const int backleg, const int module);
  Int_t    getHitTimeDiffToVPDInQT(const int backleg, const int module);
  Int_t    getHitTimeDiffToVPDInMT101(const int backleg, const int module);
  Bool_t   isHitFireTrigger(const Int_t backleg, const Int_t module);
  Bool_t   isHitHighestTwo(const Int_t backleg, const Int_t module);

 protected:
  void     reset();
  void     extractTrigInfo(StTriggerData *trigData);
  void     extractTrigInfo(StPicoDst *picoDst);
  void     findFireQT();

  enum {kNQTboard = 8};
  Int_t            mModuleToQT[gMtdNBacklegs][gMtdNModules];     // mapping between hit and QT
  Int_t            mModuleToQTPos[gMtdNBacklegs][gMtdNModules];  // mapping between hit and QT position
  Int_t            mQTtoModule[kNQTboard][8];                    // mapping between QT and hit module
  Int_t            mQTSlewBinEdge[kNQTboard][16][8];             // slewing correction bin edge
  Int_t            mQTSlewCorr[kNQTboard][16][8];                // slewing correction parameters

 private:
  Int_t            mRunYear;                                     // running year
  StEvent          *mStEvent;                                    // Pointer to StEvent
  StMuDst          *mMuDst;                                      // Pointer to MuDst event
  StPicoDst        *mPicoDst;                                    // Pointer to PicoDst event
  Bool_t           mPosCorrToQTtac;                              // switch to apply position correction

  Int_t            mVpdTacSum;                                   // VPD TacSum
  Int_t            mTHUBtime[2];                                 // Trigger time from the two THUBs
  Int_t            mQtTacSum[kNQTboard][8];                      // MTD: TACsum (j2+j3) of each position in each QT board
  Int_t            mMT101Tac[kNQTboard][2];                      // Two largest TACsum's stored in MT101 for each QT board 
  Int_t            mMT101Id[kNQTboard][2];                       // Id of two largest TACsum's for each QT board
  Int_t            mTF201TriggerBit;                             // Trigger bit in TCU used for online trigger decision. Modified from the original format in MuDst.
  Int_t            mQtPosTrig[kNQTboard][2];                     // QT channel that fire the trigger
 Int_t             mQtTacSumHighestTwo[kNQTboard][2];            // Two largest signals of the board
  Int_t            mQtPosHighestTwo[kNQTboard][2];               // QT channel having two largest signals of the board

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
  }
  
  ClassDef(StMtdTrigUtil, 1);
};

#endif

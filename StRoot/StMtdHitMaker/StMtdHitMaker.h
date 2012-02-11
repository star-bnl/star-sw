#ifndef STAR_StMtdHitMaker_H
#define STAR_StMtdHitMaker_H

/***************************************************************************
 *
 * $Id: StMtdHitMaker.h,v 1.5 2012/02/11 02:15:11 geurts Exp $ 
 * StMtdHitMaker - class to fill StEvent from DAQ reader
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/
#include "StRTSBaseMaker.h"

struct mtd_t;

class StEvent;
class StMtdCollection;
class StMtdRawHit;
class StMtdHit;
class StBTofINLCorr;

struct MtdRawHit {
  unsigned int   tdc;               /// tdc time (in bin) per hit.
  unsigned int   dataword;          /// data word before unpack
  unsigned char  fiberid;           /// 0 1 2,3
  unsigned char  backlegID;            /// 1,2,...,117
  unsigned char  globaltdcchan;     /// 0,1,..   
};

// 
#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
typedef vector<Int_t>  IntVec;
typedef vector<UInt_t>  UIntVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
typedef vector<UInt_t, allocator<UInt_t>>  UIntVec;
#endif


/**
   \class StMtdHitMaker
   
   Class to read in MTD data from DAQ and store into StMtdCollection   
*/
class StMtdHitMaker:public StRTSBaseMaker {
 private: 
  StEvent *mStEvent;
  mtd_t   *fMtd;

  Int_t UnpackMtdRawData();
  void fillMtdHeader();
  void fillMtdRawHitCollection();
  void fillMtdHitCollection();
  void fillStEvent();     //! ship collection to StEvent and check
  ///----------------------------------------------------
  vector<MtdRawHit> MtdLeadingHits;
  vector<MtdRawHit> MtdTrailingHits;

  Int_t                   mYear;                 //! RHIC run year
  Int_t                   mNValidTrays;          //! number of valid MTD trays
  unsigned int            mTriggerTimeStamp[2];  //! Trigger Time in 4 fibers
  StMtdCollection*        mMtdCollection;        //! pointer to StMtdCollection
  Int_t                   mTray2TdigMap[30][5];  //! map TDIG-Id to MTD tray
  Int_t                   mTrayId[30][5];        //! map MTD trayIDs
  Int_t                   mTdigId[118];          //! map TDIG Ids on MTD TrayIds
  StBTofINLCorr*          mINLCorr;              //! pointer to INL correction class

 protected:
  StRtsTable *GetNextRaw();
  StRtsTable *GetNextRaw(int sec);
      
  StMtdCollection *GetMtdCollection();
  Int_t tdcChan2globalStrip11(int, int, int, int);
  Int_t tdcChan2globalStrip(int, int, int, int);

 public:

  /// Default constructor
  StMtdHitMaker(const char *name="mtd_raw");     
  ~StMtdHitMaker() ;

  void   Clear(Option_t* option="");
  Int_t  Init();
  Int_t  InitRun(Int_t);
  Int_t  FinishRun(Int_t);
  Int_t  Finish();
  Int_t  Make();

  /// obtain the whole list of leading edge hits
  vector<MtdRawHit> getLeadingHits();
  /// obtain the whole list of trainling edge hits
  vector<MtdRawHit> getTrailingHits();
     
  /// cvs
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
  }
  
  ClassDef(StMtdHitMaker, 1)    ///StMtdHitMaker - class to fille the StEvent from DAQ reader
};

inline vector<MtdRawHit> StMtdHitMaker::getLeadingHits()  { return MtdLeadingHits; }
inline vector<MtdRawHit> StMtdHitMaker::getTrailingHits() { return MtdTrailingHits;}

#endif

#ifndef STAR_StMtdHitMaker_H
#define STAR_StMtdHitMaker_H

/***************************************************************************
 *
 * $Id: StMtdHitMaker.h,v 1.3 2012/02/01 06:40:01 geurts Exp $ 
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

  Int_t                   mNValidTrays;          //! number of valid MTD trays
  unsigned int            mTriggerTimeStamp[4];  //! Trigger Time in 4 fibers
  StMtdCollection*        mMtdCollection;        //! pointer to StMtdCollection
  Int_t                   mTray2TdigMap[30][5];  //! map TDIG-Id to MTD tray


 protected:
  StRtsTable *GetNextRaw();
  StRtsTable *GetNextRaw(int sec);
      
  StMtdCollection *GetMtdCollection();
  Int_t tdcchan2globalstrip(int, int, int);

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

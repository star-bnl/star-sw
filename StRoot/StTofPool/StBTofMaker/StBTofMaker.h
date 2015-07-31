#ifndef STAR_StBTofMaker_H
#define STAR_StBTofMaker_H

/***************************************************************************
 *
 * $Id: StBTofMaker.h,v 1.3 2015/07/28 22:50:03 smirnovd Exp $
 * StBTofMaker - class to fille the StEvent from DAQ reader
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/
#include "StMaker.h"
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#define VHRBIN2PS 24.4140625  // Very High resolution mode, pico-second per bin
                              // 1000*25/1024 (ps/chn)
#define HRBIN2PS 97.65625     // High resolution mode, pico-second per bin
                              // 97.65625= 1000*100/1024  (ps/chn)

class StEvent;
class StBTofCollection;
class StBTofRawHit;
class StBTofRawHitCollection;
class StBTofHit;
class StBTofHitCollection;
#include "StBTofUtil/StBTofSortRawHit.h"
class StBTofINLCorr;
class StBTofDaqMap;


#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
typedef vector<Int_t>  IntVec;
typedef vector<UInt_t>  UIntVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
typedef vector<UInt_t, allocator<UInt_t>>  UIntVec;
#endif


/**
   \class StBTofMaker
   
*/
class StBTofMaker : public StMaker
{
   private: 
     StEvent *mStEvent;

     void fillBTofHitCollection();

     Int_t                    mNValidTrays;          //! number of valid TOF trays
     unsigned int             mTriggerTimeStamp[4];  //! Trigger Time in 4 fibers
     StBTofCollection*        mBTofCollection;       //! pointer to StBTofCollection
     StBTofRawHitCollection*  mRawHitCollection;     //! pointer to StBTofRawHitCollection
     StBTofHitCollection*     mHitCollection;        //! pointer to StBTofHitCollection
     StBTofDaqMap*            mBTofDaqMap;           //! pointer to the TOF daq map
     StBTofINLCorr*           mBTofINLCorr;          //! INL corretion;
     StBTofSortRawHit*        mBTofSortRawHit;       //! to sort the TOF hits

     static const Int_t  mNVPD          = 19;
     static const Int_t  mWestVpdTrayId = 121;
     static const Int_t  mEastVpdTrayId = 122;
     
     Bool_t       mDoINLCorr;     //! Switch to turn on INL Corr or not
     Bool_t       mDoTriggerCut;  //! Switch to turn on trigger cut
          
   protected:
     StBTofCollection *GetBTofCollection();

   public:

     /// Default constructor
     StBTofMaker(const char *name="tof_raw");
     
    ~StBTofMaker() ;

     void   Clear(Option_t* option="");
     Int_t  Init();
     Int_t  InitRun(Int_t);
     Int_t  FinishRun(Int_t);
     Int_t  Finish();
     Int_t  Make();

     void   doINLCorr(const Bool_t val=kTRUE);
     void   doTriggerCut(const Bool_t val=kTRUE);
     
  /// cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
    }
  
  ClassDef(StBTofMaker, 1)    ///StBTofMaker - class to fille the StEvent from DAQ reader
};

inline void StBTofMaker::doINLCorr(const Bool_t val) { mDoINLCorr = val; }
inline void StBTofMaker::doTriggerCut(const Bool_t val) { mDoTriggerCut = val; }

#endif

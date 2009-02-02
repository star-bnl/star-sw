#ifndef STAR_StBTofHitMaker_H
#define STAR_StBTofHitMaker_H

/***************************************************************************
 *
 * $Id: StBTofHitMaker.h,v 1.1 2009/02/02 21:52:18 dongx Exp $
 * StBTofHitMaker - class to fille the StEvent from DAQ reader
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/
#include "StRTSBaseMaker.h"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

struct tof_t;

class StEvent;
class StBTofCollection;
class StBTofRawHit;
class StBTofRawHitCollection;

struct TofRawHit {
  unsigned short fiberid;           /// 0 1 2,3
  unsigned short trayID;            /// 1,2,......,120,for tray, 121, 122 for upvpd
  unsigned short globaltdcchan;     /// 0,1,......,191   
  unsigned int   tdc;               /// tdc time (in bin) per hit.
  unsigned int   dataword;          /// data word before unpack
};

/**
   \class StBTofHitMaker
   
   Class to read in TOF data from DAQ and store into StBTofCollection   
*/
class StBTofHitMaker:public StRTSBaseMaker
{
   private: 
      StEvent *mStEvent;
      tof_t   *fTof;

      Int_t UnpackTofRawData();
      void fillBTofRawHitCollection();
      void fillStEvent();     //! ship collection to StEvent
      /// TOF Raw hits info. struct
      ///----------------------------------------------------
     vector<TofRawHit> TofLeadingHits;
     vector<TofRawHit> TofTrailingHits;

     unsigned int             mTriggerTimeStamp[4];
     StBTofCollection*        mBTofCollection; //!
     StBTofRawHitCollection*  mRawHitCollection;     //!

     
   protected:
      StRtsTable *GetNextRaw();
      
      StBTofCollection *GetBTofCollection();

   public:

     /// Default constructor
     StBTofHitMaker(const char *name="tof_raw");
     
    ~StBTofHitMaker() ;

     Int_t Make();

     /// obtain the whole list of leading edge hits
     vector<TofRawHit> getLeadingHits();
     /// obtain the whole list of trainling edge hits
     vector<TofRawHit> getTrailingHits();
     
  /// cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
    }
  
  ClassDef(StBTofHitMaker, 1)    ///StBTofHitMaker - class to fille the StEvent from DAQ reader
};

inline vector<TofRawHit> StBTofHitMaker::getLeadingHits() { return TofLeadingHits; }
inline vector<TofRawHit> StBTofHitMaker::getTrailingHits() { return TofTrailingHits; }

#endif

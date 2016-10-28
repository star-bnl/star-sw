#ifndef STAR_StTofHitMaker_H
#define STAR_StTofHitMaker_H

/***************************************************************************
 *
 * $Id: StTofHitMaker.h,v 1.6 2014/08/06 11:43:47 jeromel Exp $
 * StTofHitMaker - class to fille the StEvewnt from DAQ reader
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
class StTofCollection;
class StTofData;
class StTofRawData; 
class StTofCollection;
class StTofRawDataCollection;
class StTofDataCollection;

class StTofHitMaker:public StRTSBaseMaker
{
   private: 
      StEvent *mStEvent;
      tof_t   *fTof;
      Int_t UnpackTofRawData();
      void fillTofDataCollection();
      void fillStEvent();     //! ship collection to StEvent
      /// TOF Raw hits info. struct
      ///----------------------------------------------------
public:	
      struct TofRawHit {
        unsigned short fiberid;           /// 0 1 2,3
        unsigned short trayID;            /// 1,2,......,120,for tray, 121, 122 for upvpd
        unsigned short globaltdcchan;     /// 0,1,......,191   
        unsigned int   tdc;               /// tdc time (in bin) per hit.
        unsigned int   timestamp;         /// data word before unpack
        unsigned int   triggertimestamp;  /// trigger word before unpack
     };
private:
     vector<TofRawHit> TofLeadingHits;
     vector<TofRawHit> TofTrailingHits;

     StTofCollection*         tofCollection;     //!
     StTofDataCollection*     mDataCollection;   //!
     StTofRawDataCollection*  mRawDataCollection;//!
     Bool_t                   mInitialized;      //!
     
   protected:
      StRtsTable *GetNextRaw();
      
      StTofCollection *GetTofCollection();

   public:

     StTofHitMaker(const char *name="tof_raw");
     
    ~StTofHitMaker() ;

    Int_t Make();
    Int_t InitRun(Int_t);
    Int_t FinishRun(Int_t);

  /// cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs;
    }
  
  ClassDef(StTofHitMaker, 1)    ///StTofHitMaker - class to fille the StEvewnt from DAQ reader
};

#endif

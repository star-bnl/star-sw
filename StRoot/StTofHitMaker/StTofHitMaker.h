#ifndef STAR_StTofHitMaker_H
#define STAR_StTofHitMaker_H

/***************************************************************************
 *
 * $Id: StTofHitMaker.h,v 1.2 2008/12/02 23:58:45 fine Exp $
 * StTofHitMaker - class to fille the StEvewnt from DAQ reader
 *--------------------------------------------------------------------------
 *
 ***************************************************************************/
#include "StRTSBaseMaker.h"
#include "StMaker.h"

#include <vector>
#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

class StEvent;
class StTofCollection;
class evpReader;
class StTofData;
class StTofRawData; 
class StTofCollection;
class StTofRawDataCollection;
class StTofDataCollection;

class StTofHitMaker:public StRTSBaseMaker
{
   private: 
      StEvent *mStEvent;
      evpReader  *fDaqReader;
      Int_t UnpackTofRawData();
      void fillTofDataCollection();
      void fillStEvent();     //! ship collection to StEvent
      /// TOF Raw hits info. struct
      ///----------------------------------------------------
      struct TofRawHit {
        unsigned short fiberid;           /// 0 1 2,3
        unsigned short trayID;            /// 1,2,......,120,for tray, 121, 122 for upvpd
        unsigned short globaltdcchan;     /// 0,1,......,191   
        unsigned int   tdc;               /// tdc time (in bin) per hit.
        unsigned int   timestamp;         /// data word before unpack
        unsigned int   triggertimestamp;  /// trigger word before unpack
     };
     vector<TofRawHit> TofLeadingHits;
     vector<TofRawHit> TofTrailingHits;

     StTofCollection*         tofCollection; //!
     StTofDataCollection*     mDataCollection;   //!
     StTofRawDataCollection*  mRawDataCollection;     //!

     
   protected:
      StRtsTable *GetNextRaw();
      
      StTofCollection *GetTofCollection();
      evpReader *InitReader();

   public:

     StTofHitMaker(const char *name="tof_raw");
     
    ~StTofHitMaker() ;

     Int_t Make();

  /// cvs
  virtual const char *GetCVS() const
    {
      static const char cvs[]="Tag $Name:  $Id: built "__DATE__" "__TIME__ ; return cvs;
    }
  
  ClassDef(StTofHitMaker, 1)    ///StTofHitMaker - class to fille the StEvewnt from DAQ reader
};

#endif

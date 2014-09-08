/***************************************************************************
*
* $Id: StIstRawHit.h,v 1.9 2014/09/08 19:07:05 smirnovd Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description:
* Data structure for individual IST pad (channel).
****************************************************************************/

#ifndef StIstRawHit_hh
#define StIstRawHit_hh

#include "StObject.h"
#include "StEvent/StEnumerations.h"
#include "StIstConsts.h"

class StIstRawHit : public StObject
{
public:
   //constructors
   StIstRawHit();
   StIstRawHit( const StIstRawHit & );
   StIstRawHit &operator=( const StIstRawHit & );

   //accessors
   int       	      getChannelId()  const; //!< 0-110591
   int		      getGeoId()      const; //!< 1-110592
   unsigned char     getLadder()     const; //!< 1-24
   unsigned char     getSensor()     const; //!< 1-6
   unsigned char     getRow()        const; //!< 1-64
   unsigned char     getColumn()     const; //!< 1-12
   float     	      getCharge(unsigned char tb = 0) 	 const;
   float             getChargeErr(unsigned char tb = 0) const;
   unsigned char     getMaxTimeBin() const;
   unsigned char     getRdo()        const; //!< 1-6
   unsigned char     getArm()        const; //!< 0-5
   unsigned char     getApv()        const; //!< 0-23
   unsigned char     getChannel()    const; //!< 0-127
   static unsigned char  getDefaultTimeBin();
   unsigned short    getIdTruth()    const; //!< for embedding, 0 as background

   //modifiers
   void        setChannelId(int rChannelId) ;
   void	setGeoId(int rChannelId);
   void        setCharge(float charge, unsigned char tb = -1) ;
   void	setChargeErr(float chargeErr, unsigned char tb = -1) ;
   void        setMaxTimeBin(unsigned char tb) ;
   static void setDefaultTimeBin( unsigned char tb );
   void        setIdTruth(unsigned short idTruth);

   using StObject::Print;
   void Print(int nTimeBins) const;

private:

   Int_t       mChannelId;                 ///< channel Id, numbering from 0 to 110591
   Int_t       mGeoId;                     ///< geometry Id, numbering from 1 to 110592
   Float_t     mCharge[kIstNumTimeBins];   ///< pedestal non-subtracted ADC value saved in calibration mode;
                                           ///< pedestal subtracted and/or CMN correction in physics mode
   Float_t     mChargeErr[kIstNumTimeBins];///< charge error in all time bins
   UChar_t     mMaxTimeBin;                ///< the max ADC time bin index of the raw hit
   UShort_t    mIdTruth;           	   ///< for embedding, 0 as background
   static UChar_t mDefaultTimeBin;

   ClassDef(StIstRawHit, 1)
};


/*! Functor for sorting raw hits in ascending order by geometry id mGeoId, i.e. the pad Id order. */
struct rawHitPtrLessThan {
   bool operator() (const StIstRawHit *rawHit1, const StIstRawHit *rawHit2) const;
};

#endif


/***************************************************************************
*
* $Log: StIstRawHit.h,v $
* Revision 1.9  2014/09/08 19:07:05  smirnovd
* StIstRawHit: Made methods accessing static data member static
*
* Revision 1.8  2014/09/08 19:06:57  smirnovd
* Added Print() methods to print out properties of StIstCluster and StIstRawHit objects and their respective collections
*
* Revision 1.7  2014/09/05 17:36:55  smirnovd
* Slightly cleaned up (doxygen) comments
*
* Revision 1.6  2014/03/13 22:10:13  smirnovd
* Move some constants from StIstUtil/StIstConsts.h to StEvent/StEnumerations.h to avoid external dependance of StEvent on StIstUtil
*
* Revision 1.5  2014/02/20 02:29:27  smirnovd
* Remove destructor that does nothing
*
* Revision 1.4  2014/02/13 02:35:49  smirnovd
* Moved CVS log to the bottom of the file
*
* Revision 1.3  2014/02/03 16:12:20  ypwang
* updating scripts
*
*
****************************************************************************
* StIstRawHit.h,v 1.0
* Revision 1.0 2013/11/04 15:05:30 Yaping
* Initial version
****************************************************************************/

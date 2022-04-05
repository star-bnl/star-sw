// $Id: StIstRawHit.h,v 1.15 2016/01/11 21:16:02 smirnovd Exp $

#ifndef StIstRawHit_hh
#define StIstRawHit_hh

#include "StObject.h"
#include "StEvent/StEnumerations.h"
#include "StIstConsts.h"


/**
 * Data structure for individual IST pad (channel).
 *
 * \author Yaping Wang
 * \date March 2013
 */
class StIstRawHit : public StObject
{
public:
   //constructors
   StIstRawHit();

   template<typename Container>
   StIstRawHit(int channelId, int geoId,
      const Container &charges, const Container &chargeErrs = Container{},
      UChar_t maxTimeBin = 3, UShort_t idTruth = 0);

   //accessors
   int               getChannelId()  const; //!< 0-110591
   int               getGeoId()      const; //!< 1-110592
   unsigned char     getLadder()     const; //!< 1-24
   unsigned char     getSensor()     const; //!< 1-6
   unsigned char     getRow()        const; //!< 1-64
   unsigned char     getColumn()     const; //!< 1-12
   float             getCharge(int tb = 0)    const;
   float             getChargeErr(int tb = 0) const;
   unsigned char     getMaxTimeBin() const;
   unsigned char     getRdo()        const; //!< 1-6
   unsigned char     getArm()        const; //!< 0-5
   unsigned char     getApv()        const; //!< 0-23
   unsigned char     getChannel()    const; //!< 0-127
   static unsigned char  getDefaultTimeBin();
   unsigned short    getIdTruth()    const; //!< for embedding, 0 as background

   //modifiers
   void setChannelId(int rChannelId);
   void setGeoId(int rChannelId);
   void setCharge(float charge, int tb = -1);

   /// Overwrites this channel's charges in all time bins by values in the
   /// provided container
   template<typename Container>
   void setCharges(const Container& charges) {
      std::copy( std::begin(charges), std::end(charges), mCharge);
   }

   void setChargeErr(float chargeErr, int tb = -1);
   void        setMaxTimeBin(int tb) ;
   static void setDefaultTimeBin( int tb );
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
   UShort_t    mIdTruth;                   ///< for embedding, 0 as background

   static UChar_t mDefaultTimeBin;

   ClassDef(StIstRawHit, 1)
};


/*! Functor for sorting raw hits in ascending order by geometry id mGeoId, i.e. the pad Id order. */
struct rawHitPtrLessThan {
   bool operator() (const StIstRawHit *rawHit1, const StIstRawHit *rawHit2) const;
};

#endif

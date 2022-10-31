#ifndef StFstRawHit_hh
#define StFstRawHit_hh

#include "StObject.h"
#include "StEvent/StFstConsts.h"


/**
 * Data structure for individual FST pad (channel).
 *
 * \author Shenghui Zhang
 * \date Aug. 2021
 */
class StFstRawHit : public StObject
{
public:
   //constructors
   StFstRawHit();

   template<typename Container>
   StFstRawHit(int channelId, int geoId,
      const Container &charges, const Container &chargeErrs = Container{},
      UChar_t maxTimeBin = 1, UShort_t idTruth = 0);

   StFstRawHit(const StFstRawHit &rawHit);

   //accessors
   int               getChannelId()  const; //!< 0-36863
   int               getGeoId()      const; //!< 0-36863
   int               getSeedhitflag() const; //!< 0 or 1
   unsigned char     getDisk()       const; //!< 1-3
   unsigned char     getWedge()      const; //!< 1-36
   unsigned char     getPhiStrip()   const; //!< 0-127
   unsigned char     getRStrip()     const; //!< 0-7
   float             getCharge(int tb = 0)    const;
   float             getChargeErr(int tb = 0) const;
   unsigned char     getMaxTimeBin() const;
   unsigned char     getRdo()        const; //!< 1-6
   unsigned char     getArm()        const; //!< 0-2
   unsigned char     getApv()        const; //!< 0-15
   unsigned char     getSensor()     const; //!< 0-2
   unsigned char     getChannel()    const; //!< 0-127
   static unsigned char  getDefaultTimeBin();
   unsigned short    getIdTruth()    const; //!< for embedding, 0 as background

   //modifiers
   void setChannelId(int rChannelId);
   void setGeoId(int rChannelId);
   void setSeedhitflag(int rSeedhitflag);
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

   Int_t       mChannelId;                 ///< channel Id, numbering from 0 to 36863
   Int_t       mGeoId;                     ///< geometry Id, numbering from 0 to 36863
   Int_t       mSeedhitflag;               ///< seed hit flag, 0 not a seed hit & 1 mean a seed hit
   Float_t     mCharge[kFstNumTimeBins];   ///< pedestal non-subtracted ADC value saved in calibration mode;
                                           ///< pedestal and CMN subtracted in physics mode
   Float_t     mChargeErr[kFstNumTimeBins];///< charge error in all time bins
   UChar_t     mMaxTimeBin;                ///< the max ADC time bin index of the raw hit
   UShort_t    mIdTruth;                   ///< for embedding, 0 as background

   static UChar_t mDefaultTimeBin;

   ClassDef(StFstRawHit, 1)
};


/*! Functor for sorting raw hits in ascending order by geometry id mGeoId, i.e. the pad Id order. */
struct rawHitPtrLessThan {
   bool operator() (const StFstRawHit *rawHit1, const StFstRawHit *rawHit2) const;
};

#endif

/**************************************************************************
 *
 * StMuFstRawHit.h
 *
 * Author: tchuang 2022
 **************************************************************************
 *
 * Description: Data class for FST hit in StMuDst
 *
 **************************************************************************/
#ifndef StMuFstRawHit_h
#define StMuFstRawHit_h

#include <TObject.h>
#include <TRefArray.h>
#include "StEnumerations.h"
#include "StEvent/StFstConsts.h"

class StFstRawHit;

class StMuFstRawHit : public TObject {
public:
    StMuFstRawHit();
    ~StMuFstRawHit();

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
    void setChargeErr(float chargeErr, int tb = -1);
    void        setMaxTimeBin(int tb) ;
    static void setDefaultTimeBin( int tb );
    void        setIdTruth(unsigned short idTruth);

    void print(int nTimeBins) ;

    void set( StFstRawHit *hit );

private:

    Int_t       mChannelId;                 ///< channel Id, numbering from 0 to 36863
    Int_t       mGeoId;                     ///< geometry Id, numbering from 0 to 36863
    Int_t       mSeedhitflag;               ///< seed hit flag, 0 not a seed hit & 1 mean a seed hit
    Float_t     mCharge[kFstNumTimeBins];   ///< pedestal non-subtracted ADC value saved in calibration mode;
    ///< pedestal and CMN subtracted in physics mode
    Float_t     mChargeErr[kFstNumTimeBins];///< charge error in all time bins
    UChar_t     mMaxTimeBin;                ///< the max ADC time bin index of the raw hit
    UShort_t    mIdTruth;                   ///< for embedding, 0 as background

    static UChar_t mDefaultTimeBin;   //!

    ClassDef(StMuFstRawHit, 1)
};

#endif  // StMuFstRawHit_h


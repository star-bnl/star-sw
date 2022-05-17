/**************************************************************************
 *
 * StMuFstHit.h
 *
 * Author: tchuang 2022
 **************************************************************************
 *
 * Description: Data class for FST hit in StMuDst
 *
 **************************************************************************/
#ifndef StMuFstHit_h
#define StMuFstHit_h

#include <TVector3.h>
#include <TObject.h>
#include <TRefArray.h>
#include "StEnumerations.h"
#include "StEvent/StFstConsts.h"

class StFstHit;

class StMuFstHit : public TObject {
public:
    StMuFstHit();
    ~StMuFstHit();

    int           getId() const;
    int           getIdTruth() const;
    unsigned char getDisk() const;
    unsigned char getWedge() const;
    unsigned char getSensor() const;
    unsigned char getApv() const;
    unsigned char getMaxTimeBin() const;
    float         getMeanRStrip() const;
    float         getMeanPhiStrip() const;
    float         getCharge() const;
    float         getChargeErr() const;
    unsigned char getNRawHits() const;
    unsigned char getNRawHitsR() const;
    unsigned char getNRawHitsPhi() const;
    float         localPosition(unsigned int ) const;
 
    void setId(int id);
    void setIdTruth(int idtruth);
    void setDisk(unsigned char disk);
    void setWedge(unsigned char wedge);
    void setSensor(unsigned char sensor);
    void setApv(unsigned char apv);
    void setMaxTimeBin(unsigned char tb);
    void setCharge(float charge);
    void setChargeErr(float chargeErr);
    void setMeanRStrip(float meanRStrip);
    void setMeanPhiStrip(float meanPhiStrip);
    void setNRawHits(unsigned char nRawHits);
    void setNRawHitsR(unsigned char nRawHitsR);
    void setNRawHitsPhi(unsigned char nRawHitsPhi);
    void setLocalPosition(float, float, float);

    const TVector3& xyz()   const; // position in global STAR coordinate
    void setXYZ(const TVector3& p3);

    void setHardwarePosition(int HardwarePosition);
    
    void print(int option=0);

    void set( StFstHit *hit );

private:

    Int_t 	mId;
    Int_t 	mIdTruth;
    UChar_t mApv;
    UChar_t mMaxTimeBin;
    Float_t mMeanRStrip;
    Float_t mMeanPhiStrip;
    Float_t mCharge;
    Float_t mChargeErr;
    UChar_t mNRawHits;
    UChar_t mNRawHitsR;
    UChar_t mNRawHitsPhi;
    Float_t mLocalPosition[3];

    UInt_t  mHardwarePosition;
    TVector3  mXYZ;    // position in STAR coordinate

    ClassDef(StMuFstHit, 1)
};



inline int           StMuFstHit::getId() const              { return mId;                 };
inline int           StMuFstHit::getIdTruth() const         { return mIdTruth;            };
inline unsigned char StMuFstHit::getDisk() const            { return 1 + (mHardwarePosition - 1) / kFstNumSensorsPerWedge / kFstNumWedgePerDisk;};
inline unsigned char StMuFstHit::getWedge() const           { return 1 + (mHardwarePosition - 1) / kFstNumSensorsPerWedge;};
inline unsigned char StMuFstHit::getSensor() const          { return (mHardwarePosition - 1) % kFstNumSensorsPerWedge;};
inline unsigned char StMuFstHit::getApv() const             { return mApv;                };
inline unsigned char StMuFstHit::getMaxTimeBin() const      { return mMaxTimeBin;         };
inline float StMuFstHit::getMeanRStrip() const              { return mMeanRStrip;         };
inline float StMuFstHit::getMeanPhiStrip() const            { return mMeanPhiStrip;       };
inline float StMuFstHit::getCharge()    const               { return mCharge;             };
inline float StMuFstHit::getChargeErr()    const            { return mChargeErr;          };
inline unsigned char StMuFstHit::getNRawHits() const        { return mNRawHits;           };
inline unsigned char StMuFstHit::getNRawHitsR() const       { return mNRawHitsR;          };
inline unsigned char StMuFstHit::getNRawHitsPhi() const     { return mNRawHitsPhi;        };

inline const TVector3& StMuFstHit::xyz() const              { return mXYZ;                };

inline void StMuFstHit::setId(int id)                               { mId = id;                     };
inline void StMuFstHit::setIdTruth(int idtruth)                     { mIdTruth = idtruth;           };
inline void StMuFstHit::setApv(unsigned char apv)                   { mApv = apv;                   };
inline void StMuFstHit::setMaxTimeBin(unsigned char tb)             { mMaxTimeBin = tb;             };
inline void StMuFstHit::setMeanRStrip(float meanRStrip)             { mMeanRStrip = meanRStrip;     };
inline void StMuFstHit::setMeanPhiStrip(float meanPhiStrip)         { mMeanPhiStrip = meanPhiStrip; };
inline void StMuFstHit::setCharge(float charge)                     { mCharge = charge;             };
inline void StMuFstHit::setChargeErr(float chargeErr)               { mChargeErr = chargeErr;       };
inline void StMuFstHit::setNRawHits(unsigned char nRawHits)         { mNRawHits = nRawHits;         };
inline void StMuFstHit::setNRawHitsR(unsigned char nRawHitsR)       { mNRawHitsR = nRawHitsR;       };
inline void StMuFstHit::setNRawHitsPhi(unsigned char nRawHitsPhi)   { mNRawHitsPhi = nRawHitsPhi;   };

inline void StMuFstHit::setXYZ(const TVector3& p3) { mXYZ = p3; }

inline void StMuFstHit::setHardwarePosition(int hardwarePosition)   { mHardwarePosition = hardwarePosition;   };

#endif  // StMuFstHit_h


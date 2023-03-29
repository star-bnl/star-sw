/***************************************************************************
 *
 * $Id: StFttRawHit.h,v 1.0 2021/11/18 18:52:38 jdb Exp $
 *
 * Author: Philipp Weidenkaff, April 2018
 ***************************************************************************
 *
 * Description: Data class for sTGC raw hit in StEvent
 *
 ***************************************************************************/
#ifndef STFTTRAWHIT_H
#define STFTTRAWHIT_H

#include <Stiostream.h>
#include "StObject.h"
#include "StEnumerations.h"


class StFttRawHit : public StObject {
public:
    /**
    ** @brief Default constructor.
    **/
    StFttRawHit();

    StFttRawHit(    UChar_t mSector, UChar_t mRDO, UChar_t mFEB, 
                    UChar_t mVMM, UChar_t mChannel, UShort_t mADC, 
                    UShort_t mBCID, Short_t mTB, Short_t mBCIDDelta );

    ~StFttRawHit() {}

    void setRaw(    UChar_t mSector, UChar_t mRDO, UChar_t mFEB, 
                    UChar_t mVMM, UChar_t mChannel, UShort_t mADC, 
                    UShort_t mBCID, Short_t mTB, Short_t mBCIDDelta );

    void setMapping( UChar_t mPlane, UChar_t mQuadrant, UChar_t mRow, UChar_t mStrip, UChar_t mOrientation );

    void setTime( Short_t mTime ) { this->mTime = mTime; }
    // consant getters

    UChar_t sector() const;
    UChar_t rdo() const;
    UChar_t feb() const;
    UChar_t vmm() const;
    UChar_t channel() const;
    UShort_t adc() const;
    UShort_t bcid() const;
    Short_t dbcid() const;
    Short_t tb() const;
    Short_t time() const;

    UChar_t plane() const;
    UChar_t quadrant() const;
    UChar_t row() const;
    UChar_t strip() const;
    UChar_t orientation() const;

protected:
    UChar_t mSector;
    UChar_t mRDO;
    UChar_t mFEB;
    UChar_t mVMM;
    UChar_t mChannel;
    UShort_t mADC;
    UShort_t mBCID;
    Short_t mTB;  // from the trigger
    Short_t mBCIDDelta;
    Short_t mTime;  // calibrated BCID Delta

    // mapped information
    UChar_t mPlane;
    UChar_t mQuadrant;
    UChar_t mRow;
    UChar_t mStrip;
    UChar_t mOrientation;

    // StFttCluster *mCluster;
    // StFttPoint   *mPoint;

    ClassDef( StFttRawHit, 3 );
};

ostream& operator << ( ostream&, const StFttRawHit& digi ); // Printing operator

inline UChar_t  StFttRawHit::sector()      const { return mSector;      };
inline UChar_t  StFttRawHit::rdo()         const { return mRDO;         };
inline UChar_t  StFttRawHit::feb()         const { return mFEB;         };
inline UChar_t  StFttRawHit::vmm()         const { return mVMM;         };
inline UChar_t  StFttRawHit::channel()     const { return mChannel;     };
inline UShort_t StFttRawHit::adc()         const { return mADC;         };
inline UShort_t StFttRawHit::bcid()        const { return mBCID;        };
inline Short_t  StFttRawHit::dbcid()       const { return mBCIDDelta;   };
inline Short_t  StFttRawHit::tb()          const { return mTB;          };
inline Short_t  StFttRawHit::time()        const { return mTime;        };

inline UChar_t  StFttRawHit::plane()       const { return mPlane;       };
inline UChar_t  StFttRawHit::quadrant()    const { return mQuadrant;    };
inline UChar_t  StFttRawHit::row()         const { return mRow;         };
inline UChar_t  StFttRawHit::strip()       const { return mStrip;       };
inline UChar_t  StFttRawHit::orientation() const { return mOrientation; };


#endif // STETOFDIGI_H

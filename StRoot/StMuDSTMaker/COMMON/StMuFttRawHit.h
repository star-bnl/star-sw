/***************************************************************************
 *
 * $Id: StMuFttRawHit.h
 *
 * Author: jdb, 2021
 ***************************************************************************
 *
 * Description: Data class for sTGC raw hit in StMuDst
 *
 ***************************************************************************/
#ifndef STMUFTTRAWHIT_H
#define STMUFTTRAWHIT_H

#include <iostream>
#include <TObject.h>
#include "StEnumerations.h"
class StFttRawHit;

class StMuFttRawHit : public TObject {
public:
    /**
    ** @brief Default constructor.
    **/
    StMuFttRawHit();

    StMuFttRawHit( StFttRawHit * stHit );

    StMuFttRawHit(    UChar_t mSector, UChar_t mRDO, UChar_t mFEB, 
                    UChar_t mVMM, UChar_t mChannel, UShort_t mADC, 
                    UShort_t mBCID, Short_t mTB, Short_t mBCIDDelta );

    ~StMuFttRawHit() {}

    void setRaw(    UChar_t mSector, UChar_t mRDO, UChar_t mFEB, 
                    UChar_t mVMM, UChar_t mChannel, UShort_t mADC, 
                    UShort_t mBCID, Short_t mTB, Short_t mBCIDDelta );

    void setMapping( UChar_t mPlane, UChar_t mQuadrant, UChar_t mRow, UChar_t mStrip, UChar_t mOrientation );
    void set( StFttRawHit * stHit );

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

    // mapped information
    UChar_t mPlane;
    UChar_t mQuadrant;
    UChar_t mRow;
    UChar_t mStrip;
    UChar_t mOrientation;

    // StFttCluster *mCluster;
    // StFttPoint   *mPoint;

    ClassDef( StMuFttRawHit, 2 );
};

std::ostream& operator << ( std::ostream&, const StMuFttRawHit& hit ); // Printing operator

inline UChar_t  StMuFttRawHit::sector()      const { return mSector;      };
inline UChar_t  StMuFttRawHit::rdo()         const { return mRDO;         };
inline UChar_t  StMuFttRawHit::feb()         const { return mFEB;         };
inline UChar_t  StMuFttRawHit::vmm()         const { return mVMM;         };
inline UChar_t  StMuFttRawHit::channel()     const { return mChannel;     };
inline UShort_t StMuFttRawHit::adc()         const { return mADC;         };
inline UShort_t StMuFttRawHit::bcid()        const { return mBCID;        };
inline Short_t  StMuFttRawHit::dbcid()       const { return mBCIDDelta;   };
inline Short_t  StMuFttRawHit::tb()          const { return mTB;          };

inline UChar_t  StMuFttRawHit::plane()       const { return mPlane;       };
inline UChar_t  StMuFttRawHit::quadrant()    const { return mQuadrant;    };
inline UChar_t  StMuFttRawHit::row()         const { return mRow;         };
inline UChar_t  StMuFttRawHit::strip()       const { return mStrip;       };
inline UChar_t  StMuFttRawHit::orientation() const { return mOrientation; };


#endif // STETOFDIGI_H

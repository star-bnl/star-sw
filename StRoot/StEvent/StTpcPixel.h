/*!
 * \class StTpcPixel 
 * \author Thomas Ullrich, Jan 1999
 */
/***************************************************************************
 *
 * $Id: StTpcPixel.h,v 2.5 2004/04/26 16:33:35 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcPixel.h,v $
 * Revision 2.5  2004/04/26 16:33:35  fisyak
 * Make use of StTpcPixel
 *
 * Revision 2.4  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/04/05 04:00:44  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  1999/12/13 20:16:34  ullrich
 * Changed numbering scheme for hw_position unpack methods (STAR conventions).
 *
 * Revision 2.1  1999/10/13 19:44:04  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcPixel_hh
#define StTpcPixel_hh

#include "StObject.h"
#include "Stiostream.h"
#include "StEnumerations.h"

class StTpcPixel : public StObject {
public:
    StTpcPixel(UChar_t Detector = 0, UChar_t Sector = 0, UChar_t Row = 0,
	       UChar_t Pad = 0, UShort_t TimeBin = 0,UShort_t Adc=0, 
	       UShort_t  mIdTruth=0) :
      mDetector(Detector),  mSector(Sector), mRow(Row), 
      mPad(Pad), mTimeBin(TimeBin), mAdc(Adc), mIdTruth(mIdTruth) {}
    virtual ~StTpcPixel() {}
    virtual void Print(Option_t *option="") const;
    UChar_t   detector() const {return mDetector;}  
    UChar_t   sector()   const {return  mSector;}   
    UChar_t   padrow()   const {return  mRow;}      
    UChar_t   pad()      const {return     mPad;}      
    UShort_t  timebin()  const {return mTimeBin;}  
    UShort_t  adc()      const {return     mAdc;}      
    UShort_t  idTruth()  const {return mIdTruth;}  
protected:
    //    UShort_t  mDetectorSectorRow;
    //    UInt_t    mPadTimeAdc;

    UChar_t   mDetector; 
    UChar_t   mSector;   
    UChar_t   mRow;      
    UChar_t   mPad;      
    UShort_t  mTimeBin;  
    UShort_t  mAdc;      
    UShort_t  mIdTruth;  
    ClassDef(StTpcPixel,1)  //StTpcPixel structure
};
ostream& operator<< (ostream&, const StTpcPixel&);
#endif

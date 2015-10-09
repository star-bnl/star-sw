/*!
 * \class StTpcPixel 
 * \author Thomas Ullrich, Jan 1999
 */
/***************************************************************************
 *
 * $Id: StTpcPixel.h,v 2.8 2015/10/09 17:46:15 ullrich Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcPixel.h,v $
 * Revision 2.8  2015/10/09 17:46:15  ullrich
 * Changed type of mIdTruth from ushort to int.
 *
 * Revision 2.7  2009/11/23 22:20:51  ullrich
 * Minor cleanup performed, fixed compiler warnings.
 *
 * Revision 2.6  2004/08/06 15:37:43  fisyak
 * Add clster id
 *
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
               UChar_t Pad = 0, unsigned short TimeBin = 0,unsigned short Adc=0, 
               Int_t IdTruth=0, short Id=0) :
        mDetector(Detector),  mSector(Sector), mRow(Row), 
        mPad(Pad), mTimeBin(TimeBin), mAdc(Adc), mIdTruth(IdTruth), mId(Id) {}
    virtual ~StTpcPixel() {}
    UChar_t   detector() const;  
    UChar_t   sector()   const;   
    UChar_t   padrow()   const;      
    UChar_t   pad()      const;      
    unsigned short  timebin()  const;  
    unsigned short  adc()      const;    
    Int_t           idTruth()  const;  
    short           id()       const;  
    virtual void Print(Option_t *option="") const;
    
protected:
    UChar_t   mDetector; 
    UChar_t   mSector;   
    UChar_t   mRow;      
    UChar_t   mPad;      
    UShort_t  mTimeBin;  
    UShort_t  mAdc;      
    Int_t     mIdTruth;
    Short_t   mId; // Cluster Id
    
    ClassDef(StTpcPixel, 2)  //StTpcPixel structure
};

inline UChar_t   StTpcPixel::detector() const {return mDetector;}  
inline UChar_t   StTpcPixel::sector()   const {return mSector;}   
inline UChar_t   StTpcPixel::padrow()   const {return mRow;}      
inline UChar_t   StTpcPixel::pad()      const {return mPad;}      
inline UShort_t  StTpcPixel::timebin()  const {return mTimeBin;}  
inline UShort_t  StTpcPixel::adc()      const {return mAdc;}      
inline Int_t     StTpcPixel::idTruth()  const {return mIdTruth;}  
inline Short_t   StTpcPixel::id()       const {return mId;}  

ostream& operator<< (ostream&, const StTpcPixel&);
#endif

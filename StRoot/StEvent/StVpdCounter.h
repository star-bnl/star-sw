/***************************************************************************
 *
 * $Id: StVpdCounter.h,v 1.3 1999/04/27 01:24:31 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdCounter.h,v $
 * Revision 1.3  1999/04/27 01:24:31  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.4  1999/04/28 22:27:40  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:54:25  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#define StVpdCounter_hh

class StVpdCounter : public TObject {
#include "StObject.h"
#include "StArray.h"
class StVpdCounter : public StObject {
public:
    StVpdCounter();
    StVpdCounter(Short_t id, Float_t a, Float_t t);
    ~StVpdCounter();
    // StVpdCounter(const StVpdCounter &right);
    // const StVpdCounter & operator=(const StVpdCounter &right);

    Short_t id() const;
    Float_t adc() const;
    Float_t time() const;

    void setId(Short_t);
    void setAdc(Float_t);
    void setTime(Float_t);
    
protected:
    ClassDef(StVpdCounter,1)  //StVpdCounter structure
    Float_t mAdc;
    Float_t mTime;
typedef StVecPtrVpdCounter StVecVpdCounter;
};
StCollectionDef(VpdCounter)

inline Short_t StVpdCounter::id() const { return mId; }

inline Float_t StVpdCounter::adc() const { return mAdc; }

inline Float_t StVpdCounter::time() const { return mTime; }

#endif

/***************************************************************************
 *
 * $Id: StCtbCounter.h,v 1.5 1999/04/30 13:16:26 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbCounter.h,v $
 * Revision 1.5  1999/04/30 13:16:26  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.5  1999/04/30 13:16:26  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:29  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:53:29  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StCtbCounter_hh
#define StCtbCounter_hh

#include "StObject.h"
#include "StArray.h"
class StCtbCounter : public StObject {
public:
    StCtbCounter();
    StCtbCounter(Short_t id, Float_t m, Float_t t);
    ~StCtbCounter();
    // StCtbCounter(const StCtbCounter &right);
    // const StCtbCounter & operator=(const StCtbCounter &right);

    Short_t id() const;
    Float_t mips() const;
    Float_t time() const;

    void setId(Short_t);
    void setMips(Float_t);
    void setTime(Float_t);
    
protected:
    Short_t mId;
    Float_t mMips;
    Float_t mTime;
  ClassDef(StCtbCounter,1)  //StCtbCounter structure
};
StCollectionDef(CtbCounter)
typedef StVecPtrCtbCounter StVecCtbCounter;

inline Short_t StCtbCounter::id() const { return mId; }

inline Float_t StCtbCounter::mips() const { return mMips; }

inline Float_t StCtbCounter::time() const { return mTime; }

#endif

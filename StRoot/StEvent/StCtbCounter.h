/***************************************************************************
 *
 * $Id: StCtbCounter.h,v 1.3 1999/04/27 01:24:16 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbCounter.h,v $
 * Revision 1.3  1999/04/27 01:24:16  fisyak
 * Fix intermidaiate version with pointer instead of referencies
 *
 * Revision 1.4  1999/04/28 22:27:29  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:53:29  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#define StCtbCounter_hh

class StCtbCounter : public TObject {
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
    ClassDef(StCtbCounter,1)  //StCtbCounter structure
    Float_t mMips;

    Float_t mTime;

//________________________________________________
  ClassDef(StCtbCounter,1)  //StCtbCounter structure
};
StCollectionDef(CtbCounter)
typedef StVecPtrCtbCounter StVecCtbCounter;

inline Short_t StCtbCounter::id() const { return mId; }

inline Float_t StCtbCounter::mips() const { return mMips; }

inline Float_t StCtbCounter::time() const { return mTime; }

#endif

/***************************************************************************
 *
 * $Id: StMwcSector.h,v 1.2 1999/02/09 21:07:38 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMwcSector.h,v $
 * Revision 1.2  1999/02/09 21:07:38  fisyak
 * Import new Torre staffs
 *
 * Revision 1.4  1999/04/28 22:27:34  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.2  1999/01/15 22:53:48  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifdef __ROOT__
#include "TObject.h"
#endif
class StMwcSector : public TObject {
#include "StObject.h"
#include "StArray.h"
class StMwcSector : public StObject {
public:
    StMwcSector();
    StMwcSector(Short_t id, Float_t m);
    ~StMwcSector();
    // StMwcSector(const StMwcSector &right);
    // const StMwcSector & operator=(const StMwcSector &right);

    Short_t id() const;
    Float_t mips() const;

    void setId(Short_t);
    void setMips(Float_t);
    
#ifdef __ROOT__
	ClassDef(StMwcSector,1)  //StMwcSector structure
#endif
    Short_t mId;
  ClassDef(StMwcSector,1)  //StMwcSector structure
};
StCollectionDef(MwcSector)

inline Short_t StMwcSector::id() const { return mId; }

inline Float_t StMwcSector::mips() const { return mMips; }

#endif

/***************************************************************************
 *
 * $Id: StVpdSummary.h,v 1.5 1999/04/30 13:16:31 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StVpdSummary.h,v $
 * Revision 1.5  1999/04/30 13:16:31  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.5  1999/04/30 13:16:31  fisyak
 * add StArray for StRootEvent
 *
 * Revision 1.4  1999/04/28 22:27:40  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.3  1999/03/17 22:22:19  ullrich
 * Some cosmetics
 *
 * Revision 1.2  1999/01/15 22:54:26  wenaus
 * version with constructors for table-based loading
 *
 **************************************************************************/
#ifndef StVpdSummary_hh
#define StVpdSummary_hh
#include "StObject.h"
#include "StEnumerations.h"

class StVpdSummary : public StObject {
public:
    StVpdSummary();
    StVpdSummary(Float_t te, Float_t tw, Float_t z);
    ~StVpdSummary();
    // StVpdSummary(const StVpdSummary &right);
    // const StVpdSummary & operator=(const StVpdSummary &right);

    Float_t vertexZ() const;
    Float_t minimumTime(StBeamDirection) const;

    void setMinimumTime(StBeamDirection, Float_t); 
    void setVertexZ(Float_t);     
    
protected:
    Float_t mMinimumTimeEast;
    Float_t mMinimumTimeWest;
    Float_t mVertexZ;        
  ClassDef(StVpdSummary,1)  //StVpdSummary structure
};

inline Float_t StVpdSummary::vertexZ() const { return mVertexZ; }

inline Float_t StVpdSummary::minimumTime(StBeamDirection dir) const
{
    if (dir == east)
	return mMinimumTimeEast;
    else
	return mMinimumTimeWest;
}

#endif

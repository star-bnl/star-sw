/*!
 * \class StTofMCHit 
 * \author Wei-Ming Zhang, April 2001 
 */
/***************************************************************************
 *
 * $Id: StTofMCHit.h,v 2.5 2004/07/15 16:36:25 ullrich Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCHit.h,v $
 * Revision 2.5  2004/07/15 16:36:25  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.4  2003/09/02 17:58:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.3  2003/05/21 18:22:46  ullrich
 * Major Revision of ToF classes (F. Geurts)
 *
 * Revision 2.2  2002/02/22 22:56:51  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.1  2001/04/26 01:07:42  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTofMCHit_hh
#define StTofMCHit_hh

#include <Stiostream.h>
#include "StTofHit.h"

class StTofMCHit : public StTofHit {
public:
    StTofMCHit();
    ~StTofMCHit();

    int   trkId() const;
    int   gId()   const;

    void  setTrkId(Int_t);
    void  setGId(Int_t);

protected:
    Int_t     mTrkId;
    Int_t     mGId;

    ClassDef(StTofMCHit,2)
};

inline void
StTofMCHit::setTrkId(int trkId)
{
    mTrkId = trkId;
}

inline void
StTofMCHit::setGId(int gId)
{
    mGId = gId;
}

inline int
StTofMCHit::trkId() const
{
    return mTrkId;
}

inline int
StTofMCHit::gId() const
{
    return mGId;
}

//non-members
ostream& operator<<(ostream&, const StTofMCHit&);

#endif

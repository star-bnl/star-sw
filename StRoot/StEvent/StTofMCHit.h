/*!
 * \class StTofMCHit 
 * \author Wei-Ming Zhang, April 2001 
 */
/***************************************************************************
 *
 * $Id: StTofMCHit.h,v 2.2 2002/02/22 22:56:51 jeromel Exp $
 *
 * Author: Wei-Ming Zhang, April 2001 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofMCHit.h,v $
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

#include <iostream.h>
#include "StTofHit.h"

class StTofMCHit : public StTofHit {
public:
    StTofMCHit();
    //StTofMCHit(const StTofMCHit&);
    //StTofMCHit& operator=(const StTofMCHit&);
    ~StTofMCHit();

    int       trkId()            const;
    int       gId()              const;

    void      setTrkId(Int_t);
    void      setGId(Int_t);

protected:
    StObject* clone() const;
    Int_t     mTrkId;
    Int_t     mGId;

    ClassDef(StTofMCHit,1)
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

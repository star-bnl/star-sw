/***************************************************************************
 *
 * $Id: StTofMCHit.h,v 1.1 2001/04/24 20:27:37 wzhang Exp $
 *
 * Author: Wei-Ming Zhang, April 2001
 ***************************************************************************
 *
 * Description:
 * Inherited from StTofHit with MC info (two Ids) added.
 *
 ***************************************************************************
 *
 * $Log: StTofMCHit.h,v $
 * Revision 1.1  2001/04/24 20:27:37  wzhang
 * First release
 *
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

    Int_t     trkId()            const;
    Int_t     gId()              const;

    void      setTrkId(Int_t);
    void      setGId(Int_t);


protected:
    StObject* MCClone();
    Int_t     mTrkId;
    Int_t     mGId;

    ClassDef(StTofMCHit,1)
};

inline void
StTofMCHit::setTrkId(Int_t trkId)
{
    mTrkId = trkId;
}

inline void
StTofMCHit::setGId(Int_t gId)
{
    mGId = gId;
}

inline Int_t
StTofMCHit::trkId() const
{
    return mTrkId;
}

inline Int_t
StTofMCHit::gId() const
{
    return mGId;
}

//non-members
ostream& operator<<(ostream&, const StTofMCHit&);

#endif

/***************************************************************************
 *
 * $Id: StEmcHit.h,v 1.1 1999/01/30 03:58:05 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcHit.h,v $
 * Revision 1.1  1999/01/30 03:58:05  fisyak
 * Root Version of StEvent
 *
 * Revision 1.3  1999/02/23 15:25:55  ullrich
 * Complete Revision
 *
 * Revision 1.2  1999/01/15 22:53:36  wenaus
 * version with constructors for table-based loading
 *

#ifdef __ROOT__
#include "TObject.h"
#endif
class StEmcHit : public TObject {
#include <iostream.h>
//     StEmcHit();
//     StEmcHit(const StEmcHit&);
//     ~StEmcHit();
//     const StEmcHit & operator=(const StEmcHit&);
//     Int_t operator==(const StEmcHit&) const;
//     Int_t operator!=(const StEmcHit&) const;
    void setId(Int_t);
private:
#ifdef __ROOT__
	ClassDef(StEmcHit,1)  //StEmcHit structure
#endif
    Float_t mEta;


inline void StEmcHit::setEta(Float_t val) { mEta = val; };

#endif

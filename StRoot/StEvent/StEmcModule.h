/***************************************************************************
 *
 * $Id: StEmcModule.h,v 2.2 2001/04/05 04:00:35 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcModule.h,v $
 * Revision 2.2  2001/04/05 04:00:35  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/02/23 17:34:11  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcModule_hh
#define StEmcModule_hh

#include "StObject.h"
#include "StContainers.h"

class StEmcModule : public StObject {
public:
    StEmcModule();
    ~StEmcModule();
    // StEmcModule(const StEmcModule&);            use default
    // StEmcModule& operator=(const StEmcModule&); use default
    
    unsigned int numberOfHits() const;

    StSPtrVecEmcRawHit&       hits();
    const StSPtrVecEmcRawHit& hits() const;
    
private:
    StSPtrVecEmcRawHit mHits;
    ClassDef(StEmcModule,1)
 };
#endif

/***************************************************************************
 *
 * $Id: StEmcModule.h,v 2.1 2000/02/23 17:34:11 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcModule.h,v $
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
    
    UInt_t numberOfHits() const;    

    StSPtrVecEmcRawHit&       hits();
    const StSPtrVecEmcRawHit& hits() const;
    
private:
    StSPtrVecEmcRawHit mHits;    
    ClassDef(StEmcModule,1)
 };
#endif

/***************************************************************************
 *
 * $Id: StDetectorState.h,v 2.1 2001/12/01 15:34:50 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 2001
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDetectorState.h,v $
 * Revision 2.1  2001/12/01 15:34:50  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StDetectorState_hh
#define StDetectorState_hh
#include "StObject.h"
#include "StEnumerations.h"

class StDetectorState : public StObject {
public:
    StDetectorState();
    StDetectorState(StDetectorId, bool);
    // StDetectorState(const StDetectorState&);            use default
    // StDetectorState& operator=(const StDetectorState&); use default
    virtual ~StDetectorState();
    
    StDetectorId detector() const;
    bool         good() const;
    bool         bad() const;

    void setDetector(StDetectorId);
    void setGood(bool);

private:
    StDetectorId mDetectorId;
    Bool_t       mIsGood;
    
    ClassDef(StDetectorState,1)
};
#endif

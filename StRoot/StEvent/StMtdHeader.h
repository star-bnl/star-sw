/*!
 * \class StMtdHeader 
 */
/***************************************************************************
 *
 * $Id: StMtdHeader.h,v 2.2 2013/04/06 12:18:35 ullrich Exp $
 *
 * Author: Frank Geurts, April 25, 2011
 ***************************************************************************
 *
 * Description:
 *   Barrel MTD header data, contains the MTD data header
 *
 ***************************************************************************
 *
 * $Log: StMtdHeader.h,v $
 * Revision 2.2  2013/04/06 12:18:35  ullrich
 * Increase MAXFIBER from 1 to 2.
 *
 * Revision 2.1  2011/04/25 21:24:02  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StMtdHeader_hh
#define StMtdHeader_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StEnumerations.h"

class StMtdHeader : public StObject {
public:
    enum {MAXFIBER=2};
    
    StMtdHeader();
    ~StMtdHeader();
    
    short          fiberHeader(int fiberId) const;
    unsigned int   fiberTriggerWord(int fiberId) const;
    unsigned int   triggerTime(int fiberId) const;
    
    void         setFiberHeader(int fiberId, short val);
    void         setFiberTriggerWord(int fiberId, unsigned int val);
    void         setTriggerTime(unsigned int tdc, int fiberId);
    
protected:
    Short_t      mFiberHeader[MAXFIBER];
    UInt_t       mFiberTriggerWord[MAXFIBER];
    UInt_t       mTriggerTime[MAXFIBER];
    
    ClassDef(StMtdHeader,1)
};

#endif

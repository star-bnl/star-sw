/***************************************************************************
 *
 * $Id: StPsd.h,v 2.2 2001/03/24 03:34:54 perev Exp $
 *
 * Author: Thomas Ullrich, Mar 2001
 ***************************************************************************
 *
 * Description: Base class for all Physics Summary Data (PSD) classes.
 *
 ***************************************************************************
 *
 * $Log: StPsd.h,v $
 * Revision 2.2  2001/03/24 03:34:54  perev
 * clone() -> clone() const
 *
 * Revision 2.1  2001/03/14 02:27:50  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StPsd_hh
#define StPsd_hh
#include "StObject.h"
#include "StEnumerations.h"

class StPsd : public StObject {
public:
    StPsd();
    StPsd(StPwg, int);
    // StPsd(const StPsd&);             use default
    // StPsd& operator=(const StPsd&);  used default
    virtual ~StPsd();

    StPwg   pwg() const;
    int     id() const;

    void    setPwg(StPwg);
    void    setId(int);
    
protected:
    virtual StObject* clone() const = 0;

private:
    StPwg   mPwg;
    int     mId;
    
    ClassDef(StPsd,1)
};
#endif

/*!
 * \class StPsd 
 * \author Thomas Ullrich, Mar 2001
 */
/***************************************************************************
 *
 * $Id: StPsd.h,v 2.5 2004/07/15 16:36:24 ullrich Exp $
 *
 * Author: Thomas Ullrich, Mar 2001
 ***************************************************************************
 *
 * Description: Base class for all Physics Summary Data (PSD) classes.
 *
 ***************************************************************************
 *
 * $Log: StPsd.h,v $
 * Revision 2.5  2004/07/15 16:36:24  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.4  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2001/04/05 04:00:39  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
    
private:
    StPwg   mPwg;
    Int_t   mId;
    
    ClassDef(StPsd,1)
};
#endif

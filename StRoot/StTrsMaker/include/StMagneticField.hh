/***************************************************************************
 *
 * $Id: StMagneticField.hh,v 1.2 2000/06/23 00:12:23 snelling Exp $
 *
 * Author: Thomas Ullrich, May 1998 
 ***************************************************************************
 *
 * Description: Abstract class
 *
 ***************************************************************************
 *
 * $Log: StMagneticField.hh,v $
 * Revision 1.2  2000/06/23 00:12:23  snelling
 * Removed dependence on local files now pointed to StDbUtilities
 *
 * Revision 1.1  1998/11/10 17:12:04  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/05/25 16:59:24  lasiuk
 * remove virtual destructor
 *
 * Revision 1.1  1998/05/20 14:58:25  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_MAGNETIC_FIELD
#define ST_MAGNETIC_FIELD

#include "StThreeVector.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"

class StMagneticField {
public:
    virtual ~StMagneticField() {/* nopt */};
    virtual const StThreeVector<double>& at(const StGlobalCoordinate&) const = 0;
};

#endif

/***********************************************************************
 *
 * $Id: StRichMomentumTransform.h,v 2.0 2000/08/09 16:17:02 gans Exp $
 *
 * Author: brian made this on Jan 27, 2000
 *
 ***********************************************************************
 * Description:
 *
 * Geometrical transformation Routines for:
 *  local <-->global moemtnum
 * 
 *
 ***********************************************************************
 *
 * $Log: StRichMomentumTransform.h,v $
 * Revision 2.0  2000/08/09 16:17:02  gans
 * Readded Files That were not added in last CVS. Cosmetic Changes, naming convention
 * for StRichDrawableT(foo)
 *
 * Revision 1.2  2000/03/17 14:54:54  lasiuk
 * Large scale revisions after ROOT dependent memory leak
 *
 * Revision 1.1  2000/03/12 22:19:26  lasiuk
 * Initial Revision
 *
 ***********************************************************************/
#ifndef ST_RICH_MOMENTUM_TRANSFORM_H
#define ST_RICH_MOMENTUM_TRANSFORM_H

#include <stdlib.h>

// SCL
#include "StGlobals.hh"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"

#include "StRichGeometryDbInterface.h"

class StRichMomentumTransform {
public:
    static StRichMomentumTransform* getTransform(StRichGeometryDbInterface*);
    static StRichMomentumTransform* getTransform(); // do not call!

    virtual ~StRichMomentumTransform();
    //StRichMomentumTransform(const StRichMomentumTransform&);
    //StRichMomentumTransform& operator=(const StRichMomentumTransform&);
    
    void localMomentum(const StThreeVector<double>& a, StThreeVector<double>& b) const;
    void globalMomentum(const StThreeVector<double>& a, StThreeVector<double>& b) const;

protected:
    StRichMomentumTransform(StRichGeometryDbInterface*);
    StRichMomentumTransform(); // DO NOT CALL!

private:
    // Transformation Routines!!
    StRichGeometryDbInterface    *mGeomDb;

private:
    static StRichMomentumTransform* mInstance;

    //
    // Survey
    double mLocalAngleZ;
    double mLocalAngleY;
    double mLocalAngleX;

    double mLocalRotate[3][3];
    

};

#endif

/***********************************************************************
 *
 * $Id: StRichMomentumTransform.h,v 1.1 2000/03/12 22:19:26 lasiuk Exp $
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
 * Revision 1.1  2000/03/12 22:19:26  lasiuk
 * Initial Revision
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

    ~StRichMomentumTransform();
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
    double mCosB;
    double mSinB;
};

#endif

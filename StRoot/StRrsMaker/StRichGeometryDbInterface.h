/***********************************************
 * $Id: StRichGeometryDbInterface.h,v 1.1 2000/01/25 22:02:20 lasiuk Exp $
 *
 * Description:
 *
 * $Log: StRichGeometryDbInterface.h,v $
 * Revision 1.1  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 * Revision 1.1  2000/01/25 22:02:20  lasiuk
 * Second Revision
 *
 ***********************************************************/
#ifndef ST_RICH_GEOMETRY_INTERFACE_H
#define ST_RICH_GEOMETRY_INTERFACE_H

#include <iostream.h>
#include "StGlobals.hh"

class StRichGeometryDbInterface {
public:
    
    virtual ~StRichGeometryDbInterface() {}
    //StRichGeometryInterface(const StRichGeometryInterface&);
    //StRichGeometryInterface&(const StRichGeometryInterface&);

    virtual double version() const = 0;
    virtual double detectorLength() const = 0;
    virtual double detectorWidth() const = 0;

    virtual double quadrantZ0(int) const = 0;
    virtual double quadrantX0(int) const = 0;

    virtual double quadrantGapsInZ() const = 0;
    virtual double quadrantGapsInX() const = 0;    

    virtual int    numberOfPadsInRow() const = 0; // X
    virtual int    numberOfPadsInColumn() const = 0; // Z
    virtual int    numberOfPads() const = 0;

    virtual double padPitch() const = 0;
    virtual double padLength() const = 0;
    virtual double padWidth() const = 0;
	
    virtual double wirePitch() const = 0;
    virtual double firstWirePositionInX() const = 0;
    virtual int    numberOfWires() const = 0;

    virtual double anodeToPadSpacing() const = 0;
	
    virtual double gasGainAmplificationFactor() const = 0;

    virtual void   print(ostream& os = cout) const = 0;
};
#endif

/***********************************************************************
 *
 * $Id: StSvtLocalCoordinate.hh,v 1.2 2000/02/02 23:01:38 calderon Exp $
 *
 * Author:  Manuel CBS Oct 1999
 *
 ************************************************************************
 *
 * Description:  Svt Local Coordinate
 *
 ************************************************************************
 *
 * $Log: StSvtLocalCoordinate.hh,v $
 * Revision 1.2  2000/02/02 23:01:38  calderon
 * Changes for CC5
 * Tests withs StTpcDb still going.
 *
 * Revision 1.1  1999/11/19 19:01:08  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 *
 ***********************************************************************/
#ifndef ST_SVT_LOCAL_COORDINATE_HH
#define ST_SVT_LOCAL_COORDINATE_HH
#include <iostream.h>

#include "StThreeVector.hh"

class StSvtLocalCoordinate
{
public:
    StSvtLocalCoordinate();
    StSvtLocalCoordinate(const double, const double, const double);
    StSvtLocalCoordinate(const StThreeVector<double>&);

    virtual ~StSvtLocalCoordinate();
    //StSvtLocalCoordinate(const StTpcLocalCoordinate&);
    //StSvtLocalCoordinate& operator=(const StTpcLocalCoordinate&);
    
    int operator==(const StSvtLocalCoordinate&) const;
    int operator!=(const StSvtLocalCoordinate&) const;

    // access functions provided by StThreeVector
    const StThreeVector<double>& position()  const;
    StThreeVector<double>& position();
    void setPosition(const StThreeVector<double>&);
protected:
    StThreeVector<double> mPosition;

};

inline const StThreeVector<double>& StSvtLocalCoordinate::position() const { return(mPosition); }
inline StThreeVector<double>& StSvtLocalCoordinate::position() { return(mPosition); }
inline void StSvtLocalCoordinate::setPosition(const StThreeVector<double>& val) { mPosition = val; }

// Non-member
ostream& operator<<(ostream&, const StSvtLocalCoordinate&);
#endif
    

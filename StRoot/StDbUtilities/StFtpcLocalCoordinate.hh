/***********************************************************************
 *
 * $Id: StFtpcLocalCoordinate.hh,v 1.3 2003/09/02 17:57:51 perev Exp $
 *
 * Author:  Manuel CBS Oct 1999
 *
 ************************************************************************
 *
 * Description:  Ftpc Local Coordinate
 *
 ************************************************************************
 *
 * $Log: StFtpcLocalCoordinate.hh,v $
 * Revision 1.3  2003/09/02 17:57:51  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2000/02/02 23:01:38  calderon
 * Changes for CC5
 * Tests withs StTpcDb still going.
 *
 * Revision 1.1  1999/11/19 19:01:07  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
 *
 *
 ***********************************************************************/
#ifndef ST_FTPC_LOCAL_COORDINATE_HH
#define ST_FTPC_LOCAL_COORDINATE_HH
#include <Stiostream.h>

#include "StThreeVector.hh"

class StFtpcLocalCoordinate
{
public:
    StFtpcLocalCoordinate();
    StFtpcLocalCoordinate(const double, const double, const double);
    StFtpcLocalCoordinate(const StThreeVector<double>&);

    virtual ~StFtpcLocalCoordinate();
    //StFtpcLocalCoordinate(const StTpcLocalCoordinate&);
    //StFtpcLocalCoordinate& operator=(const StTpcLocalCoordinate&);
    
    int operator==(const StFtpcLocalCoordinate&) const;
    int operator!=(const StFtpcLocalCoordinate&) const;
    // access functions provided by StThreeVector
    const StThreeVector<double>& position()  const;
    StThreeVector<double>& position();
    void setPosition(const StThreeVector<double>&);
    
private:
    StThreeVector<double> mPosition;
};

inline const StThreeVector<double>& StFtpcLocalCoordinate::position() const { return(mPosition); }
inline StThreeVector<double>& StFtpcLocalCoordinate::position() { return(mPosition); }
inline void StFtpcLocalCoordinate::setPosition(const StThreeVector<double>& val) { mPosition = val; }

// Non-member
ostream& operator<<(ostream&, const StFtpcLocalCoordinate&);
#endif
    

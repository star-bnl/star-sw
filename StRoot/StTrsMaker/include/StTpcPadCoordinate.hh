/***********************************************************************
 *
 * $Id: StTpcPadCoordinate.hh,v 1.2 2000/02/10 01:21:46 calderon Exp $
 *
 * Author: brian Feb 6, 1998
 *
 ************************************************************************
 *
 * Description:  Raw data information along with access functions
 *
 ************************************************************************
 *
 * $Log: StTpcPadCoordinate.hh,v $
 * Revision 1.2  2000/02/10 01:21:46  calderon
 * Switch to use StTpcDb.
 * Coordinates checked for consistency.
 * Fixed problems with StTrsIstream & StTrsOstream.
 *
 * Revision 1.1  1998/11/10 17:12:07  fisyak
 * Put Brian trs versin into StRoot
 *
 * Revision 1.2  1998/11/01 16:21:02  lasiuk
 * remove 'St' from variable declarations
 * add set functions in local Coordinates
 *
 * Revision 1.1  1998/05/21 21:27:38  lasiuk
 * Initial revision
 *
 *
 ***********************************************************************/
#ifndef ST_TPC_PAD_COORDINATE_HH
#define ST_TPC_PAD_COORDINATE_HH
#include <iostream.h>

//#include "StGlobals.hh"

class StTpcPadCoordinate {
public:
    StTpcPadCoordinate();
    StTpcPadCoordinate(const int, const int, const int, const int);

    virtual ~StTpcPadCoordinate();
    //StTpcPadCoordinate(const StTpcPadCoordinate&);
    //StTpcPadCoordinate& operator=(cont StTpcPadCoordinate&);
    
    // access functions
    const int sector()       const;
    const int row()          const;
    const int pad()          const;
    const int timeBucket()   const;

    void setSector(int);
    void setRow(int);
    void setPad(int);
    void setTimeBucket(int);
    
private:
    int mSector;
    int mRow;
    int mPad;
    int mTimeBucket;
};

const inline int StTpcPadCoordinate::sector()     const {return(mSector);}
const inline int StTpcPadCoordinate::row()        const {return(mRow);}
const inline int StTpcPadCoordinate::pad()        const {return(mPad);}
const inline int StTpcPadCoordinate::timeBucket() const {return(mTimeBucket);}
inline void StTpcPadCoordinate::setSector(int s) {mSector = s;}
inline void StTpcPadCoordinate::setRow(int r) {mRow = r;}
inline void StTpcPadCoordinate::setPad(int p) {mPad = p;}
inline void StTpcPadCoordinate::setTimeBucket(int t) {mTimeBucket = t;}
// Non-member
ostream& operator<<(ostream&, const StTpcPadCoordinate&);

#endif

/***********************************************************************
 *
 * $Id: StTpcPadCoordinate.hh,v 1.2 2000/02/02 23:01:39 calderon Exp $
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
 * Revision 1.2  2000/02/02 23:01:39  calderon
 * Changes for CC5
 * Tests withs StTpcDb still going.
 *
 * Revision 1.1  1999/11/19 19:01:09  calderon
 * First version of files for StDbUtilities.
 * Note: this package uses StTpcDb.
 * There are some parameters
 * that are not yet kept in StTpcDb.  When StTpcDb has them, the code
 * will be changed to use them from StTpcDb.
 * There are no Ftpc or Svt Coordinate transformations in here yet.
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

class StTpcPadCoordinate
{
public:
    StTpcPadCoordinate();
    StTpcPadCoordinate(const int, const int, const int, const int);

    virtual ~StTpcPadCoordinate();
    //StTpcPadCoordinate(const StTpcPadCoordinate&);
    //StTpcPadCoordinate& operator=(cont StTpcPadCoordinate&);
    
    int operator==(const StTpcPadCoordinate&) const;
    int operator!=(const StTpcPadCoordinate&) const;
    // access functions
    const int sector()       const;
    const int row()          const;
    const int pad()          const;
    const int timeBucket()   const;

    void setSector(int);
    void setRow(int);
    void setPad(int);
    void setTimeBucket(int);
    
protected:
    int mSector;
    int mRow;
    int mPad;
    int mTimeBucket;

};

const inline int StTpcPadCoordinate::sector()     const {return(mSector);}
const inline int StTpcPadCoordinate::row()        const {return(mRow);}
const inline int StTpcPadCoordinate::pad()        const {return(mPad);}
const inline int StTpcPadCoordinate::timeBucket() const {return(mTimeBucket);}
inline void StTpcPadCoordinate::setSector(int s)        {mSector = s;}
inline void StTpcPadCoordinate::setRow(int r)           {mRow = r;}
inline void StTpcPadCoordinate::setPad(int p)           {mPad = p;}
inline void StTpcPadCoordinate::setTimeBucket(int t)    {mTimeBucket = t;}

// Non-member
ostream& operator<<(ostream&, const StTpcPadCoordinate&);

#endif

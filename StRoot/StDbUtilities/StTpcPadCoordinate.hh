/***********************************************************************
 *
 * $Id: StTpcPadCoordinate.hh,v 1.4 2004/01/14 22:40:05 fisyak Exp $
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
 * Revision 1.4  2004/01/14 22:40:05  fisyak
 * remove constness to make alpha happy
 *
 * Revision 1.3  2003/09/02 17:57:51  perev
 * gcc 3.2 updates + WarnOff
 *
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
#include <Stiostream.h>

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
    int sector()           const {return mSector;}    
    int row()              const {return mRow;}       
    int pad()          	   const {return mPad;}       
    int timeBucket()       const {return mTimeBucket;}
    int sector()                 {return mSector;}    
    int row()          		 {return mRow;}       
    int pad()          		 {return mPad;}       
    int timeBucket()    	 {return mTimeBucket;}

    void setSector(int s)        {mSector = s;}
    void setRow(int r)           {mRow = r;}
    void setPad(int p)           {mPad = p;}
    void setTimeBucket(int t)    {mTimeBucket = t;}
    
protected:
    int mSector;
    int mRow;
    int mPad;
    int mTimeBucket;

};
// Non-member
ostream& operator<<(ostream&, const StTpcPadCoordinate&);

#endif

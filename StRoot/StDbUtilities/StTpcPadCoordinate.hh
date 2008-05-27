/***********************************************************************
 *
 * $Id: StTpcPadCoordinate.hh,v 1.5 2008/05/27 14:26:40 fisyak Exp $
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
 * Revision 1.5  2008/05/27 14:26:40  fisyak
 * Use TChairs, absorb shift tau shift, introduce sector to sector time offset
 *
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
#include "Rtypes.h"
class StTpcPadCoordinate {
public:
  StTpcPadCoordinate(const Int_t sector = 0, const Int_t row = 0, const Float_t pad = 0, const Float_t tb = 0) : mSector(sector), mRow(row), mPad(pad), mTimeBucket(tb) {/**/}
  virtual ~StTpcPadCoordinate() {/**/}
  Int_t operator==(const StTpcPadCoordinate& p) const {return (p.mSector == mSector && p.mRow == mRow && p.mPad == mPad && p.mTimeBucket == mTimeBucket);}
  Int_t operator!=(const StTpcPadCoordinate& p) const {return !(*this == p);};
  // access functions
  Int_t sector()           const {return mSector;}    
  Int_t row()              const {return mRow;}       
  Float_t pad()            const {return mPad;}       
  Float_t timeBucket()     const {return mTimeBucket;}
  Int_t sector()                 {return mSector;}    
  Int_t row()          		 {return mRow;}       
  Float_t pad()          	 {return mPad;}       
  Float_t timeBucket()    	 {return mTimeBucket;}
  
  void setSector(Int_t s)        {mSector = s;}
  void setRow(Int_t r)           {mRow = r;}
  void setPad(Float_t p)           {mPad = p;}
  void setTimeBucket(Float_t t)    {mTimeBucket = t;}
  
protected:
  Int_t mSector;
  Int_t mRow;
  Float_t mPad;
  Float_t mTimeBucket;
  
};
// Non-member
ostream& operator<<(ostream&, const StTpcPadCoordinate&);

#endif

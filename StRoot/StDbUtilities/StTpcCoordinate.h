/*********************************************************************
 * $Id: StTpcCoordinate.h,v 1.1 2004/06/05 23:31:09 fisyak Exp $
 * base class for TPC coordinate / direction transformations
 */
#ifndef ST_TPC_COORDINATE_H
#define ST_TPC_COORDINATE_H
#include "Stiostream.h"
#include "StThreeVector.hh"

class StTpcCoordinate {
 public:
  StTpcCoordinate(const double x, const double y, const double z, const int sect, int row) : 
    mPosition(x,y,z), mFromSector(sect), mFromRow(row) {}
  StTpcCoordinate(const StThreeVector<double>& position, const int sect, int row) :
    mPosition(position), mFromSector(sect), mFromRow(row)  {}
  virtual ~StTpcCoordinate() {}
  int operator==(const StTpcCoordinate& p) const {return p.mPosition == mPosition;}
  int operator!=(const StTpcCoordinate& p) const {return !(*this == p);}
  
  // access functions provided by StThreeVector
  virtual const StThreeVector<double>& position()  const { return mPosition; }
  int  fromSector()                        const { return mFromSector; }
  int  fromRow()                           const { return mFromRow; }
  StThreeVector<double>& position()              { return mPosition; }
  virtual void  setPosition(StThreeVector<double>& position) { mPosition = position; }
  virtual void  setSector(int sector)                        { mFromSector = sector; }
  virtual void  setRow(int row)                              { mFromSector = row; }
protected:
  StThreeVector<double> mPosition;
  int            mFromSector;
  int            mFromRow;
};
// Non-member
ostream& operator<<(ostream&, const StTpcCoordinate&);
#endif

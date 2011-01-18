/*********************************************************************
 * $Id: StTpcCoordinate.h,v 1.2 2011/01/18 14:34:28 fisyak Exp $
 * base class for TPC coordinate / direction transformations
 */
#ifndef ST_TPC_COORDINATE_H
#define ST_TPC_COORDINATE_H
#include "Stiostream.h"
#include "StThreeVector.hh"

class StTpcCoordinate {
 public:
  StTpcCoordinate(const Double_t x, const Double_t y, const Double_t z, const Int_t sect, Int_t row) : 
    mPosition(x,y,z), mFromSector(sect), mFromRow(row) {}
  StTpcCoordinate(const StThreeVector<double>& position, const Int_t sect, Int_t row) :
    mPosition(position), mFromSector(sect), mFromRow(row)  {}
  virtual ~StTpcCoordinate() {}
  Int_t operator==(const StTpcCoordinate& p) const {return p.mPosition == mPosition;}
  Int_t operator!=(const StTpcCoordinate& p) const {return !(*this == p);}
  
  // access functions provided by StThreeVector
  virtual const StThreeVector<double>& position()  const { return mPosition; }
  Int_t  fromSector()                        const { return mFromSector; }
  Int_t  fromRow()                           const { return mFromRow; }
  Int_t  sector()                            const { return mFromSector; }
  Int_t  row()                               const { return mFromRow; }
  StThreeVector<double>& position()                { return mPosition; }
  virtual void  setPosition(StThreeVector<double>& position) { mPosition = position; }
  virtual void  setSector(Int_t sector)            { mFromSector = sector; }
  virtual void  setRow(Int_t row)                  { mFromRow   = row; }
protected:
  StThreeVector<double> mPosition;
  Int_t                 mFromSector;
  Int_t                 mFromRow;
};
// Non-member
ostream& operator<<(ostream&, const StTpcCoordinate&);
#endif

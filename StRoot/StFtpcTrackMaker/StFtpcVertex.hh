// $Id: StFtpcVertex.hh,v 1.3 2000/05/15 14:28:16 oldi Exp $
// $Log: StFtpcVertex.hh,v $
// Revision 1.3  2000/05/15 14:28:16  oldi
// problem of preVertex solved: if no main vertex is found (z = NaN) StFtpcTrackMaker stops with kStWarn,
// refitting procedure completed and included in StFtpcTrackMaker (commented),
// new constructor of StFtpcVertex due to refitting procedure,
// minor cosmetic changes
//
// Revision 1.2  2000/05/11 15:14:54  oldi
// Changed class names *Hit.* due to already existing class StFtpcHit.cxx in StEvent
//
// Revision 1.1  2000/05/10 13:39:36  oldi
// Initial version of StFtpcTrackMaker
//

/////////////////////////////////////////////////////////////////
//                                                             //
// StFtpcVertex class - representation of the main vertex one  //
//                                                             //
/////////////////////////////////////////////////////////////////

#ifndef STAR_StFtpcVertex
#define STAR_StFtpcVertex

#include "TObject.h"
#include "StThreeVector.hh"
#include "St_DataSet.h"

class fcl_fppoint_st;

class StFtpcVertex : public TObject {

private:

  StThreeVector<double> mCoord;
  StThreeVector<double> mError;

public:

  StFtpcVertex();                                                 // default constructor
  StFtpcVertex(fcl_fppoint_st *thisFppoint, Int_t numFppoints);   // constructor from points        
  StFtpcVertex(St_DataSet *const geant);                          // constructor from geant
  StFtpcVertex(Double_t pos[3]);                                  // constructor from array of doubles
  StFtpcVertex(Double_t x, Double_t y, Double_t z);               // constructor from doubles
  virtual  ~StFtpcVertex();                                       // destructor

  // getter
  Double_t GetX()    const { return mCoord.x(); }
  Double_t GetY()    const { return mCoord.y(); }
  Double_t GetZ()    const { return mCoord.z(); }
  Double_t GetXerr() const { return mError.x(); }
  Double_t GetYerr() const { return mError.y(); }
  Double_t GetZerr() const { return mError.z(); }
  StThreeVector<double> GetCoord() const {return mCoord;}
  StThreeVector<double> GetError() const {return mError;}

  // setter
  void SetX(Double_t f)    { mCoord.setX(f); }
  void SetY(Double_t f)    { mCoord.setY(f); }
  void SetZ(Double_t f)    { mCoord.setZ(f); }
  void SetXerr(Double_t f) { mError.setX(f); }
  void SetYerr(Double_t f) { mError.setY(f); }
  void SetZerr(Double_t f) { mError.setZ(f); }
  
  ClassDef(StFtpcVertex, 1)   //Ftpc vertex class
};

#endif
  

// $Id: StFtpcVertex.hh,v 1.1 2000/05/10 13:39:36 oldi Exp $
// $Log: StFtpcVertex.hh,v $
// Revision 1.1  2000/05/10 13:39:36  oldi
// Initial version of StFtpcTrackMaker
//

/////////////////////////////////////////////////////////////////////////////////
//                                                                             //
// StFtpcHit class - representation of one FTPC cluster for the FTPC trackers. //
//                                                                             //
/////////////////////////////////////////////////////////////////////////////////

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

  StFtpcVertex();   // default constructor
  StFtpcVertex(fcl_fppoint_st *thisFppoint, Int_t numFppoints); // constructor from points        
  StFtpcVertex(St_DataSet *const geant);  // constructor from geant
  StFtpcVertex(Double_t pos[3]);          // constructor from Doubles
  virtual  ~StFtpcVertex();

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
  

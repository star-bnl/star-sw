// $Id: StFtpcVertex.hh,v 1.6 2001/01/25 15:22:43 oldi Exp $
// $Log: StFtpcVertex.hh,v $
// Revision 1.6  2001/01/25 15:22:43  oldi
// Review of the complete code.
// Fix of several bugs which caused memory leaks:
//  - Tracks were not allocated properly.
//  - Tracks (especially split tracks) were not deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way). I changed all occurences to TObjArray which makes the
//    program slightly slower but much more save (in terms of memory usage).
// Speed up of HandleSplitTracks() which is now 12.5 times faster than before.
// Cleanup.
//
// Revision 1.5  2000/11/10 18:39:25  oldi
// Changes due to replacement of StThreeVector by TVector3.
// New constructor added to find the main vertex with given point array.
//
// Revision 1.4  2000/07/18 21:22:17  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
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
#include "TObjArray.h"
#include "TVector3.h"
#include "St_DataSet.h"

class fcl_fppoint_st;

class StFtpcVertex : public TObject {

private:

  TVector3 mCoord;  // coordinates of vertex
  TVector3 mError;  // errors on coordinates

public:

  StFtpcVertex();                                                 // default constructor
  StFtpcVertex(fcl_fppoint_st *thisFppoint, Int_t numFppoints);   // constructor from points        
  StFtpcVertex(TObjArray *hits);                                  // constructor from point array   
  StFtpcVertex(St_DataSet *const geant);                          // constructor from geant
  StFtpcVertex(Double_t pos[3]);                                  // constructor from array of doubles
  StFtpcVertex(Double_t x, Double_t y, Double_t z);               // constructor from doubles
  virtual  ~StFtpcVertex();                                       // destructor

  // getter
  Double_t GetX()     const { return mCoord.X(); }
  Double_t GetY()     const { return mCoord.Y(); }
  Double_t GetZ()     const { return mCoord.Z(); }
  Double_t GetXerr()  const { return mError.X(); }
  Double_t GetYerr()  const { return mError.Y(); }
  Double_t GetZerr()  const { return mError.Z(); }
  TVector3 GetCoord() const { return mCoord;}
  TVector3 GetError() const { return mError;}

  // setter
  void SetX(Double_t f)    { mCoord.SetX(f); }
  void SetY(Double_t f)    { mCoord.SetY(f); }
  void SetZ(Double_t f)    { mCoord.SetZ(f); }
  void SetXerr(Double_t f) { mError.SetX(f); }
  void SetYerr(Double_t f) { mError.SetY(f); }
  void SetZerr(Double_t f) { mError.SetZ(f); }
  
  ClassDef(StFtpcVertex, 1)   //Ftpc vertex class
};

#endif
  

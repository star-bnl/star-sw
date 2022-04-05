// $Id: StFtpcVertex.hh,v 1.16 2015/07/20 17:21:31 jeromel Exp $
// $Log: StFtpcVertex.hh,v $
// Revision 1.16  2015/07/20 17:21:31  jeromel
// Use std::isnan instead for C++11
//
// Revision 1.15  2009/11/23 16:38:11  fisyak
// Remove dependence on dst_vertex_st
//
// Revision 1.14  2004/04/06 18:59:21  oldi
// New constructor which takes input data from StVertex added.
//
// Revision 1.13  2004/02/12 19:37:11  oldi
// *** empty log message ***
//
// Revision 1.12  2003/09/02 17:58:17  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.11  2002/11/06 13:48:06  oldi
// IFlag and Id added as data members.
// New functionality introduced (to clean up StFtpcTrackMaker.cxx).
//
// Revision 1.10  2002/06/04 13:43:54  oldi
// Minor change: 'west' -> 'hemisphere' (just a naming convention)
//
// Revision 1.9  2002/04/05 16:51:20  oldi
// Cleanup of MomentumFit (StFtpcMomentumFit is now part of StFtpcTrack).
// Each Track inherits from StHelix, now.
// Therefore it is possible to calculate, now:
//  - residuals
//  - vertex estimations obtained by back extrapolations of FTPC tracks
// Chi2 was fixed.
// Many additional minor (and major) changes.
//
// Revision 1.8  2002/03/15 10:04:41  oldi
// Adjust eta segments not only to z-position of vertex but to x,y as well.
// Avoid tracking if vertex position is outside of the inner radius of the Ftpc.
//
// Revision 1.7  2001/07/12 13:05:03  oldi
// QA histogram of FTPC vertex estimation is generated.
// FTPC vertex estimation is stored as pre vertex (id = 301) in any case, now.
//
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

#include <Stiostream.h>
#include "TObject.h"
#include "TObjArray.h"
#include "TH1.h"
#include "TMath.h"
#include "TVector3.h"
#include "St_DataSet.h"

class StVertex;

class StFtpcVertex : public TObject {

private:

  TVector3 mCoord;  // coordinates of vertex
  TVector3 mError;  // errors on coordinates
     Int_t mIFlag;  // flag to indicate vertex state
     Int_t mId;     // vertex identification number

public:

  StFtpcVertex();                                                                    // default constructor
  StFtpcVertex(TObjArray *hits, TH1F *vtx_pos = 0);                                  // constructor from point array   
  StFtpcVertex(St_DataSet *const geant);                                             // constructor from geant
  StFtpcVertex(StVertex *vertex);                                               // constructor from StPrimaryVertex
  StFtpcVertex(TObjArray *tracks, StFtpcVertex *vertex, Char_t hemisphere);          // constructor form track array
  StFtpcVertex(Double_t pos[6], Int_t iFlag = 0, Int_t id = 0);                      // constructor from array of doubles
  StFtpcVertex(Double_t pos[3], Double_t err[3],
	       Int_t iFlag = 0, Int_t id = 0);                                       // constructor from arrays of doubles (with errors)
  StFtpcVertex(Double_t x,     Double_t y,     Double_t z, 
	       Double_t x_err, Double_t y_err, Double_t z_err,
	       Int_t iFlag = 0, Int_t id = 0);                                       // constructor from doubles with errors
  StFtpcVertex(const StFtpcVertex &vertex);                                          // Copy constructor
  virtual  ~StFtpcVertex();                                                          // destructor

  // getter
  Double_t GetX()       const { return mCoord.X(); }
  Double_t GetY()       const { return mCoord.Y(); }
  Double_t GetZ()       const { return mCoord.Z(); }
  Double_t GetAbsZ()    const { return TMath::Abs(mCoord.Z()); }
  Double_t GetXerr()    const { return mError.X(); }
  Double_t GetYerr()    const { return mError.Y(); }
  Double_t GetZerr()    const { return mError.Z(); }
  Double_t GetRadius2() const { return TMath::Sqrt(GetX()*GetX() + GetY()*GetY()); }
  Double_t GetRadius3() const { return mCoord.Mag(); }
  TVector3 GetCoord()   const { return mCoord; }
  TVector3 GetError()   const { return mError; }
     Int_t GetIFlag()   const { return mIFlag; }
     Int_t GetId()      const { return mId;    }

  // setter
  void SetX(Double_t f)    { mCoord.SetX(f); }
  void SetY(Double_t f)    { mCoord.SetY(f); }
  void SetZ(Double_t f)    { mCoord.SetZ(f); }
  void SetXerr(Double_t f) { mError.SetX(f); }
  void SetYerr(Double_t f) { mError.SetY(f); }
  void SetZerr(Double_t f) { mError.SetZ(f); }
  void SetIFlag(Int_t f)   { mIFlag = f;     }
  void SetId(Int_t f)      { mId = f;        }
  
  void CheckXerr()  { if ( std::isnan( mError.x()) ) mError.SetX(0.); }
  void CheckYerr()  { if ( std::isnan( mError.y()) ) mError.SetY(0.); }
  void CheckZerr()  { if ( std::isnan( mError.z()) ) mError.SetZ(0.); }
  void CheckErr()   { CheckXerr(); CheckYerr(); CheckZerr();  }
  Bool_t CoordIsNan() { return (Bool_t)( std::isnan(GetX()) || std::isnan(GetY()) || std::isnan(GetZ())); }
   Int_t CheckVertex();

  StFtpcVertex& operator=(const StFtpcVertex &vertex);    // Assignment operator

  ClassDef(StFtpcVertex, 1)   //Ftpc vertex class
};

ostream& operator<< (ostream& s, const StFtpcVertex &vertex);  // cout

#endif
  

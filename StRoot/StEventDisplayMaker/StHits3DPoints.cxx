//*-- Author :    Valery Fine   18/05/99  (E-mail: fine@bnl.gov)
// $Id: StHits3DPoints.cxx,v 1.2 2000/07/03 02:07:48 perev Exp $
// $Log: StHits3DPoints.cxx,v $
// Revision 1.2  2000/07/03 02:07:48  perev
// StEvent: vector<TObject*>
//
// Revision 1.1  1999/11/02 01:49:52  fine
// The primitives moved from StEvent
//
// Revision 1.5  1999/07/14 15:25:28  fine
// GetLastPosition +- 1 bug fixed
//
// Revision 1.4  1999/07/14 01:38:04  fine
// Previous version has been restored
//
// Revision 1.3  1999/07/13 23:32:23  fine
// Methods GetXYZ has been removed since it is defined by base class
//
// Revision 1.2  1999/05/22 18:59:32  fine
// New class to draw StHelix3D and StTrack has been introduced
//
// Revision 1.1  1999/05/19 22:46:38  fine
//  New class to 3D drawing operation for StHit commections have been introduced
//

#include "StHits3DPoints.h"
#include "StArray.h"
#include "StHit.h"
#include "StThreeVectorF.hh"
#include <TMath.h>

ClassImp(StHits3DPoints)

//________________________________________________________________________________
StHits3DPoints::StHits3DPoints(StObjArray *hitCollection)
{
   m_HitCollection = hitCollection;
}
//________________________________________________________________________________
StHits3DPoints::~StHits3DPoints(){;}
 
//________________________________________________________________________________
Int_t  StHits3DPoints::GetLastPosition()const {
  return m_HitCollection?m_HitCollection->size()-1:-1;
}

//________________________________________________________________________________
Float_t StHits3DPoints::GetAnyPoint(Int_t idx,Int_t iAxis)  const  
{
  // returns the value (x,y,z) of the selected component of ThreeVectorF object
  Float_t point = 0;
#ifdef STEVENT
  StHit *hitObj = (StHit *)m_HitCollection->At(idx);
  if (hitObj) {
    const StThreeVectorF &hitVector = hitObj->position();
    point =  hitVector[iAxis];
  }
#endif
  return point;
}
//________________________________________________________________________________
Option_t *StHits3DPoints::GetOption()      const {return "";}
//________________________________________________________________________________
void StHits3DPoints::PaintPoints(Int_t , Float_t *,Option_t *){}
//________________________________________________________________________________
Int_t StHits3DPoints::SetLastPosition(Int_t ){return 0;}
//________________________________________________________________________________
void StHits3DPoints::SetOption(Option_t *){;}
//________________________________________________________________________________
Int_t StHits3DPoints::SetPoint(Int_t , Float_t, Float_t , Float_t ){return 0;}
//________________________________________________________________________________
Int_t StHits3DPoints::SetPoints(Int_t , Float_t *, Option_t *) {return 0;}
//______________________________________________________________________________
Float_t *StHits3DPoints::GetXYZ(Float_t *xyz,Int_t idx,Int_t num)  const
{
  //
  // GetXYZ(Float_t *xyz,Int_t idx,Int_t num=1) fills the buffer supplied
  // by the calling code with the points information.
  //
  //  Input parameters:
  //  ----------------
  //   Float_t *xyz - an external user supplied floating point array.
  //   Int_t    num - the total number of the points to be copied
  //                  the dimension of that array the size of the
  //                  array is num*sizeof(Float_t) at least
  //   Int_t    idx - The index of the first copy to be taken.
  //
  //  Return: The pointer to the buffer array supplied
  //  ------

  if (xyz) {
    Int_t size = TMath::Min(idx+num,Size());
    Int_t j=0;
    Int_t i = 0;
    for (i=idx;i<size;i++) {
      xyz[j++] = GetX(i); 
      xyz[j++] = GetY(i); 
      xyz[j++] = GetZ(i);
    }
  }
  return xyz;
}

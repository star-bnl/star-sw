//*-- Author :    Valery Fine   18/05/99  (E-mail: fine@bnl.gov)
// $Id: StHits3DPoints.cxx,v 1.3 1999/07/13 23:32:23 fine Exp $
// $Log: StHits3DPoints.cxx,v $
// Revision 1.3  1999/07/13 23:32:23  fine
// Methods GetXYZ has been removed since it is defined by base class
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
  return m_HitCollection?m_HitCollection->GetLast()+1:-1;
//________________________________________________________________________________
Int_t  StHits3DPoints::GetLastPosition()const {
  return m_HitCollection?m_HitCollection->GetLast():-1;
}

//________________________________________________________________________________
Float_t StHits3DPoints::GetAnyPoint(Int_t idx,Int_t iAxis)  const  
{
  // returns the value (x,y,z) of the selected component of ThreeVectorF object
  StHit *hitObj = (StHit *)m_HitCollection->At(idx);
  Float_t point = 0;
  if (hitObj) {
    const StThreeVectorF &hitVector = hitObj->position();
    point =  hitVector[iAxis];
  }
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
  //
  }
  return xyz;
}

//*-- Author :    Valery Fine   03/08/99  (E-mail: fine@bnl.gov)
// $Id: StVertices3DPoints.cxx,v 1.2 2000/07/03 02:07:48 perev Exp $
// $Log: StVertices3DPoints.cxx,v $
// Revision 1.2  2000/07/03 02:07:48  perev
// StEvent: vector<TObject*>
//
// Revision 1.1  1999/11/02 01:49:52  fine
// The primitives moved from StEvent
//
// Revision 1.1  1999/08/03 18:52:16  fine
// New class StVertices3DPoints to draw StVetex collections has been introduced
//

#include "StVertices3DPoints.h"
#include "StArray.h"
#include "StVertex.h"
#include "StThreeVectorF.hh"
#include <TMath.h>

ClassImp(StVertices3DPoints)

//________________________________________________________________________________
StVertices3DPoints::StVertices3DPoints(StObjArray *vertexCollection)
{
   m_VertexCollection = vertexCollection;
}
//________________________________________________________________________________
StVertices3DPoints::~StVertices3DPoints(){;}
 
//________________________________________________________________________________
Int_t  StVertices3DPoints::GetLastPosition()const {
  return m_VertexCollection?m_VertexCollection->size()-1:-1;
}

//________________________________________________________________________________
Float_t StVertices3DPoints::GetAnyPoint(Int_t idx,Int_t iAxis)  const  
{
  // returns the value (x,y,z) of the selected component of ThreeVectorF object
  Float_t point = 0;
#ifdef STEVENT
  StVertex *vertexObj = (StVertex *)m_VertexCollection->At(idx);
  if (vertexObj) {
    const StThreeVectorF &vertexVector = vertexObj->position();
    point =  vertexVector[iAxis];
  }
#endif
  return point;
}
//________________________________________________________________________________
Option_t *StVertices3DPoints::GetOption()      const {return "";}
//________________________________________________________________________________
void StVertices3DPoints::PaintPoints(Int_t , Float_t *,Option_t *){}
//________________________________________________________________________________
Int_t StVertices3DPoints::SetLastPosition(Int_t ){return 0;}
//________________________________________________________________________________
void StVertices3DPoints::SetOption(Option_t *){;}
//________________________________________________________________________________
Int_t StVertices3DPoints::SetPoint(Int_t , Float_t, Float_t , Float_t ){return 0;}
//________________________________________________________________________________
Int_t StVertices3DPoints::SetPoints(Int_t , Float_t *, Option_t *) {return 0;}
//______________________________________________________________________________
Float_t *StVertices3DPoints::GetXYZ(Float_t *xyz,Int_t idx,Int_t num)  const
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

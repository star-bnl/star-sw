//*-- Author :    Valery Fine   21/05/99  (E-mail: fine@bnl.gov)
// $Id: StHelix3DPoints.cxx,v 1.4 1999/08/04 03:51:49 fine Exp $
// $Log: StHelix3DPoints.cxx,v $
// Revision 1.4  1999/08/04 03:51:49  fine
// Helix drawing improvements
//
// Revision 1.4  1999/08/04 03:51:49  fine
// Helix drawing improvements
//
// Revision 1.3  1999/07/14 01:38:03  fine
// Previous version has been restored
//
// Revision 1.2  1999/05/31 18:37:44  fine
// 3D graphics interface clean up
//
// Revision 1.1  1999/05/22 18:59:30  fine
// New class to draw StHelix3D and StTrack has been introduced
//
//

#include "StHelix3DPoints.h"
#include "StHelixD.hh"
#include "StTrack.h"

#include <TMath.h>

StHelix3DPoints hh;

ClassImp(StHelix3DPoints)

//________________________________________________________________________________
StHelix3DPoints::StHelix3DPoints(StTrack *track)
{
  m_Helix = 0;
  if (!track) return;
  m_Helix = &(track->helix());
  if (m_Helix) 
  {
    Float_t length = track->length();
    Int_t nSteps = 4*length*m_Helix->curvature() + 1; 
    SetLastPosition( nSteps);
    SetStep(length/GetLastPosition());
  }
  else 
  {
    SetLastPosition(-1);
    SetStep(0);
  } 
  
}

//________________________________________________________________________________
StHelix3DPoints::StHelix3DPoints(StHelixD *trackHelix,Float_t step,Int_t lastPosition)
{
   m_Helix = trackHelix;
   if (m_Helix) {
     SetLastPosition(lastPosition);
     SetStep(step);
   }
   else {
     SetLastPosition(-1);
     SetStep(0);
   }
}
//________________________________________________________________________________
StHelix3DPoints::StHelix3DPoints(StTrack *track, Float_t length,Int_t lastPosition)
{
  m_Helix = 0;
  if (!track) return; 
  m_Helix = &(track->helix());
  if (m_Helix) 
  {
    if (lastPosition) SetLastPosition(lastPosition);
    else              SetLastPosition(1);
    SetStep(length/GetLastPosition());
  }
  else 
  {
    SetLastPosition(-1);
    SetStep(0);
  }
}

//________________________________________________________________________________
StHelix3DPoints::~StHelix3DPoints(){;}
 
//________________________________________________________________________________
Int_t  StHelix3DPoints::GetLastPosition()const {
  return Size()-1;
}

//________________________________________________________________________________
Float_t StHelix3DPoints::GetAnyPoint(Int_t idx,Int_t iAxis)  const  
{
  // returns the value (x,y,z) of the selected component of StHelixD object
  Float_t point = 0;
  if (m_Helix) { 
    if (idx >= 0) {
      Float_t segment = idx*GetStep();
      switch (iAxis) {
        case 0:
          point = m_Helix->x(segment); 
  	  break;
        case 1:
          point = m_Helix->y(segment); 
	  break;
        case 2:
          point = m_Helix->z(segment); 
	  break;
        default:
          Error("GetAnyPoint","Wrong dimension %d",iAxis);
          point = -99999999.0;
          break;
      }
    }
  }
  return point;
}
//________________________________________________________________________________
Option_t *StHelix3DPoints::GetOption()      const {return "";}
//________________________________________________________________________________
void StHelix3DPoints::PaintPoints(Int_t , Float_t *,Option_t *){}
//________________________________________________________________________________
Int_t StHelix3DPoints::SetLastPosition(Int_t lastPosition)
{
  Int_t previousPosition = GetLastPosition();
  m_N = lastPosition+1;  // compute the size
  return previousPosition;
}
//________________________________________________________________________________
void StHelix3DPoints::SetOption(Option_t *){;}
//________________________________________________________________________________
Int_t StHelix3DPoints::SetPoint(Int_t , Float_t, Float_t , Float_t ){return 0;}
//________________________________________________________________________________
Int_t StHelix3DPoints::SetPoints(Int_t , Float_t *, Option_t *) {return 0;}

//______________________________________________________________________________
Float_t *StHelix3DPoints::GetXYZ(Float_t *xyz,Int_t idx,Int_t num)  const
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

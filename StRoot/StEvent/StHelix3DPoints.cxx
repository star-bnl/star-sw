//*-- Author :    Valery Fine   21/05/99  (E-mail: fine@bnl.gov)
// $Id: StHelix3DPoints.cxx,v 1.2 1999/05/31 18:37:44 fine Exp $
// $Log: StHelix3DPoints.cxx,v $
// Revision 1.2  1999/05/31 18:37:44  fine
// 3D graphics interface clean up
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
{
  m_Helix = 0;
  if (!track) return; 
    SetLastPosition(lastPosition);
    SetStep(length/Size());
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
  return m_N;
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
  m_N = lastPosition+1;
{
  Int_t previousPosition = GetLastPosition();
  m_N = lastPosition+1;  // compute the size
  return previousPosition;
}
//________________________________________________________________________________
void StHelix3DPoints::SetOption(Option_t *){;}
//________________________________________________________________________________
 
//________________________________________________________________________________
Float_t *StHelix3DPoints::GetXYZ(Float_t *xyz,Int_t idx, Int_t num)const

  //  Return: The pointer to the buffer array supplied
  //  ------

    for (Int_t i=idx;i<size;i++) {
      xyz[j++] = GetX(i);
      xyz[j++] = GetY(i);
    for (i=idx;i<size;i++) {
      xyz[j++] = GetX(i); 
      xyz[j++] = GetY(i); 
      xyz[j++] = GetZ(i);
    }
  }
  return xyz;
}

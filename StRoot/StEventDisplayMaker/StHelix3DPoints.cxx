//*-- Author :    Valery Fine   21/05/99  (E-mail: fine@bnl.gov)
// $Id: StHelix3DPoints.cxx,v 1.2 1999/11/16 14:41:03 fine Exp $
// $Log: StHelix3DPoints.cxx,v $
// Revision 1.2  1999/11/16 14:41:03  fine
// TObject::Distancetoprimitive implementation, GetXYZ has been removed
//
// Revision 1.1  1999/11/02 01:49:52  fine
// The primitives moved from StEvent
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
StHelix3DPoints::StHelix3DPoints(StTrack *track) : m_Owner (kFALSE) 
{
  m_Helix = 0;
  if (!track) return;
#ifdef STEVENT
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
#endif  
}

//________________________________________________________________________________
StHelix3DPoints::StHelix3DPoints(StHelixD *trackHelix,Float_t step,Int_t lastPosition,Bool_t own)
 : m_Owner (own) {
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
 : m_Owner (kFALSE) {
  m_Helix = 0;
  if (!track) return; 
#ifdef STEVENT
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
#endif
}

//________________________________________________________________________________
StHelix3DPoints::~StHelix3DPoints(){ if (m_Owner) delete m_Helix; }
 


//______________________________________________________________________________
Int_t StHelix3DPoints::DistancetoPrimitive(Int_t px, Int_t py)
{  assert(kFALSE); return -1; }

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

// $Id: MIntArray.cc,v 1.5 2003/09/16 15:27:01 jcs Exp $
// $Log: MIntArray.cc,v $
// Revision 1.5  2003/09/16 15:27:01  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.4  2001/07/12 08:24:11  oldi
// New function CountAppearance() introduced.
//
// Revision 1.3  2000/11/10 18:32:36  oldi
// Introduced new function ShiftByOneAndAddAtFirst(Int_t value).
// Cleanup.
//
// Revision 1.2  2000/07/18 21:22:14  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.1  2000/05/10 13:39:00  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 18.10.2000
//----------Copyright:     &copy MDO Production 2000

#include "MIntArray.h"

////////////////////////////////////////////////////////////////////////////////////
//                                                                                //
// MIntArray class - this class extends the possibilities of the usual TArrayI    //
//                                                                                //
////////////////////////////////////////////////////////////////////////////////////

ClassImp(MIntArray)

MIntArray::MIntArray()
{
  // Constructer.
  // Has nothing to do.

  fN = 0;
  Set(0);
}


MIntArray::~MIntArray()
{
  // Destructor.
  // Has nothing to do.
}


void MIntArray::ShiftByOneAndAddAtFirst(Int_t value)
{
  // Shifts every array element in the next slot and adds the value in the first slot.
  
  Set(GetSize() + 1);
  
  {for (Int_t i = GetSize() - 2; i >= 0; i--) {
    AddAt(At(i), i+1);
  }}
  
  AddAt(value, 0);
  
  return;
}


Int_t MIntArray::CountAppearance(Int_t value)
{
  // Loops over array and counts appearances of 'value'.

  Int_t result = 0;

  {for (Int_t i = 0; i < GetSize(); i++) {
   
    if (At(i) == value) {
      result++;
    }
  }}

  return result;
}

void MIntArray::AddLast(Int_t value) 
{
  // Adds the value after the last entry of the array.
  // Therefore the array size has to be increased by one.

  Set(GetSize() + 1);
  AddAt(value, GetSize() - 1);
}


Int_t MIntArray::AtLast()
{
  // Returns the value of the last array element.

  return At(GetSize()-1);
}


Int_t MIntArray::AtFirst()
{
  // Returns the value of the last array element.

  return At(0);
}


void MIntArray::Fill(Int_t value)
{
  // Fills the whole array with the value 'value'.

  for (Int_t i = 0; i < GetSize(); AddAt(value, i), i++);

  return;
}


void MIntArray::SetFill(Int_t size, Int_t value)
{
  // Set the array size to 'size' and fill the whole array with 'value'.

  if (GetSize() > 0) {
    Set(0);
  }
  
  for (Int_t i = 0; i < size; AddLast(value), i++);

  return;
}


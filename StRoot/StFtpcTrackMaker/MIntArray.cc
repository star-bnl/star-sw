// $Id: MIntArray.cc,v 1.1 2000/05/10 13:39:00 oldi Exp $
// $Log: MIntArray.cc,v $
// Revision 1.1  2000/05/10 13:39:00  oldi
// Initial version of StFtpcTrackMaker
//

//----------Author:        Markus D. Oldenburg
//----------Last Modified: 10.05.2000
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

  return this->At(GetSize()-1);
}


Int_t MIntArray::AtFirst()
{
  // Returns the value of the last array element.

  return this->At(0);
}


void MIntArray::Fill(Int_t value)
{
  // Fills the whole array with the value 'value'.

  for (Int_t i = 0; i < GetSize(); i++) {
    this->AddAt(value, i);
  }
  
  return;
}


void MIntArray::SetFill(Int_t size, Int_t value)
{
  // Set the array size to 'size' and fill the whole array with 'value'.

  if (GetSize() > 0) {
    Set(0);
  }
  
  for (Int_t i = 0; i < size; i++) {
    AddLast(value);
  }
}

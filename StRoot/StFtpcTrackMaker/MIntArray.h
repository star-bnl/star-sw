// $Id: MIntArray.h,v 1.2 2000/07/18 21:22:15 oldi Exp $
// $Log: MIntArray.h,v $
// Revision 1.2  2000/07/18 21:22:15  oldi
// Changes due to be able to find laser tracks.
// Cleanup: - new functions in StFtpcConfMapper, StFtpcTrack, and StFtpcPoint
//            to bundle often called functions
//          - short functions inlined
//          - formulas of StFormulary made static
//          - avoid streaming of objects of unknown size
//            (removes the bunch of CINT warnings during compile time)
//          - two or three minor bugs cured
//
// Revision 1.1  2000/05/10 13:39:02  oldi
// Initial version of StFtpcTrackMaker
//

////////////////////////////////////////////////////////////////////////////////////
//                                                                                //
// MIntArray class - this class extends the possibilities of the usual TArrayI    //
//                                                                                //
////////////////////////////////////////////////////////////////////////////////////

#ifndef ROOT_MIntArray
#define ROOT_MIntArray

#include "TArrayI.h"
#include "TObject.h"


class MIntArray : public TArrayI, public TObject {

public:

           MIntArray();
          ~MIntArray();
    void   AddLast(Int_t value);
   Int_t   AtFirst();
   Int_t   AtLast();
    void   Fill(Int_t value);
    void   SetFill(Int_t size, Int_t value);

  ClassDef(MIntArray, 1)    // Class of an array of integers
};


inline void MIntArray::AddLast(Int_t value) 
{
  // Adds the value after the last entry of the array.
  // Therefore the array size has to be increased by one.

  Set(GetSize() + 1);
  AddAt(value, GetSize() - 1);
}


inline Int_t MIntArray::AtLast()
{
  // Returns the value of the last array element.

  return this->At(GetSize()-1);
}


inline Int_t MIntArray::AtFirst()
{
  // Returns the value of the last array element.

  return this->At(0);
}


inline void MIntArray::Fill(Int_t value)
{
  // Fills the whole array with the value 'value'.

  for (Int_t i = 0; i < GetSize(); this->AddAt(value, i), i++);

  return;
}


inline void MIntArray::SetFill(Int_t size, Int_t value)
{
  // Set the array size to 'size' and fill the whole array with 'value'.

  if (GetSize() > 0) {
    Set(0);
  }
  
  for (Int_t i = 0; i < size; AddLast(value), i++);

  return;
}


#endif

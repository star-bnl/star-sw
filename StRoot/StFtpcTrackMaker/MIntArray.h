// $Id: MIntArray.h,v 1.5 2003/09/16 15:27:01 jcs Exp $
// $Log: MIntArray.h,v $
// Revision 1.5  2003/09/16 15:27:01  jcs
// removed inline as it would leave a few undefined reference
//
// Revision 1.4  2001/07/12 08:24:12  oldi
// New function CountAppearance() introduced.
//
// Revision 1.3  2000/11/10 18:32:37  oldi
// Introduced new function ShiftByOneAndAddAtFirst(Int_t value).
// Cleanup.
//
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
    void   ShiftByOneAndAddAtFirst(Int_t value);
   Int_t   CountAppearance(Int_t value);

  ClassDef(MIntArray, 1)    // Class of an array of integers
};




#endif

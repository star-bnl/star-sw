// $Id: MIntArray.h,v 1.1 2000/05/10 13:39:02 oldi Exp $
// $Log: MIntArray.h,v $
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

#endif

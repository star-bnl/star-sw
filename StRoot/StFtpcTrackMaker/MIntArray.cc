// $Id: MIntArray.cc,v 1.2 2000/07/18 21:22:14 oldi Exp $
// $Log: MIntArray.cc,v $
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
//----------Last Modified: 18.07.2000
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

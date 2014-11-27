// @(#)root/gui:$Id: TRedirectGuardInterface.h,v 1.2 2013/08/30 16:00:22 perev Exp $
// Author: G. Ganis   10/10/2005
// Adopted for Qt: V. Fine   13/05/2010

/*************************************************************************
 * Copyright (C) 1995-2005, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TRedirectGuardInterface
#define ROOT_TRedirectGuardInterface

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TRedirectGuardInterface                                              //
//                                                                      //
// This class provides output redirection to a TGTextView in guaranteed //
// exception safe way. Use like this:                                   //
// {                                                                    //
//    TRedirectGuardInterface guard(textview);                          //
//    ... // do something                                               //
//    guard.Update();                                                   //
//    ... // do something else                                          //
// }                                                                    //
// when guard goes out of scope, Update() is called to flush what left  //
// on the screed and the output is automatically redirected again to    //
// the standard units.                                                  //
// The exception mechanism takes care of calling the dtors              //
// of local objects so it is exception safe.                            //
// Optionally the output can also be saved into a file:                 //
// {                                                                    //
//    TRedirectGuardInterface guard(textview, file, mode);              //
//    ... // do something                                               //
// }                                                                    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TString
#include "TString.h"
#endif

class TRedirectGuardInterface {

private:
   TString      fLogFile;
   Bool_t       fTmpFile;
   FILE        *fLogFileRead;

public:
   TRedirectGuardInterface(const char *flog = 0, const char *mode = "a");
   virtual ~TRedirectGuardInterface();

   void Update(); // Update window with file content
   virtual void AddLine(const TString &) = 0;

   // lassDef(TRedirectGuardInterface,0)  // Exception safe output redirection
};

#endif

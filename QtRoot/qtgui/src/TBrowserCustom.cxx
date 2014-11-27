// @(#)root/base:$Name:  $:$Id: TBrowserCustom.cxx,v 1.4 2013/08/30 16:00:23 perev Exp $
// Author: Valeri Fine  10/01/2004

/****************************************************************************
** $Id: TBrowserCustom.cxx,v 1.4 2013/08/30 16:00:23 perev Exp $
**
** Copyright (C) 2004 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// Using a TBrowser one can browse all ROOT objects. It shows in a list //
// on the left side of the window all browsable ROOT classes. Selecting //
// one of the classes displays, in the iconbox on the right side, all   //
// objects in the class. Selecting one of the objects in the iconbox,   //
// will place all browsable objects in a new list and draws the         //
// contents of the selected class in the iconbox. And so on....         //
//                                                                      //
//Begin_Html <img src="gif/browser.gif"> End_Html                       //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <assert.h>
#include "TBrowserCustom.h"
#include "TClass.h"

ClassImp(TBrowserCustom)

//______________________________________________________________________________
TBrowserCustom::TBrowserCustom(TBrowserImp *imp) : TBrowser()
{
   // This class  is a "fake" browser with the external implemantion.
   //  to foll the base class ctor we have to call it via TClass::New only :-)

   if ( !TClass::IsCallingNew() ) {
//      fprintf(stderr," ** Error ** TBrowserCustom::TBrowserCustom has to be called via TClass::New ONLY %s %s %s!!!\n", __FILE__, __FUNCTION__, __LINE__);
      fprintf(stderr," ** Error ** TBrowserCustom::TBrowserCustom has to be called via TClass::New ONLY !!!\n");
      assert(0);
   }
   fImp = imp;
}
//______________________________________________________________________________
void TBrowserCustom::SetBrowserImp(TBrowserImp *imp)
{ fImp=imp;}

//______________________________________________________________________________
TBrowserCustom::~TBrowserCustom()
{
   // Delete the browser.

   // We don't own fImp !!!
   // so we have to szero pointer to foolinsh
   // the base class dtor
   fImp=0;
}

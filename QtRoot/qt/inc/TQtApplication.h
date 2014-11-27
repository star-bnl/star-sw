// @(#)root/qt:$Name:  $:$Id: TQtApplication.h,v 1.6 2013/08/30 15:59:48 perev Exp $
// Author: Valeri Fine   21/01/2002
/****************************************************************************
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtApplication
#define ROOT_TQtApplication

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtApplication -  Instantiate the Qt system within ROOT environment  //
//                                                                      //
// Instantiate the Qt package by creating Qapplication object if any   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TQtRConfig.h"
#include "Rtypes.h"

class TQtApplicationThread;

class TQtApplication { // : public TApplicationImp
  
private:
  friend class TQtApplicationThread;
  TQtApplicationThread  *fGUIThread;

  void    CreateGUIThread(int &argc, char **argv);

  static void CreateQApplication(int &argc, char ** argv, bool GUIenabled);

  void operator=(const TQtApplication&);
  TQtApplication(const TQtApplication&);

protected:
   static TQtApplication *fgQtApplication;

public:

   TQtApplication() : fGUIThread() {};
   TQtApplication(const char *appClassName, int &argc, char **argv);
   virtual ~TQtApplication();
   static bool Terminate();

   static TQtApplication *GetQtApplication();
   static bool IsThisGuiThread();
   static Int_t QtVersion();
   ClassDef(TQtApplication,0) // Instantiate the Qt system within ROOT environment

};
#endif

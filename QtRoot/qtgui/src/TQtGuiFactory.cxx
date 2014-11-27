// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtGuiFactory.cxx,v 1.5 2013/08/30 16:00:24 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

//________________________________________________________________________
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtGUIFactory                                                        //
//                                                                      //
// This class is a factory for Qt GUI components. It overrides       //
// the member functions of the ABS TGuiFactory.                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


#include "TQtGuiFactory.h"

# include "TQtCanvasImp.h"
# include "TQtInspectImp.h"
# include "TQtContextMenuImp.h"
# include "TQtRootBrowserImp.h"
# include "TQtControlBarImp.h"

# include "TQtClientFilter.h"

#include <QApplication>
#include <QDebug>
#include "TApplication.h"
#include "TQtApplication.h"

#include "TSystem.h"
#include "TClass.h"
#include "TBrowser.h"
#ifdef WIN32OLD
# include "TWin32Application.h" 
#else
# include "TROOT.h"
# include "TQtRootApplication.h"
#endif

#include "TGClient.h"
#include "TGQt.h"

ClassImp(TQtGUIFactory)

//______________________________________________________________________________
TQtGUIFactory::TQtGUIFactory(): TGuiFactory()
{
   // TQtGUIFactory ctor.
   // Restore the right TVirtulaX pointer
   if (TGQt::GetVirtualX())  gVirtualX = TGQt::GetVirtualX();
   qDebug() << "TQtGuiFactory.cxx 2615 2007-11-02 17:00:33Z fine $" << gVirtualX;
}
//______________________________________________________________________________
TQtGUIFactory::TQtGUIFactory(const char *name, const char *title)
   : TGuiFactory(name, title)
{
   // TQtGUIFactory ctor.
   // Restore the right TVirtulaX pointer      
   if (TGQt::GetVirtualX())  gVirtualX = TGQt::GetVirtualX();
   qDebug() << "TQtGuiFactory.cxx 2615 2007-11-02 17:00:33Z fine $" << gVirtualX;
}
//______________________________________________________________________________
TApplicationImp *TQtGUIFactory::CreateApplicationImp(const char *classname, int *argc, char **argv)
{
 TGQt::CreateQtApplicationImp();
#ifdef WIN32OLD
  TApplicationImp *app = 
       new TWin32Application(classname, argc, argv);
#else
  TApplicationImp *app = 
       new TQtRootApplication (classname, argc, argv);
#endif
  CreateQClient();
  return app;        
}
//______________________________________________________________________________
void TQtGUIFactory::CreateQClient()
{ 
    // gSystem->Load("libGui");
    if (!gClient) new TGClient();
   // ((TGQt *)TGQt::GetVirtualX())->SetQClientFilter(new TQClientFilter(new TGClient()));
}
//______________________________________________________________________________
TCanvasImp *TQtGUIFactory::CreateCanvasImp(TCanvas *c, const char *title, UInt_t width, UInt_t height)
{
   // Create a Qt version of TCanvasImp

 //  return new TQtCanvas(c, title, width, height);
  // fprintf(stderr, " Creating 1. CreateCanvasImp %s  \n",title);
  // -- CreateQtApplicationImp();
  return new TQtCanvasImp(c,title,width,height );
}

//______________________________________________________________________________
TCanvasImp *TQtGUIFactory::CreateCanvasImp(TCanvas *c, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height)
{
   // Create a Qt version of TCanvasImp

   //  CreateQtApplicationImp();
  return new TQtCanvasImp(c,title,x,y,width,height );
  // return 0;
}

//______________________________________________________________________________
TBrowserImp *TQtGUIFactory::CreateBrowserImp(TBrowser *b, const char *title, UInt_t width, UInt_t height)
{
   // make sure TBrowser  virtual function table is Ok (ROOT > 5.17/05 compliant version)
   return CreateBrowserImp(b, title, width, height, (Option_t *)0);
}

//______________________________________________________________________________
TBrowserImp *TQtGUIFactory::CreateBrowserImp(TBrowser *b, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height)
{
  // make sure TBrowser  virtual function table is Ok (ROOT > 5.17/05 compliant version)
  return CreateBrowserImp(b, title, x, y, width, height, (Option_t *)0);
}

//______________________________________________________________________________
TBrowserImp *TQtGUIFactory::CreateBrowserImp(TBrowser *b, const char *title, UInt_t width, UInt_t height, Option_t *)
{
   // make sure TBrowser  virtual function table is Ok
   TQtRootBrowserImp *browserImp = new TQtRootBrowserImp(b,title,width, height,false);
   browserImp->InitWindow();
   return browserImp;
}

//______________________________________________________________________________
TBrowserImp *TQtGUIFactory::CreateBrowserImp(TBrowser *b, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height, Option_t *)
{
  TQtRootBrowserImp *browserImp = new TQtRootBrowserImp(b,title,x,y,width, height,false);
  browserImp->InitWindow();
  return browserImp;
}

//______________________________________________________________________________
TContextMenuImp *TQtGUIFactory::CreateContextMenuImp(TContextMenu *c, const char * /*name*/, const char * /*title*/ )
{
  return new TQtContextMenuImp(c);
}

//______________________________________________________________________________
TControlBarImp *TQtGUIFactory::CreateControlBarImp(TControlBar *c, const char *title )
{
  return new TQtControlBarImp(c,title);
}

//______________________________________________________________________________
TControlBarImp *TQtGUIFactory::CreateControlBarImp(TControlBar *c, const char *title, Int_t x, Int_t y )
{
  return new TQtControlBarImp(c, title,x, y);
}

//______________________________________________________________________________
TInspectorImp *TQtGUIFactory::CreateInspectorImp(const TObject *obj, UInt_t width, UInt_t height)
{
    return new TQtInspectImp(obj, width, height);
}

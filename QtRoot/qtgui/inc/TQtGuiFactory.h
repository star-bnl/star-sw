// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtGuiFactory.h,v 1.5 2013/08/30 16:00:21 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtGUIFactory
#define ROOT_TQtGUIFactory

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtGUIFactory                                                        //
//                                                                      //
// This class is a factory for Qt GUI components. It overrides          //
// the member functions of the ABS TGuiFactory.                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "TGuiFactory.h"

class TQtApplication;
class TVirtualX;
class TGClient;

class TQtGUIFactory : public TGuiFactory {
protected:
  friend class TQtGUIFactoryThread;
  static void CreateQClient();
  
public:
   TQtGUIFactory();
   TQtGUIFactory(const char *name, const char *title);
   virtual ~TQtGUIFactory() { }

   virtual TApplicationImp *CreateApplicationImp(const char *classname, int *argc, char **argv);

   virtual TCanvasImp *CreateCanvasImp(TCanvas *c, const char *title, UInt_t width, UInt_t height);
   virtual TCanvasImp *CreateCanvasImp(TCanvas *c, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height);

   virtual TBrowserImp *CreateBrowserImp(TBrowser *b, const char *title, UInt_t width, UInt_t height);
   virtual TBrowserImp *CreateBrowserImp(TBrowser *b, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height);

   virtual TBrowserImp *CreateBrowserImp(TBrowser *b, const char *title, UInt_t width, UInt_t height, Option_t *opt);
   virtual TBrowserImp *CreateBrowserImp(TBrowser *b, const char *title, Int_t x, Int_t y, UInt_t width, UInt_t height, Option_t *opt);

   virtual TContextMenuImp *CreateContextMenuImp( TContextMenu *c, const char *name, const char *title );

   virtual TControlBarImp *CreateControlBarImp( TControlBar *c, const char *title );
   virtual TControlBarImp *CreateControlBarImp( TControlBar *c, const char *title, Int_t x, Int_t y );

   virtual TInspectorImp *CreateInspectorImp(const TObject *obj, UInt_t width, UInt_t height);
   ClassDef(TQtGUIFactory,0)  //Factory for Qt GUI components
};

#endif

// @(#)root/ged:$Name:  $:$Id: TQtGedEditor.h,v 1.3 2013/08/30 15:59:52 perev Exp $
// Author: Valeri Fine 10/07/2004

/****************************************************************************
** $Id: TQtGedEditor.h,v 1.3 2013/08/30 15:59:52 perev Exp $
**
** Copyright (C) 2004 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

/*************************************************************************
 * This source is based on TGedEditor, a ROOT GUI toolkit.               *
 * Copyright (C) 1995-2002, Rene Brun and Fons Rademakers.               *
 * All rights reserved.                                                  *
 * Author: Marek Biskup, Ilka Antcheva   02/12/2003                      *
 *                                                                       *
 * For the licensing terms see $ROOTSYS/LICENSE.                         *
 * For the list of contributors see $ROOTSYS/README/CREDITS.             *
 *************************************************************************/

#ifndef ROOT_TQtGedEditor
#define ROOT_TQtGedEditor

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtGedEditor (very first prototype)                                  //
//                                                                      //
// Editor is a collection of TQtGedAttFrame QDock windows               //
// It connects to a Canvas and listens for selected objects             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TVirtualPadEditor
#include "TVirtualPadEditor.h"
#endif

#include <list>

#ifndef __CINT__
#  include <qobject.h>
#endif
#include <TObject.h>

#ifdef R__GLOBALSTL
namespace std { using ::list; using ::stack; }
#endif

class QToolBar;
class QMainWindow;
class QDockArea;
class QDockWidget;

class QWidget;
class TCanvas;
class TQtGedAttInterfaceB;
class TQtGedFactoryI;

class TQtGedEditor : public TVirtualPadEditor {
   friend class TQtGedAttInterfaceB;
protected:
   TCanvas                 *fCanvas;
   QDockWidget             *fDock;
   QMainWindow             *fCanvasImpID;
   QWidget                 *fCanvasID;
   Bool_t                   fGlobal;           //true if editor is global
   std::list<TQtGedAttInterfaceB *>  fGedPropertyFrames;
//   static std::list<stack<TQtGedFactoryI *>> *fgGedFactoriesList; // the list of the primitive editors
   static std::list<TQtGedFactoryI *> *fgGedFactoriesList; // the list of the primitive editors
   int                fIsMainWindow;
#ifndef __CINT__
   static Qt::DockWidgetArea  fgDockPosition;
#endif
   virtual void Build();

   void Insert(TQtGedAttInterfaceB  *frame);

public:
   TQtGedEditor(TCanvas* canvas = 0);
   virtual ~TQtGedEditor();
   static bool Register(TQtGedFactoryI *gedFactory,bool replace=true);
   static bool UnRegister(TQtGedFactoryI *gedFactory);
#ifndef __CINT__
   static void SetDefaultPosition(Qt::DockWidgetArea dockPosition);
   static Qt::DockWidgetArea DefaultPosition();
#endif
   virtual Bool_t   IsGlobal() const;
   virtual void     SetGlobal(Bool_t);
   virtual TCanvas* GetCanvas() const;

   virtual void CloseWindow();
   TQtGedAttInterfaceB *CanEdit(const char *className) const;

   virtual void Show();
   virtual void Hide();
   virtual void DeleteEditors() { }

   ClassDef(TQtGedEditor,0)  //new Qt-based editor (very first prototype)
};

#endif

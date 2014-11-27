// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtCanvasImp.h,v 1.8 2013/08/30 16:00:20 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtCanvasImp
#define ROOT_TQtCanvasImp

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtCanvasImp                                                         //
//                                                                      //
// This class creates a main window with menubar, scrollbars and a      //
// drawing area.                                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////


#include <QObject>
#include <QVector>
#include <QMap>
#include <QPixmap>
#include <QEvent>
#include <QMenu>
#include <QLabel>

#include "TCanvasImp.h"
#include "TQtRootAction.h"

class TVirtualPadEditor;

class QMenu;
class QMenuBar;
class QToolBar;
class QPixmap;

class TQtCanvasWidget;
class TQtRootAction;
class TQtWidget;
class TQtZoomPadWidget;

class TQtCanvasImp : public QObject, public TCanvasImp {

Q_OBJECT

#include "TCanvasImp.interface.h"

private:

   friend  class TQtContextMenuImp;

   TQtCanvasWidget  *fCanvasImpID;
   TQtWidget        *fCanvasID;
   Int_t       fX;
   Int_t       fY; 
   UInt_t      fWidth;
   UInt_t      fHeight;
   QToolBar   *fFileToolBar;
   QToolBar   *fToolBar;
   QToolBar   *fEditToolBar;
   QMap<int, TQtRootAction*> fActions;
   TVirtualPadEditor   *fEditor;     // pointer to currently loaded pad editor

   
protected:
#if 0      
  enum { 
          kStatusBar      =100, kOptionStatistics=200, kOptionHistTitle=300
         ,kOptionFitParams=400, kOptionCanEdit   =500, kPadAutoExec    =600
       };
#endif          
  static TQtZoomPadWidget  *fgZoomingWidget;
  QMenuBar   *fMenuBar;
  QMenu *fOptionMenu;
  Bool_t      fDoubleBuffer;
  QVector<QLabel *> fStatusBar;
  QString     fSaveFileName;
  QString     fSaveType;


public:

   TQtCanvasImp();

   TQtCanvasImp(TCanvas *c, const char *name, UInt_t width, UInt_t height,bool initFlag=true);
   TQtCanvasImp(TCanvas *c, const char *name, Int_t x, Int_t y, UInt_t width, UInt_t height,bool initFlag=true);
   virtual ~TQtCanvasImp();
   virtual void Delete();

   virtual void   DrawEventStatus(const char *text = 0, Int_t partidx = 0);
   virtual void   MakeMenu();

   virtual void   FitCanvas();
   virtual void   NewCanvas();
   virtual void   CreateStatusBar(Int_t nparts=1);
   virtual void   CreateStatusBar(Int_t *parts, Int_t nparts=1);
   virtual void   RootExec(const char *cmd);

   virtual bool   event(QEvent *){return FALSE;}

protected:
   virtual void  MakeActions();
   virtual void  MakeToolBarActions(Int_t firstId);
   void ModifiedUpdate();
   void CreateEditor();

   void CB(){/* the empty call back  slot */}

protected slots:
   void Disconnect();
   void SaveFile(const QString &thatFile, const QString &selectedFilter);
   void ProcessMessage();

public slots:

  // file menu 
   void NewCB();
   void OpenCB();
   void SaveCB();
   void SaveAsCB();
   void SaveAsWebCB();
   void PrintCB();
   void CloseCB();
   void QuitCB();

  // editor menu 
   void CopyCB();
   void CopyFrameCB();
   void EditorCB();
   void ClearPadCB();
   void ClearCanvasCB();

  // view  menu 
   void MenuBarCb();
   void ToolBarCB();
   void BrowserCB();
   void ColorsCB();
   void FontsCB();
   void MarkersCB();
   void IconifyCB();
   void X3DViewCB();
   void GLViewCB();
   void GLIVViewCB();
   void InterruptCB();
   void ZoomCB();

  // option  menu 
   void EventStatusCB();
   void PadAutoExecCB();
   void AutoFitCanvasCB();
   void FitCanvasCB();
   void RefreshCB();

   void OptStatCB();
   void OptTitleCB();
   void OptFitCB();
   void CanEditHistogramsCB();

  //*-*   Items for the Inspect Menu
   void ROOTInspectCB();
   
   //*-*   Items for the Class Menu
   void FullTreeCB();

  // Help  menu 
   void AboutCB();
   void HelpCB();
   void HelpOnCanvasCB();
   void HelpOnMenusCB();
   void HelpOnGraphicsEdCB();
   void HelpOnBrowserCB();
   void HelpOnObjectsCB();
   void HelpOnPSCB();
   void HelpOn(const char *title, const char *body);
   void ReallyDeleteCB();

   // ClassDef(TQtCanvasImp,0)  //Win32Canvas class describing main window protocol
};

#endif

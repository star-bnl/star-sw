// Author: Valeri Fine   16/06/2006
/****************************************************************************
** $Id: TQtToolBar.cxx,v 1.8 2013/08/30 16:00:25 perev Exp $
**
** Copyright (C) 2006 by Valeri Fine. Brookhaven National Laboratory.
**                                    All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
*****************************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtToolBar                                                           //
//                                                                      //
// This class creates a main window with menubar, scrollbars and a      //
// drawing area.                                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <assert.h>

#include "TROOT.h"

#include "TQtToolBar.h"
#include "TQtRootAction.h"


#include <QToolBar>
#include <QMainWindow>

#include <QLabel>

// Canvas menu command ids
enum EToolBarCommands {
   kToolModify,
   kToolArc,
   kToolLine,
   kToolArrow,
   kToolDiamond,
   kToolEllipse,
   kToolPad,
   kToolPave,
   kToolPLabel,
   kToolPText,
   kToolPsText,
   kToolGraph,
   kToolCurlyLine,
   kToolCurlyArc,
   kToolLatex,
   kToolMarker,
   kToolCutG
};

static TQtBrowserMenuItem_t gToolBarData[] = {
   // See TROOT::SetEditMode(const char *mode)
  { "Modify",   kToolModify,    0, "Modify",        "pointer.xpm"  },
  { "Arc",      kToolArc,       0, "Arc",           "arc.xpm"      },
  { "Line",     kToolLine,      0, "Line",          "line.xpm"     },
  { "Arrow",    kToolArrow,     0, "Arrow",         "arrow.xpm"    },
  { "Diamond",  kToolDiamond,   0, "Diamond",       "diamond.xpm"  },
  { "Ellipse",  kToolEllipse,   0, "Ellipse",       "ellipse.xpm"  },
  { "Pad",      kToolPad,       0, "Pad",           "pad.xpm"      },
  { "Pave",     kToolPave,      0, "Pave",          "pave.xpm"     },
  { "PaveLabel",kToolPLabel,    0, "Pave Label",    "pavelabel.xpm"},
  { "PaveText", kToolPText,     0, "Pave Text",     "pavetext.xpm" },
  { "PavesText",kToolPsText,    0, "Paves Text",    "pavestext.xpm"},
  { "PolyLine", kToolGraph,     0, "Graph",         "graph.xpm"    },
  { "CurlyLine",kToolCurlyLine, 0, "Curly Line",    "curlyline.xpm"},
  { "CurlyArc", kToolCurlyArc,  0, "Curly Arc",     "curlyarc.xpm" },
  { "Text",     kToolLatex,     0, "Text/Latex",    "latex.xpm"    },
  { "Marker",   kToolMarker,    0, "Marker",        "marker.xpm"   },
  { "CutG",     kToolCutG,      0, "Graphical Cut", "cut.xpm"      },
  {0,0,0,"",""}
};
#if 0
#if QT_VERSION < 0x40000
   fEditToolBar = new QToolBar(fCanvasImpID);
#else /* QT_VERSION */
   fEditToolBar = new QToolBar(fCanvasImpID);
#endif /* QT_VERSION */
#endif
//______________________________________________________________________________
TQtToolBar::TQtToolBar(const QString &label, QMainWindow *mainWindow, QWidget *parent, bool newLine, const char *name,Qt::WindowFlags f)
#if QT_VERSION < 0x40000
      : TOOLBARCLASSNAME  ( label, mainWindow,parent,newLine, name ,f)
#else
      : TOOLBARCLASSNAME  ( label, mainWindow)
#endif
{
  // Constructs an empty horizontal toolbar. 
  // The toolbar is called name and is a child of parent and is managed by mainWindow. 
  // The label and newLine parameters are passed straight to QMainWindow::addDockWindow(). 
  // name and the widget flags f are passed on to the QDockWindow constructor. 

  // Use this constructor if you want to create torn-off (undocked, floating) toolbars 
  // or toolbars in the status bar. 
#if QT_VERSION >= 0x40000
   assert( !(parent || name || f));
#endif
  Build();
}

//______________________________________________________________________________
TQtToolBar::TQtToolBar(QMainWindow *parent) 
      : TOOLBARCLASSNAME (parent) 
{   
   // This is an overloaded member function, provided for convenience. 
   // It behaves essentially like the above function. 
   
   // Constructs an empty toolbar called name, with parent parent, 
   // in its parent's top dock area, without any label and without 
   // requiring a newline. 

   Build();
}

//______________________________________________________________________________
TQtToolBar::~TQtToolBar()
{ 
   //  dtor 
}

//______________________________________________________________________________
void  TQtToolBar::Build()
{
//  Populate the Qt QToolBar with the ROOT object create tools
   int i = 1;
   while (gToolBarData[i].fMenuText!=NULL) {
      AddAction(gToolBarData[i]);
      i++;
   }
}
//______________________________________________________________________________
void TQtToolBar::AddAction(const TQtBrowserMenuItem_t &action)
{
    // Add one ROOT tool define dby "actopn" to the tool bar 
    AddAction(new TQtRootAction((QObject *)parentWidget(),action));
}

//______________________________________________________________________________
void TQtToolBar::AddAction(TQtRootAction *action) 
{
   // Add one ROOT tool defined by "action" to the tool bar 
   // It behaves essentially like the above function. 

   if (action) {
     fActions.insert(action->Id(),action);
     connect( action, SIGNAL( activated() ) , this, SLOT(ProcessToolMessage()) );
     action->addTo(this); 
   }
}
//______________________________________________________________________________
void  TQtToolBar::ProcessToolMessage()
{
   //  The protected SLOT to notify the ROOT  
   //  about the last selected tool
   
   TQtRootAction *actionSender =  (TQtRootAction *)sender();
   if (actionSender->Id() == kToolModify) {
      gROOT->SetEditorMode("");
   } else {
      const QString &actionName = actionSender->menuText();
      gROOT->SetEditorMode((const char *)actionName);
   }
}

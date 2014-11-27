// Author: Valeri Fine   16/06/2006
/****************************************************************************
** $Id: TQtToolBar.h,v 1.6 2013/08/30 16:00:22 perev Exp $
**
** Copyright (C) 2006 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtToolBar
#define ROOT_TQtToolBar

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtToolBar                                                           //
//                                                                      //
// This class creates a main window with menubar, scrollbars and a      //
// drawing area.                                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <QtGlobal>

#include <q3intdict.h>
#include <QToolBar>
#include "TQtRootAction.h"


#define TOOLBARCLASSNAME QToolBar

class QMainWindow;
class TQtRootAction;
struct TQtBrowserMenuItem_t;
class TQtRootAction;

class TQtToolBar : public TOOLBARCLASSNAME {

Q_OBJECT

private:
   Q3IntDict<TQtRootAction> fActions;

public:

   TQtToolBar(const QString &label, QMainWindow *mainWindow, QWidget *parent, bool newLine=FALSE, const char *name=0,Qt::WindowFlags f=0);
   TQtToolBar(QMainWindow *parent);
   virtual ~TQtToolBar();
   void AddAction(const TQtBrowserMenuItem_t &action);
   void AddAction(TQtRootAction *action);

protected:
   void  Build();

protected slots:
   void ProcessToolMessage();

   // ClassDef(TQtToolBar,0)  // Class to provide the ROOT tool bar
};

#endif

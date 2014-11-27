// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtRootAction.h,v 1.5 2013/08/30 16:00:22 perev Exp $
**
** Copyright (C) 2003 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtRootAction
#define ROOT_TQtRootAction

#include <QtGlobal>
#include <QAction>
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TQtRootAction                                                        //
//                                                                      //
// QAction Qt class to build ROOT-related menu and tool bars     .      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

// Structure to initialize the QAction class
struct TQtBrowserMenuItem_t  {
   const char *fMenuText;
   int         fId;
   int         fAccelerator;
   const char *fToolTip;
   const char *iconName;
};  

class TQtRootAction : public QAction {
  public:
    TQtRootAction(QObject * parent, const TQtBrowserMenuItem_t  &data);
    int Id() const { return fId; }
  private:
     int  fId;
};
#endif

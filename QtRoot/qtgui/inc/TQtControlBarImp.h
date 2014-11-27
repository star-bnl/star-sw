// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtControlBarImp.h,v 1.4 2013/08/30 16:00:20 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtControlBarImp
#define ROOT_TQtControlBarImp


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// TQtControlBarImp                                                        //
//                                                                            //
// is an implemetation of the ControlBarImp ABC class                         //
// describing GUI independent control bar  for WIN32 API                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

#include "TControlBarImp.h"
#include "TObjectExecute.h"
#include "Rtypes.h"

#include <QtGlobal>
#include "q3ptrstack.h"
#include <QObject>

class TControlBarButton;
class TObjectExecute;
class QPushButton;
class QWidget;

//___________________________________________________________________
class TQtControlBarItem : public QObject {
  Q_OBJECT
private:
  TControlBarButton *fButton;
  QPushButton       *fButtonImp;
  TMethod           *fActionMethod;
  TObjectExecute     fExecute;
public:
  TQtControlBarItem( TControlBarButton *b,QPushButton *i);
  ~TQtControlBarItem() { }
  TControlBarButton *GetClicked() const { return fButton; }
public slots:
  void Exec(bool on);
signals:
   void ClickedItem(bool);
};

//___________________________________________________________________
class TQtControlBarImp : public QObject, public TControlBarImp {
Q_OBJECT
 private:
   QWidget  *fWidget;
#if QT_VERSION < 0x40000
   QPtrStack<TQtControlBarItem> fItems;
#else /* QT_VERSION */
   Q3PtrStack<TQtControlBarItem> fItems;
#endif /* QT_VERSION */
   TControlBarButton *fClicked;

 public:
   TQtControlBarImp(TControlBar *c = 0, const char *title="");
   TQtControlBarImp(TControlBar *c, const char *title, Int_t x, Int_t y);
   virtual ~TQtControlBarImp();

   virtual TControlBarButton *GetClicked();

   virtual void Create();
   virtual void Hide();
   virtual void Show();

 public slots:
   void Clicked(bool);
   virtual void Disconnect();

   // ClassDef(TQtControlBarImp,0) //Control bar implementation
};

#endif

// Author: Valeri Fine   21/01/2002
/****************************************************************************
** $Id: TQtEvent.h,v 1.4 2013/08/30 15:59:49 perev Exp $
**
** Copyright (C) 2002 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/

#ifndef ROOT_TQtEvent
#define ROOT_TQtEvent

#include <QEvent>

class TQtObject;
class TWaitCondition;
//______________________________________________________________________________
class TQtEvent : public QEvent 
{

private:
    TWaitCondition *fCondition;
    unsigned long *fResult;   // QApplication owns QEvent and will destroy it
    QObject *fReceiver;
    QEvent  *fThatEvent;

public:
    TQtEvent(int code);
    TQtEvent(QObject *o, QEvent *e);
    virtual ~TQtEvent(){}
    void SetWait(TWaitCondition &condition, unsigned long  &result);
    void SetWait(TWaitCondition &condition);
    void SetResult(unsigned long e=0);
 //   QEvent *WaitResult(); too dangerous
    bool Notify();
    virtual void ExecuteCB(){;}
};

#endif

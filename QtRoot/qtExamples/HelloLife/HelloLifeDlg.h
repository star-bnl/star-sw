/****************************************************************************
** $Id: HelloLifeDlg.h,v 1.4 2013/08/30 15:59:59 perev Exp $
**
** Copyright (C) 1992-2000 Trolltech AS.  All rights reserved.
**
** This file is part of an example program for Qt.  This example
** program may be used, distributed and modified without limitation.
**
*****************************************************************************/

#ifndef LIFEDLG_H
#define LIFEDLG_H

#include <qtimer.h>
#include <qwidget.h>

class QSlider;
class QPushButton;
class QLabel;
class QComboBox;

#include "HelloLife.h"


class LifeTimer : public QTimer
{
    Q_OBJECT
public:
    LifeTimer( QWidget *parent );
    enum { MAXSPEED = 1000 };

public slots:
    void	setSpeed( int speed );
    void	pause( bool );
    void	next();

private:
    int		interval;
    bool    go;
};


class LifeDialog : public QWidget
{
    Q_OBJECT
public:
    LifeDialog( int scale = 10, QWidget *parent = 0, const char *name = 0 );
public slots:
    void	getPattern( int );
    void nextGeneration();
protected:
    virtual void resizeEvent( QResizeEvent * e );

private:
    enum { TOPBORDER = 70, SIDEBORDER = 10 };

    LifeWidget	*life;
    QPushButton *qb;
    LifeTimer	*timer;
    QPushButton *pb;
    QComboBox	*cb;
    QLabel	*sp;
    QSlider	*scroll;
};


#endif // LIFEDLG_H

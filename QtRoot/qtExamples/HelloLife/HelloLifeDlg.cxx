/****************************************************************************
** $Id: HelloLifeDlg.cxx,v 1.4 2013/08/30 15:59:59 perev Exp $
**
** Copyright (C) 1992-2000 Trolltech AS.  All rights reserved.
**
** This file is part of an example program for Qt.  This example
** program may be used, distributed and modified without limitation.
**
*****************************************************************************/

#include "HelloLifeDlg.h"
#include <QApplication>
#include <QPushButton>
#include <QLabel>
#include <QSlider>
#include <QComboBox>
#include <QDateTime>
#include <QDesktopWidget>
#include <QResizeEvent>

#include <stdlib.h>

#include "patterns.cxx"


// A simple timer which has a pause and a setSpeed slot

//_____________________________________________________________________________________
LifeTimer::LifeTimer( QWidget *parent ) : QTimer( parent ), interval( 500 ), go(TRUE)
{
   next();
}

//_____________________________________________________________________________________
void LifeTimer::next()
{ 
   if (go)
        start(interval, TRUE );
}
//_____________________________________________________________________________________
void LifeTimer::pause( bool )
{
   go = !go;
   next();
}


//_____________________________________________________________________________________
void LifeTimer::setSpeed( int speed )
{
    interval = MAXSPEED - speed; 
}


// A top-level container widget to organize the others

//_____________________________________________________________________________________
LifeDialog::LifeDialog( int scale, QWidget * parent, const char * name )
    : QWidget( parent, name )
{
    qb = new QPushButton( "Quit!", this );
    cb = new QComboBox( this, "comboBox" );
    life = new LifeWidget(scale, this);
    life->move( SIDEBORDER, TOPBORDER );


    connect( qb, SIGNAL(clicked()), qApp, SLOT(quit()) );
    qb->setGeometry( SIDEBORDER, SIDEBORDER, qb->sizeHint().width(), 25 );
    timer = new LifeTimer( this );

    connect( timer, SIGNAL(timeout()), this, SLOT(nextGeneration()) );
    pb = new QPushButton( "Pause", this );
    pb->setToggleButton( TRUE );
    connect( pb, SIGNAL(toggled(bool)), timer, SLOT(pause(bool)) );
    pb->resize( pb->sizeHint().width(), 25 );
    pb->move( width() - SIDEBORDER - pb->width(), SIDEBORDER );

    sp = new QLabel( "Speed:", this );
    sp->adjustSize();
    sp->move( SIDEBORDER, 45 );
    scroll = new QSlider( Qt::Horizontal, this );
    scroll->setMinimum(0);
    scroll->setMaximum(LifeTimer::MAXSPEED);
    scroll->setPageStep(50);
    scroll->setValue(LifeTimer::MAXSPEED / 2);
    connect( scroll, SIGNAL(valueChanged(int)),
             timer,  SLOT(setSpeed(int)) );

    scroll->move( sp->width() + 2 * SIDEBORDER, 45 );
    scroll->resize( 200, 15 );

    life->setFrameStyle( QFrame::Panel | QFrame::Sunken );
    life->show();

    srand( QTime(0,0,0).msecsTo(QTime::currentTime()) );
    int sel =  rand() % NPATS;
    getPattern( sel );

    cb->move( 2*SIDEBORDER + qb->width(), SIDEBORDER);
    cb->insertItem( "Glider Gun " );
    cb->insertItem( "Figure Eight " );
    cb->insertItem( "Pulsar " );
    cb->insertItem( "Barber Pole P2 " );
    cb->insertItem( "Achim P5 " );
    cb->insertItem( "Hertz P4 " );
    cb->insertItem( "Tumbler " );
    cb->insertItem( "Pulse1 P4" );
    cb->insertItem( "Shining Flower P5 " );
    cb->insertItem( "Pulse2 P6 " );
    cb->insertItem( "Pinwheel, Clock P4 " );
    cb->insertItem( "Pentadecatholon " );
    cb->insertItem( "Piston " );
    cb->insertItem( "Piston2 " );
    cb->insertItem( "Switch Engine " );
    cb->insertItem( "Gears (Gear, Flywheel, Blinker) " );
    cb->insertItem( "Turbine8 " );
    cb->insertItem( "P16 " );
    cb->insertItem( "Puffer " );
    cb->insertItem( "Escort " );
    cb->insertItem( "Dart Speed 1/3 " );
    cb->insertItem( "Period 4 Speed 1/2 " );
    cb->insertItem( "Another Period 4 Speed 1/2 " );
    cb->insertItem( "Smallest Known Period 3 Spaceship Speed 1/3 " );
    cb->insertItem( "Turtle Speed 1/3 " );
    cb->insertItem( "Smallest Known Period 5 Speed 2/5 " );
    cb->insertItem( "Sym Puffer " );
    cb->insertItem( "], Near Ship, Pi Heptomino " );
    cb->insertItem( "R Pentomino " );
    cb->setCurrentItem( sel );
    cb->show();
    connect( cb, SIGNAL(activated(int)), SLOT(getPattern(int)) );

    QSize s;
    s = life->minimumSize();
    setMinimumSize( s.width() + 2 * SIDEBORDER, 
		    s.height() + TOPBORDER + SIDEBORDER );
    s = life->maximumSize();
    setMaximumSize( s.width() + 2 * SIDEBORDER, 
		    s.height() + TOPBORDER + SIDEBORDER );
    s = life->sizeIncrement();
    setSizeIncrement( s.width(), s.height() );

    resize( QMIN(512, qApp->desktop()->width()),
	    QMIN(480, qApp->desktop()->height()) );
}


//_____________________________________________________________________________________
void LifeDialog::resizeEvent( QResizeEvent * e )
{
    life->resize( e->size() - QSize( 2 * SIDEBORDER, TOPBORDER + SIDEBORDER ));
    pb->move( e->size().width() - SIDEBORDER - pb->width(), SIDEBORDER );
    scroll->resize( e->size().width() - sp->width() - 3 * SIDEBORDER,
		    scroll->height() );
    cb->resize( width() - 4*SIDEBORDER - qb->width() - pb->width()  , 25 );
}

//_____________________________________________________________________________________
void LifeDialog::nextGeneration() {
   life->nextGeneration();
#ifdef ANIMATE
   static int nFile = 0;
   QString lifes = "life" + QString::number(nFile++)+ ".png";
   QPixmap::grabWidget(this).save(lifes,"PNG");
#endif   
   timer->next();
}
// Adapted from xlock, see pattern.cpp for copyright info.

//_____________________________________________________________________________________
void LifeDialog::getPattern( int pat )
{
    life->clear();
    int i = pat % NPATS;
    int col;
    int * patptr = &patterns[i][0];
    while ( (col = *patptr++) != 127 ) {
       int row = *patptr++;
       col += life->maxCol() / 2;
       row += life->maxRow() / 2;
       life->setPoint( col, row );
    }
}

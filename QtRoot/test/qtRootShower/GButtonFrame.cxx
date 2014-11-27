// Author: Bertrand Bellenot   22/08/02

/*************************************************************************
 * Copyright (C) 1995-2002, Bertrand Bellenot.                           *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see the LICENSE file.                         *
 *************************************************************************/

/*************************************************************************
 *   Modified by Valeri Fine to use Qt GUI labrary                       *
 *************************************************************************/

#include <qapplication.h>
#include <qpushbutton.h>
#include <qtooltip.h>
#include "GButtonFrame.h"
#include <QDebug>

//______________________________________________________________________________
// GButtonFrame
//
// A GButtonFrame is a frame containing the RootShower buttons. 
//______________________________________________________________________________

//______________________________________________________________________________
GButtonFrame::GButtonFrame(QWidget* p) : Q3VButtonGroup(p)
{
    // Create GButtonFrame object, with QWidget parent *p.
    //
    // Create Event Buttons
    fNextEventButton = new QPushButton("Start &New Event",this);
    // fNextEventButton->setToggleButton(true);
    QToolTip::add(fNextEventButton,"Start new simulation event");
    connect(this,SIGNAL(clicked(int)),this,SLOT(Clicked(int)));

    fStopSimButton = new QPushButton("&Interrupt Simulation",this);
    QToolTip::add(fStopSimButton,"Interrupts the current simulation");
    // connect(fStopSimButton,SIGNAL(clicked(bool)),this,SLOT(Clicked(bool)));

    fShowTrackButton = new QPushButton("&Show Selection",this); // showTrackId
    QToolTip::add(fShowTrackButton,"Shows the selected track");
    // connect(fShowTrackButton,SIGNAL(clicked(bool)),this,SLOT(Clicked(bool)));

    fShowTrackButton->setEnabled (false );
    fStopSimButton->setEnabled (false );
}
//______________________________________________________________________________
GButtonFrame::~GButtonFrame(){ }
//______________________________________________________________________________
void GButtonFrame::SetState(EState state)
{
    // Set the state of the GButtonFrame. This sets the state of
    // the TGButton's in this frame.
    switch (state) {
        case kAllActive:
            fNextEventButton->setEnabled (true);
            fShowTrackButton->setEnabled (true);
            fStopSimButton->setEnabled (false );
            break;

        case kNoneActive:
            fNextEventButton->setEnabled (false );
            fShowTrackButton->setEnabled (false );
            fStopSimButton->setEnabled (true );
            break;

    } // switch state 
    // make sure window gets updated...
    qApp->processEvents();
}

//______________________________________________________________________________
void GButtonFrame::Clicked(int id)
{
   // Emit signals:
   switch (id) {
     case 0: emit NextEvent();   break;
     case 1: emit Interrupt();   break;
     case 2: emit SelectEvent(); break;
   }
}

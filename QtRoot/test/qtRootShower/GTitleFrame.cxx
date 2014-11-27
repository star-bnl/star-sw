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

#include <TSystem.h>
#include <TGClient.h>
#include <qlabel.h>
#include <qpixmap.h>
//Added by qt3to4:
#include <Q3Frame>
#include "GTitleFrame.h"
#include "ProgramPath.h"

//______________________________________________________________________________
//
// GTitleFrame
//______________________________________________________________________________

//______________________________________________________________________________
GTitleFrame::GTitleFrame(QWidget *p,
			     const Text_t *mainText, const Text_t *subText,
			     UInt_t /*w*/, UInt_t /*h*/,
			     UInt_t /*options*/): Q3HBox(p)
{
    QLabel *fLeftIconPicture  = new QLabel(this,"leftlogo");
    QLabel *fTextFrame        = new QLabel(this,"title");
    fRightIconPicture         = new QLabel(this,"rightlogo");
    
    // Create GTitleFrame object, with TGWindow parent 'p', text 'mainText'
    // with sub text 'subText'. 
    // add text
    fTextFrame->setFrameStyle( Q3Frame::Panel | Q3Frame::Sunken );
    fTextFrame->setText( QString(mainText) + QString("\n") + QString(subText) );
    fTextFrame->setAlignment( Qt::AlignCenter );
   
    QFont *labelfont;
    labelfont = (QFont *)gClient->GetFontByName("-*-times-bold-r-*-*-24-*-*-*-*-*-*-*");
    fTextFrame->setFont(*labelfont);
    fTextFrame->setPaletteForegroundColor(QColor("red"));

    // add pictures
    fLeftIconPicture->setPixmap(QPixmap(ProgramPath("icons/left.xpm")));
    fLeftIconPicture->setSizePolicy(QSizePolicy::Fixed ,QSizePolicy::Fixed);
    
    fRightIconPicture->setPixmap(QPixmap(ProgramPath("anim/anim01.xpm")));
    fRightIconPicture->setAlignment(Qt::AlignRight);
    fRightIconPicture->setSizePolicy(QSizePolicy::Fixed ,QSizePolicy::Fixed);

    setSizePolicy(QSizePolicy::MinimumExpanding  ,QSizePolicy::Fixed);
}
//______________________________________________________________________________
void GTitleFrame::ChangeRightLogo(Int_t frame)
{
    // Change the right logo ( used for animation )
    Char_t name[12];
    sprintf(name,"%02d.xpm",frame);
    QString theRightLogoFilename = "anim/anim";
    theRightLogoFilename += (const char *)name;
    fRightIconPicture->setPixmap(ProgramPath(theRightLogoFilename));
}
//______________________________________________________________________________
GTitleFrame::~GTitleFrame()
{ }

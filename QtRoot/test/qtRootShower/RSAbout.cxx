// Author: Bertrand Bellenot   22/08/02

/*************************************************************************
 * Copyright (C) 1995-2002, Bertrand Bellenot.                           *
 * All rights reserved.                                                  *
 *                                                                       *
 * For the licensing terms see the LICENSE file.                         *
 *************************************************************************/

#include <TSystem.h>
#include <TROOT.h>
// #include <TRootHelpDialog.h>

#include "RSAbout.h"
#include "RSVersion.h"
#include "ProgramPath.h"

#include <qpixmap.h>

//______________________________________________________________________________
RootShowerAbout::RootShowerAbout(QWidget *p,
                       UInt_t w, UInt_t h, UInt_t options) :
    QMessageBox (p,"About RootShower...")
     
{
   setTextFormat (Qt::RichText);
   Int_t iday,imonth,iyear;
   Char_t message1[80];
   static const Char_t *months[] = {"January","February", "March","April","May","June","July",
      "August", "September","October","November","December"};
   UInt_t wh1 = (UInt_t)(0.6 * h);
   UInt_t wh2 = h - wh1;

   const Char_t *root_version = gROOT->GetVersion();
   Int_t idatqq = gROOT->GetVersionDate();
   iday   = idatqq%100;
   imonth = (idatqq/100)%100;
   iyear  = (idatqq/10000);
   Char_t *root_date = Form("%s %d %4d",months[imonth-1],iday,iyear);

   setBackgroundMode(Qt::FixedPixmap,Qt::FixedPixmap);
   setPaletteBackgroundPixmap (QPixmap(ProgramPath("icons/lmclogo01.xpm")));
   setErasePixmap(QPixmap(ProgramPath("icons/lmclogo01.xpm"))); 

   QString fullText;
   sprintf(message1,"<center>ROOT Shower Monte Carlo / Event Display v %s<p>",ROOTSHOWER_RELEASE);
   fullText = message1;
   //sprintf(message1,"<img source=\"%s\">",(const char*)theLogoFilename);
   //printf("%s\n",message1);
   //fullText += message1;
   sprintf(message1,"Compiled with Root version %s, release date : %s<p>",root_version, root_date);
   fullText += message1;
   fullText += "Copyright © Bertrand Bellenot 1994 - 2002";

   setText(fullText);
   resize(w,h);
}
//______________________________________________________________________________
void RootShowerAbout::About(QWidget *p, UInt_t w, UInt_t h)
{
    // Create and show the "Root Shower About" message
   RootShowerAbout message(p,w,h);
   message.exec();
}

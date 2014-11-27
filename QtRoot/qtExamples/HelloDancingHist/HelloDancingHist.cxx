// @(#)root/qt:$Id: HelloDancingHist.cxx,v 1.1 2013/08/30 15:59:58 perev Exp $
// Author: Valeri Fine   16/08/2009
/****************************************************************************
**
** Copyright (C) 2009 by Valeri Fine.  All rights reserved.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
*****************************************************************************/
#include "HelloDancingHist.h"

#include <qpainter.h>
#include <qdrawutil.h>
#include <qcheckbox.h>
#include <qevent.h>
#include <qapplication.h>
#include <QTimer>
#include <QDebug>

#include "TH1.h"
#if ROOT_VERSION_CODE == ROOT_VERSION(5,24,0)
#error Due the ROOT 5.24 bug this example can work with any ROOT version but 5.24
#endif
#include "TEmbeddedPad.h"
#include "TRandom.h"
#include "TFrame.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TCanvas.h"
#include "TGQt.h"
#include "TQtWidget.h"
#include "TApplication.h"
#include "TQtUtil.h"

//_____________________________________________________________________________________
HelloDancingHist::HelloDancingHist( int , QWidget *parent, const char *name )
    : QFrame( parent, name )
{
    // Init QtRoot framework
    TQtWidget::InitRint();
    // Create the histograms and pads
    for (int i=0;i<3;i++) {
       fPads[i] = new TEmbeddedPad(Form("c%d",i),"Dynamic Filling Example",240,240);
       fPads[i]->SetFillColor(42+2*i);
       fPads[i]->GetFrame()->SetFillColor(21+3*i);
       fPads[i]->GetFrame()->SetBorderSize(6);
    }

   // Create some histograms, a profile histogram.
   fHpx    = new TH1F("hpx","This is the px distribution",100,-4,4);
   fHpx->SetLineWidth(2);
   fHpx->SetLineColor(kRed);
   fHpxpy  = new TH2F("hpxpy","py vs px",40,-4,4,40,-4,4);
   fHprof  = new TProfile("hprof","Profile of pz versus px",100,-4,4,0,20);
   fHprof->SetLineWidth(2);
   fHprof->SetLineColor(kBlue);
   gRandom->SetSeed();
   QTimer::singleShot (0,this, SLOT(nextGeneration())); // start event loop
}

//_____________________________________________________________________________________
HelloDancingHist::~HelloDancingHist()
{
   for ( int i=0;i<3;i++) {      delete fPads[i]; fPads[i] = 0;    }
}

//_____________________________________________________________________________________
void HelloDancingHist::updateHistograms()
{
   static int ii = 0;

   Float_t px, py, pz;
   Int_t j = 0;
   
   for (Int_t i = 0; i < 50; i++,ii++) {
       gRandom->Rannor(px,py);
       pz = px*px + py*py;
       fHpx->Fill(px);
       fHpxpy->Fill(px,py);
       fHprof->Fill(px,pz);
       if (ii == 48)  {
           // Attach each object to the dedicated pad at once
           fPads[0]->cd();   fHpx  ->Draw();
           fPads[1]->cd();   fHpxpy->Draw("COLZ");
           fPads[2]->cd();   fHprof->Draw();
        }
        if (ii > 6000) {
           fHpx->Reset();
           fHpxpy->Reset();
           fHprof->Reset();
           ii = 100;
        }
   }
   for (j=0;j<3;j++) {
       fPads[j]->Modified();
       fPads[j]->Update();
   }
}

//_____________________________________________________________________________________
void HelloDancingHist::nextGeneration()
{
   updateHistograms();
   update();
   QTimer::singleShot (600,this, SLOT(nextGeneration()));
   QPixmap p = QPixmap::grabWidget(this);
   static int counter = 0;
   p.save(QString("HelloDancingHist.%1.png").arg(counter++));
}

//_____________________________________________________________________________________
void HelloDancingHist::paintEvent( QPaintEvent *e )
{
   if (!e) return;
   QPainter paint( this );
   QSize wsize = size();
   for (int i=0;i<3;i++) {
       Double_t rotn = gRandom->Rndm()*10; // defien the rotation angle
       int ax = int(gRandom->Rndm()*2.3);
       Qt::Axis axis = Qt::ZAxis; 
       switch (ax) {
          case 0:  axis = Qt::XAxis;  break;
          case 1:  axis = Qt::YAxis;  break;
          case 2:
          default: axis = Qt::ZAxis;  break;
       };
       QPixmap pix = TQtUtil::padPixmap(fPads[i])->transformed(QTransform().rotate(rotn*36.,axis));
       QSize psize = pix.size();
       int x = int(gRandom->Rndm() * float( wsize.width() - psize.width()));
       int y = int(gRandom->Rndm() * float( wsize.height() - psize.height()));
       paint.drawPixmap(x,y,pix);
   }
}

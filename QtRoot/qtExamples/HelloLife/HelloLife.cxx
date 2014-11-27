/****************************************************************************
** $Id: HelloLife.cxx,v 1.5 2013/08/30 15:59:59 perev Exp $
**
** Copyright (C) 1992-2000 Trolltech AS.  All rights reserved.
**
** This file is part of an example program for Qt.  This example
** program may be used, distributed and modified without limitation.
**
*****************************************************************************/

#include "HelloLife.h"

#include <qpainter.h>
#include <qdrawutil.h>
#include <qcheckbox.h>
#include <qevent.h>
#include <qapplication.h>

#include "TH1.h"
#ifdef EMBEDDEDTPAD   
#  include "TEmbeddedPad.h"
#endif
#include "TRandom.h"
#include "TFrame.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TQtWidget.h"
#include "TCanvas.h"
#include "TGQt.h"


// The main game of life widget

//_____________________________________________________________________________________
LifeWidget::LifeWidget( int s, QWidget *parent, const char *name )
    : QFrame( parent, name )
{
#ifdef EMBEDDEDTPAD   
    TQtWidget::InitRint();
#endif
    SCALE = s;

    maxi = maxj = 50;
    setMinimumSize( MINSIZE * SCALE + 2 * BORDER,
		   MINSIZE * SCALE + 2 * BORDER );
    setMaximumSize( MAXSIZE * SCALE + 2 * BORDER,
		   MAXSIZE * SCALE + 2 * BORDER );
    setSizeIncrement( SCALE, SCALE);

    clear();
    resize( maxi * SCALE + 2 * BORDER , maxj * SCALE + 2 * BORDER );
    // Create the histograms and pads
   int i =0;
   for (i=0;i<3;i++) {
#ifndef EMBEDDEDTPAD
      fPads[i] = new TQtWidget(this); 
      // Make it invisible
      
      fPads[i]->hide(); 
      fPads[i]->resize(SCALE-1,SCALE-1);
      fPads[i]->GetCanvas()->SetFillColor(42);
      fPads[i]->GetCanvas()->GetFrame()->SetFillColor(21);
      fPads[i]->GetCanvas()->GetFrame()->SetBorderSize(6);
      fPads[i]->GetCanvas()->GetFrame()->SetBorderMode(-1);
      Int_t id = fPads[i]->GetCanvas()->GetPixmapID();
#else
      fPads[i] = new TEmbeddedPad(Form("c%d",i),"Dynamic Filling Example",SCALE-1,SCALE-1);
      fPads[i]->SetFillColor(42);
      fPads[i]->GetFrame()->SetFillColor(21);
      fPads[i]->GetFrame()->SetBorderSize(6);
      fPads[i]->GetFrame()->SetBorderMode(-1);
      Int_t id = fPads[i]->GetPixmapID();
#endif
      fPixmaps[i] = (QPixmap *)TGQt::iwid(id);
   }

   // Create some histograms, a profile histogram.
   fHpx    = new TH1F("hpx","This is the px distribution",100,-4,4);
   fHpxpy  = new TH2F("hpxpy","py vs px",40,-4,4,40,-4,4);
   fHprof  = new TProfile("hprof","Profile of pz versus px",100,-4,4,0,20);
   gRandom->SetSeed();
}

//_____________________________________________________________________________________
LifeWidget::~LifeWidget()
{
#ifdef EMBEDDEDTPAD
   int i =0;
   for (i=0;i<3;i++) {
      delete fPads[i]; fPads[i] = 0;
   }
#endif
}


//_____________________________________________________________________________________
void LifeWidget::updateHistograms()
{
   static int ii = 0;
   static int iCounter = 0;
   if ((iCounter++)%3 != 0) return;

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
           fPads[1]->cd();   fHpxpy->Draw("COL");
           fPads[2]->cd();   fHprof->Draw();
        }
        if (ii > 2500) {
           fHpx->Reset();
           fHpxpy->Reset();
           fHprof->Reset();
           ii = 100;
        }
   }
   for (j=0;j<3;j++) {
#ifndef EMBEDDEDTPAD
       fPads[j]->Refresh();
#else
       fPads[j]->Modified();
       fPads[j]->Update();
#endif

   }
}

//_____________________________________________________________________________________
void LifeWidget::clear()
{
    current = 0;
    for ( int t = 0; t < 2; t++ )
       for ( int i = 0; i < MAXSIZE + 2; i++ )
           for ( int j = 0; j < MAXSIZE + 2; j++ )
             cells[t][i][j] = FALSE;

    repaint();
}


// We assume that the size will never be beyond the maximum size set
// this is not in general TRUE, but in practice it's good enough for
// this program

//_____________________________________________________________________________________
void LifeWidget::resizeEvent( QResizeEvent * e )
{
    maxi = (e->size().width()  - 2 * BORDER) / SCALE;
    maxj = (e->size().height() - 2 * BORDER) / SCALE;
}

//_____________________________________________________________________________________
void LifeWidget::setPoint( int i, int j )
{
    if ( i < 1 || i > maxi || j < 1 || j > maxi )
       return;
    cells[current][i][j] = TRUE;
    repaint( index2pos(i), index2pos(j), SCALE, SCALE, FALSE );
}

//_____________________________________________________________________________________
void LifeWidget::mouseHandle( const QPoint &pos )
{
    int i = pos2index( pos.x() );
    int j = pos2index( pos.y() );
    setPoint( i, j );
}

//_____________________________________________________________________________________
void LifeWidget::mouseMoveEvent( QMouseEvent *e )
{
    mouseHandle( e->pos() );
}

//_____________________________________________________________________________________
void LifeWidget::mousePressEvent( QMouseEvent *e )
{
   if ( e->button() == 
#if QT_VERSION < 0x40000
   QMouseEvent::LeftButton
#else      
   Qt::LeftButton
#endif               
   )   mouseHandle( e->pos() );
}

//_____________________________________________________________________________________
void LifeWidget::nextGeneration()
{
   for ( int i = 1; i <= MAXSIZE; i++ ) {
     for ( int j = 1; j <= MAXSIZE; j++ ) {
       int t = cells[current][i - 1][j - 1]
                + cells[current][i - 1][j]
                + cells[current][i - 1][j + 1]
                + cells[current][i][j - 1]
                + cells[current][i][j + 1]
                + cells[current][i + 1][j - 1]
                + cells[current][i + 1][j]
                + cells[current][i + 1][j + 1];
       cells[!current][i][j] = ( t == 3 || t == 2 && cells[current][i][j] );
     }
   }
   current = !current;
   updateHistograms();
   repaint( FALSE );		// repaint without erase
}

//_____________________________________________________________________________________
void LifeWidget::paintEvent( QPaintEvent * e )
{
    int starti = pos2index( e->rect().left() );
    int stopi  = pos2index( e->rect().right() );
    int startj = pos2index( e->rect().top() );
    int stopj  = pos2index( e->rect().bottom() );

    if (stopi > maxi)
       stopi = maxi;
    if (stopj > maxj)
       stopj = maxj;

    QPainter paint( this );
    for ( int i = starti; i <= stopi; i++ ) {
       for ( int j = startj; j <= stopj; j++ ) {
           if ( cells[current][i][j] ) {
              if (fPixmaps[i%3]) 
                 paint.drawPixmap(index2pos( i ), index2pos( j ),*fPixmaps[j%3]);
              else
                 qDrawShadePanel( &paint, index2pos( i ), index2pos( j ),
                                  SCALE - 1, SCALE - 1, colorGroup() );
            } else if ( cells[!current][i][j] )
                 erase(index2pos( i ), index2pos( j ), SCALE - 1, SCALE - 1);
       }
    }
    drawFrame( &paint );
}

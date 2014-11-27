#include <TRint.h>
#include <TQtPixmapBox.h>
#include <TCanvas.h>
#include <qpixmap.h>
#include <qpainter.h>
#include <qfont.h>
#include <qbitmap.h>

//__________________________________________________________________
void drawColorWheel( QPainter *p )
{
//
// This function draws a color wheel.
// The coordinate system x=(0..500), y=(0..500) spans the paint device.
//
// This file is part of an example program for Qt.  This example
// program may be used, distributed and modified without limitation.
//
    QFont f( "times", 18, QFont::Bold );
    p->setFont( f );
    p->setPen( Qt::black );
    p->setWindow( 0, 0, 500, 500 );		// defines coordinate system

    for ( int i=0; i<36; i++ ) {		// draws 36 rotated rectangles

        QMatrix matrix;
        matrix.translate( 250.0F, 250.0F );	// move to center
        matrix.shear( 0.0F, 0.3F );		// twist it
        matrix.rotate( (float)i*10 );		// rotate 0,10,20,.. degrees
        p->setWorldMatrix( matrix );		// use this world matrix

        QColor c;
        c.setHsv( i*10, 255, 255 );		// rainbow effect
        p->setBrush( c );			// solid fill with color c
        p->drawRect( 70, -10, 80, 10 );		// draw the rectangle

        QString n;
        n.sprintf( "H=%d", i*10 );
        p->drawText( 80+70+5, 0, n );		// draw the hue number
    }
}
//__________________________________________________________________
int main( int argc, char **argv )
{  
    // Create an interactive ROOT application
    TRint *theApp = new TRint("Rint", &argc, argv);

    new TCanvas("c1","c1",600,600);
    // Create  "colorwheel" Pixmap
    QPixmap src(600,600);
    //QBitmap mask(src.size(),TRUE);
    //src.setMask(mask);
    src.fill();
    QPainter p(&src);
    drawColorWheel(&p);
    TQtPixmapBox box(src,0.1,0.9,0.9,0.2);
    box.Draw();

    // and enter the ROOT event loop...
    gApplication->Run();
    printf(" Goob bye ROOT !\n");
}

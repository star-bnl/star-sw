#include <QApplication>
#include <QPixmap>
#include <QToolTip>
#include <cstdio>
//#include <QMainWindow>
//#include <QScrollArea>

#include "PixmapWidget.h"

int main( int argc, char **argv )
{
	QApplication a( argc, argv );
   fprintf(stderr,"Attention: One has to hit the <CTRL-C> to terminate this example !\n");
//	QMainWindow *mw = new QMainWindow();
	//QScrollArea *sa = new QScrollArea( mw );
//	PixmapWidget *pw = new PixmapWidget( QString("./test.png"), sa );
	PixmapWidget *pw = new PixmapWidget( QPixmap("./test.png"), 0);
#if QT_VERSION < 0x40000
   QToolTip::add ( pw, "Rotate the mouse wheel to zoom the image, move the mouse pointer out the picture to hide it");
#else   
   pw->setToolTip("Rotate the mouse wheel to zoom the image, move the mouse pointer out the picture to hide it");
#endif   
//	sa->setWidgetResizable( true );
//	sa->setWidget( pw );

//	mw->setCentralWidget( sa );
//	mw->show();

	pw->show();

	a.connect( &a, SIGNAL(lastWindowClosed()), &a, SLOT(quit()) );
	
	return a.exec();
}

#include "simpleQT.h"

#include <TROOT.h>
#include <TH1F.h>
#include <TSystem.h>
#include <TQtWidget.h>

#include <QTimer>
#include <QObject>
//#include <qapplication.h>
#include <QApplication>
#include <QLabel>
//#include <q3mainwindow.h>
#include <QMainWindow>
#include "canvas.h"
#include "JevpScreenWidget.h"

simpleQT *qt;



int simpleQT::main() 
{
    int argc=0;
    char *argv[1];

    QApplication app(argc, argv, true);

#define SCREENWIDGET
#ifdef SCREENWIDGET

    MetaWidget mywidget(NULL);
    mywidget.init();
    mywidget.show();
    mywidget.updates();


#endif

    //#define WORKS
#ifdef WORKS
	QMainCanvas *window = NULL;
	if(1) {
	    window = new QMainCanvas();
	    window->show();
	}
#endif

    //#define TWORKS
#ifdef TWORKS
    QRootCanvas *canvas = new QRootCanvas();
	
    TH1F *h = new TH1F("blah", "blah", 1000, -5, 5);
    h->FillRandom("gaus", 100000);
    
    canvas->getCanvas()->Clear();
    canvas->getCanvas()->cd();
    h->Draw();
    canvas->show();
   
    

    //QObject::connect(qApp, SIGNAL(aboutToQuit()), window, SLOT(handle_close_event()));
#endif
    printf("exec...\n");
    return app.exec();
}

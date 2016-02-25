#include "simpleQT.h"

#include <TROOT.h>
#include <TH1F.h>
#include <TSystem.h>
#include <TQtWidget.h>

#include <QObject>
#include <qapplication.h>
#include <qlabel.h>
#include <q3mainwindow.h>

simpleQT *qt;


int simpleQT::main() 
{
    QApplication app(0, NULL);
    Q3MainWindow *window = new Q3MainWindow();


    printf("TQtWidget... comment this line and program works... \n");
    //TQtWidget *wid = new TQtWidget(window);
  

    printf("create label...\n");
    QLabel *lab = new QLabel(QObject::tr("My Window"), window);
    window->show();	
    app.exec();
    return 0;  // just returns to roo4star...
}

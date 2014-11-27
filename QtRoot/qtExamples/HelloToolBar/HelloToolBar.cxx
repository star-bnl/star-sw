/****************************************************************
**
** Minimal ROOT based Qt QMainWindow example
**
****************************************************************/
#include <qapplication.h>

#include "TGraph.h"
#include "TQtWidget.h"
#include "TQtToolBar.h"
#include "TCanvas.h"
#include <qtooltip.h>
#include <qmainwindow.h>
#include <qlabel.h>

int main( int argc, char **argv )
{
    QApplication *app = new QApplication(argc, argv);
    //  Terminate the application as soon as the user closes the last widget
    app->connect(app,SIGNAL(lastWindowClosed ()),app,SLOT(quit()));
    
    // Create the Qt main window
    QMainWindow *mainWindow = new QMainWindow;
    mainWindow->setCaption("ROOT-based tool bar");
    
    // Create the embedded TCanvas
    TQtWidget *MyWidget= new TQtWidget(mainWindow,"MyWidget");
    // Add some tool tip:
    MyWidget->setToolTip("Close this widget to terminate your application");
    
    // Add the embedded TCanvas to the Qt QMainWindow 
    mainWindow->setCentralWidget(MyWidget);     
    
    // Create ROOT tool bar
    mainWindow->addToolBar(new TQtToolBar(mainWindow));
    
    // Create any other Qt-widget here
    //         . . .

    // Make the the embedded TCanvas to be the current ROOT TCanvas
    MyWidget->cd();
    
    // Add some ROOT object to the current TCanvas
    TGraph *mygraph;
    float x[3] = {1,2,3};
    float y[3] = {1.5, 3.0, 4.5};
    mygraph  = new TGraph(3,x,y);
    mygraph->SetMarkerStyle(20);
    mygraph->Draw("AP");

    mainWindow->resize(600,400);
    mainWindow->show();
    
    app->exec();
    printf(" Good bye, ROOT Canvas\n");
    return 0;
}

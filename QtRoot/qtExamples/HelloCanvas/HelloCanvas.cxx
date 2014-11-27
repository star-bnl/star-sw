/****************************************************************
**
** Minimal ROOT based Qt example
**
****************************************************************/
#include <qapplication.h>

#include "TGraph.h"
#include "TQtWidget.h"
#include "TCanvas.h"
#include "TDatime.h"
#include "TAxis.h"
#include <QLabel>
int main( int argc, char **argv )
{

    QApplication *app = new QApplication(argc, argv);
    app->connect(app,SIGNAL(lastWindowClosed ()),app,SLOT(quit()));

    TQtWidget *MyWidget= new TQtWidget(0,"MyWidget");
    MyWidget->resize(300,200);

    // Add some tool tip:
    MyWidget->setToolTip("Close this widget to terminate your application");
    
    // Create any other Qt-widget here
    //         . . .

    // Make the the embedded TCanvas to be the current ROOT TCanvas
    MyWidget->GetCanvas()->SetFillColor(3);    

    TDatime datTime (2009,3,4,17,2,0);
    double timeOffset = (double) datTime.Convert();

    float x[] = {1,2,3,4,5};
    float y[] = {1.5f, 3.0f, 4.5f, 3.8f,5.2f};
    TGraph*  m_graph  = new TGraph(sizeof(x)/sizeof(float),x,y);

    m_graph->GetXaxis()->SetLabelOffset(0.04f);
    m_graph->GetXaxis()->SetLabelSize(0.02f);
    m_graph->GetXaxis()->SetTimeDisplay(1);
    m_graph->GetXaxis()->SetTimeOffset(timeOffset, "local");
    char timeFormat[] = "#splitline{%d/%b/%Y}{%H:%M:%S}";
    m_graph->GetXaxis()->SetTimeFormat(timeFormat);
    m_graph->GetYaxis()->SetLabelSize(0.04f);
    m_graph->SetTitle("Just a test");

    m_graph->SetMarkerStyle(7);
    MyWidget->Draw(m_graph,"AC*");


    //Add Qt QLabel onto the top of the ROOT TCanvas
    QLabel *label = new QLabel("<b>HelloCanvas</b> Example",MyWidget);
    label->setStyleSheet("border: 2px solid blue; border-radius: 10px; padding: 0 8px;");
    label->move(40,40);
    label->resize(148,26);
    label->setToolTip("This is a QLabel object");

    // Raise the widget on the top
    MyWidget->show();
    MyWidget->Refresh();

    // Create the png file
    MyWidget->Save("HelloCanvas.png");
    app->exec();
    printf(" Good bye, ROOT Canvas\n");
    return 0;
}

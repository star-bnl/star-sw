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
#include "qtmmlwidget.h"
#include <QTextStream>
#include <QString>
#include <QMessageBox>
#include <QDebug>
#include <QFile>
#include <QPixmap>

int main( int argc, char **argv )
{

        QApplication *app = new QApplication(argc, argv);
    app->connect(app,SIGNAL(lastWindowClosed ()),app,SLOT(quit()));

    TQtWidget *MyWidget= new TQtWidget(0,"MyWidget");
    MyWidget->resize(600,400);

    // Add some tool tip:
    MyWidget->setToolTip("Close this widget to terminate your application");
    
    // Create any other Qt-widget here
    //         . . .

    // Make the the embedded TCanvas to be the current ROOT TCanvas
    MyWidget->GetCanvas()->cd();

    TDatime datTime (2009,3,4,17,2,0);
    double timeOffset = (double) datTime.Convert();

    float x[] = {1,2,3,4,5};
    float y[] = {1.5f, 3.0f, 4.5f, 3.8f,5.2f};
    TGraph*  m_graph  = new TGraph(sizeof(x)/sizeof(float),x,y);

    m_graph->GetXaxis()->SetLabelOffset(0.04f);
    m_graph->GetXaxis()->SetLabelSize(0.02f);
    m_graph->GetXaxis()->SetTimeDisplay(1);
    m_graph->GetXaxis()->SetTimeOffset(timeOffset, "local");
    char timeFormat[] = "#splitline{%d\\/%b\\/%Y}{%H:%M:%S}";
    m_graph->GetXaxis()->SetTimeFormat(timeFormat);
    m_graph->GetYaxis()->SetLabelSize(0.04f);
    m_graph->SetTitle("Just a test");

    m_graph->SetMarkerStyle(6);
    m_graph->Draw("AC*");


    //Add Qt Label in the top the ROOT TCanvas
    QtMmlWidget *label = new QtMmlWidget(MyWidget);
    label->setBaseFontPointSize(16);
    
    QtMmlDocument *bareDoc = new QtMmlDocument;
    bareDoc->setBaseFontPointSize(16);

    QFile file("example.mml");
    if (!file.open(QIODevice::ReadOnly)) {
       QMessageBox::warning(0, "HelloMML File error",QString("Could not open \"")
		                         + "example.mml"
                               + "\": " + file.errorString());
       return 1;
    }
    QTextStream stream(&file);
    QString text = stream.readAll();
    qDebug() << "QTextStream: " << text;
    file.close();   
    QString error_msg;
    int error_line, error_column;
    bool result = label->setContent(text, &error_msg, &error_line,
						&error_column);

    if (!result) {
       QMessageBox::warning(0,"MML","Parse error: line " + QString::number(error_line)
                            + ", col " + QString::number(error_column)
                            + ": " + error_msg);
    }
    
    label->setStyleSheet("QFrame {border: 2px solid blue; border-radius: 10px; padding: 2 8px;}");
    // take in account the style sheet padding:
    label->setGeometry(70,40,label->sizeHint().width()+18,label->sizeHint().height()+ 10 );
    label->setToolTip("This is a QtMmlWidget object");
    bareDoc->setContent(text, &error_msg, &error_line,
						&error_column);
    qDebug() << bareDoc->size()<<label->sizeHint();
    // Raise the widget on the top
    MyWidget->show();
    MyWidget->Refresh();
    
    // Create the png file
    MyWidget->Save("HelloCanvas.png");
    QPixmap graphAndLabel = QPixmap::grabWidget(MyWidget);
    graphAndLabel.save("HelloCanvasMML.png");
    app->exec();
    printf(" Good bye, ROOT Canvas with MML\n");
    return 0;
}

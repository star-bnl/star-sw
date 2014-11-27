/****************************************************************
**
** Minimal ROOT based Qt example
**
****************************************************************/
#include <qapplication.h>

#include "TGraph.h"
#include "TEmbeddedPad.h"
#include <qtooltip.h>
#include <QFrame>
#include <QPixmap>
#include <QLayout>
#include <QDebug>

int main( int argc, char **argv )
{

    QApplication *app = new QApplication(argc, argv);
    app->connect(app,SIGNAL(lastWindowClosed ()),app,SLOT(quit()));
    QFrame *frame = new QFrame();
    frame->resize(800,800);
    QGridLayout *layout = new QGridLayout(frame);

    int x,y,w,h;
    frame->rect().rect(&x,&y,&w,&h);
    QPixmap graphPixmap;

    TEmbeddedPad *pad = new TEmbeddedPad("Graph","Embedded",w/3,h/3);
    pad->cd();
    // Add some ROOT object to the current TEmbeddedPad
    TGraph *mygraph;
    float xg[3] = {1,2,3};
    float yg[3] = {1.5, 3.0, 4.5};
    mygraph  = new TGraph(3,xg,yg);
    mygraph->SetMarkerStyle(20);
    mygraph->Draw("AP");
    pad->Modified(); pad->Update();pad->Print("pad.png","PNG");
    // 
    float angle = 0;
    int row,col;
    for (int i = 0; i < 4; i++) {
       switch (i) {
          case 0: row = 0, col = 1; angle =  0; break;
          case 1: row = 1, col = 2; angle = 90; break;
          case 2: row = 2, col = 1; angle = 180; break;
          case 3: row = 1, col = 0; angle = 270; break;
       };
       QWidget *w = new QWidget(frame);
       layout->addWidget(w,row,col);
       w->setAutoFillBackground (true); 
       QPalette p =  w->palette();
       QPixmap &padImage = *(QPixmap*)pad->GetHandleRotate((ULong_t)&graphPixmap,angle);
       p.setBrush(w->backgroundRole(), QBrush(padImage));
       w->setPalette(p);
       w->setToolTip(QString("angle=%1").arg(angle));
    }
    // Add some tool tip:
    frame->setToolTip("Close this widget to terminate your application");
    // Raise the widget on the top
    frame->show();
    
    // Create the png file
    QPixmap::grabWidget(frame).save("RotatePad.png","PNG");
    app->exec();
    printf(" Good bye, Rotate Pad\n");
    return 0;
}

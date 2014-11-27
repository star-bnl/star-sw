#include "histogram.h"
#include "animation.h"
#include <qapplication.h>
#include <qlayout.h>
#include <qframe.h>
#include "TQtRootSlot.h"


// Combine:
// http://artis.imag.fr/Members/Gilles.Debunne/QGLViewer/examples/animation.html
// and TQtWidget in one application

int main(int argc, char** argv)
{
  QApplication application(argc,argv);

  QFrame *frame  = new QFrame();
  Viewer *viewer = new Viewer(frame);
  Histogram *histogram = new Histogram(frame);
  QVBoxLayout * layout = new QVBoxLayout(frame);
  layout->addWidget(histogram,1);
  layout->addWidget(viewer,1);
  
  QObject::connect(viewer, SIGNAL(speed(float,float)) ,  histogram,            SLOT(fill(float,float)) );
  QObject::connect(viewer, SIGNAL(resetNeeded())      ,  histogram,            SLOT(init())            );
  QObject::connect(viewer, SIGNAL(drawFinished(bool)) ,  histogram,            SLOT(animate(bool))     );
  // Connect to the terminate slot to close both Qt and ROOT smoothly:
  QObject::connect(qApp,   SIGNAL(lastWindowClosed ()), TQtRootSlot::CintSlot(),SLOT(Terminate())      );
#if QT_VERSION < 0x040000
  application.setMainWidget(frame);
#else
  viewer->setWindowTitle("animation");
#endif

  frame->resize(3*viewer->size().width(), 12*viewer->size().height());
  histogram->create("Speed : Pos ", 30,0,0.4,30,0,0.4);
  frame->show();
  return application.exec();
}


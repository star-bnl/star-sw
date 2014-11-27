#include <qapplication.h>
#include <QMessageBox>
#include "CustomWidgets.h"
#include "TApplication.h"
#include "TQtWidget.h"

int main( int argc, char ** argv )
{
    QApplication a( argc, argv );
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,16,0)
    static struct needgraph {  
       needgraph () { 
          TQtWidget::InitRint(); 
          TApplication::NeedGraphicsLibs() ;  
          gApplication->InitializeGraphics();
       } 
    }  needgraph;
#endif

    CustomWidgets  *w = new  CustomWidgets;
    
    if (argc ==1 || (argc == 2 && QString("-h") == argv[1])){
       qWarning("Usage: CustomWidgets [-h | [-style=[windows | platinum | cgi | kde ]");
    }

    w->resize(640,480);
    w->show();  
    a.connect( w, SIGNAL( destroyed()),         &a, SLOT( quit() ) );

 return a.exec();
}

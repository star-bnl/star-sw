#include <qapplication.h>
#include <qmessagebox.h>
#include "HelloClick.h"

int main( int argc, char ** argv )
{
    QApplication a( argc, argv );
    HelloClick  *w = new  HelloClick;
    
    if (argc ==1 || (argc == 2 && QString("-h") == argv[1])){
       qWarning("Usage: HelloClick [-h | [-style=[windows | platinum | cgi | kde ]");
    }

    w->resize(640,480);
    w->show();  
    a.connect( w, SIGNAL( destroyed()),         &a, SLOT( quit() ) );

 return a.exec();
}

#include <qapplication.h>
#include <qmessagebox.h>
#include "TQtTextEditor.h"
int main( int argc, char ** argv )
{
    QApplication a( argc, argv );
    TQtTextEditor  *w = new  TQtTextEditor;
    
    if (argc ==1 || (argc == 2 && QString("-h") == argv[1])){
       qWarning("Usage: HelloRootConsole [-h | [-style=[windows | platinum | cgi | kde ]");
    }

    w->resize(640,480);
    w->show();  
    a.connect( w, SIGNAL( destroyed()),         &a, SLOT( quit() ) );

 return a.exec();
}

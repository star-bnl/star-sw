#include <qapplication.h>
#include <QFileInfo>
#include <QMessageBox>
#include "HelloFileBrowser.h"

int main( int argc, char ** argv )
{
    QApplication a( argc, argv );
    HelloFileBrowser  *w = new  HelloFileBrowser;
    
    if (argc ==1 || (argc == 2 && QString("-h") == argv[1])){
       qWarning("Usage: HelloFileBrowser [-h | [-style=[windows | platinum | cgi | kde ]");
    }

    w->resize(640,480);
    w->show();  
    a.connect( w, SIGNAL( destroyed()),         &a, SLOT( quit() ) );

 return a.exec();
}

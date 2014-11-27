#include <qapplication.h>
#include <QFileInfo>
#include <QMessageBox>
#include "TQtObjectViewFrame.h"

int main( int argc, char ** argv )
{
    QApplication a( argc, argv );
    TQtObjectViewFrame  *w = new  TQtObjectViewFrame;
    
    if (argc ==1 || (argc == 2 && QString("-h") == argv[1])){
       qWarning("Usage: HelloObjectTree [-h | [ [-style=[windows | platinum | cgi | kde ] | [myRootfile.root]]");
    } else if (QFileInfo(argv[1]).exists()) {
       w->SetRootFile(argv[1]);
       w->SetLastWorkingDir(argv[1]);
    } else {
       QString ms = QString("File %1 does not exists").arg(argv[1]);
       QMessageBox::warning(0, "ObjectView",ms);
    }

    w->resize(640,480);
    w->show();  
    a.connect( w, SIGNAL( destroyed()),         &a, SLOT( quit() ) );

 return a.exec();
}

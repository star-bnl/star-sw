#include <qapplication.h>
#include "qtrootexample1.h"

int main( int argc, char ** argv )
{
    QApplication a( argc, argv );
    qtrootexample1 w;
    w.show();
    a.connect( &a, SIGNAL( lastWindowClosed() ), &a, SLOT( quit() ) );
    return a.exec();
}

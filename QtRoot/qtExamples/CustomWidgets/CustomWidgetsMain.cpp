#include <qapplication.h>
#include "CustomWidgets.h"

int main( int argc, char ** argv )
{
    QApplication a( argc, argv );
    RootCustomWidget w;
    w.show();
    a.connect( &a, SIGNAL( lastWindowClosed() ), &a, SLOT( quit() ) );
    return a.exec();
}

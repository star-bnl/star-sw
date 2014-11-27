#include <TRint.h>
#include <QtGui/QPushButton>
int main( int argc, char **argv )
{  
    // Create an interactive ROOT application
    TRint *theApp = new TRint("Rint", &argc, argv);
    
    // Create Qt object within ROOT application
    QPushButton hello( "Hello world!", 0 );
    hello.resize( 100, 30 );
    hello.show();

    // and enter the ROOT event loop...
    theApp->Run();
    printf(" Goob bye ROOT !\n");
}

#include <qapplication.h>
#include <qfileinfo.h>
#include "TSystem.h"
#include "GeomBrowser.h"

void usage(const char *wrongPar=0,int i=-1) {
  static bool hasBeenPrinted = false;
  if (wrongPar) {
     printf(" Error: Wrong %d-th command line parameter <%s>\n", i, (const char *)wrongPar);
  }
  if (!hasBeenPrinted) {
     printf(" Interactive geometry browser: \n"
            " Usage: %s [[-f ] filename.{ root | C | iv | wrl }  ] [-help] [-style {windows | motif | cde | platinum | sgi } ] \n"
            " ------ \n\tE-mail: fine@bnl.gov\n",qApp->argv()[0]);
     hasBeenPrinted = true;
  }
}
int main( int Argc, char ** Argv )
{
    QApplication a( Argc, Argv );
    QString filename;
    QString macroFilename;
    QString rootFilename;
    QString coin3DFilename;
    bool    filepar = true;
    int fcounter = 0;
    if ( a.argc() > 1 ) {
       for ( int i = 1; i < a.argc(); i++ ) {
          QString nextpar =  a.argv()[i];
          if (nextpar.startsWith("-help",Qt::CaseInsensitive) ) 
             usage();
          else if ((fcounter==0) && nextpar.startsWith("-f")) {
             fcounter++;
             filepar = true;
          } else if ( (!nextpar.startsWith("-")) && filepar) {
               filepar = false; 
               filename = a.argv()[i];
               if ( filename.endsWith( ".C" ) )
                   macroFilename = filename;
               else if (filename.endsWith( ".root" ) )
                   rootFilename = filename;
               else if (filename.endsWith( ".iv" ) )
                   coin3DFilename = filename;
               else if (filename.endsWith( ".wrl" ) )
                   coin3DFilename = filename;
               else 
                  usage(a.argv()[i],i);
           }
           else
              usage(a.argv()[i],2*i+1);
        }
    }
    GeomBrowser &w= *new GeomBrowser() ;
    if (!filename.isEmpty()){
       QFileInfo openFile(filename);
       if (openFile.isReadable () ) {
          if (openFile.suffix().endsWith("C"))
             w.fileOpenMacro(filename);
          else if (openFile.suffix().endsWith("root")) 
             w.fileOpenRoot(filename);
          else if (openFile.suffix().endsWith("iv")) 
             w.fileOpenInventor(filename);
          else if (openFile.suffix().endsWith("wrl")) 
             w.fileOpenInventor(filename);
          
       }
    }

    w.show();
    qApp->connect( qApp, SIGNAL( lastWindowClosed() ), qApp, SLOT( quit() ) );
    return a.exec();
}

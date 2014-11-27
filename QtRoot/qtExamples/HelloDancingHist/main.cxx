/****************************************************************************
** $Id: main.cxx,v 1.2 2013/08/30 15:59:58 perev Exp $
**
** Copyright (C) 1992-2000 Trolltech AS.  All rights reserved.
**
** This file is part of an example program for Qt.  This example
** program may be used, distributed and modified without limitation.
**
*****************************************************************************/

#include "HelloDancingHist.h"
#include <qapplication.h>
#include <stdlib.h>
 
void usage()
{
    qWarning( "Usage: life [-scale scale]" );
}

int main( int argc, char **argv )
{
    QApplication a( argc, argv );

    int scale = 80;

    for ( int i = 1; i < argc; i++ ){
        QString arg = argv[i];
        if ( arg == "-scale" )
           scale = atoi( argv[++i] );
        else {
          usage();
          exit(1);
        }
    }

    if ( scale < 2 )
       scale = 2;

    HelloDancingHist *hist = new  HelloDancingHist;
    hist->setWindowTitle("Qt/Root Example - HelloDancingHist");
    hist->resize(500,500);
    hist->show();
    return a.exec();
}

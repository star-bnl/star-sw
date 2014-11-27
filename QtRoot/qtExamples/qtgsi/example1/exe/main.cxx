/****************************************************************************
** $Id: main.cxx,v 1.4 2013/08/30 16:00:09 perev Exp $
**
** Copyright (C) 1992-2000 Trolltech AS.  All rights reserved.
**
** This file is part of an example program for Qt.  This example
** program may be used, distributed and modified without limitation.
**
*****************************************************************************/

#include "example1.h"
#include <qapplication.h>
#include <stdlib.h>


void usage()
{
    qWarning( "\nUsage: qtgsitest [-style style] mode " );
    qWarning( "-----  mode = 0 Qtroot (gsi) alone " );
    qWarning( "              1 QtROOT + TBrowser" );
    qWarning( "              2 QtROOT + TBrowser + Guitest\n" );
}

int main( int argc, char **argv )
{
  // This is simplified version of $ROOTSYS/qtgsi/test/example/main.cpp
  // of a main program using the Qt-layer qtgsi compcompliant TQRootCanvas interface. 
  // Here above a variable "mode"
  // defines different programs .ie.  
  // mode 0 : Qtroot alone
  // mode 1 QtROOT + TBrowser
  // mode 2 QtROOT + TBrowser + Guitest (ROOT GUI"S examples)

   QApplication a( argc, argv );
   int mode = 0;
   if (a.argc() > 1 )  mode = atoi(a.argv()[1]); 
   else          usage();
   
   example1 qtGsiExample;
  
   qtGsiExample.execute(mode);
   QObject::connect( qApp, SIGNAL(lastWindowClosed ()), qApp, SLOT(quit()) );
   return a.exec();
}

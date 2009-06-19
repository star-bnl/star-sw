#include <sstream>
#include <iostream>
#include <fstream>

#include "TxEventLogFactory.h"
#include "TxEventLogWeb.h"
#include "TxEventLogCollector.h"

using namespace TxLogging;

TxEventLog* TxEventLogFactory::create(const char *technology)
{
   TxEventLog *log = 0;
   if (technology && (*technology=='w' || *technology=='W' ))
      log= new TxEventLogWeb;
   if (technology && (*technology=='c' || *technology=='C'|| *technology=='U'  || *technology=='u' ))
      log= new TxEventLogCollector;
   else
      log=new TxEventLogFile;
   return  log;
}

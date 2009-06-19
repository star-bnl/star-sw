#include <sstream>
#include <iostream>
#include <fstream>

#include "TxEventLogFactory.h"
#include "TxEventLogWeb.h"
#include "TxEventLogCollector.h"

using namespace TxLogging;

TxEventLog* TxEventLogFactory::create(const char *technology)
{
   if (technology && (*technology=='w' | *technology=='W'))
      return new TxEventLogWeb;
   if (technology && (*technology=='c' | *technology=='C'))
      return new TxEventLogCollector;
   else
      return new TxEventLogFile;
}

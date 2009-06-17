#include <sstream>
#include <iostream>
#include <fstream>

#include "TxEventLogFactory.h"
#include "TxEventLogWeb.h"

using namespace TxLogging;

TxEventLog* TxEventLogFactory::create(const char *technology)
{
   if (technology && (*technology=='w' | *technology=='W'))
      return new TxEventLogWeb;
   else
      return new TxEventLogFile;
}

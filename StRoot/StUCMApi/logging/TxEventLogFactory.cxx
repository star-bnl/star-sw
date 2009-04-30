#include <sstream>
#include <iostream>
#include <fstream>

#include "TxEventLogFactory.h"
#include "TxEventLogFile.h"

using namespace TxLogging;

TxEventLog* TxEventLogFactory::create(const char *technology)
{
    return new TxEventLogFile;
}

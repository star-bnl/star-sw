/*****************************************************************
 * @file TxEventLogFile.cpp
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogCollector.cpp,v 1.2 2009/06/19 20:10:52 fine Exp $
 *
 * Please see TxEventLogFile.h for more documentation.
 *****************************************************************/

#include "TxEventLogCollector.h"
#include "TxUCMCollector.h"

#include <string>
//______________________________________________________________
TxLogging::TxEventLogCollector::TxEventLogCollector()
{
  fCollector  = new TxUCMCollector;
}

//______________________________________________________________
TxLogging::TxEventLogCollector::~TxEventLogCollector ()
{
   delete fCollector; fCollector = 0;
}
//______________________________________________________________
void TxLogging::TxEventLogCollector::writeDown(const std::string& message)
{
   fCollector->processMessage(message);
}

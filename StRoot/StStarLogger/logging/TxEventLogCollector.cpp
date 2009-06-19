/*****************************************************************
 * @file TxEventLogFile.cpp
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogCollector.cpp,v 1.1 2009/06/19 20:02:31 fine Exp $
 *
 * Please see TxEventLogFile.h for more documentation.
 *****************************************************************/

#include "TxEventLogCollector.h"

#include <string>

void TxLogging::TxEventLogCollector::writeDown(const std::string& message)
{
  std::string httpstring="wget -b  -q -o /dev/null ";
  httpstring+= "-O /dev/null \'http://connery.star.bnl.gov/ucm/?m=";

  std::string qmessage = message;
  std::string searchString( "'" ); 
  std::string replaceString( "%27" );

  std::string::size_type pos = 0;
  while ( (pos = qmessage.find(searchString, pos)) != std::string::npos ) {
        qmessage.replace( pos, searchString.size(), replaceString );
        pos++;
  }
  httpstring+=qmessage;
  httpstring+="\'>/dev/null";
  system( httpstring.c_str());
}

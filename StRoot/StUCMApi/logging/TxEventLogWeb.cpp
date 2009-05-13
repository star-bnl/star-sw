/*****************************************************************
 * @file TxEventLogFile.cpp
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogWeb.cpp,v 1.2 2009/05/13 23:18:12 fine Exp $
 *
 * Please see TxEventLogFile.h for more documentation.
 *****************************************************************/

#include "TxEventLogWeb.h"

#include <string>

void TxLogging::TxEventLogWeb::writeDown(const std::string& message)
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
  httpstring+="\'";
  system( httpstring.c_str());
}

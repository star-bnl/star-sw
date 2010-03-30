/*****************************************************************
 * @file TxEventLogFile.cpp
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogWeb.cpp,v 1.3 2010/03/30 20:05:37 fine Exp $
 *
 * Please see TxEventLogFile.h for more documentation.
 *****************************************************************/

#include "TxEventLogWeb.h"

#include <string>
#include <cassert>

using namespace TxLogging;

void TxEventLogWeb::writeDown(const std::string& message)
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
	// --- 
TXEVENT_DEFAULT_IMPLEMENTAION(TxEventLogWeb)	
TXEVENT_DEFAULT_IMPLEMENTAION_2(TxEventLogWeb)	
/*****************************************************************
 * @file TxEventLogFile.cpp
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogWeb.cpp,v 1.8 2010/04/15 20:23:05 fine Exp $
 *
 * Please see TxEventLogFile.h for more documentation.
 *****************************************************************/

#include "TxEventLogWeb.h"

#include <string>
#include <cassert>

using namespace TxLogging;
namespace {
//const char *WebServiceURL="http://connery.star.bnl.gov/ucm/?m=";
const char *WebServiceURL=" http://fc3.star.bnl.gov:8080/UCMCollector/UCMCollector --post-data=\'ucm=";
}
TxEventLogWeb::TxEventLogWeb() : TxEventLogFile() { }


void TxEventLogWeb::writeDown(const std::string& message)
{
  std::string httpstring="wget -b  -q -o /dev/null ";
  httpstring+= "-O /dev/null ";
 
  httpstring+= WebServiceURL;

  std::string qmessage = message;
  std::string searchString( "'" ); 
  std::string replaceString( "%27" );

  std::string::size_type pos = 0;
  while ( (pos = qmessage.find(searchString, pos)) != std::string::npos ) {
        qmessage.replace( pos, searchString.size(), replaceString );
        pos++;
  }
  httpstring+=qmessage;
  httpstring+="\' \'>&/dev/null";
  bool delay = false;
  if (message.find(TxUCMConstants::newTask)== std::string::npos) {
      httpstring+="&";
      delay = true;
  }
  httpstring+="\'";
  system( httpstring.c_str());
  if (delay) {
    // Sleep milliSec milliseconds.
    int milliSec = 240;
    struct timeval tv;
    tv.tv_sec  = milliSec / 1000;
    tv.tv_usec = (milliSec % 1000) * 1000;
    select(0, 0, 0, 0, &tv);
  }
//  printf("%s %s \n", "----------------------------------------",httpstring.c_str());
}
	// --- 
TXEVENT_DEFAULT_IMPLEMENTAION(TxEventLogWeb)	
TXEVENT_DEFAULT_IMPLEMENTAION_2(TxEventLogWeb)	

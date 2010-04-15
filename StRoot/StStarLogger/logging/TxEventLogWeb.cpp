/*****************************************************************
 * @file TxEventLogFile.cpp
 * @author Valeri Fine
 *
 * @(#)cpp/api:$Id: TxEventLogWeb.cpp,v 1.7 2010/04/15 17:50:30 fine Exp $
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
  if (message.find(TxUCMConstants::newTask)== std::string::npos)  httpstring+="&";
  httpstring+="\'";
  system( httpstring.c_str());
//  printf("%s %s \n", "----------------------------------------",httpstring.c_str());
}
	// --- 
TXEVENT_DEFAULT_IMPLEMENTAION(TxEventLogWeb)	
TXEVENT_DEFAULT_IMPLEMENTAION_2(TxEventLogWeb)	

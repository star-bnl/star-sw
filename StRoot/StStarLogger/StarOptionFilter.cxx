/*
 * Copyright 2003,2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
 
#include "StarOptionFilter.h"
#include <log4cxx/spi/loggingevent.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/helpers/optionconverter.h>

using namespace log4cxx;
using namespace log4cxx::varia;
using namespace log4cxx::spi;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(StarOptionFilter)

String StarOptionFilter::ACCEPT_REPEAT_COUNTER  = _T("RepeatMessage");
String StarOptionFilter::STRING_TO_COUNT_OPTION =_T("StringToCount");
// String StarOptionFilter::ACCEPT_ON_MATCH_OPTION = _T("AcceptOnMatch");

//______________________________________________________________________________
StarOptionFilter::StarOptionFilter() : acceptRepeatCounter(-1),currentRepeatCounter(0)
,matchPredefinedStringOnly(false)
{
}

//______________________________________________________________________________
void StarOptionFilter::setOption(const String& option,
	const String& value)
{
	if (StringHelper::equalsIgnoreCase(option, ACCEPT_REPEAT_COUNTER))
	{
		acceptRepeatCounter = OptionConverter::toInt(value,acceptRepeatCounter);
	} 
   else if (StringHelper::equalsIgnoreCase(option,STRING_TO_COUNT_OPTION)) 
   {  
      if ( lastLoggerMessageToCompare != value)   {
          currentRepeatCounter = 0;
          lastLoggerMessageToCompare = value;  
          matchPredefinedStringOnly  = true;
      }
      if (lastLoggerMessageToCompare.empty())  
         matchPredefinedStringOnly  = false;
   }

}
//______________________________________________________________________________
void StarOptionFilter::setRepeatCounterOption(int value)      
{ 
   // value  < 0; do not count the messages
   // value  >= 0  the total number of the messsages before it is filtered out
   acceptRepeatCounter = value;  
}

//______________________________________________________________________________
Filter::FilterDecision StarOptionFilter::decide(
	const log4cxx::spi::LoggingEventPtr& event) const
{
   Filter::FilterDecision decision = Filter::NEUTRAL;
      	const String& msg = event->getRenderedMessage();
// || stringToMatch.empty())
	if( !( msg.empty() && acceptRepeatCounter < 0 ) ) {
	   if( msg.find(lastLoggerMessageToCompare) == String::npos )
   	{
         if (!matchPredefinedStringOnly) {
           currentRepeatCounter = 0;
		     lastLoggerMessageToCompare = msg;
         }
	   }
	   else 
	   { // we've got a match
		   if(currentRepeatCounter <= acceptRepeatCounter)
		   {
            currentRepeatCounter++;
			   decision = Filter::ACCEPT;
		   }
		   else
	   	{
		    	decision = Filter::DENY;
		   }
	   }
   }
//   printf(" ~~~~~~~~~~~~~~~~~~~~~~~~~~  FILETR MAKINGF DECISION %d n = %d  treschold = %d %s %d \n",decision
//         , currentRepeatCounter, acceptRepeatCounter,(const char *) msg.c_str());
   return decision;
}


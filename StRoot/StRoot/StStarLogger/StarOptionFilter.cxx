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
 
#include <cstring>
#include "StarOptionFilter.h"
#include <log4cxx/spi/loggingevent.h>
#include <log4cxx/helpers/optionconverter.h>

// #include <iostream>

using namespace log4cxx;
using namespace log4cxx::varia;
using namespace log4cxx::spi;
using namespace log4cxx::helpers;

IMPLEMENT_LOG4CXX_OBJECT(StarOptionFilter)


 String StarOptionFilter::ACCEPT_REPEAT_COUNTER  = _T("RepeatMessageQuota");
 String StarOptionFilter::TOTAL_MESSAGE_LIMIT    = _T("TotalMessagesQuota");
 String StarOptionFilter::STRING_TO_COUNT_OPTION = _T("StringToCount");


//______________________________________________________________________________
StarOptionFilter::StarOptionFilter() : acceptRepeatCounter(-1),acceptTotalCounter(-1)
      ,currentRepeatCounter(0),currentTotalCounter(0)
      ,matchPredefinedStringOnly(false)
{
}

//______________________________________________________________________________
void StarOptionFilter::setOption(const String& option,
	const String& value)
{
   // fprintf(stderr, " StarOptionFilter::setOption option = %s; value = %s\n"
   //    , option.c_str(), value.c_str());
	if (equalsIgnoreCase(option, ACCEPT_REPEAT_COUNTER))
	{
		acceptRepeatCounter = OptionConverter::toInt(value,acceptRepeatCounter);
	} 
   else if (equalsIgnoreCase(option,STRING_TO_COUNT_OPTION)) 
   {  
      if ( lastLoggerMessageToCompare != value)  {
          currentRepeatCounter = 0;
          lastLoggerMessageToCompare = value;  
          matchPredefinedStringOnly  = true;
      }
      if (lastLoggerMessageToCompare.empty())  
         matchPredefinedStringOnly  = false;
   } 
   else if (equalsIgnoreCase(option,TOTAL_MESSAGE_LIMIT)) 
   {
		acceptTotalCounter = OptionConverter::toInt(value,acceptTotalCounter);      
  }
}
//______________________________________________________________________________
void StarOptionFilter::setRepeatCounterOption(int value)      
{ 
  //  value  = -1  there is no limit
  //         >  0  the number of times the any or preselected message can be 
  //               printed out sequiencially
  // 
  //        Attn: the value zero and one have one and the same meaning
  //               0 - the message can not be printed at all
  //               1 - the message can be printed one times only

   acceptRepeatCounter = value;  
}

//______________________________________________________________________________
void StarOptionFilter::setTotalCounterOption(int value)      
{ 
  //  value  = -1  there is no limit
  //         >  0  the number of times the message can be printed out sequiencially
  // 
  //        Attn: the value zero and one have one and the same meaning
  //               0 - the message can not be printed at all
  //               1 - the message can be printed one times only

   acceptTotalCounter = value;  
}
//______________________________________________________________________________
Filter::FilterDecision StarOptionFilter::decide(
	const log4cxx::spi::LoggingEventPtr& event) const
{
   Filter::FilterDecision decision = Filter::NEUTRAL;
  	const String& msg               = event->getRenderedMessage();
   //fprintf(stderr," StarOptionFilter::decide:  %s, string quota=%d, totalQuota=%d match =%d\n"
   //      , msg.c_str(),currentRepeatCounter,currentTotalCounter, matchPredefinedStringOnly);
	if( !msg.empty() ) {
#if 1
      if ( (acceptRepeatCounter >= 0 ) || (acceptTotalCounter >= 0 ) ) 
      {
          bool count =  !matchPredefinedStringOnly 
                     || 
                       (matchPredefinedStringOnly && strstr(msg.c_str(),lastLoggerMessageToCompare.c_str())) ;
        if (count) {
            if (acceptRepeatCounter >= 0 ) currentRepeatCounter++;
            if (acceptTotalCounter  >= 0 ) currentTotalCounter++;
         } else {
            // reset the repeat counter
            currentRepeatCounter = 0;
            if (!matchPredefinedStringOnly && (acceptRepeatCounter >= 0 ) )
                lastLoggerMessageToCompare = msg;
         }    
         // we've got a match
         if( count && (acceptRepeatCounter >=0 ) && (currentRepeatCounter > acceptRepeatCounter) )
             	decision = Filter::DENY;
               
         if( count && (acceptTotalCounter  >=0)  && (currentTotalCounter  > acceptTotalCounter) ) 	
            decision = Filter::DENY;
      }      
#else           
      if (acceptRepeatCounter >= 0 )   {   
	      if( strcmp(msg.c_str(),lastLoggerMessageToCompare.c_str() ) )
      	{
            if (!matchPredefinedStringOnly) {
               currentRepeatCounter       = 2;
		         lastLoggerMessageToCompare = msg;
            }
         }
   	   else 
	      { 
           // we've got a match
		     if(currentRepeatCounter > acceptRepeatCounter) 	decision = Filter::DENY;
           currentRepeatCounter++;
	      }
      }
#endif      
   }
   return decision;
}

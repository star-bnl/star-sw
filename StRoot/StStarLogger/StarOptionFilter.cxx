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

String StarOptionFilter::ACCEPT_REPEAT_COUNTER = _T("RepeatMessage");
// String StarOptionFilter::ACCEPT_ON_MATCH_OPTION = _T("AcceptOnMatch");

StarOptionFilter::StarOptionFilter() : acceptRepeatCounter(0),currentRepeatCounter(0)
{
}

void StarOptionFilter::setOption(const String& option,
	const String& value)
{
	if (StringHelper::equalsIgnoreCase(option, ACCEPT_REPEAT_COUNTER))
	{
		acceptRepeatCounter = OptionConverter::toInt(value,acceptRepeatCounter);
	}
}

Filter::FilterDecision StarOptionFilter::decide(
	const log4cxx::spi::LoggingEventPtr& event) const
{
	const String& msg = event->getRenderedMessage();
// || stringToMatch.empty())
	if(msg.empty()) return Filter::NEUTRAL;

	if( msg.find(lastLoggerMessageToCompare) == String::npos )
	{
      ((StarOptionFilter*)this)->currentRepeatCounter = 0;
		((StarOptionFilter*)this)->lastLoggerMessageToCompare = msg;
      return Filter::NEUTRAL;
	}
	else
	{ // we've got a match
		if(acceptRepeatCounter && currentRepeatCounter < acceptRepeatCounter)
		{
         (((StarOptionFilter*)this)->acceptRepeatCounter)++;
			return Filter::ACCEPT;
		}
		else
		{
			return Filter::DENY;
		}
	}
}


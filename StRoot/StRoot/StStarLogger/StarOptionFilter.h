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
 
#ifndef _LOG4CXX_VARIA_STAR_OPTION_FILTER_H
#define _LOG4CXX_VARIA_STAR_OPTION_FILTER_H

#include "StLoggerConfig.h"
#include <log4cxx/spi/filter.h>

namespace log4cxx
{
	namespace varia
	{
		/**
		This is a simple filter to based on string matching and counter

		<p>The filter admits three options <b>StringToCount</b>,
		<b>RepeatMessageQuota</b>, and <b>TotalMessagesQuota</b>. 
      If there is a match between the value of the
		<b>StringToCount</b> option and the message of the {@link spi::LoggingEvent 
		LoggingEvent} then  the #decide method returns {@link spi::Filter#DENY DENY} 
      if the <b>RepeatMessageQuota</b> option
		value is already less then the number of the messages met or total 
      number of all messages has reached the threshold  defined by <b>TotalMessagesQuota</b> 
      option value.
      Otherwise {@link spi::Filter#NEUTRAL NEUTRAL} is returned.
          
            
      It is not recommended to define both kind of thresholds, 
      namely <b>RepeatMessageQuota</b>, and <b>TotalMessagesQuota</b> with one the same 
      filter instance. 
      Create the first filter with the <b>TotalMessagesQuota</b> only followed by 
      the chain of the filters for each pair of <b>StringToCount</b>,<b>RepeatMessageQuota</b> 
      you want to monitor.

<!--
		<p>See configuration files <a
		href="../xml/doc-files/test6.xml">test6.xml</a>, <a
		href="../xml/doc-files/test7.xml">test7.xml</a>, <a
		href="../xml/doc-files/test8.xml">test8.xml</a>, <a
		href="../xml/doc-files/test9.xml">test9.xml</a>, and <a
		href="../xml/doc-files/test10.xml">test10.xml</a> for examples of
		seeting up a <code>StarOptionFilter</code>.
-->      
		*/

		class StarOptionFilter;
		typedef helpers::ObjectPtrT<StarOptionFilter> StarOptionFilterPtr;

		class LOG4CXX_EXPORT StarOptionFilter : public spi::Filter
		{
		private:
			static String ACCEPT_REPEAT_COUNTER;
			static String STRING_TO_COUNT_OPTION;
			static String TOTAL_MESSAGE_LIMIT;
         
			        int    acceptRepeatCounter;
			        int    acceptTotalCounter;
         mutable int    currentRepeatCounter;
         mutable int    currentTotalCounter;
			mutable String lastLoggerMessageToCompare;
         bool  matchPredefinedStringOnly;

		public:
			typedef spi::Filter BASE_CLASS;
			DECLARE_LOG4CXX_OBJECT(StarOptionFilter)
			BEGIN_LOG4CXX_CAST_MAP()
				LOG4CXX_CAST_ENTRY(StarOptionFilter)
				LOG4CXX_CAST_ENTRY_CHAIN(BASE_CLASS)
			END_LOG4CXX_CAST_MAP()

			StarOptionFilter();

			/**
			Set options
			*/
			virtual void setOption(const String& option,
				const String& value);
         
			void setRepeatCounterOption(int value);
			void setTotalCounterOption(int value);

			inline void setAcceptRepeatCounter(int repeat)
				{ this->acceptRepeatCounter = repeat; }

			inline int RepeatCounter() const
				{ return acceptRepeatCounter; }

			inline int TotalCounter() const
				{ return acceptTotalCounter; }
	
			inline const String& lastLoggerMessage() const
				{ return lastLoggerMessageToCompare; }

			/**
			Returns {@link spi::Filter#NEUTRAL NEUTRAL} 
			is there is no string match.
			*/
			FilterDecision decide(const spi::LoggingEventPtr& event) const;
}; // class StarOptionFilter
	}  // namespace varia
}; // namespace log4cxx

#endif // _LOG4CXX_VARIA_STAR_OPTION_FILTER_H

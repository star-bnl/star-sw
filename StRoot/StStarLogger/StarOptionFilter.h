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

#include <log4cxx/spi/filter.h>

namespace log4cxx
{
	namespace varia
	{
		/**
		This is a very simple filter based on string matching.

		<p>The filter admits two options <b>StringToMatch</b> and
		<b>AcceptOnMatch</b>. If there is a match between the value of the
		StringToMatch option and the message of the {@link spi::LoggingEvent 
		LoggingEvent}, then the #decide method returns 
		{@link spi::Filter#ACCEPT ACCEPT} if the <b>AcceptOnMatch</b> option
		value is true, if it is false then {@link spi::Filter#DENY DENY} is 
		returned. If there is no match, {@link spi::Filter#NEUTRAL NEUTRAL}
		is returned.

		<p>See configuration files <a
		href="../xml/doc-files/test6.xml">test6.xml</a>, <a
		href="../xml/doc-files/test7.xml">test7.xml</a>, <a
		href="../xml/doc-files/test8.xml">test8.xml</a>, <a
		href="../xml/doc-files/test9.xml">test9.xml</a>, and <a
		href="../xml/doc-files/test10.xml">test10.xml</a> for examples of
		seeting up a <code>StarOptionFilter</code>.
		*/

		class StarOptionFilter;
		typedef helpers::ObjectPtrT<StarOptionFilter> StarOptionFilterPtr;

		class LOG4CXX_EXPORT StarOptionFilter : public spi::Filter
		{
		private:
			static String ACCEPT_REPEAT_COUNTER;
			static String STRING_TO_COUNT_OPTION;

			int  acceptRepeatCounter;
         mutable int  currentRepeatCounter;
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

			inline void setAcceptRepeatCounter(int repeat)
				{ this->acceptRepeatCounter = repeat; }

			inline int RepeatCounter() const
				{ return acceptRepeatCounter; }

	
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

#ifndef _LOG4CXX_ST_WS_APPENDER_H
#define _LOG4CXX_ST_WS_APPENDER_H

#include "StLoggerConfig.h"
 
#include <log4cxx/helpers/exception.h>
#include <log4cxx/appenderskeleton.h>
#include <log4cxx/spi/loggingevent.h>
#include <list>

namespace log4cxx
{
	namespace db
	{
		class StWsAppender;
		typedef helpers::ObjectPtrT<StWsAppender> StWsAppenderPtr;

		class LOG4CXX_EXPORT StWsAppender : public AppenderSkeleton
		{
		protected:			
			/**
			* ArrayList holding the buffer of Logging Events.
			*/
			std::list<spi::LoggingEventPtr> buffer;
         unsigned long fLastId;
 			
		public:				
            DECLARE_LOG4CXX_OBJECT(StWsAppender)
			BEGIN_LOG4CXX_CAST_MAP()
				LOG4CXX_CAST_ENTRY(StWsAppender)
				LOG4CXX_CAST_ENTRY_CHAIN(AppenderSkeleton)
			END_LOG4CXX_CAST_MAP()

			StWsAppender();
			virtual ~StWsAppender();
			
		    /**
		    Set options
		    */
			virtual void setOption(const String& option, const String& value);

			/**
			* Adds the event to the buffer.  When full the buffer is flushed.
			*/
			void append(const spi::LoggingEventPtr& event);
			
			/**
			* By default getLogStatement sends the event to the required Layout object.
			* The layout will format the given pattern into a workable SQL string.
			*
			* Overriding this provides direct access to the LoggingEvent
			* when constructing the logging statement.
			*
			*/
		protected:
			String getLogStatement(const spi::LoggingEventPtr& event);

#if (STAR_LOG4CXX_VERSION == 10) 
      protected:
         virtual void append(const spi::LoggingEventPtr& event, log4cxx::helpers::Pool& p);
#endif
						
		public:
			virtual void close();
						
			/**
			* StWsAppender requires a layout.
			* */
			virtual bool requiresLayout() const
				{ return true; }

		}; // class StWsAppender
    } // namespace db
}; // namespace log4cxx

#endif // _LOG4CXX_WS_APPENDER_H

/***************************************************************************
                          StUCMAppender.h  -  class StUCMAppender
                             -------------------
     begin                : Julu 2008
    copyright            : (C) 2008 by Valeri Fine
    email                : bnl.gov
 ***************************************************************************/

/***************************************************************************
 * Copyright (C) The Apache Software Foundation. All rights reserved.      *
 *                                                                         *
 * This software is published under the terms of the Apache Software       *
 * License version 1.1, a copy of which has been included with this        *
 * distribution in the LICENSE.txt file.                                   *
 ***************************************************************************/

#ifndef _LOG4CXX_UCM_APPENDER_H
#define _LOG4CXX_UCM_APPENDER_H
#ifdef   _UCMLOGGER_
#include "StLoggerConfig.h"

#include <log4cxx/helpers/exception.h>
#include <log4cxx/appenderskeleton.h>
#include <log4cxx/spi/loggingevent.h>
#include <list>

namespace TxLogging {
  class TxEventLog;
}

namespace log4cxx
{
	namespace db
	{
#if REDUNDANT_EXCEPTION
		class LOG4CXX_EXPORT UCMException : public helpers::Exception
		{
		public:
			UCMException(int code) : code(code) {}
			virtual String getMessage() { return String(); }
         ~UCMException() {}
			int code;
		};
#endif

		class StUCMAppender;
#if (STAR_LOG4CXX_VERSION != 10) 
      typedef helpers::ObjectPtrT<StUCMAppender> StUCMAppenderPtr;
#endif

		/**
		<p><b>WARNING: This version of StUCMAppender
		is very likely to be completely replaced in the future. Moreoever,
		it does not log exceptions.</b> </p>

		The StUCMAppender provides for sending log events to a database.


		<p>Each append call adds to an <code>ArrayList</code> buffer.  When
		the buffer is filled each log event is placed in a sql statement
		(configurable) and executed.

		<b>BufferSize</b>, <b>db URL</b>, <b>User</b>, & <b>Password</b> are
		configurable options in the standard log4j ways.

		<p>The <code>setSql(String sql)</code> sets the SQL statement to be
		used for logging -- this statement is sent to a
		<code>PatternLayout</code> (either created automaticly by the
		appender or added by the user).  Therefore by default all the
		conversion patterns in <code>PatternLayout</code> can be used
		inside of the statement.  (see the test cases for examples)

		<p>Overriding the {@link #getLogStatement} method allows more
		explicit control of the statement used for logging.

		<p>For use as a base class:

		<ul>

		<li>Override getConnection() to pass any connection
		you want.  Typically this is used to enable application wide
		connection pooling.

		<li>Override closeConnection -- if
		you override getConnection make sure to implement
		<code>closeConnection</code> to handle the connection you
		generated.  Typically this would return the connection to the
		pool it came from.

		<li>Override getLogStatement to
		produce specialized or dynamic statements. The default uses the
		sql option value.

		</ul>
		*/
		class LOG4CXX_EXPORT StUCMAppender : public AppenderSkeleton
		{
		protected:
			/**
			* URL of the DB for default connection handling
			*/
			String databaseURL;
			
			/**
			* User to connect as for default connection handling
			*/
			String databaseUser;
			
			/**
			* User to use for default connection handling
			*/
			String databasePassword;
			
			/**
			* Connection used by default.  The connection is opened the first time it
			* is needed and then held open until the appender is closed (usually at
			* garbage collection).  This behavior is best modified by creating a
			* sub-class and overriding the <code>getConnection</code> and
			* <code>closeConnection</code> methods.
			*/
			TxLogging::TxEventLog *connection;

         /**
          * Technology to save the logging messages 
			 */
         String technology;
         
			/**
			* Stores the string given to the pattern layout for conversion into a SQL
			* statement, eg: insert into LogTable (Thread, File, Message) values
			* ("%t", "%F", "%m")
			*
			* Be careful of quotes in your messages!
			*
			* Also see PatternLayout.
			*/
			
			/**
			* size of LoggingEvent buffer before writting to the database.
			* Default is 1.
			*/
			size_t bufferSize;
			
			/**
			* ArrayList holding the buffer of Logging Events.
			*/
			std::list<spi::LoggingEventPtr> buffer;
         unsigned long fLastId;
         bool fIsConnectionOpen;
#if (STAR_LOG4CXX_VERSION == 10) 
      protected:
         virtual void append(const spi::LoggingEventPtr& event, log4cxx::helpers::Pool& p);
#endif
 			
		public:				
         DECLARE_LOG4CXX_OBJECT(StUCMAppender)
			BEGIN_LOG4CXX_CAST_MAP()
				LOG4CXX_CAST_ENTRY(StUCMAppender)
				LOG4CXX_CAST_ENTRY_CHAIN(AppenderSkeleton)
			END_LOG4CXX_CAST_MAP()

			StUCMAppender(const char *mode="F");
			virtual ~StUCMAppender();
			
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
//			String getLogStatement(const spi::LoggingEventPtr& event) const;

			/**
			* Override this to return the connection to a pool, or to clean up the
			* resource.
			*
			* The default behavior holds a single connection open until the appender
			* is closed (typically when garbage collected).
			*/
			virtual void closeConnection();
			
			/**
			* Override this to link with your connection pooling system.
			*
			* By default this creates a single connection which is held open
			* until the object is garbage collected.
			*/
			virtual TxLogging::TxEventLog *getConnection() /*throw(SQLException)*/;
			
			/**
			* Closes the appender, flushing the buffer first then closing the default
			* connection if it is open.
			*/
		public:
			virtual void close();

			/* loops through the buffer of LoggingEvents, gets a
			* sql string from getLogStatement() and sends it to execute().
			* Errors are sent to the errorHandler.
			*
			* If a statement fails the LoggingEvent stays in the buffer!
			*/
			void flushBuffer();
         			
			/**
			* StUCMAppender requires a layout.
			* */
       /**
        Configurators call this method to determine if the appender
        requires a layout. If this method returns <code>true</code>,
        meaning that layout is required, then the configurator will
        configure an layout using the configuration information at its
        disposal.  If this method returns <code>false</code>, meaning that
        a layout is not required, then layout configuration will be
        skipped even if there is available layout configuration
        information at the disposal of the configurator..

        <p>In the rather exceptional case, where the appender
        implementation admits a layout but can also work without it, then
        the appender should return <code>true</code>.
       */
        
//			virtual bool requiresLayout()
			virtual bool requiresLayout() const
				{ return true; }
			
			inline void setUser(const String& user)
				{ databaseUser = user; }
			
			
			inline void setURL(const String& url)
				{ databaseURL = url; }
			
			
			inline void setPassword(const String& password)
				{ databasePassword = password; }
			
			
			inline void setBufferSize(size_t newBufferSize)
				{ bufferSize = newBufferSize; }
			
			inline const String& getUser() const
				{ return databaseUser; }
			
			
			inline const String& getURL() const
				{ return databaseURL; }
			
			
			inline const String& getPassword() const
				{ return databasePassword; }
			
			inline size_t getBufferSize() const
				{ return bufferSize; }
		}; // class StUCMAppender
#if (STAR_LOG4CXX_VERSION == 10) 
      LOG4CXX_PTR_DEF(StUCMAppender);
#endif
    } // namespace db
}; // namespace log4cxx
#endif
#endif // _LOG4CXX_UCM_APPENDER_H

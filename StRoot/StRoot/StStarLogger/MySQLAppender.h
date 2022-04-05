/***************************************************************************
                          MySQLAppender.h  -  class MySQLAppender
                             -------------------
    begin                : jeu mai 8 2003
    copyright            : (C) 2003 by Michael CATANZARITI
    email                : mcatan@free.fr
 ***************************************************************************/

/***************************************************************************
 * Copyright (C) The Apache Software Foundation. All rights reserved.      *
 *                                                                         *
 * This software is published under the terms of the Apache Software       *
 * License version 1.1, a copy of which has been included with this        *
 * distribution in the LICENSE.txt file.                                   *
 ***************************************************************************/

#ifndef _LOG4CXX_DB_MYSQL_APPENDER_H
#define _LOG4CXX_DB_MYSQL_APPENDER_H

#include "StLoggerConfig.h"
#if 1
// def HAVE_MYSQL
 
#include <log4cxx/helpers/exception.h>
#include <log4cxx/appenderskeleton.h>
#include <log4cxx/spi/loggingevent.h>
#include <list>
#ifdef HAVE_MS_MYSQL
#include <windows.h>
#endif

#include "mysql.h"
namespace log4cxx
{
	namespace db
	{
#if  0
		class LOG4CXX_EXPORT SQLException : public helpers::Exception
		{
		public:
			SQLException(int code) : code(code) {}
			virtual String getMessage() { return String(); }

			int code;
		};
#endif
		class MySQLAppender;
		typedef helpers::ObjectPtrT<MySQLAppender> MySQLAppenderPtr;

		/**
		<p><b>WARNING: This version of MySQLAppender
		is very likely to be completely replaced in the future. Moreoever,
		it does not log exceptions.</b> </p>

		The MySQLAppender provides for sending log events to a database.


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
#ifndef SQLHDBC
#define SQLHDBC MYSQL*
#endif
		class LOG4CXX_EXPORT MySQLAppender : public AppenderSkeleton
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
			MYSQL *connection;
			
			/**
			* Stores the string given to the pattern layout for conversion into a SQL
			* statement, eg: insert into LogTable (Thread, File, Message) values
			* ("%t", "%F", "%m")
			*
			* Be careful of quotes in your messages!
			*
			* Also see PatternLayout.
			*/
			String sqlStatement;
			
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
 			
		public:				
                        DECLARE_LOG4CXX_OBJECT(MySQLAppender)
			BEGIN_LOG4CXX_CAST_MAP()
				LOG4CXX_CAST_ENTRY(MySQLAppender)
				LOG4CXX_CAST_ENTRY_CHAIN(AppenderSkeleton)
			END_LOG4CXX_CAST_MAP()

			MySQLAppender();
			virtual ~MySQLAppender();
			
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
#if (STAR_LOG4CXX_VERSION == 10) 
      protected:
         virtual void append(const spi::LoggingEventPtr& event, log4cxx::helpers::Pool& p);
#endif

			/**
			*
			* Override this to provide an alertnate method of getting
			* connections (such as caching).  One method to fix this is to open
			* connections at the start of flushBuffer() and close them at the
			* end.  I use a connection pool outside of MySQLAppender which is
			* accessed in an override of this method.
			* */
			unsigned int execute(const String& sql) /*throw(SQLException)*/;
			
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
			virtual MYSQL *getConnection() /*throw(SQLException)*/;
			
			/**
			* Closes the appender, flushing the buffer first then closing the default
			* connection if it is open.
			*/
		public:
			virtual void close();
			
			/**
			* loops through the buffer of LoggingEvents, gets a
			* sql string from getLogStatement() and sends it to execute().
			* Errors are sent to the errorHandler.
			*
			* If a statement fails the LoggingEvent stays in the buffer!
			*/
			void flushBuffer();
			
			/**
			* MySQLAppender requires a layout.
			* */
//			virtual bool requiresLayout()
			virtual bool requiresLayout() const
				{ return true; }
			
			/**
			* Set pre-formated statement eg: insert into LogTable (msg) values ("%m")
			*/
			void setSql(const String& s);

			/**
			* Returns pre-formated statement eg: insert into LogTable (msg) values ("%m")
			*/
			inline const String& getSql() const
				{ return sqlStatement; }
			
			
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
		}; // class MySQLAppender
    } // namespace db
}; // namespace log4cxx

#endif // HAVE_MYSQL
#endif // _LOG4CXX_DB_MYSQL_APPENDER_H

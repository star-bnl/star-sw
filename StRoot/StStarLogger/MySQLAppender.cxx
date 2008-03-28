/***************************************************************************
                          MySQLappender.cpp  -  class MySQLAppender
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

#include <log4cxx/config.h>

#ifdef WIN32
#include <windows.h>
#endif

#include "MySQLAppender.h"
#include "TSystem.h"
#include "TString.h"

#if 1
// def HAVE_MySQL

#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/patternlayout.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::db;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(MySQLAppender)

//_________________________________________________________________________
MySQLAppender::MySQLAppender()
: connection(SQL_NULL_HDBC), env(SQL_NULL_HENV), bufferSize(1),fLastId(0),fIsConnectionOpen(false)
{ 
   fprintf(stderr,"MySQLAppender::MySQLAppender() \n");
}

//_________________________________________________________________________
MySQLAppender::~MySQLAppender()
{
	 fprintf(stderr,"MySQLAppender::~MySQLAppender()\n" );
    finalize();
}

//_________________________________________________________________________
void MySQLAppender::setOption(const String& option,
	const String& value)
{
	if (StringHelper::equalsIgnoreCase(option, _T("buffersize")))
	{
		setBufferSize((size_t)OptionConverter::toInt(value, 1));
	}
	else if (StringHelper::equalsIgnoreCase(option, _T("password")))
	{
		setPassword(value);
	}
	else if (StringHelper::equalsIgnoreCase(option, _T("sql")))
	{
		setSql(value);
	}
	else if (StringHelper::equalsIgnoreCase(option, _T("url"))
		|| StringHelper::equalsIgnoreCase(option, _T("dns")))
	{
		setURL(value);
	}
	else if (StringHelper::equalsIgnoreCase(option, _T("user")))
	{
		setUser(value);
	}
	else
	{
		AppenderSkeleton::setOption(name, value);
	}
}

//_________________________________________________________________________
void MySQLAppender::append(const spi::LoggingEventPtr& event)
{
	buffer.push_back(event);
	
	if (buffer.size() >= bufferSize)
		flushBuffer();
}

//_________________________________________________________________________
// String MySQLAppender::getLogStatement(const spi::LoggingEventPtr& event) const
String MySQLAppender::getLogStatement(const spi::LoggingEventPtr& event)
{
	StringBuffer sbuf;
	((MySQLAppender*)this)->getLayout()->format(sbuf, event);
	return sbuf.str();
}

//_________________________________________________________________________
void MySQLAppender::execute(const String& sql)
{
	SQLRETURN ret;
	SQLHDBC con = SQL_NULL_HDBC;
	SQLHSTMT stmt = SQL_NULL_HSTMT;


#ifdef TRY
try
#endif
	{
		con = getConnection();
      fprintf(stderr,"MYSQL:  ---- >  execute the MySQL query <%s> \n\n",sql.c_str());
//       String query = "INSERT INTO StarLogger VALUES (\"";
//       query += sql;
//       query += "\");";

      String query = sql; // 
		if (mysql_query(con,query.c_str())) {
         printf("QUERY: %s  \n",mysql_error(connection));
       }  else {
//       
//         unsigned int last = mysql_insert_id(con);
//         if (last && !fLastId) fLastId = last;
//         fprintf(stderr," ID = %d\n",fLastId);
       }

      
      ret = 1;   

//		ret = SQLAllocHandle(SQL_HANDLE_STMT, con, &stmt);
		if (ret < 0)
		{
			SQLFreeHandle(SQL_HANDLE_STMT, stmt);
	      closeConnection(con);
	      throw SQLException(ret);
		}

#if defined(HAVE_MS_MySQL)
		ret = SQLExecDirect(stmt, (SQLTCHAR *)sql.c_str(), SQL_NTS);
#else
		USES_CONVERSION;
		ret = SQLExecDirect(stmt, (SQLCHAR *)T2A(sql.c_str()), SQL_NTS);
#endif
		if (ret < 0)
		{
			SQLFreeHandle(SQL_HANDLE_STMT, stmt);
	      closeConnection(con);
			throw SQLException(ret);
		}
	} 
#ifdef TRY   
	catch (SQLException& e)
	{
		if (stmt != SQL_NULL_HSTMT)
		{
			SQLFreeHandle(SQL_HANDLE_STMT, stmt);
		}
      fprintf(stderr," Catching the execute exceptions: %s\n",(const char*)sql.c_str());
	   closeConnection(con);
		throw e;
	}
#endif   
 
	SQLFreeHandle(SQL_HANDLE_STMT, stmt);
	closeConnection(con);
	
	//tcout << _T("Execute: ") << sql << std::endl;
}

//_________________________________________________________________________
/* The default behavior holds a single connection open until the appender
is closed (typically when garbage collected).*/
void MySQLAppender::closeConnection(SQLHDBC con)
{
  if (fIsConnectionOpen ) {
     fprintf(stderr,"\n ++++++++ ----> closing the connection\n");
     mysql_close(connection); connection = 0;
     fIsConnectionOpen = false;
  }
}

//_________________________________________________________________________
MYSQL *MySQLAppender::getConnection()
{
	SQLRETURN ret;
   
   if (!fIsConnectionOpen) {
   
     if ( !(connection= mysql_init(connection)) ) 
         printf("No init connection \n");
     else {    

         const char *host   = "heston.star.bnl.gov";
         const char *user   = "StarLogger";
         const char *passwd = "logger";
         const char *db     = "logger";
         unsigned int port  = 3306;
          fprintf(stderr,"MYSQL:  ---- >  Establishing MySQL connection open %d \n\n", fIsConnectionOpen);
         if (!(connection = mysql_real_connect(connection
                     , host
                     , user
                     , passwd
                     , db
                     , port
                     , 0,0
                     )))
         {
                     printf("No connection: %s  \n",mysql_error(connection));
                     
         } else {
            fIsConnectionOpen = true;
         }
      }
   }


#if 0
	if (env == SQL_NULL_HENV)
	{
		ret = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env);
		if (ret < 0)
		{
			env = SQL_NULL_HENV;
			throw SQLException(ret);
		}
		
		ret = SQLSetEnvAttr(env, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) SQL_OV_ODBC3, SQL_IS_INTEGER);
		if (ret < 0)
		{
			SQLFreeHandle(SQL_HANDLE_ENV, env);
			env = SQL_NULL_HENV;
			throw SQLException(ret);
		}
	}
	
	if (connection == SQL_NULL_HDBC)
	{
		ret = SQLAllocHandle(SQL_HANDLE_DBC, env, &connection);
		if (ret < 0)
		{
			connection = SQL_NULL_HDBC;
			throw SQLException(ret);
		}


#if defined(HAVE_MS_MySQL)
		ret = SQLConnect(connection,
			(SQLTCHAR *)databaseURL.c_str(), SQL_NTS,
			(SQLTCHAR *)databaseUser.c_str(), SQL_NTS,
			(SQLTCHAR *)databasePassword.c_str(), SQL_NTS);
#else
		USES_CONVERSION;
		std::string URL = T2A(databaseURL.c_str());
		std::string user = T2A(databaseUser.c_str());
		std::string password = T2A(databasePassword.c_str());
      tcout << _T("getConnection: ") << _T("URL=") << URL << _T(": password=") << databasePassword << std::endl;
//      fprintf(stderr," %s:%s -  %s\n", __FUNCTION__, __LINE__, (const char *)URL.c_str());
		ret = SQLConnect(connection,
			(SQLCHAR *)URL.c_str(), SQL_NTS,
			(SQLCHAR *)user.c_str(), SQL_NTS,
			(SQLCHAR *)password.c_str(), SQL_NTS);
#endif
		if (ret < 0)
		{
         tcout << _T("getConnection: ") << _T("ret=") << ret  << std::endl;
			SQLFreeHandle(SQL_HANDLE_DBC, connection);
			connection = SQL_NULL_HDBC;
			throw SQLException(ret);
		}
	}
#endif 	
	return connection;
}

//_________________________________________________________________________
void MySQLAppender::close()
{
	try
	{
		flushBuffer();
	} 
	catch (SQLException& e)
	{
		errorHandler->error(_T("Error closing connection"), 
			e, ErrorCode::GENERIC_FAILURE);
	}
#if 0
	if (connection != SQL_NULL_HDBC)
	{
		SQLDisconnect(connection);
		SQLFreeHandle(SQL_HANDLE_DBC, connection);
	}
	
	if (env != SQL_NULL_HENV)
	{
		SQLFreeHandle(SQL_HANDLE_ENV, env);
	}
#endif
   closeConnection(connection);
	this->closed = true;
}
//_________________________________________________________________________
void ReplaceVariable(TString &string, const char *var)
{
// replace the $VAR with its value if any
   TString spec;
   const char *varValue = gSystem->Getenv(var);
   if (!varValue) {
   // Special cases
      spec = var;
      if (spec == "REQUESTID") {
          spec.Form("%d",gSystem->GetPid());
          varValue= spec.Data();
      } else if (spec == "PROCESSID") {
          spec.Form("%d",0);
          varValue= spec.Data();
      }
   }

   if (varValue) {
      TString fullName = "$";  fullName += var;
      // fullName.ToUpper();
      string.ReplaceAll(fullName,varValue);
   }
}
//_________________________________________________________________________
void MySQLAppender::flushBuffer()
{
	//Do the actual logging
	//removes.ensureCapacity(buffer.size());
   static bool TaskEntryDone = false;
	std::list<spi::LoggingEventPtr>::iterator i;
	for (i = buffer.begin(); i != buffer.end(); i++)
	{
	  TString expandCommand;
     if (!TaskEntryDone) {
   	  try
	  	  {
			  const LoggingEventPtr& logEvent = *i;
			  String sql;  
         
///--- Task description         
         
         expandCommand ="INSERT DELAYED IGNORE  TaskDescription (taskId, jobID_MD5, nProcesses, submissionTime, time, TaskUser,JobName,JobDescription,TaskJobUser)"
         " VALUES  ( DEFAULT, \"$REQUESTID\", \"$SUMS_nProcesses\",\"$SUBMIT_TIME\",DEFAULT,\"$SUMS_USER\",\"$SUMS_name\",\"Test Task\",\"$SUMS_AUTHENTICATED_USER\");";
// Edit meta symbols
//-----------------------
//  $hostid        = $HOSTNAME            
//  $JobUser       = $USER
//  $SUMSJobId     = $REQUESTID 
//  $SUMSProcessID = $PROCESSID

           ReplaceVariable(expandCommand, "REQUESTID");
           ReplaceVariable(expandCommand, "SUMS_nProcesses");
           ReplaceVariable(expandCommand, "SUBMIT_TIME");
           
           ReplaceVariable(expandCommand, "SUMS_name");
           ReplaceVariable(expandCommand, "SUMS_USER");
           ReplaceVariable(expandCommand, "SUMS_AUTHENTICATED_USER");
           sql = expandCommand.Data();
           TaskEntryDone = true;
		  	  execute(sql);
        }
         
		  catch (SQLException& e)
		  {
	        TaskEntryDone = false;
			  errorHandler->error(_T("Failed to execute TaskDescription sql"), e,
				  ErrorCode::FLUSH_FAILURE);
		  }
   	  try
	  	  {
			  const LoggingEventPtr& logEvent = *i;
			  String sql;  
//--- Job description         

         expandCommand ="INSERT DELAYED IGNORE INTO JobDescription SET ";

         expandCommand +=  "taskId = (SELECT taskId FROM TaskDescription WHERE  jobID_MD5=\"$REQUESTID\")";
                           expandCommand += ", ";                  
         expandCommand += "jobID_MD5=\"$REQUESTID\"";
                           expandCommand += ", ";
         expandCommand += "processID=\"$PROCESSID\"";
                           expandCommand += ", ";
         expandCommand +=  "node=\"$HOSTNAME\"";
                           expandCommand += ", ";
         expandCommand +=  "JobUser=\"$USER\"";
                           expandCommand += "; ";
// Edit meta symbols
//-----------------------
//  $hostid        = $HOSTNAME            
//  $JobUser       = $USER
//  $SUMSJobId     = $REQUESTID 
//  $SUMSProcessID = $PROCESSID

           ReplaceVariable(expandCommand, "USER");
           ReplaceVariable(expandCommand, "HOSTNAME");
           ReplaceVariable(expandCommand, "REQUESTID");
           ReplaceVariable(expandCommand, "PROCESSID");
           sql = expandCommand.Data();
           TaskEntryDone = true;
			  execute(sql);
 		  }
		  catch (SQLException& e)
		  {
	        TaskEntryDone = false;
  		       errorHandler->error(_T("Failed to execute JobDescription sql"), e,
				  ErrorCode::FLUSH_FAILURE);
		  }

      }
      // -------------------
  		try
		{
			const LoggingEventPtr& logEvent = *i;
			String sql = getLogStatement(logEvent);
         expandCommand = sql.c_str();
         
         ReplaceVariable(expandCommand, "REQUESTID");
         ReplaceVariable(expandCommand, "PROCESSID");
         
         sql = expandCommand.Data();
			execute(sql);                  
		}
		catch (SQLException& e)
		{
			errorHandler->error(_T("Failed to excute sql"), e,
				ErrorCode::FLUSH_FAILURE);
		}

	}
	
	// clear the buffer of reported events
	buffer.clear();
}

//_________________________________________________________________________
void MySQLAppender::setSql(const String& s)
{
	sqlStatement = s;
	if (getLayout() == 0)
	{
		this->setLayout(new PatternLayout(s));
	}
	else
	{
		PatternLayoutPtr patternLayout = this->getLayout();
		if (patternLayout != 0)
		{
			patternLayout->setConversionPattern(s);
		}
	}
}

#endif //HAVE_MySQL

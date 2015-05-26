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
#include <log4cxx/patternlayout.h>

using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::db;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(MySQLAppender)

//_________________________________________________________________________
MySQLAppender::MySQLAppender()
: connection(0), bufferSize(5),fLastId(0),fIsConnectionOpen(false)
{ 
  // fprintf(stderr,"MySQLAppender::MySQLAppender() \n");
}

//_________________________________________________________________________
MySQLAppender::~MySQLAppender()
{
	 // fprintf(stderr,"MySQLAppender::~MySQLAppender()\n" );
    finalize();
}

//_________________________________________________________________________
void MySQLAppender::setOption(const String& option,
	const String& value)
{
	if (equalsIgnoreCase(option, _T("buffersize")))
	{
		setBufferSize((size_t)OptionConverter::toInt(value, 1));
	}
	else if (equalsIgnoreCase(option, _T("password")))
	{
		setPassword(value);
	}
	else if (equalsIgnoreCase(option, _T("sql")))
	{
		setSql(value);
	}
	else if (equalsIgnoreCase(option, _T("url"))
		|| equalsIgnoreCase(option, _T("dns")))
	{
		setURL(value);
	}
	else if (equalsIgnoreCase(option, _T("user")))
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
#if (STAR_LOG4CXX_VERSION == 9)
       StringBuffer sbuf;
       ((MySQLAppender*)this)->getLayout()->format(sbuf, event);
       return sbuf.str();
#else
	String sbuf;
	((MySQLAppender*)this)->getLayout()->format(sbuf, event,pool);
	return sbuf;
#endif
}

//_________________________________________________________________________
unsigned int  MySQLAppender::execute(const String& sql)
{
	unsigned int ret=1;
   if (getConnection()) {
//      fprintf(stderr,"MYSQL:  ---- >  execute the MySQL query <%s> \n\n",sql.c_str());
//          String query = "INSERT INTO StarLogger VALUES (\"";
//          query += sql;
//          query += "\");";

      String query = sql; // 
      if (( ret = mysql_query(connection,query.c_str()) )) {
          fprintf(stderr, "MYSQL QUERY: %s  \n",mysql_error(connection));
      }  else {
//        
//         unsigned int last = mysql_insert_id(connection);
//         if (last && !fLastId) fLastId = last;
//         fprintf(stderr," ID = %d\n",fLastId);
      }
    }
//    fprintf(stderr,"MYSQL:  ---- >  return=%d \n",ret);
	 return ret;
	//tcout << _T("Execute: ") << sql << std::endl;
}

//_________________________________________________________________________
/* The default behavior holds a single connection open until the appender is closed (typically when garbage collected). */
void MySQLAppender::closeConnection()
{
  if (fIsConnectionOpen) {
  //   fprintf(stderr," ++++++++ ----> closing the connection %p\n", (void *)connection);
     mysql_close(connection); 
     if (mysql_errno(connection))   fprintf(stderr,"MYSQL close ERROR %s  \n",mysql_error(connection));
     connection = 0;
     fIsConnectionOpen = false;
  }
}

//_________________________________________________________________________
MYSQL *MySQLAppender::getConnection()
{   
   if (!fIsConnectionOpen) {
   
     if ( !(connection= mysql_init(connection)) ) {
         fprintf(stderr,"MYSQL:  ---- > No init connection \n");
     } else {    

         const char *host   = "heston.star.bnl.gov";
         const char *user   = "StarLogger";
         const char *passwd = "logger";
         const char *db     = "logger";
         unsigned int port  = 3306;
//         fprintf(stderr,"MYSQL:  ---- >  Establishing MySQL connection open %d \n", fIsConnectionOpen);
         if (!(mysql_real_connect(connection
                     , host
                     , user
                     , passwd
                     , db
                     , port
                     , 0,0
                     )))
         {
             fprintf(stderr, "MYSQL:  ---- > No connection: %s  \n",mysql_error(connection));
             connection = 0;
             fIsConnectionOpen = false;
         } else {
            fIsConnectionOpen = true;
         }
      }
   }
	return connection;
}

//_________________________________________________________________________
void MySQLAppender::close()
{
   flushBuffer();
   closeConnection();
	this->closed = true;
}
//_________________________________________________________________________
static void ReplaceVariable(TString &string, const char *var)
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
      } else if (spec == "JOBINDEX") {
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
   if ( getConnection()) {
      for (i = buffer.begin(); i != buffer.end(); i++)
	   {
	     TString expandCommand;
        String sql;  
        if (!TaskEntryDone) {
         
///--- Task description         
           expandCommand =         
//         expandCommand ="INSERT         IGNORE  TaskDescription (taskId, jobID_MD5, nProcesses, submissionTime, time, TaskUser,JobName,JobDescription,TaskJobUser)"
#ifdef  OLDTABLE
           "INSERT DELAYED IGNORE  TaskDescription (TaskDescriptionID, TaskRequestID_MD5, TaskSize, TaskRemainSize, EntryTime, UpdateTime, TaskUser,TaskDescription,TaskCredential,BrokerID)"
           " VALUES  ( DEFAULT, \"$REQUESTID\", \"$SUMS_nProcesses\",\"$SUMS_nProcesses\",\"$SUBMIT_TIME\",DEFAULT,\"$SUMS_USER\",\"$SUMS_name\",\"$SUMS_AUTHENTICATED_USER\",\"SUMS\");";
#else
           "INSERT DELAYED IGNORE  Tasks (taskID, brokerTaskID, taskName, taskSize, taskRemainSize, submitTime, updateTime, requesterID,taskDescription)"
           " VALUES  ( DEFAULT, \"$REQUESTID\", \"Short name of task\", \"$SUMS_nProcesses\",\"$SUMS_nProcesses\",\"$SUBMIT_TIME\",DEFAULT,\"$SUMS_USER\",\"$SUMS_name\");";
#endif                      
// Edit meta symbols
//-----------------------
//  $hostid        = $HOSTNAME            
//  $JobUser       = $USER
//  $SUMSJobId     = $REQUESTID 
//  $SUMSProcessID = $JOBINDEX (was $PROCESSID)

             ReplaceVariable(expandCommand, "REQUESTID");
             ReplaceVariable(expandCommand, "SUMS_nProcesses");
             ReplaceVariable(expandCommand, "SUBMIT_TIME");
           
             ReplaceVariable(expandCommand, "SUMS_name");
             ReplaceVariable(expandCommand, "SUMS_USER");
             ReplaceVariable(expandCommand, "SUMS_AUTHENTICATED_USER");
             sql = expandCommand.Data();
		       if (!execute(sql))  TaskEntryDone = true;
       }
// -- TaskDescription block 
       if (TaskEntryDone) {
//--- Job description         

//         expandCommand ="INSERT         IGNORE INTO JobDescription SET ";
          
#ifdef  OLDTABLE
           expandCommand ="INSERT DELAYED IGNORE INTO JobDescription SET ";

           expandCommand +=  "TaskDescriptionID = (SELECT TaskDescriptionID FROM TaskDescription WHERE  TaskRequestID_MD5=\"$REQUESTID\")";
                             expandCommand += ", ";                  
           expandCommand += "TaskRequestID_MD5=\"$REQUESTID\"";
                             expandCommand += ", ";
           expandCommand += "BrokerProcessID=\"$JOBINDEX\"";
                             expandCommand += ", ";
           expandCommand +=  "JobLocationURL=\"$HOSTNAME\"";
                             expandCommand += ", ";
           expandCommand +=  "JobUser=\"$USER\"";
                             expandCommand += "; ";
#else
           expandCommand ="INSERT DELAYED IGNORE INTO Jobs SET ";

           expandCommand +=  "taskID = (SELECT taskID FROM Tasks WHERE  brokerTaskID=\"$REQUESTID\")";
                             expandCommand += ", ";                  
           expandCommand += "brokerJobID=\"$JOBINDEX\"";
                             expandCommand += ", ";
           expandCommand += "startTime=NOW()";
                             expandCommand += ", ";
           expandCommand +=  "nodeLocation=\"$HOSTNAME\"";
                             expandCommand += ", ";
           expandCommand +=  "stateID=\"4\"";  // (4, 'Active', 'Scheduler running job', 'ucmAdmin', '2007-07-12 10:25:35')
                             expandCommand += ", ";
           expandCommand +=  "executionUserName=\"$USER\"";
                             expandCommand += "; ";
#endif                                                          
// Edit meta symbols
//-----------------------
//  $hostid        = $HOSTNAME            
//  $JobUser       = $USER
//  $SUMSJobId     = $REQUESTID 
//  $SUMSProcessID = $JOBINDEX (was $PROCESSID)

             ReplaceVariable(expandCommand, "USER");
             ReplaceVariable(expandCommand, "HOSTNAME");
             ReplaceVariable(expandCommand, "REQUESTID");
             ReplaceVariable(expandCommand, "JOBINDEX");
             sql = expandCommand.Data();
             if (!execute(sql) ) {
 		  
        
// Job tracking block
			       const LoggingEventPtr& logEvent = *i;
			       String sql = getLogStatement(logEvent);
                expandCommand = sql.c_str();
                // fprintf(stderr," MYSQL QUERY: <%s>\n", sql.c_str());         
                ReplaceVariable(expandCommand, "REQUESTID");
                ReplaceVariable(expandCommand, "JOBINDEX");
         
                sql = expandCommand.Data();
			       if (!execute(sql)) {
#ifdef  NEWTABLE_EXPANSION
                  expandCommand = "UPDATE LOW_PRIORITY IGNORE Jobs SET updateTime=NOW() WHERE brokerJobID=\"$JOBINDEX\" AND taskID=(SELECT taskID FROM Tasks WHERE brokerTaskID=\"$REQUESTID\");";
                  ReplaceVariable(expandCommand, "REQUESTID");
                  ReplaceVariable(expandCommand, "JOBINDEX");
			         if (execute(sql)) {
                     fprintf(stderr," MYSQL ----> can not update the Jobs record%s \n", expandCommand.c_str());
                  }
#endif                  
                } else {
                   // clear the buffer of reported events
     	             fprintf(stderr," MYSQL ----> skip and lose event \n");
                } 
            }
         }
      }
      buffer.clear();
   } 
   closeConnection();	
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
#if (STAR_LOG4CXX_VERSION == 10)
//_________________________________________________________________________
void MySQLAppender::append(const spi::LoggingEventPtr& event, log4cxx::helpers::Pool& p)
{
    append(event);
}
#endif
#endif //HAVE_MySQL

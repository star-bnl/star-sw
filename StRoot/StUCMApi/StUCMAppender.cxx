/***************************************************************************
                          StUCMAppender.cpp  -  class StUCMAppender
                             -------------------
    begin                : 2007
    copyright            : (C) 2007 by Valeri Fine
    email                :  fine@bnl.gov
 ***************************************************************************/
#ifdef _UCMLOGGER_
#include <log4cxx/config.h>


#include "StUCMAppender.h"
#include "TSystem.h"
#include "TString.h"
#include "TObjString.h"

#include "StUCMApi/TxEventLog.h"

#include <log4cxx/helpers/loglog.h>
#include <log4cxx/level.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/helpers/stringhelper.h>
#include <log4cxx/patternlayout.h>
#include <data/TxUCMException.h>
#include "TObjArray.h"
using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::db;
using namespace log4cxx::spi;
using namespace TxTrackingAPI;

IMPLEMENT_LOG4CXX_OBJECT(StUCMAppender)

//_________________________________________________________________________
StUCMAppender::StUCMAppender()
: connection(0), bufferSize(1),fLastId(0),fIsConnectionOpen(false)
{ 
   fprintf(stderr,"StUCMAppender::StUCMAppender() \n");
}

//_________________________________________________________________________
StUCMAppender::~StUCMAppender()
{
	 // fprintf(stderr,"StUCMAppender::~StUCMAppender()\n" );
    finalize();
    if (connection) { delete connection; connection = 0; }
}

//_________________________________________________________________________
void StUCMAppender::setOption(const String& option,
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
void StUCMAppender::append(const spi::LoggingEventPtr& event)
{
	buffer.push_back(event);
	
	if (buffer.size() >= bufferSize)
		flushBuffer();
}

//_________________________________________________________________________
// String StUCMAppender::getLogStatement(const spi::LoggingEventPtr& event) const
String StUCMAppender::getLogStatement(const spi::LoggingEventPtr& event)
{
	StringBuffer sbuf;
	((StUCMAppender*)this)->getLayout()->format(sbuf, event);
	return sbuf.str();
}

//_________________________________________________________________________
/* The default behavior holds a single connection open until the appender is closed (typically when garbage collected). */
void StUCMAppender::closeConnection()
{
  if (fIsConnectionOpen && connection) {
     try {
        delete connection; 
     }  catch (const TxUCMException& e) {
           fprintf(stderr,"StUCMAppender::closeConnection() %s \n"
                , e.getDescription().c_str());
     }
  }
  connection = 0;
  fIsConnectionOpen = false;
}

//_________________________________________________________________________
TxEventLog *StUCMAppender::getConnection()
{   
   if (!fIsConnectionOpen) {
   
     if (!connection) {
        try {
           // Generates the UCM_STORE_INFO
          // setenv UCM_STORE_INFO 
          // "mysql:StarLogger(logger)@heston.star.bnl.gov:3306/logger"
         const char *host   = "heston.star.bnl.gov";
         const char *user   = "StarLogger";
         const char *passwd = "logger";
         const char *db     = "logger";
      //   unsigned int port  = 3306;
         const char *port   = "3306";
         std::string ucmStore = "mysql";
                     ucmStore += ":";
                     ucmStore += user;
                     ucmStore +="("; ucmStore += passwd; ucmStore +=")";
                     ucmStore += "@";
                     ucmStore += host;
                     ucmStore += ":";
                     ucmStore += port;
                     ucmStore += "/";
                     ucmStore += db;
          if ( getenv("JOBINDEX") && getenv("REQUESTID") ) {
               const char *JOBINDEX = getenv("JOBINDEX");
                std::string UCMJOB   =  getenv("REQUESTID");
               UCMJOB +=JOBINDEX;
                fprintf(stderr,"StUCMAppender::getConnection() about to open the connection %s for JOBINDEX \n"
                        , ucmStore.c_str(),JOBINDEX);
                 connection = new TxEventLog(ucmStore.c_str(),JOBINDEX);

           // However, what we really need is:
           // const char *REQUESTID = getenv("REQUESTID");
           // const char *PROCESSID = getenv("PROCESSID");
           // connection = new TxEventLog(ucmStore.c_str(),REQUESTID,JOBINDEX);
              } else {
                 fprintf(stderr,"StUCMAppender::getConnection() no JOBINDEX/REQUESTID was provided \n");
                 connection = 0;
              }
        } catch (const TxUCMException& e) {
           fprintf(stderr,"Exception:  StUCMAppender::getConnection() %s \n"
                , e.getDescription().c_str());
        connection = 0;
     }
    }
   }
	return connection;
}

//_________________________________________________________________________
void StUCMAppender::close()
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
void StUCMAppender::flushBuffer()
{
   //Do the actual logging
   std::list<spi::LoggingEventPtr>::iterator i;
   if ( getConnection()) {
      for (i = buffer.begin(); i != buffer.end(); i++)
      {
         // Job tracking block
         const LoggingEventPtr& logEvent = *i;
         const LevelPtr &level = logEvent->getLevel();
         TxEventLog::Level trackingLevel =TxEventLog::LEVEL_INFO;
         if        (level == Level::FATAL) {
              trackingLevel = TxEventLog::LEVEL_FATAL;
         } else if (level == Level::ERROR) {
              trackingLevel = TxEventLog::LEVEL_ERROR;
         } else if (level == Level::WARN)  {
              trackingLevel = TxEventLog::LEVEL_WARNING;
         } else if (level == Level::DEBUG) {
              trackingLevel = TxEventLog::LEVEL_DEBUG;
         };
         // void logUserEvent(Stage stage, Level level, const std::string& userContext, 
         //      const std::string& userKey, const std::string& userMsg);
         String sql = getLogStatement(logEvent);
         TString userKeys = sql.c_str();
         TObjArray *pair = userKeys.Tokenize(",");
         // Parse the statement
         TIter next(pair);
         TObjString *nextPair = 0;
         int keyCounter=0; // to workaround of the ROOT error
         TString ucmParamters[3];
         while ( (nextPair = (TObjString *)next()) && (keyCounter<3)) {
             assert(nextPair);
             TString nextString = nextPair->String();
             TObjArray &keyValue = *nextString.Tokenize("=");
             // expect:
             // StageID='1',MessageKey='ProgSize',MessageValue='419'
             // More srobust parser should be added later on
             ucmParamters[keyCounter]  = ((TObjString *)keyValue[1])->String().Strip();
             delete &keyValue;
             keyCounter++;
          }
          String context = logEvent->getNDC();
          // Map Stage tp TxEventLog Stage
          ucmParamters[0].ReplaceAll("'","");ucmParamters[1].ReplaceAll("'","");ucmParamters[2].ReplaceAll("'","");
          int ucmStage = ucmParamters[0].Atoi();
          assert (ucmStage >=TxEventLog::START && ucmStage <=TxEventLog::END);
          try {
               connection->logUserEvent(  TxEventLog::Stage(ucmStage)
                                        , trackingLevel
                                        , context
                                        , ucmParamters[1].Data()
                                        , ucmParamters[2].Data());
          } catch (const TxUCMException& e) {
              fprintf(stderr,"Exception:  StUCMAppender::flushBuffer() %s \n"
                  , e.getDescription().c_str());
          }
          pair->Delete();
          delete pair;
      }
      buffer.clear();
   }
   closeConnection();	
}
#endif

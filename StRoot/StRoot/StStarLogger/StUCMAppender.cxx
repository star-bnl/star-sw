/***************************************************************************
                          StUCMAppender.cpp  -  class StUCMAppender
                             -------------------
    begin                : 2007
    copyright            : (C) 2007 by Valeri Fine
    email                :  fine@bnl.gov
 ***************************************************************************/
#include <cstdlib>
#include <cassert>
#include "StUCMAppender.h"
#include "TSystem.h"
#include "TString.h"
#include "TObjString.h"

#include "StStarLogger/logging/TxEventLog.h"
#include "StStarLogger/logging/TxEventLogFactory.h"
//#include "StUCMApi/TxEventLog.h"

#include <log4cxx/helpers/loglog.h>
#include <log4cxx/level.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/patternlayout.h>
// #include <data/TxUCMException.h>
#include "TObjArray.h"
using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::db;
using namespace log4cxx::spi;
using namespace TxLogging;

static int lockUcm  =0;

IMPLEMENT_LOG4CXX_OBJECT(StUCMAppender)
//_________________________________________________________________________
StUCMAppender::StUCMAppender(const char *mode)
: connection(0),technology(mode), bufferSize(1),fLastId(0),fIsConnectionOpen(false)
{ 
   fprintf(stderr,"StUCMAppender::StUCMAppender() %p %s %i\n", this, mode, lockUcm);
   lockUcm++;
}

//_________________________________________________________________________
StUCMAppender::~StUCMAppender()
{
	 // fprintf(stderr,"StUCMAppender::~StUCMAppender()\n" );
    finalize();
    if (connection) { 
       delete connection; 
       connection = 0; 
    }
}

//_________________________________________________________________________
void StUCMAppender::setOption(const String& option,
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
void StUCMAppender::append(const spi::LoggingEventPtr& event)
{
   if (!this->closed) {
      buffer.push_back(event);

      if (buffer.size() >= bufferSize)
		  flushBuffer();
   }
}

//_________________________________________________________________________
// String StUCMAppender::getLogStatement(const spi::LoggingEventPtr& event) const
String StUCMAppender::getLogStatement(const spi::LoggingEventPtr& event)
{
#if (STAR_LOG4CXX_VERSION == 9)
  StringBuffer sbuf;
  ((StUCMAppender*)this)->getLayout()->format(sbuf, event);
  return sbuf.str();
#else
	String sbuf;
	((StUCMAppender*)this)->getLayout()->format(sbuf,event,pool);
	return sbuf;
#endif
}

//_________________________________________________________________________
/* The default behavior holds a single connection open until the appender is closed (typically when garbage collected). */
void StUCMAppender::closeConnection()
{
  if (connection) {
      // connection->logEnd(); 
      delete connection; 
  }
  connection = 0;
  fIsConnectionOpen = false;
}

//_________________________________________________________________________
TxEventLog *StUCMAppender::getConnection()
{
   if (!fIsConnectionOpen) {

     if (!connection) {
         connection = TxEventLogFactory::create("WEB"); // create ucm collector factory
//       connection = TxEventLogFactory::create("U"); // create ucm collector factory
//       connection = TxEventLogFactory::create(technology.c_str()); // create ucm collector factory
//       connection = TxEventLogFactory::create();
       if ( getenv("JOBINDEX") && getenv("REQUESTID") ) {
           const char *JOBINDEX = getenv("JOBINDEX");
           std::string UCMJOB   = getenv("REQUESTID");
           UCMJOB +=JOBINDEX;
           // However, what we really need is:
           // const char *REQUESTID = getenv("REQUESTID");
           // const char *PROCESSID = getenv("PROCESSID");
       } else {
          fprintf(stderr,"StUCMAppender::getConnection() no JOBINDEX/REQUESTID was provided \n");
//          connection = TxEventLogFactory::create("WEB"); // create ucm collector factory
//          connection = TxEventLogFactory::create("U"); // create ucm collector factory
//          connection = TxEventLogFactory::create(technology.c_str()); // create ucm collector factory
//          connection = TxEventLogFactory::create();
//       connection = TxEventLogFactory::create("w"); // to access the Web interface
       }
     }
   }
	return connection;
}

//_________________________________________________________________________
void StUCMAppender::close()
{
   if (!this->closed) {
     flushBuffer();
     closeConnection();
     this->closed = true;
  }
}
#if 0
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
#endif
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
         if        (level == LOG4CXX_LEVEL_FATAL) {
              trackingLevel = TxEventLog::LEVEL_FATAL;
         } else if (level == LOG4CXX_LEVEL_ERROR) {
              trackingLevel = TxEventLog::LEVEL_ERROR;
         } else if (level == LOG4CXX_LEVEL_WARN)  {
              trackingLevel = TxEventLog::LEVEL_WARNING;
         } else if (level == LOG4CXX_LEVEL_DEBUG) {
              trackingLevel = TxEventLog::LEVEL_DEBUG;
         } else {
 //             continue;
         }

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
             // More robust parser should be added later on
             ucmParamters[keyCounter]  = ((TObjString *)keyValue[1])->String().Strip();
             delete &keyValue;
             keyCounter++;
          }
          String context;
#if (STAR_LOG4CXX_VERSION == 9)
          context = logEvent->getNDC();
#else
          logEvent->getNDC(context);
#endif                    
          // Map Stage to TxEventLog Stage
          ucmParamters[0].ReplaceAll("'","");ucmParamters[1].ReplaceAll("'","");ucmParamters[2].ReplaceAll("'","");
          int ucmStage = ucmParamters[0].Atoi();
 //         assert (ucmStage >=TxEventLog::START && ucmStage <=TxEventLog::END);
          // log task at once
          static bool taskDone = false;
          if (!taskDone) {
              taskDone = true;
              const char *taskSize = getenv("SUMS_nProcesses");
              int nSize = 1;
              if (taskSize && taskSize[0]) nSize = atoi(taskSize);
              connection->logTask(nSize);
          }
          fprintf(stderr,"%s\n","StUCMAppender::flushBuffer() . . . . . . . . ." );
  
          connection->logEvent(  ucmParamters[1].Data()
                               , ucmParamters[2].Data()
                               , trackingLevel
                               , TxEventLog::Stage(ucmStage)
                               , context
                              );
          pair->Delete();
          delete pair;
      }
      buffer.clear();        
   }
   closeConnection();
}
#if (STAR_LOG4CXX_VERSION == 10) 
//_________________________________________________________________________
void StUCMAppender::append(const spi::LoggingEventPtr& event, log4cxx::helpers::Pool& p)
{
    append(event);
}
#endif


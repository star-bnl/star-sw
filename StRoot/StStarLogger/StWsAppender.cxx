
#include "StWsAppender.h"
#include "TSystem.h"
#include "TString.h"

#include "StWsLogger.h"
#include "picojson.h"

#include <log4cxx/helpers/loglog.h>
#include <log4cxx/helpers/optionconverter.h>
#include <log4cxx/patternlayout.h>


using namespace log4cxx;
using namespace log4cxx::helpers;
using namespace log4cxx::db;
using namespace log4cxx::spi;

IMPLEMENT_LOG4CXX_OBJECT(StWsAppender)

//_________________________________________________________________________
StWsAppender::StWsAppender()
{ 

}

//_________________________________________________________________________
StWsAppender::~StWsAppender()
{
    finalize();
}

//_________________________________________________________________________
void StWsAppender::setOption(const String& option,
	const String& value)
{
	if (equalsIgnoreCase(option, _T("WS_TASK_ID")))
	{
		StWsLoggerS::Instance().setTaskID(value);
	} else if (equalsIgnoreCase(option, _T("WS_JOB_ID"))) {
		StWsLoggerS::Instance().setJobID(value);
	} else if (equalsIgnoreCase(option, _T("WS_LOG_URL"))) {
		StWsLoggerS::Instance().setServiceUrl(value);
	}
	else
	{
		AppenderSkeleton::setOption(name, value);
	}
}

//_________________________________________________________________________
void StWsAppender::append(const spi::LoggingEventPtr& event)
{
	time_t time = event->getTimeStamp();
	const LevelPtr& lvl = event->getLevel();
	const String& msg = event->getMessage();

	StWsLogger::LEVEL log_level = StWsLogger::INFO;

	if (lvl == LOG4CXX_LEVEL_FATAL) {
		log_level = StWsLogger::FATAL;
	} else if (lvl == LOG4CXX_LEVEL_ERROR) {
		log_level = StWsLogger::ERROR;
	} else if (lvl == LOG4CXX_LEVEL_WARN) {
		log_level = StWsLogger::WARNING;
	} else if (lvl == LOG4CXX_LEVEL_DEBUG) {
		log_level = StWsLogger::DEBUG;
	} else {
		//return; // FIXME
	}

    picojson::object evt;                                                                                                                                           
    evt["timestamp"] = picojson::value((double)time);                                                                                                                          
    evt["msg_line"] = picojson::value(msg.c_str());                                                                                                                        

	StWsLoggerS::Instance().logEvent(log_level, evt, StWsLogger::STATUS);
}

//_________________________________________________________________________
String StWsAppender::getLogStatement(const spi::LoggingEventPtr& event)
{
#if (STAR_LOG4CXX_VERSION == 9)
       StringBuffer sbuf;
       ((StWsAppender*)this)->getLayout()->format(sbuf, event);
       return sbuf.str();
#else
	String sbuf;
	((StWsAppender*)this)->getLayout()->format(sbuf, event,pool);
	return sbuf;
#endif
}

//_________________________________________________________________________
void StWsAppender::close()
{
	this->closed = true;
}


#if (STAR_LOG4CXX_VERSION == 10)
//_________________________________________________________________________
void StWsAppender::append(const spi::LoggingEventPtr& event, log4cxx::helpers::Pool& p)
{
    append(event);
}
#endif


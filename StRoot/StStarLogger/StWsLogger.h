#ifndef StWsLogger_hh                                                                                                                                              
#define StWsLogger_hh

#include "picojson.h"

#include <string>
#include <vector>

template <class T> struct StWsCreateMeyers {
    static T* Create() {
        static T _instance;
        return &_instance;
    }
};

template <class T, template<class> class StWsCreationPolicy=StWsCreateMeyers>
class StWsSingleton
{
public:
    static T& Instance() {
        if (!m_pInstance)
            m_pInstance=StWsCreationPolicy<T>::Create();
        return *m_pInstance;
    }
private:
    StWsSingleton();
    ~StWsSingleton();
    StWsSingleton(StWsSingleton const&);
    StWsSingleton& operator=(StWsSingleton const&);
    static T* m_pInstance;
};

template <class T, template<class> class C>
T* StWsSingleton<T,C>::m_pInstance = 0;


class StWsLogger;
typedef StWsSingleton<StWsLogger, StWsCreateMeyers> StWsLoggerS;

class StWsLogger
{
	public:
    	StWsLogger();
	    virtual ~StWsLogger();

		enum LEVEL {TRACE = 1, DEBUG = 2, INFO = 3, NOTICE = 4, WARNING = 5, ERROR = 6, CRITICAL = 7, ALERT = 8, FATAL = 9 };
		enum STAGE {START = 1, STATUS = 2, END = 3};

		void logEvent(LEVEL lvl, const picojson::object& evt, STAGE stg = STATUS, bool nobacklog = false);
		void logJobUpdate(const picojson::object& evt, bool nobacklog = false);

		void setServiceUrl(const std::string& url);
		void setJobID(const std::string& id);
		void setTaskID(const std::string& id);

	private:
		double getTimeMs() const;
		std::string getUserName() const;
		std::string getHostName() const;

		std::string getServiceUrl();
		std::string getJobID();
		std::string getTaskID();
		int makeHttpPostCurl(const char* url, const std::string& json_data, std::string& response);

		void checkBackLogs();

		std::string mServiceUrl;
		std::string mJobID;
		std::string mTaskID;
		bool mIsDisabled;

		std::vector<std::string> mEventBackLog;
		std::vector<std::string> mJobUpdateBackLog;
};

#define WS_JOB_LOG(evt) StWsLoggerS::Instance().logJobUpdate(evt);

#define WS_LOG_TRACE(evt) StWsLoggerS::Instance().logEvent(StWsLogger::TRACE, evt, StWsLogger::STATUS);
#define WS_LOG_DEBUG(evt) StWsLoggerS::Instance().logEvent(StWsLogger::DEBUG, evt, StWsLogger::STATUS);
#define WS_LOG_INFO(evt) StWsLoggerS::Instance().logEvent(StWsLogger::INFO, evt, StWsLogger::STATUS);

#define WS_LOG_NOTICE(evt) StWsLoggerS::Instance().logEvent(StWsLogger::NOTICE, evt, StWsLogger::STATUS);
#define WS_LOG_WARNING(evt) StWsLoggerS::Instance().logEvent(StWsLogger::WARNING, evt, StWsLogger::STATUS);
#define WS_LOG_ERROR(evt) StWsLoggerS::Instance().logEvent(StWsLogger::ERROR, evt, StWsLogger::STATUS);

#define WS_LOG_CRITICAL(evt) StWsLoggerS::Instance().logEvent(StWsLogger::CRITICAL, evt, StWsLogger::STATUS);
#define WS_LOG_ALERT(evt) StWsLoggerS::Instance().logEvent(StWsLogger::ALERT, evt, StWsLogger::STATUS);
#define WS_LOG_FATAL(evt) StWsLoggerS::Instance().logEvent(StWsLogger::FATAL, evt, StWsLogger::STATUS);

#endif

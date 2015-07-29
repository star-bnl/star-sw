#include "StWsLogger.h"

#include <curl/curl.h>

#include <unistd.h>
#include <cstdio>
#include <ctime>
#include <cstdlib>
#include <pwd.h>
#include <sys/time.h>

#include <sstream>

namespace {

template <class T>
std::string toString(T& arg)
{
    std::ostringstream os;
    os << arg;
    return os.str();
}

template <class T>
std::string toString(T arg)
{
    std::ostringstream os;
    os << arg;
    return os.str();
}

template<class T>
T fromString(const std::string& s)
{
    std::istringstream stream (s);
    T t;
    stream >> t;
    return t;
}

size_t curl_write_to_string(void *ptr, size_t size, size_t count, void *stream) {
  ((std::string*)stream)->append((char*)ptr, 0, size*count);
  return size*count;
}

}

StWsLogger::StWsLogger()
	: mServiceUrl(""),
	  mJobID(""),
	  mTaskID(""),
	  mIsDisabled(false)
{
	mServiceUrl = getServiceUrl();
	mJobID = getJobID();
	mTaskID = getTaskID();
}

StWsLogger::~StWsLogger()
{

}

void StWsLogger::logEvent(LEVEL lvl, const picojson::object& evt, STAGE stg, bool nobacklog) {
	if (mIsDisabled) { return; }

    picojson::object event;
    event["timestamp"]  = picojson::value(getTimeMs());
    event["level_id"]   = picojson::value((double)lvl);
    event["stage_id"]   = picojson::value((double)stg);
	event["message"]    = picojson::value(evt);

    picojson::value v(event);
    std::string response;

	//std::cout << "*** WS_LOGGER: " << v.serialize() << std::endl; // FIXME

	return; // FIXME

    int ret = makeHttpPostCurl(
		(	mServiceUrl + std::string("?action=event_append&task_id=") + mTaskID + std::string("&job_id=") + mJobID ).c_str(),
		v.serialize(),
		response
	);

    if (ret != 0) {
        // HTTP POST ERROR OCCURED, add evt to backlog
		if (!nobacklog) {
			mEventBackLog.push_back(v.serialize());
		}
    }

	checkBackLogs();
}

void StWsLogger::logJobUpdate(const picojson::object& evt, bool nobacklog) {
	if (mIsDisabled) { return; }

    picojson::object event;
    event["timestamp"]  = picojson::value(getTimeMs());
	event["message"]    = picojson::value(evt);

    picojson::value v(event);
    std::string response;

    int ret = makeHttpPostCurl(
		(	mServiceUrl + std::string("?action=job_add_info&task_id=") + mTaskID + std::string("&job_id=") + mJobID ).c_str(),
		v.serialize(),
		response
	);

    if (ret != 0) {
        // HTTP POST ERROR OCCURED, add evt to backlog
		mJobUpdateBackLog.push_back(v.serialize());
    }

	checkBackLogs();
}

double StWsLogger::getTimeMs() const {
    timeval tim;
    gettimeofday(&tim, NULL);
    double t1=tim.tv_sec+(tim.tv_usec/1000000.0);
    return t1;
}

std::string StWsLogger::getUserName() const {
	register struct passwd *pw;
	register uid_t uid;
	uid = geteuid();
	pw = getpwuid (uid);
	if (pw) {
      return (std::string(pw->pw_name));
    }
	return std::string("UNKNOWN");
}

std::string StWsLogger::getHostName() const {
    char buf[512];
    gethostname(buf,sizeof buf);
    return std::string(buf);
}

void StWsLogger::setServiceUrl(const std::string& url) {
	mServiceUrl = url;
	if (!mServiceUrl.empty() && !mJobID.empty() && !mTaskID.empty()) { mIsDisabled = false; }
	//std::cout << "WS_LOGGER_CHECK: " << mServiceUrl << ", " << mJobID << ", " << mTaskID << ", C: " << mIsDisabled << "\n";
}

void StWsLogger::setJobID(const std::string& id) {
	mJobID = id;
	if (!mServiceUrl.empty() && !mJobID.empty() && !mTaskID.empty()) { mIsDisabled = false; }
	//std::cout << "WS_LOGGER_CHECK: " << mServiceUrl << ", " << mJobID << ", " << mTaskID << ", C:" << mIsDisabled << "\n";
} 

void StWsLogger::setTaskID(const std::string& id) {
	mTaskID = id;
	if (!mServiceUrl.empty() && !mJobID.empty() && !mTaskID.empty()) { mIsDisabled = false; }
	//std::cout << "WS_LOGGER_CHECK: " << mServiceUrl << ", " << mJobID << ", " << mTaskID << ", C: " << mIsDisabled << "\n";
}

std::string StWsLogger::getServiceUrl() {
	if (!mServiceUrl.empty()) { return mServiceUrl; }

	char* surl = 0;
	surl = getenv("WS_LOG_URL");
	if (surl != NULL) {
		return std::string(surl);
	}
	//std::cout << "NO LOG URL, DISABLING WS_LOGGER\n";
	mIsDisabled = true;
	return std::string("");
}

std::string StWsLogger::getJobID() {
	if (!mJobID.empty()) { return mJobID; }

	char* jid = 0;
	jid = getenv("WS_JOB_ID");
	if (jid != NULL) {
		return std::string(jid);
	}
	//std::cout << "NO JOB ID, DISABLING WS_LOGGER\n";
	mIsDisabled = true;
	return std::string("");
}

std::string StWsLogger::getTaskID() {
	if (!mTaskID.empty()) { return mTaskID; }

	char* tid = 0;
	tid = getenv("WS_TASK_ID");
	if (tid != NULL) {
		return std::string(tid);
	}
	//std::cout << "NO TASK ID, DISABLING WS_LOGGER\n";
	mIsDisabled = true;
	return std::string("");
}

int StWsLogger::makeHttpPostCurl(const char* url, const std::string& json_data, std::string& response) {
    // no need to code proxy in, it is autodetected by libcurl (env.vars)

    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if (curl) {
        curl_easy_setopt(curl, CURLOPT_URL, url);
    }
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0L);
    curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0L);
    curl_easy_setopt(curl, CURLOPT_POST, 1L);
//  curl_easy_setopt(curl, CURLOPT_VERBOSE, 1L);

    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curl_write_to_string);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

    struct curl_slist *chunk = NULL;
    chunk = curl_slist_append(chunk, "Expect:");
    res = curl_easy_setopt(curl, CURLOPT_HTTPHEADER, chunk);

    curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json_data.c_str());
    curl_easy_setopt(curl, CURLOPT_POSTFIELDSIZE, json_data.length());

    res = curl_easy_perform(curl);
    if(res != CURLE_OK) {
        std::cerr << "HTTPS POST request failed: " << curl_easy_strerror(res) << std::endl;
        return res;
    }
    curl_slist_free_all(chunk);
    curl_easy_cleanup(curl);
    return 0;
}

void StWsLogger::checkBackLogs() {
	if (!mEventBackLog.empty()) {
		std::string response;
		for (std::vector<std::string>::iterator it = mEventBackLog.begin(); it != mEventBackLog.end(); ++it) {
			int ret = makeHttpPostCurl(                                                                                                                                        
        		( mServiceUrl + std::string("?action=event_append&task_id=") + mTaskID + std::string("&job_id=") + mJobID ).c_str(),                                         
        		*it,                                                                                                                                                 
        		response                                                                                                                                                       
    		);        
			if (ret != 0) return;
			it = mEventBackLog.erase(it);
		}
	}

	if (!mJobUpdateBackLog.empty()) {
		std::string response;
		for (std::vector<std::string>::iterator it = mJobUpdateBackLog.begin(); it != mJobUpdateBackLog.end(); ++it) {
			int ret = makeHttpPostCurl(
				( mServiceUrl + std::string("?action=job_add_info&task_id=") + mTaskID + std::string("&job_id=") + mJobID ).c_str(),
				*it,
				response
			);
			if (ret != 0) return; 
			it = mJobUpdateBackLog.erase(it);
		}
	}
}

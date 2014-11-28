#include "StHyperCacheWebservice.h"

#include <cassert>
#include <sstream>
#include <fstream>
#include <algorithm>
#include <iostream>

#include "StHyperHashSha256.h"
#include "StHyperHashMd5.h"
#include "StHyperUtilGeneric.h"
#include "StHyperUtilPlatform.h"
#include "StHyperUtilFilesystem.h"
#include "StHyperCacheConfig.h"

#include <curl/curlver.h>

namespace { // to localize json_writer helper function to this unit

// This is the writer call back function used by curl
static int json_writer(char *data, size_t size, size_t nmemb,
                       std::string *json_buffer)
{
    // What we will return
    int result = 0;
    // Is there anything in the buffer?
    if (json_buffer != NULL) {
        // Append the data to the buffer
        json_buffer->append(data, size * nmemb);
        // How much did we write?
        result = size * nmemb;
    }
    return result;
}

}

StHyperCacheWebservice::StHyperCacheWebservice()
    : m_Name("WEBSERVICE"), m_Version("1.0.0"), m_Type("ST_HYPERCACHE_WEBSERVICE"), curl(0)
{
}

StHyperCacheWebservice::~StHyperCacheWebservice()
{
	curl_easy_cleanup(curl);
}

bool StHyperCacheWebservice::init()
{
	curl = curl_easy_init();

	StHyperCacheConfig& cfg = StHyperCacheConfigSingleton::Instance();
	m_URI = cfg.getParameter<std::string>(m_Name+"uri");

	return true;
}

const char* StHyperCacheWebservice::get(const std::string& group_key, const std::string& key, size_t& value_length) {
	// the only allowed method for this pseudo-cache type
	// group_key = database, key = mysql query
	value_length = 0;

#ifdef LIBCURL_VERSION_NUM
# if LIBCURL_VERSION_NUM > 0x70F03
	char* escaped_key = curl_easy_escape(curl , key.c_str() , key.size());
# else 
	char* escaped_key = curl_escape(key.c_str() , key.size());
# endif 
#else
	char* escaped_key = curl_easy_escape(curl , key.c_str() , key.size());
#endif

//	char* escaped_key = curl_easy_escape(curl , key.c_str() , key.size());
	std::string query = escaped_key;
    curl_free(escaped_key);

	// URL created
	std::string full_url = m_URI + "database/" + group_key + "/query/" + query;

	std::string post_query_url;
	if (full_url.size() > 1024) {
		// SWITCH TO HTTP POST
        std::string m_post_query_url = "{ \"database\" : \"";
        m_post_query_url += group_key;
        m_post_query_url += "\", \"query\" : \"";
        m_post_query_url += key;
        m_post_query_url += "\" }";

#ifdef LIBCURL_VERSION_NUM
# if LIBCURL_VERSION_NUM > 0x70F03
        post_query_url = curl_easy_escape(curl, m_post_query_url.c_str(), m_post_query_url.size());
# else 
        post_query_url = curl_escape(m_post_query_url.c_str(), m_post_query_url.size());
# endif 
#else
        post_query_url = curl_easy_escape(curl, m_post_query_url.c_str(), m_post_query_url.size());
#endif

        curl_easy_setopt(curl, CURLOPT_VERBOSE, 0);
        curl_easy_setopt(curl, CURLOPT_POST, 1);
        curl_easy_setopt(curl, CURLOPT_POSTFIELDS, post_query_url.c_str() );
        curl_easy_setopt(curl, CURLOPT_URL, m_URI.c_str());
	} else {
        curl_easy_setopt(curl, CURLOPT_URL, full_url.c_str());
	}

    curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, m_JsonErrorBuffer);
    curl_easy_setopt(curl, CURLOPT_HEADER, 0);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, json_writer);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &m_JsonBuffer);
    curl_easy_setopt(curl, CURLOPT_CONNECTTIMEOUT, 2400); // 30 seconds connection timeout
    curl_easy_setopt(curl, CURLOPT_TIMEOUT, 2400); // 300 seconds timeout to execute call
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "STAR DB API WITH WS SUPPORT v.1.0");

    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 1);

	CURLcode rc = curl_easy_perform(curl);

    if (rc != 0) {                                                                                                                                                      
        return 0; // no data available, return instantly
    } 
	value_length = m_JsonBuffer.size();
	return m_JsonBuffer.c_str();
}

bool StHyperCacheWebservice::set(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime) {
	return false; // noop, webservice is read-only
}

bool StHyperCacheWebservice::replace(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime) {
	return false; // noop, webservice is read-only
}

bool StHyperCacheWebservice::remove(const std::string& group_key, const std::string& key) {
	return false; // noop, webservice is read-only
}

void StHyperCacheWebservice::clear() {
	return; // noop, webservice is read-only
}

std::string StHyperCacheWebservice::getStat() {
	return std::string("webservice stats");
}

StHyperCacheI* StHyperCacheWebservice::ptr = 0;

bool StHyperCacheWebservice::m_IsInitialized =
    StHyperCacheFactory::registerStHyperCacheInstantiator( "WEBSERVICE", getStHyperCacheWebservice );

StHyperCacheI* getStHyperCacheWebservice() {
    if (!StHyperCacheWebservice::ptr) {
        StHyperCacheWebservice::ptr = new StHyperCacheWebservice();
    }
    return StHyperCacheWebservice::ptr;
}




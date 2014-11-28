// StHyperCacheWebservice.h
#ifndef __ST_HYPERCACHEWEBSERVICE_H
#define __ST_HYPERCACHEWEBSERVICE_H

#include "StHyperCacheI.h"
#include "StHyperCacheFactory.h"

#include <curl/curl.h>
#include <curl/easy.h>

class StHyperCacheWebservice : public StHyperCacheI
{
public:
    StHyperCacheWebservice();
    ~StHyperCacheWebservice();

    // initialization procedures like memcache server connect or file cache initialization
    bool init();

    // get cached object(s) by primary key. returns pointer to data, which length is passed by reference
    const char* get(const std::string& group_key, const std::string& key, size_t& value_length);
    // put object to cache
    bool set(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime = 0);
    // replace existing object in cache with one provided
    bool replace(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime = 0);
    // remove existing object from cache, if exists
    bool remove(const std::string& group_key, const std::string& key);
    // remove all entries from cache
    void clear();

    // return some statistics parameters for this cache
          std::string  getStat();
    // return cache name (underlying engine)
    const std::string& getName() { return m_Name; };
    // return cache implementation version
    const std::string& getVersion() { return m_Version; };
    // return cache type
    const std::string& getType() { return m_Type; };

    static StHyperCacheI* ptr;

protected:

	static bool m_IsInitialized;

    std::string m_Name;
    std::string m_Version;
    std::string m_Type;

	CURL* curl;
	
	std::string m_URI;
	char m_JsonErrorBuffer[CURL_ERROR_SIZE];                                                                                                                          
	std::string m_JsonBuffer;

};

StHyperCacheI* getStHyperCacheWebservice();

#endif // __ST_HYPERCACHEWEBSERVICE_H


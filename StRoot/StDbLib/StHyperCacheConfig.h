#ifndef __ST_HYPERCACHECONFIG_H
#define __ST_HYPERCACHECONFIG_H

#include "StHyperUtilGeneric.h"
#include "StHyperSingleton.h"
#include <map>
#include <string>

class StHyperCacheConfig;
typedef StHyperSingleton<StHyperCacheConfig, StHyperCreateMeyers> StHyperCacheConfigSingleton;

class StHyperCacheConfig
{

public:
    StHyperCacheConfig();
    ~StHyperCacheConfig();

	bool setParameter(const std::string key, std::string value) {
		std::pair<std::map<std::string, std::string>::iterator,bool> ret;
		ret = m_CacheConfig.insert( std::pair<std::string, std::string>(key, value) );
		return ret.second;
	};

	template <class T>
	bool setParameter(const std::string key, T value) { // set specific parameter to config
		std::string val = StHyperUtilGeneric::toString(value);
		std::pair<std::map<std::string, std::string>::iterator,bool> ret;
		ret = m_CacheConfig.insert( std::pair<std::string, std::string>(key, value) );
		return ret.second;
	};

	template <class T>
	T getParameter(const std::string key) { // get specific parameter from config
		T obj;
		std::map<std::string,std::string>::iterator it;
		it = m_CacheConfig.find(key);
		if ( it != m_CacheConfig.end() ) {
			obj = StHyperUtilGeneric::fromString<T>((*it).second);
		} else {
			obj = StHyperUtilGeneric::fromString<T>("");
		}
		return obj;
	};


	void clear(); // remove all parameters from config cache

	friend std::ostream& operator<< (std::ostream& o, StHyperCacheConfig const& cfg);

protected:
	std::map<std::string,std::string> m_CacheConfig;

};

#endif // __ST_HYPERCACHE_CONFIG_H

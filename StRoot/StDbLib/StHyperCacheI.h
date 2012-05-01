#ifndef __ST_HYPERCACHEI_H
#define __ST_HYPERCACHEI_H

#include <ctime>
#include <string>
#include <vector>

class StHyperCacheI
{
public:
    StHyperCacheI();
    virtual ~StHyperCacheI();

    // initialization procedures like memcache server connect or file cache initialization
    virtual bool init() = 0;

    // get cached object(s) by primary key. returns pointer to data, which length is passed by reference
   virtual const char* get(const std::string& group_key, const std::string& key, size_t& value_length) = 0; 
    // put object to cache
    virtual bool set(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime = 0) = 0;
    // replace existing object in cache with one provided
    virtual bool replace(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime = 0) = 0;
    // remove existing object from cache, if exists
    virtual bool remove(const std::string& group_key, const std::string& key) = 0;
    // remove all entries from cache
    virtual void clear() = 0;

    // return some statistics parameters for this cache
    virtual       std::string getStat() = 0;
    // return cache name (underlying engine)
    virtual const std::string& getName() = 0;
    // return cache implementation version
    virtual const std::string& getVersion() = 0;
    // return cache type
    virtual const std::string& getType() = 0;

};

#endif // __ST_HYPERCACHEI_H


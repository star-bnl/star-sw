// StHyperCacheFactory.h
#ifndef __ST_HYPERCACHEFACTORY_H
#define __ST_HYPERCACHEFACTORY_H

#include <map>
#include <string>
#include <sstream>
#include "StHyperCacheI.h"
#include "StHyperSingleton.h"

typedef StHyperCacheI* (*StHyperCacheInstantiator)();
typedef std::map< std::string, StHyperCacheInstantiator > StHyperCacheInstantiatorMap;

class StHyperCacheFactory;
typedef StHyperSingleton<StHyperCacheFactory, StHyperCreateMeyers> StHyperCacheFactoryS;

class StHyperCacheFactory
{

public:
    StHyperCacheFactory();
    virtual ~StHyperCacheFactory();

    StHyperCacheI* getStHyperCacheInstance( std::string id ) {
		if (!m_InstantiatorMap) {
			m_InstantiatorMap = new StHyperCacheInstantiatorMap();
		}
        StHyperCacheInstantiatorMap::iterator iter = (*m_InstantiatorMap).find(id);
        if( iter != (*m_InstantiatorMap).end() ) {
            return (iter->second)();
        }
        //LOG_DEBUG << "[ " << id << " ] cache type implementation was not found" << LOG_EOM;
		return 0;
    }

    static bool registerStHyperCacheInstantiator(std::string id, StHyperCacheInstantiator func) {
		if (!m_InstantiatorMap) {
			m_InstantiatorMap = new StHyperCacheInstantiatorMap();
		}
        (*m_InstantiatorMap).insert( std::make_pair( id, func ) );
        return true;
    }

private:
    static StHyperCacheInstantiatorMap* m_InstantiatorMap;
};

#endif // __ST_HYPERCACHEFACTORY_H

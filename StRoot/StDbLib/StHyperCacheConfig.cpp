#include "StHyperCacheConfig.h"

StHyperCacheConfig::StHyperCacheConfig() {
}

StHyperCacheConfig::~StHyperCacheConfig() {
}

std::ostream& operator<< (std::ostream& o, StHyperCacheConfig const& cfg) {
    std::map<std::string,std::string>::const_iterator it;
    for (it = cfg.m_CacheConfig.begin(); it != cfg.m_CacheConfig.end(); ++it) {
        o << (*it).first << " = " << (*it).second << "\n";
    }
    return o;
}


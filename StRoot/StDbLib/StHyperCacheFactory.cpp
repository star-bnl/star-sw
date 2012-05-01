#include "StHyperCacheFactory.h"

StHyperCacheFactory::StHyperCacheFactory() {
}

StHyperCacheFactory::~StHyperCacheFactory() {
	m_InstantiatorMap->clear();
	delete m_InstantiatorMap;
}

StHyperCacheInstantiatorMap* StHyperCacheFactory::m_InstantiatorMap = 0;


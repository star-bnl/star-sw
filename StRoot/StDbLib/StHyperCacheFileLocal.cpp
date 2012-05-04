#include "StHyperCacheFileLocal.h"

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

StHyperCacheFileLocal::StHyperCacheFileLocal()
    : m_Name("FILE_LOCAL"), m_Version("1.0.0"), m_Type("ST_HYPERCACHE_FILE_LOCAL"),
	m_BasePath("/tmp/HyperCache/"), m_AltBasePath("/scratch/HyperCache/"), m_Path(""), m_MaxCacheSizeMb(0), m_MaxItemSizeKb(0)
{
}

StHyperCacheFileLocal::~StHyperCacheFileLocal()
{
}

bool StHyperCacheFileLocal::init()
{
	//std::cout << "READING FILELOCAL CONFIG: \n" ;

	StHyperCacheConfig& cfg = StHyperCacheConfigSingleton::Instance();

	m_BasePath       = cfg.getParameter<std::string>(m_Name+"_base_path");
	m_AltBasePath    = cfg.getParameter<std::string>(m_Name+"_alt_base_path");
	m_MaxCacheSizeMb = cfg.getParameter<int>(m_Name+"_max_cache_size_mb");
	m_MaxItemSizeKb  = cfg.getParameter<int>(m_Name+"_max_item_size_kb");
	m_IgnoreKeywords = cfg.getParameter<int>(m_Name+"_ignore");

	//std::cout << "FILELOCAL CONFIG COMPLETE \n" ;

	if ( !StHyperUtilFilesystem::path_exists(m_BasePath) ) { 
		StHyperUtilFilesystem::create_dir_recursive(m_BasePath); 
		if (!StHyperUtilFilesystem::path_exists(m_BasePath)) {
			StHyperUtilFilesystem::create_dir_recursive(m_AltBasePath); 
			if (!StHyperUtilFilesystem::path_exists(m_BasePath)) {
				return false;
			} 
			m_Path = m_AltBasePath;
		} else {
			m_Path = m_BasePath;
		}
	} else {
		m_Path = m_BasePath;
	};
	return true;
}

const char* StHyperCacheFileLocal::get(const std::string& group_key, const std::string& key, size_t& value_length) {
	std::string cache_file = m_Path + StHyperHash::md5sum(group_key + key)+std::string(".cache");
	if (!StHyperUtilFilesystem::path_exists(cache_file)) { value_length = 0; return 0; }

	std::ifstream cf(cache_file.c_str(), std::ios::in | std::ios::binary | std::ios::ate);
	std::ifstream::pos_type file_size = cf.tellg();
	if ((size_t)file_size == (size_t)0) { return 0; }
	char* memblock = new char[(size_t)(file_size)+(size_t)(1)];
    cf.seekg (0, std::ios::beg);
    cf.read (memblock, file_size);
    cf.close();
	memblock[(size_t)(file_size)] = '\0';
	value_length = file_size;
	return memblock;
}

bool StHyperCacheFileLocal::set(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime) {
	std::string cache_file = m_Path + StHyperHash::md5sum(group_key + key)+std::string(".cache");
	std::ofstream cf(cache_file.c_str(), std::ios::out | std::ios::binary);
    cf.write (data, dataLength);
//	cf << " | " << group_key << " | " << key << " |";
	cf.close();
	return false;
}

bool StHyperCacheFileLocal::replace(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime) {
	std::string cache_key = StHyperHash::md5sum(group_key + key);
	if (!remove(group_key, key)) {
		return false;
	}	
	return set(group_key, key, data, dataLength, expirationTime);
}

bool StHyperCacheFileLocal::remove(const std::string& group_key, const std::string& key) {
	std::string cache_file = m_Path + StHyperHash::md5sum(group_key + key)+std::string(".cache");
	return unlink(cache_file.c_str());
}

void StHyperCacheFileLocal::clear() {
	// FIXME
}

std::string StHyperCacheFileLocal::getStat() {
	return std::string("stats");
}

StHyperCacheI* StHyperCacheFileLocal::ptr = 0;

bool StHyperCacheFileLocal::m_IsInitialized =
    StHyperCacheFactory::registerStHyperCacheInstantiator( "FILE_LOCAL", getStHyperCacheFileLocal );

StHyperCacheI* getStHyperCacheFileLocal() {
    if (!StHyperCacheFileLocal::ptr) {
        StHyperCacheFileLocal::ptr = new StHyperCacheFileLocal();
    }
    return StHyperCacheFileLocal::ptr;
}




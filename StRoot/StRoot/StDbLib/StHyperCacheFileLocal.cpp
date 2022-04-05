#include "StHyperCacheFileLocal.h"

#include <cassert>
#include <sstream>
#include <fstream>
#include <algorithm>
#include <iostream>
#include <unistd.h>
#include <sys/statvfs.h>
#include "StHyperHashSha256.h"
#include "StHyperHashMd5.h"
#include "StHyperUtilGeneric.h"
#include "StHyperUtilPlatform.h"
#include "StHyperUtilFilesystem.h"
#include "StHyperCacheConfig.h"
#include "StHyperLock.h"

StHyperCacheFileLocal::StHyperCacheFileLocal()
    : m_Name("FILE_LOCAL"), m_Version("1.0.0"), m_Type("ST_HYPERCACHE_FILE_LOCAL"),
	m_BasePath("/tmp/HyperCache/"), m_AltBasePath("/scratch/HyperCache/"), m_Path(""), m_Policy("default"), m_MaxCacheSizeMb(0), m_MaxItemSizeKb(0),
	m_DiskFreeUpper(0.1), m_DiskFreeLower(0.5), m_StartEmpty("no")
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
	m_Policy         = cfg.getParameter<std::string>(m_Name+"_policy");
	std::transform(m_Policy.begin(), m_Policy.end(), m_Policy.begin(), ::tolower);
	m_MaxCacheSizeMb = cfg.getParameter<int>(m_Name+"_max_cache_size_mb");
	m_MaxItemSizeKb  = cfg.getParameter<int>(m_Name+"_max_item_size_kb");
	m_DiskFreeUpper  = cfg.getParameter<float>(m_Name+"_disk_free_upper");
	m_DiskFreeLower  = cfg.getParameter<float>(m_Name+"_disk_free_lower");
	m_IgnoreKeywords = cfg.getParameter<int>(m_Name+"_ignore");
	m_StartEmpty     = cfg.getParameter<std::string>(m_Name+"_start_empty");
	std::transform(m_StartEmpty.begin(), m_StartEmpty.end(), m_StartEmpty.begin(), ::tolower);

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

	StHyperUtilGeneric::rtrim(m_Path, " /\\\n\r\t");
	m_Path += '/';

	if (m_StartEmpty == "yes" || m_StartEmpty == "1") {
		doCacheCleanup();
		if (!StHyperUtilFilesystem::path_exists(m_Path)) {
			StHyperUtilFilesystem::create_dir_recursive(m_Path);
		}
	}

	return true;
}

void StHyperCacheFileLocal::doCacheCleanup() {
	StHyperLock lock(m_Path+"global.lock", true); // global lock on "cache cleanup"

	size_t bytes_free = 0, bytes_total = 0;
	getDiskUsage(bytes_free, bytes_total);
	if (bytes_total <= 0) {
		// something is wrong, no fs stats => no cleanup
		return;
	} else { 
		double percentage = (double)bytes_free / (double)bytes_total;
		if (percentage < m_DiskFreeUpper) {
			if (lock.try_lock(15000000)) { // try getting lock within 15 seconds
			// do cleanup until it reaches m_DiskFreeLower				
				if (m_Policy == "fifo") {
					StHyperUtilFilesystem::remove_dir_fifo(m_Path, m_Path, (double)bytes_total * (double)m_DiskFreeLower);
				} else if (m_Policy == "lru") {
					StHyperUtilFilesystem::remove_dir_lru(m_Path, m_Path, (double)bytes_total * (double)m_DiskFreeLower);
				} else { // default policy
					StHyperUtilFilesystem::remove_dir_recursive(m_Path, m_Path);
				}
			}
		}
	}
	return;
}

const char* StHyperCacheFileLocal::get(const std::string& group_key, const std::string& key, size_t& value_length) {
	std::string cache_file = m_Path + StHyperHash::md5sum(group_key + key)+std::string(".cache");
	if (!StHyperUtilFilesystem::path_exists(cache_file)) { value_length = 0; return 0; }

	StHyperLock lock(cache_file); // local file lock
	if ( lock.try_lock(5000000) ) {
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
	};
 
	return 0;
}

bool StHyperCacheFileLocal::set(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime) {
	std::string cache_file = m_Path + StHyperHash::md5sum(group_key + key)+std::string(".cache");

	StHyperLock lock(m_Path+"global.lock", true); // global lock on "add file"

	if ( lock.try_lock(5000000) ) { // 5 seconds
		std::ofstream cf(cache_file.c_str(), std::ios::out | std::ios::binary);
	    cf.write (data, dataLength);
		cf.close();
		return true;
	}

	return false;
}

bool StHyperCacheFileLocal::replace(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime) {
	std::string cache_key = StHyperHash::md5sum(group_key + key);

	StHyperLock lock(m_Path+"global.lock", true); // global lock on "add file"
	if ( lock.try_lock(5000000) ) {
		if (!remove(group_key, key)) {
			return false;
		}	
	}
	return set(group_key, key, data, dataLength, expirationTime);
}

bool StHyperCacheFileLocal::remove(const std::string& group_key, const std::string& key) {
	std::string cache_file = m_Path + StHyperHash::md5sum(group_key + key)+std::string(".cache");
	StHyperLock lock(m_Path+"global.lock", true);
	if ( lock.try_lock(5000000) ) {
		return unlink(cache_file.c_str());
	}
	return false;
}

void StHyperCacheFileLocal::clear() {
	doCacheCleanup();
}

std::string StHyperCacheFileLocal::getStat() {
	return std::string("stats");
}

void StHyperCacheFileLocal::getDiskUsage(size_t& bytes_free, size_t& bytes_total) {
  struct statvfs buf;
  if (statvfs(m_Path.c_str(), &buf) == -1) {
    // error, no stats for this filesystem!
	bytes_free = 0; bytes_total = 0;
  } else {
	// ok, got stats
	bytes_free = buf.f_bavail * buf.f_bsize;
	bytes_total = buf.f_blocks * buf.f_bsize;
  }
  return;
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




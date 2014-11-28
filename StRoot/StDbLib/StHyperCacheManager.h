#ifndef __ST_HYPERCACHEMANAGER_H
#define __ST_HYPERCACHEMANAGER_H

#include "StDbBuffer.h"
#include "StHyperCacheI.h"
#include <mysql.h>
#include "picojson.h"

class StHyperCacheManager : StHyperCacheI
{

public:
    StHyperCacheManager();
    ~StHyperCacheManager();

    bool init();

    const char* get(const std::string& group_key, const std::string& key, size_t& value_length);
    bool set(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime = 0);
    bool set(const std::string& group_key, const std::string& key, MYSQL_RES* result, time_t expirationTime = 0);
    bool replace(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime = 0);
    bool remove (const std::string& group_key, const std::string& key);
	void clear();

          std::string  getStat();
    const std::string& getName()    { return m_Name; };
    const std::string& getVersion() { return m_Version; };
    const std::string& getType()    { return m_Type; };

	const std::string& getLastGroupKey() { return m_LastGroupKey; }
	const std::string& getLastKey() { return m_LastKey; }

	size_t getNumRows()   { return m_NumRows; }
	size_t getNumFields() { return m_NumFields; }

	bool  setParameter(const std::string& param, const std::string& value);

	bool isActive() { return m_Active; }
	bool isValueFound() { return m_ValueFound; }

	bool processOutput(StDbBuffer* aBuff);

protected:

	bool readParametersFromJsonFile(); // read JSON-encoded params from config file
	std::string convertMysqlResultToJson(MYSQL_RES* result);
	bool parseJsonBuffer(); // parse raw buffer into traversable JSON document
	bool isJsonError();     // check json document to see if it contains db error conditions
	void calculateRowsFields(); // scan json document, get number of rows and fields
	char** DecodeStrArray(const char* strinput , int &aLen); // copied from MysqlDb.cc, used to break comma-separated string of numbers into char array

	std::string m_Name;    // cache name    - not critical for Manager, just part of interface
	std::string m_Version; // cache version --||--
	std::string m_Type;    // cache type    --||--

	bool m_ValueFound;     // was value found in any cache from the list?

	bool m_Active;         // if config was not found, manager is deactivated

	std::string m_PathToConfigFile; // configuration file location, from env.var

	char*  m_JsonBuffer;     // raw json buffer
	size_t m_JsonBufferSize; // raw json buffer size
	size_t m_NumRows;        // number of rows in parsed json buffer
	size_t m_NumFields;      // number of fields in parsed json buffer
	picojson::array m_JsonDocument; // parsed json document
	picojson::array::const_iterator m_JsonDocumentIter; // json document iterator, pointing at current row

	std::string m_LastGroupKey;
	std::string m_LastKey;

	std::vector<StHyperCacheI*> m_CacheImp;

};

#endif // __ST_HYPERCACHE_MANAGER_H

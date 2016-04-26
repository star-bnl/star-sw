#include "StHyperCacheManager.h"
#include "StHyperCacheFactory.h"
#include "StHyperUtilGeneric.h"
#include "StHyperUtilFilesystem.h"
#include "StHyperCacheConfig.h"
#include "picojson.h"

#include <iterator>
#include <iostream>
#include <algorithm>
#include <fstream>
#include <cstdlib>
#include <utility>

StHyperCacheManager::StHyperCacheManager() : m_Name("STAR HyperCache Manager"), m_Version("1.0.0"), m_Type("ST_HYPERCACHE_MANAGER"),
	m_ValueFound(false), m_Active(false), m_PathToConfigFile(""),
	m_JsonBuffer(0), m_JsonBufferSize(0), m_NumRows(0), m_NumFields(0)
{
}

StHyperCacheManager::~StHyperCacheManager()
{
	// delete all cached engines from storage
	for(std::vector<StHyperCacheI*>::iterator it = m_CacheImp.begin(); it != m_CacheImp.end(); ++it) {
		if (*it) {
			delete (*it);
		}
	}
}

bool StHyperCacheManager::init()
{
	m_JsonBuffer = 0;
	m_JsonBufferSize = 0;
	m_NumRows = 0;
	m_NumFields = 0;
	m_JsonDocument.clear();

	if (!readParametersFromJsonFile()) {
		m_Active = false;
		return m_Active;
	} else {
		m_Active = true;
	}

	// initialize cache engines
	for(std::vector<StHyperCacheI*>::iterator it = m_CacheImp.begin(); it != m_CacheImp.end(); ++it) {
		if (*it) {
			(*it)->init();
			std::cout << "Initialized: " << (*it)->getName() << " : " << (*it)->getVersion() << " : " << (*it)->getType() << "\n";
		}
	}

	return m_Active;
}

bool StHyperCacheManager::readParametersFromJsonFile() {
	std::vector<std::string> pathList;
	pathList.reserve(3);
	pathList.push_back("hyperconfig.json");
	pathList.push_back("config/hyperconfig.json");
	if (getenv("HYPERCONFIG_LOCATION")) {
		pathList.push_back(getenv("HYPERCONFIG_LOCATION"));
	}
	// uncomment block for a site-wide deployment
	if (getenv("STAR_PATH")) {
		pathList.push_back(std::string(getenv("STAR_PATH"))+std::string("/conf/hyperconfig.json"));
	}

	for(std::vector<std::string>::iterator it = pathList.begin(); it != pathList.end(); ++it) {
		if (StHyperUtilFilesystem::path_exists(*it)) {
			m_PathToConfigFile = *it;
			break;
		}
	}
	if (m_PathToConfigFile.empty()) { return false; }

	std::ifstream ifs(m_PathToConfigFile.c_str(), std::ifstream::in);
	if (!ifs.is_open()) { return false; }

	picojson::value v;
	std::cout << "StHyperCacheManager: reading cache config from " << m_PathToConfigFile << std::endl;
	ifs >> v;

	ifs.close();

	StHyperCacheConfig& cfg = StHyperCacheConfigSingleton::Instance();

	picojson::object& obj = v.get<picojson::object>();
	picojson::array& caches = obj["caches"].get<picojson::array>();
	bool cache_active = false;
	for (picojson::array::iterator it = caches.begin(); it != caches.end(); ++it) {
		picojson::object& cache = (*it).get<picojson::object>();
		if (cache["enabled"].get<std::string>() != std::string("yes")) { 
			continue; 
		} else {
			cache_active = true;
		}
		std::string cache_name = cache["type"].get<std::string>();
		//std::cout << "CACHE NAME: " << cache_name << "\n";
		StHyperCacheI* cacheInstance = StHyperCacheFactoryS::Instance().getStHyperCacheInstance( cache_name );
		if (!cacheInstance) { continue; }
		m_CacheImp.push_back(cacheInstance); // add cache instance to storage
		picojson::object& params = cache["params"].get<picojson::object>();
		for (picojson::object::iterator pit = params.begin(); pit != params.end(); ++pit) {
			const std::string& name = pit->first;
			//std::cout << cache_name << " :: param = " << name << " ";
			cfg.setParameter( cache_name + std::string("_") + name, (pit->second).to_str() );
		}
		//std::cout << "\n";
	}
	return cache_active;
}

std::string StHyperCacheManager::getStat() {
	return "StHyperCacheManager::getStat()";
}

const char* StHyperCacheManager::get(const std::string& group_key, const std::string& key, size_t& value_length) {
	if (!isActive()) return 0;
	if (m_JsonBuffer && m_JsonBufferSize > 0) {
		delete [] m_JsonBuffer;
		m_JsonBuffer = 0;
		m_JsonBufferSize = 0;
	}
	m_LastGroupKey = group_key;
	m_LastKey = key;
	StHyperUtilGeneric::trim(m_LastGroupKey);
	StHyperUtilGeneric::trim(m_LastKey);
	m_ValueFound = false;

    for(std::vector<StHyperCacheI*>::iterator it = m_CacheImp.begin(); it != m_CacheImp.end(); ++it) {
		value_length = 0;
		if (*it) {
        	const char* result = (*it)->get(group_key, key, value_length);
			if (result && value_length > 4) { // FIXME: 4 
				m_ValueFound = true;
				m_JsonBuffer = const_cast<char*>(result);
				m_JsonBufferSize = value_length;
				m_JsonDocument.clear();
				m_JsonDocumentIter == m_JsonDocument.begin();
				if (parseJsonBuffer()) { return result; }
				return 0;
			}
		}
    }

	return 0;
}

bool StHyperCacheManager::set(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime) {
	if (!isActive()) return false;
	if (dataLength < 5) return false;
	m_LastGroupKey = "";
	m_LastKey = "";
	//std::cout << "TRYING TO SET SOMETHING TO CACHE: \n";
	m_ValueFound = false;
	bool result = false;
    for(std::vector<StHyperCacheI*>::iterator it = m_CacheImp.begin(); it != m_CacheImp.end(); ++it) {
		if (*it) {
        	result = (*it)->set(group_key, key, data, dataLength, expirationTime);
			if (result) { 
				return result; 
			}
		}
    }
	return false;
}

bool StHyperCacheManager::set(const std::string& group_key, const std::string& key, MYSQL_RES* myresult, time_t expirationTime) {
	if (!isActive()) return false;
	m_LastGroupKey = "";
	m_LastKey = "";
	m_ValueFound = false;
	bool result = false;
	std::string res = convertMysqlResultToJson(myresult);
	if (res.size() < 5) return false;
	result = set(group_key, key, res.c_str(), res.length(), expirationTime);
	//std::cout << "set " << group_key << "| Q: " << key << " | result = " << result << "\n";
	return result;
}

bool StHyperCacheManager::replace(const std::string& group_key, const std::string& key, const char* data, size_t dataLength, time_t expirationTime) {
	if (!isActive()) return false;
	m_LastGroupKey = "";
	m_LastKey = "";
	m_ValueFound = false;
	bool result = false;
    for(std::vector<StHyperCacheI*>::iterator it = m_CacheImp.begin(); it != m_CacheImp.end(); ++it) {
        result = (*it)->replace(group_key, key, data, dataLength, expirationTime);
    }
	return result; // FIXME
}

bool StHyperCacheManager::remove(const std::string& group_key, const std::string& key) {
	if (!isActive()) return false;
	m_LastGroupKey = "";
	m_LastKey = "";
	m_ValueFound = false;
	bool result = false;
    for(std::vector<StHyperCacheI*>::iterator it = m_CacheImp.begin(); it != m_CacheImp.end(); ++it) {
        result = (*it)->remove(group_key, key);
    }
	return result;
}

void StHyperCacheManager::clear() {
	if (!isActive()) return;
	m_LastGroupKey = "";
	m_LastKey = "";
	m_ValueFound = false;
    for(std::vector<StHyperCacheI*>::iterator it = m_CacheImp.begin(); it != m_CacheImp.end(); ++it) {
		(*it)->clear();
    }
	return;
}

std::string StHyperCacheManager::convertMysqlResultToJson(MYSQL_RES* result) {
	if (!isActive()) return "";
    MYSQL_ROW row;
    MYSQL_FIELD *fields;
    int num_fields;
    unsigned long *lengths;

    num_fields = mysql_num_fields(result);
    fields = mysql_fetch_fields(result);

    std::ostringstream out;

    int ii = 0;
    out << "[";

    // populate json array with values
    while ((row = mysql_fetch_row(result))) {
        lengths = mysql_fetch_lengths(result);
        if (ii != 0) { out << ","; }
        ii++;
        out << "{";
        for (int i = 0; i < num_fields; i++) {
            if (i != 0) { out << ","; }
            if (fields[i].type == MYSQL_TYPE_BLOB) {
                out << "\"" << fields[i].name << "\" : \"" << StHyperUtilGeneric::base64_encode(row[i], lengths[i]) << "\"";
            } else {
                // plaintext
                out << "\"" << fields[i].name << "\" : \"" << (row[i] ? row[i] : "NULL") << "\"";
            }
        }
        out << "}\n";
    }
    // check types, report blobs and texts
    std::vector<std::string> attr;
    for (int i = 0; i < num_fields; i++) {
        if (fields[i].type == MYSQL_TYPE_BLOB) { // blob OR text
            if (fields[i].flags & BINARY_FLAG) {
                // blob
                std::string tmp = std::string("\"") + fields[i].name + std::string("\":\"binary_base64\"");
                attr.push_back(tmp);
            } else {
                // text
                std::string tmp = std::string("\"") + fields[i].name + std::string("\":\"text_base64\"");
                attr.push_back(tmp);
            }
        }
    }
    if (attr.size() != 0) {
        out << ",{\"@attributes\":{" << StHyperUtilGeneric::implode(attr, std::string(",")) << "}";
    }

    out << "]" << std::endl;

    return out.str();
}

bool  StHyperCacheManager::setParameter(const std::string& param, const std::string& value) {
	// noop, this method is not supposed to be called upon StHyperCacheManager;
	return true;
}

bool StHyperCacheManager::parseJsonBuffer() {
	//std::cout << "Preparing to parse JSON Buffer " << std::endl;
	if (!m_JsonBuffer || m_JsonBufferSize <= 0) {
		return false; // buffer is empty for some reason. Likely, query produced no results (no rows found).
	}
	if (!m_JsonDocument.empty()) { return true; } // buffer was already parsed..
	//std::cout << "Preparing to parse JSON Buffer - checks completed" << std::endl;
	m_NumRows = 0; m_NumFields = 0;
	picojson::value v;
	std::istringstream istr(m_JsonBuffer);
	//std::cout << "Preparing to parse JSON Buffer, raw: " << m_JsonBuffer << std::endl;
    istr >> v;
	m_JsonDocument = v.get<picojson::array>();
	if (isJsonError()) { 
		//std::cout << "JSON ERROR: " << v << std::endl;
		return false; 
	} // db reported error
	//std::cout << "Preparing to calculate Rows n Fields:" << std::endl;
	calculateRowsFields();
	m_JsonDocumentIter = m_JsonDocument.begin();
	//std::cout << "Preparing to parse JSON Buffer parsed" << std::endl;
	return true; // true = raw json parsed successully into json object
}

void StHyperCacheManager::calculateRowsFields() {
    picojson::object o = m_JsonDocument[m_JsonDocument.size()-1].get<picojson::object>();
    picojson::object::const_iterator it = o.find("@attributes");
    if (it != o.end()) {
        picojson::object& attr = o["@attributes"].get<picojson::object>();
        if (attr.size() > 0) {
            m_NumRows = m_JsonDocument.size() - 1;
        } else {
            m_NumRows = m_JsonDocument.size();
        }
    } else {
        m_NumRows = m_JsonDocument.size();
    }
    picojson::object& row0 = m_JsonDocument[0].get<picojson::object>();
    m_NumFields = row0.size();
}

bool StHyperCacheManager::isJsonError() {

    picojson::object& check = m_JsonDocument[0].get<picojson::object>();
    picojson::object::const_iterator it = check.begin();
    std::string name = (*it).first;
	std::transform(name.begin(), name.end(), name.begin(), ::tolower);
    if (name == "database_error") {
		// const std::string& value = ((*it).second).get<std::string>();
		// std::cerr << "database error: " << value << std::endl;
		m_JsonDocument.clear();
		m_JsonDocumentIter = m_JsonDocument.end();
		return true;
    }
	return false; // false = no errors found
}

bool StHyperCacheManager::processOutput(StDbBuffer* aBuff) {
	//std::cout << "processOutput: START" << std::endl;
	// Sanity checks
	if (!m_JsonBuffer || m_JsonBufferSize <= 0) {
		//std::cout << "processOutput ERROR: json buffer is empty" << std::endl;
		return false; // buffer is empty for some reason. Likely, query produced no results (no rows found).
	} else {
		//std::cout << "processOutput: json buffer found.." << std::endl;
	}
	if (m_JsonDocument.size() <= 0) {
		bool res = parseJsonBuffer();
		if (!res) { 
			//std::cout << "processOutput ERROR: something is wrong, invalid json?" << std::endl;
			return false; // something is deeply wrong with buffer - invalid json?
		} else {
			//std::cout << "processOuput: json buffer parsed successfully.." << std::endl;
		}
	}
	if (m_JsonDocumentIter == m_JsonDocument.end()) {
		//std::cout << "processOutput WARN: no more rows left to process" << std::endl;
		/*
		m_JsonDocument.clear();
		m_JsonDocumentIter == m_JsonDocument.begin();
		m_LastGroupKey = "";
		m_LastKey = "";
		m_ValueFound = false;
		*/
		return false; // no more rows left..
	} else {
		//std::cout << "processOutput: json buffer still has rows.." << std::endl;
	}
	//std::cout << "processOutput: START, no errors" << std::endl;

	// Scan for binary attributes - will need decoding from base64
	std::map<std::string,std::string> encCols; // list of columns, which needs to be decoded

	picojson::object& o = m_JsonDocument[m_JsonDocument.size()-1].get<picojson::object>();
	picojson::object::const_iterator it = o.find("@attributes");

	if (it != o.end()) {
		picojson::object& attr = o["@attributes"].get<picojson::object>();
		for (picojson::object::const_iterator ait = attr.begin(); ait != attr.end(); ++ait) {
			std::string aname = (*ait).first;
			const std::string& avalue = ((*ait).second).get<std::string>();
			encCols.insert(std::make_pair(aname, avalue));
		}
	}
	//std::cout << "processOutput: binary attribute search complete" << std::endl;

	// Parse data at current json iterator position
	std::map<std::string,std::string>::iterator iter;
	const picojson::object& obj = (*m_JsonDocumentIter).get<picojson::object>();
	for (picojson::object::const_iterator oit = obj.begin(); oit != obj.end(); ++oit) {
		const std::string& name = (*oit).first;
		if (name == "@attributes") continue; // already parsed attributes, skip row
		std::string value = ((*oit).second).to_str();
		iter = encCols.find(name);
		if (iter != encCols.end()) {
			if ( (*iter).second == "binary_base64") {
				std::string decoded_value = StHyperUtilGeneric::base64_decode(value);
				aBuff->WriteArray((unsigned char*)(decoded_value.c_str()),decoded_value.size(),name.c_str());
			} else if ( (*iter).second == "text_base64" ) {
				std::string decoded_value = StHyperUtilGeneric::base64_decode(value);
				char** tStrPtr;
				int len;
				tStrPtr = DecodeStrArray(decoded_value.c_str(),len);
				aBuff->WriteArray(tStrPtr,len,name.c_str());
				for (int k=0; k<len; k++) { delete [] tStrPtr[k]; }
				delete [] tStrPtr;
			}
		} else {
			if (!value.empty()) {
				aBuff->WriteScalar(value.c_str(), name.c_str());
			} else {
				aBuff->WriteScalar("", name.c_str());
			}
		}
	}
	++m_JsonDocumentIter;
	//std::cout << "processOutput: data at current json iter parsed" << std::endl;

	return true;
}

char** StHyperCacheManager::DecodeStrArray(const char* strinput , int &aLen) {
    if (!strinput) { // shouldn't happen - should have checked before here
        //   cout<< "null input string from mysql " << endl;
        char** tmparr = new char*[1];
        aLen = 1;
		tmparr[0] = (char*)"0";
        //*tmparr = new char[2];
        //strcpy(*tmparr,"0");
        return tmparr;
    }

    const char* tPnt=strinput;
    aLen=0;
    //MPD: bumped this limit up from 100 for the 768 ssd strips - with fingers crossed
    while (tPnt&&aLen<1024) { // 1024 is a limit on # comma separated values
        tPnt=strpbrk( tPnt,"\\,");
        if (tPnt!=0) {
            if (*tPnt==',') {
                aLen++;
                tPnt++;
            } else { //(*tPnt=='\\')
                tPnt++;
                tPnt++;
            };
        };
    };
    aLen++;
    char** strarr=new char*[aLen];
    tPnt=strinput;
    const char* tPntNew=tPnt;
    char *tBuff=new char[strlen(strinput)+1];
    char *tBuffInd=tBuff;
    int tCount=0;
    while (tPntNew) {
        tPntNew=strpbrk( tPnt,"\\,");
        if ((tPntNew==0)||*tPntNew==',') {
            if (tPntNew==0) {
                strcpy(tBuffInd,tPnt);

            } else {
                strncpy(tBuffInd,tPnt,tPntNew-tPnt);
                *(tBuffInd+(tPntNew-tPnt))='\0';
            }
            strarr[tCount]=new char[strlen(tBuff)+1];
            strcpy(strarr[tCount],tBuff);
            tBuffInd=tBuff;
            tPnt=tPntNew+1;
            tCount++;
        } else { //(*tPntNew=='\\')
            strncpy(tBuffInd,tPnt,tPntNew-tPnt);
            tBuffInd=tBuffInd+(tPntNew-tPnt);
            tPntNew++;
            if (*tPntNew=='\\'||*tPntNew==',') {
                *tBuffInd=*tPntNew;
                tBuffInd++;
            };
            *(tBuffInd)='\0';
            tPnt=tPntNew+1;
        };
    };
    delete [] tBuff;
    return strarr;
}






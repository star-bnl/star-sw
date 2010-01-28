//******************************************************************************

/*! \class StEmcDBHandler
    \author Marcia Maria de Moura
		
		  2003/01/02
		  Class to retrieve Emc database tables
*/		
//******************************************************************************

#ifndef StEmcDbHandler_HH
#define StEmcDbHandler_HH

#include <iostream>
using namespace std;


//#include <strstream>
//#include <Stsstream.h>

#include "TObject.h"

#include "StDbLib/StDbManager.hh"
#include "StDbLib/StDbConfigNode.hh"
#include "StDbLib/StDbTable.h"
#include "StDbLib/StDataBaseI.hh"

#include <string>
#include <vector>

class StEmcDbHandler : public TObject 
{
public:
    StEmcDbHandler();  ///< Class ctor
    virtual ~StEmcDbHandler();  ///< Class dtor
    
    StDbTable* getDbTable();
    std::vector<std::string> getTimeStampList(const char * beginTime, const char * endTime);

	void   setTableNode(const char* node = "Calibrations_emc") { mTableNode = node; }
	void   setTableName(const char* name) { mTableName = name; }
	void   setTimeStamp(const char* stamp = "2030-01-01 00:00:00") { mTimeStamp = stamp; }
	void   setMaxEntryTime(const char* stamp = "2030-01-01 00:00:00") { mMaxTime = stamp; }
	void   setFlavor(const char* flavor = "ofl") { mFlavor = flavor; }
    
    void   writeToDb(char* data);
    
    static std::string timeToSqlTime(const char* apiTime);

protected:
    std::string     mFlavor;                ///< flavor for BEMC tables
    std::string     mTableNode;             ///< Database node for EMC tables
    std::string     mTableName;             ///< EMC database table name, according to input
    std::string     mTimeStamp;             ///< EMC database table time stamp
    std::string     mMaxTime;               ///< EMC database max entry time
    std::vector<std::string> mTimeStampList;///< Time stamp list of specified EMC database table
    
    ClassDef(StEmcDbHandler,1)
};
#endif

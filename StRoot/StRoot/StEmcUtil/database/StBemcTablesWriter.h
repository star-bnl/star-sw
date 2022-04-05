/***************************************************************************
 *
 * $Id: StBemcTablesWriter.h,v 1.2 2007/12/11 19:54:46 kocolosk Exp $
 * Author:      Adam Kocoloski, MIT, kocolosk@mit.edu
 *
 ***************************************************************************/

#ifndef StBemcTablesWriter_HH
#define StBemcTablesWriter_HH

#include <map> 
using std::map;

#include <string> 
using std::string;

#include "StEmcDbHandler.h"
#include "StBemcTables.h"

class StBemcTablesWriter : public StBemcTables
{
public:
    StBemcTablesWriter();
    virtual ~StBemcTablesWriter();

    void setMaxEntryTime(char * maxEntryTime) { mDbHandler->setMaxEntryTime(maxEntryTime); }
    
    void loadTables(const char *sqlTime, const char *flavor="ofl"); ///< load directly from DB, no Maker needed 
    void loadTableFromFile(TFile *f);
    
    void setTable(const char *tableName, void *data);
    
    void setCalib(int det, int softId, int power, float val);
    void setPedestal(int det, int softId, int cap, float val);
    void setPedestalRMS(int det, int softId, int cap, float val);
    void setGain(int det, int softId, float val);
    void setStatus(int det, int softId, unsigned short val);
    
    void setCalibStatus(int det, int softId, unsigned short val);
    void setPedestalStatus(int det, int softId, unsigned short val);
    void setGainStatus(int det, int softId, unsigned short val);
    
    void writeToDb(const char * tableName, const char * timeStamp, const char * flavor = "ofl");
    void writeToFile(const char * fileName);
    
private:
    //keep the StDbTables stored here so we can test validity ranges, etc.
    map<string, StDbTable*> mDbTables;
    
    StEmcDbHandler* mDbHandler;
    
    ClassDef(StBemcTablesWriter,1)
};

#endif

/***************************************************************************
 *
 * $Log: StBemcTablesWriter.h,v $
 * Revision 1.2  2007/12/11 19:54:46  kocolosk
 * allow direct setting of void * table
 *
 * Revision 1.1  2007/09/08 01:22:37  kocolosk
 * StBemcTablesWriter provides common interface for inserting DB tables
 *
 *
 **************************************************************************/

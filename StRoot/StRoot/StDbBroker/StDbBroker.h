/***************************************************************************
 *
 * $Id: StDbBroker.h,v 1.33 2016/05/24 17:44:16 dmitry Exp $
 *
 * Author: S. Vanyashin, V. Perevoztchikov
 * Updated by:  R. Jeff Porter
 ***************************************************************************
 *
 * Description: Offline Interface from the Offline Maker interface to the
 *              Database interface. 
 *
 ***************************************************************************
 *
 * $Log: StDbBroker.h,v $
 * Revision 1.33  2016/05/24 17:44:16  dmitry
 * first batch of fixes for Coverity findings
 *
 * Revision 1.32  2011/11/28 17:03:07  dmitry
 * dbv override support in StDbLib,StDbBroker,St_db_Maker
 *
 * Revision 1.31  2011/02/10 17:31:01  dmitry
 * added an option to blacklist domains
 *
 * Revision 1.30  2008/01/15 20:37:44  deph
 * Removed DbFill and corresponding calls from StDbBroker
 *
 * Revision 1.29  2007/09/10 02:36:08  perev
 * StDbBroker::Release added
 *
 * Revision 1.28  2005/08/29 21:43:15  fisyak
 * replace UInt_t to Int_t for m_runNumber to avoid problem with undefined run no.
 *
 * Revision 1.27  2004/07/14 18:46:51  perev
 * UInt=>Int for new ROOT
 *
 * Revision 1.26  2003/01/08 19:43:10  perev
 * CleanUp
 *
 * Revision 1.25  2002/01/15 17:15:36  porter
 * moved timestamp translation to a separate method
 *
 * Revision 1.24  2001/10/30 20:43:29  porter
 * timestamp set for failure on query by runNumber
 *
 * Revision 1.23  2001/10/26 16:36:17  porter
 * more runNumber implementation
 *
 * Revision 1.22  2001/10/26 15:44:02  porter
 * add query by runNumber
 *
 * Revision 1.21  2001/10/24 04:05:56  porter
 * added zombie designation per Victor's suggestion
 *
 * Revision 1.20  2001/09/13 16:54:54  porter
 * propogate falvor by table through the brokery
 *
 * Revision 1.19  2001/01/22 18:40:25  porter
 * Added a wrapper for StMessage so one can use it in StDbLib
 *
 * Revision 1.18  2000/08/15 22:53:14  porter
 * Added 2 write methods.
 *  - 1 works once "list" is requested from database
 *  - 1 works just by specifying the full path from which
 *    the code extracts the database name.
 *
 * Revision 1.17  2000/06/30 02:00:42  porter
 * fixed memory leak introduced when making sure top level returned to
 * offline is always a database type name
 *
 * Revision 1.16  2000/06/14 13:39:05  didenko
 * Add ClassDef/ClassImp
 *
 * Revision 1.15  2000/04/25 18:27:48  porter
 * Added flavor and production time as query fields to pass to db-api
 *
 * Revision 1.14  2000/04/14 14:46:41  fine
 * new method for Victor has been introduced
 *
 * Revision 1.13  2000/04/13 20:22:57  porter
 * - reconnected tableDescriptor that had been broken via St_tableDescriptor.
 * - added unix timestamp as standard
 * - top node returned via InitConfig will be a database type
 *
 * Revision 1.12  2000/03/26 16:47:13  fine
 * Adjusted to ROOT 2.24
 *
 * Revision 1.11  2000/02/28 15:24:20  porter
 * add more StDbLib methods to broker: this time, StDbManager::closeAllConnections()
 *
 * Revision 1.10  2000/01/31 17:11:18  porter
 * fix break caused by the interaction design between
 * 'StRoot/St_base/tableDescriptor.h' & 'StDbBroker::Descriptor'
 * Now  StDbBroker::Descriptor==tableDescriptor_st
 * And  StDbBroker::GetTableDescriptor() returns abstract StTableDescriptorI*
 * Interface to StDbLib is (and was) handle correctly.
 * StDbBroker is now tied to StRoot/St_base via tableDescriptor.h
 * No problems would have occured if St_base interactions were based
 * on StTableDesciptorI in the first place.
 *
 * Revision 1.9  2000/01/27 20:30:40  porter
 * cleaned up dtor & error logic
 *
 * Revision 1.8  2000/01/14 14:49:10  porter
 * set verbose level for checking, added $Id & $Logs, & made node container
 * more robust for interactions with StDbLib
 *
 * Revision 1.7  2000/01/10 20:31:16  porter
 * modified StDbBroker to be an interface to the DB-interface, StDbLib.
 *  - old functionality is retained for the short-term & modifications
 *    are extensions
 *
 *
 **************************************************************************/
//  

#ifndef STAR_StDbBroker
#define STAR_StDbBroker

#include "Rtypes.h"
#include "dbNodeArray.h"
#include "dbConfig.h"
//
// --> had to remove independence from 'St_base'
//     since St_base doesn't know interface
//    
//     Now Broker uses StDbLib's descriptor interface
//     and St_base's concrete descriptor.
//
//#include "tableDescriptor.h" 
#include "St_tableDescriptor.h" 

#include <map>
#include <utility>

class StDbConfigNode;
class StDbManager;
class StTableDescriptorI;
class StDbTable;

/* needed for GetComments only   */
class TTable;
/*this is a temporary quick-and-dirty class for db access*/

class StDbBroker  {
  public:
      enum EColumnType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt
                             ,kULong, kUShort, kUChar, kChar };

typedef St_tableDescriptor Descriptor;

//--> for Sasha's (now obsolete?) Fill & Use routines 
struct oldDescriptor {
    char         fColumnName[32];  /* The name of this data-member: */
    unsigned int fIndexArray[3];   /* The array of the sizes for each dimen*/
    unsigned int fOffset;      /* The first byte in the row of this column  */
    unsigned int fSize;   /* The full size of the selected column in bytes  */
    unsigned int fTypeSize; /* The type size of the selected column in byte */
    unsigned int fDimensions;/* The number of the dimensions for array   */
    Int_t        fType;        /* The data type of the selected column   */
};

  protected:
    StDbTable*   m_node = 0;
    oldDescriptor *m_descriptor = 0;
    Descriptor  *mdescriptor = 0;
    Char_t *     m_structName = 0;  //name of the struct type used in this TTable
    Char_t *     m_tableName = 0;   //name of this instance of TTable
    UInt_t       m_sizeOfStruct = 0;// byte size of this struct
    UInt_t       m_nElements = 0;   // Number of variables in the structure
    UInt_t       m_nRows = 0;       // number of rows in the table

    Int_t        m_DateTime[2]; // Current DateTime

    Int_t        m_BeginDate = 0;   // begin date: 2021 05 31
    Int_t        m_BeginTime = 0;   // begin date: HH MM SS
    Int_t        m_EndDate = 0;     // end date
    Int_t        m_EndTime = 0;     // end time

    UInt_t       m_beginTimeStamp = 0; // unix beginTime
    UInt_t       m_endTimeStamp = 0; // unix endTime
    UInt_t       m_requestTimeStamp = 0; // unix requestTime

    Int_t        m_runNumber = 0;  // run number of queries of runlog

    char*        m_tableVersion = 0; // name of the version of the table
    char*        m_database = 0;     // name of the database for this table
    char*        m_ParentType = 0;   // named dbType when "top" db is domain-level

    int       m_isVerbose = 0;
    dbNodeArray *m_Nodes = 0;
    StDbConfigNode* m_Tree = 0;

    char*        m_flavor = 0;
    unsigned int m_prodTime = 0;
    std::map<std::pair<char*,char*>,unsigned int> m_prodTimeOverride; // DBV override for specific subsystems

    dbConfig_st*  buildConfig(int& numRows);
    int           buildNodes(StDbConfigNode* node, int pID);

    Bool_t         m_isZombie = false;
    
    void          makeDateTime(const char* dateTime, Int_t & iDate, Int_t & iTime); 

  public:

    StDbBroker();
    virtual ~StDbBroker();
    
    //    int Init(const char *dbname);// return 0 if OK

    void * Use();
    void   SetTableFlavor(const char* flavor, int tabID, int parID);
    void * Use(int tabID, int parID);
    bool   UseRunLog(StDbTable* table);

    char  **GetComments(St_Table *parentTable);
  //  void   Fill(void * pArray, const char **ElementComment);

    // Write Into Database methods
    //  with tabID -> assumes StDbBroker::InitConfig() has been called 
    //  which returns the list of table names with unique tabID association.
    Int_t WriteToDb(void* pArray, int tabID);

    //  with fullPath -> find the database from the path. Table information
    //  is requested from db but over-ridden by input options (idList).
    //  This method will be slow for writing several tables but speed of writes
    //  should not be an issue.
    Int_t WriteToDb(void* pArray, const char* fullPath, int* idList=0);

    StDbTable* findTable(const char* databaseName);

    UInt_t GetNRows()                {return m_nRows;       }
    Int_t  GetBeginDate()            {return m_BeginDate;   }
    Int_t  GetBeginTime()            {return m_BeginTime;   }
const char *GetFlavor();              
    Int_t  GetEndDate()              {return m_EndDate;     }
    Int_t  GetEndTime()              {return m_EndTime;     }
    UInt_t GetRequestTimeStamp()     {return m_requestTimeStamp; }
    UInt_t GetBeginTimeStamp()       {return m_beginTimeStamp; }
    UInt_t GetEndTimeStamp()         {return m_endTimeStamp; }
    UInt_t GetProdTime()             {return m_prodTime;}
    Bool_t   IsZombie()              {return m_isZombie; }


    StTableDescriptorI* GetTableDescriptor();

    void loadOldDescriptor(){};
    static const Char_t * GetTypeName( EColumnType type) {
      switch (type)
	{
	case kFloat:  return "float";
	case kInt:    return "int";
	case kLong:   return "long";
	case kShort:  return "short";
	case kDouble: return "double";
	case kChar:   return "char";
	  //temporarily
	case kUInt:   return "int";
	case kULong:  return "long";
	case kUShort: return "short";
	case kUChar:  return "char";

	case kNAN:    return "";
	default:      return "";
	} 
    };

    void   SetDateTime(Int_t  date,Int_t  time);
    void   SetRunNumber(Int_t runNumber)  {m_runNumber=runNumber;};
    void   SetDictionary(UInt_t nElements, Descriptor *D)
                                     {m_nElements=nElements; mdescriptor = D;}
    void   SetDictionary(Descriptor *D)
                                     {if (D) { mdescriptor = D;m_nElements=D->NumberOfColumns();}}
    void   SetTableName(const Char_t *table_name)
                                  {if(m_tableName) delete [] m_tableName;
                                   m_tableName=new char[strlen(table_name)+1]; 
                                   strcpy(m_tableName,table_name);};
    void   SetStructName(const Char_t *struct_name)
                                {if(m_structName) delete [] m_structName;
                                 m_structName=new char[strlen(struct_name)+1];
                                 strcpy(m_structName,struct_name);};
    void   SetVersionName(const char *version) 
                                 {if(m_tableVersion) delete [] m_tableVersion;
                                  m_tableVersion= new char[strlen(version)+1];
                                  strcpy(m_tableVersion,version);}
    void   SetDataBaseName(const char *dbName) 
                                 {if(m_database) delete [] m_database;
                                  m_database= new char[strlen(dbName)+1]; 
                                  strcpy(m_database,dbName);}
                                           
    void   SetStructSize(UInt_t size)     {m_sizeOfStruct=size;    };
    void   SetNRows(UInt_t nRows)         {m_nRows = nRows;        }
    void   SetBeginDate(Int_t  BeginDate) {m_BeginDate = BeginDate;}
    void   SetBeginTime(Int_t  BeginTime) {m_BeginTime = BeginTime;}
    void   SetEndDate(Int_t  EndDate)     {m_EndDate = EndDate;    }
    void   SetEndTime(Int_t  EndTime)     {m_EndTime = EndTime;    }
    void   SetRequestTimeStamp(UInt_t utime) {m_requestTimeStamp = utime; }
    void   SetBeginTimeStamp(UInt_t utime)   {m_beginTimeStamp   = utime; }
    void   SetEndTimeStamp(UInt_t utime)     {m_endTimeStamp     = utime; }
    void   SetProdTime(UInt_t ptime);
    void   AddProdTimeOverride(UInt_t ptime, char* dbType = 0, char* dbDomain = 0); // DBV override
    void   SetFlavor(const char* flavor);
    void   SetZombie(Bool_t zombie)          { m_isZombie=true; }

	void   addBlacklistedDomain(const char* domainName);

    static int DbInit(const char *);  		//Sasha's dbInit 
    void   setVerbose(int isVerbose) { m_isVerbose = isVerbose; } 
    void   printStatistics();
    void   CloseAllConnections();
    void   Release();

  //-> here's all the real stuff now
    dbConfig_st* InitConfig(const char* configName, int& numRows, char* versionName=0);
    StDbManager* mgr;
  
  ClassDef(StDbBroker,0)
};

// The old C-functions from Sasha's prototype & 1st integration
//extern "C" void DbFill(unsigned int *,         //datetime[4]
//		       const char *,  //tableName
//		       const char *,  //StructName
//		       unsigned int,           //nVar
//		       StDbBroker::oldDescriptor *d,
//		       const char **,       //Comments
//		       unsigned int,           //nRows
//		       unsigned int,           //sizeOfStruct
//		       void *);       //pData
		       
extern "C" void *DbUse(unsigned int*,           //&nRows,
		       unsigned int *,         //datetime[4]
		       const char *,  //tableName
		       const char *,  //StructName
		       unsigned int,           //nVar
		       unsigned int,           //sizeOfStruct
		       StDbBroker::oldDescriptor *d);

extern "C" void *DbRead(unsigned int*,           //&nRows,
		       unsigned int *,         //datetime[4]
		       const char *,  //tableName
		       const char *,  //StructName
		       unsigned int,           //nVar
		       unsigned int,           //sizeOfStruct
		       StDbBroker::oldDescriptor *d,
               const char*,            //  database Name
               const char*);           // versionName

extern "C" int DbInit(const char *);  		//dbInit

#endif












/***************************************************************************
 *
 * $Id: StDbBroker.h,v 1.14 2000/04/14 14:46:41 fine Exp $
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

class StDbConfigNode;
class StDbManager;
class StTableDescriptorI;

/* needed for GetComments only   */
class TTable;
/*this is a temporary quick-and-dirty class for db access*/


class StDbBroker  {
  public:
      enum EColumnType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt
                             ,kULong, kUShort, kUChar, kChar };

typedef St_tableDescriptor Descriptor;

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
    oldDescriptor *m_descriptor;
    Descriptor  *mdescriptor;
    Char_t *     m_structName;  //name of the struct type used in this TTable
    Char_t *     m_tableName;   //name of this instance of TTable
    UInt_t       m_sizeOfStruct;// byte size of this struct
    UInt_t       m_nElements;   // Number of variables in the structure
    UInt_t       m_nRows;       // number of rows in the table

    UInt_t       m_DateTime[2]; // Current DateTime

    UInt_t       m_BeginDate;   // begin date: 2021 05 31
    UInt_t       m_BeginTime;   // begin date: HH MM SS
    UInt_t       m_EndDate;     // end date
    UInt_t       m_EndTime;     // end time

    UInt_t       m_beginTimeStamp; // unix beginTime
    UInt_t       m_endTimeStamp; // unix endTime
    UInt_t       m_requestTimeStamp; // unix requestTime

    char*        m_tableVersion; // name of the version of the table
    char*        m_database;     // name of the database for this table
    char*        m_ParentType;   // named dbType when "top" db is domain-level

    int       m_isVerbose;
    dbNodeArray *m_Nodes;
    StDbConfigNode* m_Tree;

    dbConfig_st*  buildConfig(int& numRows);
    int           buildNodes(StDbConfigNode* node, int pID);

  public:

    StDbBroker();
    virtual ~StDbBroker();
    
    //    int Init(const char *dbname);// return 0 if OK

    void * Use();
    void * Use(int tabID, int parID);

    char  **GetComments(St_Table *parentTable);
    void   Fill(void * pArray, const char **ElementComment);

    UInt_t GetNRows()                {return m_nRows;       }
    UInt_t GetBeginDate()            {return m_BeginDate;   }
    UInt_t GetBeginTime()            {return m_BeginTime;   }
    UInt_t GetEndDate()              {return m_EndDate;     }
    UInt_t GetEndTime()              {return m_EndTime;     }
    UInt_t GetRequestTimeStamp()     {return m_requestTimeStamp; }
    UInt_t GetBeginTimeStamp()       {return m_beginTimeStamp; }
    UInt_t GetEndTimeStamp()         {return m_endTimeStamp; }


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


    void   SetDateTime(UInt_t date,UInt_t time);

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
    void   SetBeginDate(UInt_t BeginDate) {m_BeginDate = BeginDate;}
    void   SetBeginTime(UInt_t BeginTime) {m_BeginTime = BeginTime;}
    void   SetEndDate(UInt_t EndDate)     {m_EndDate = EndDate;    }
    void   SetEndTime(UInt_t EndTime)     {m_EndTime = EndTime;    }
    void   SetRequestTimeStamp(UInt_t utime) {m_requestTimeStamp = utime; }
    void   SetBeginTimeStamp(UInt_t utime)   {m_beginTimeStamp   = utime; }
    void   SetEndTimeStamp(UInt_t utime)     {m_endTimeStamp     = utime; }

    static int DbInit(const char *);  		//dbInit

    void   setVerbose(int isVerbose) { m_isVerbose = isVerbose; } 
    void   CloseAllConnections();

  //-> here's all the real stuff now
   
    dbConfig_st* InitConfig(const char* configName, int& numRows, char* versionName=0);

    StDbManager* mgr;


  //ClassDef(StDbBroker,0)
};

extern "C" void DbFill(unsigned int *,         //datetime[4]
		       const char *,  //tableName
		       const char *,  //StructName
		       unsigned int,           //nVar
		       StDbBroker::oldDescriptor *d,
		       const char **,       //Comments
		       unsigned int,           //nRows
		       unsigned int,           //sizeOfStruct
		       void *);       //pData
		       
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












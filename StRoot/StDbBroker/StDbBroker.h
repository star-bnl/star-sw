/***************************************************************************
 *
 * $Id: StDbBroker.h,v 1.10 2000/01/31 17:11:18 porter Exp $
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
#include "tableDescriptor.h" 

class StDbConfigNode;
class StDbManager;
class StTableDescriptorI;

/* needed for GetComments only   */
class St_Table;
/*this is a temporary quick-and-dirty class for db access*/


class StDbBroker  {
  public:
      enum EColumnType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt
                             ,kULong, kUShort, kUChar, kChar };

typedef tableDescriptor_st Descriptor;

  /*     struct Descriptor{
     char name[32];		//variable name 
     int firstDimension;		//first dimension, if this is an array
     int secondDimension;	//second dimension
     int offset;			//variable offset
     int size;			//total size of element
     int typeSize;		//unit size
     int dimensions;		//number of dimensions
     EColumnType type;		//type of element
     };
  */

  protected:
    Descriptor  *m_descriptor;
    Char_t *     m_structName;  //name of the struct type used in this St_Table
    Char_t *     m_tableName;   //name of this instance of St_Table
    UInt_t       m_sizeOfStruct;// byte size of this struct
    UInt_t       m_nElements;   // Number of variables in the structure
    UInt_t       m_nRows;       // number of rows in the table

    UInt_t       m_DateTime[2]; // Current DateTime

    UInt_t       m_BeginDate;   // begin date: 2021 05 31
    UInt_t       m_BeginTime;   // begin date: HH MM SS
    UInt_t       m_EndDate;     // end date
    UInt_t       m_EndTime;     // end time

    char*        m_tableVersion; // name of the version of the table
    char*        m_database;     // name of the database for this table

    int       m_isVerbose;
    dbNodeArray *m_Nodes;
    StDbConfigNode* m_Tree;

    dbConfig_st*  buildConfig(int numRows);
    int       buildNodes(StDbConfigNode* node, int pID);

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

    StTableDescriptorI* GetTableDescriptor();

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
                                     {m_nElements=nElements; m_descriptor = D;}

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
    static int DbInit(const char *);  		//dbInit

    void   setVerbose(int isVerbose) { m_isVerbose = isVerbose; } 
  //-> here's all the real stuff now
   
    dbConfig_st* InitConfig(const char* configName, int& numRows, char* versionName=0);

    StDbManager* mgr;


  //ClassDef(StDbBroker,0)
};

extern "C" void DbFill(unsigned int *,         //datetime[4]
		       const char *,  //tableName
		       const char *,  //StructName
		       unsigned int,           //nVar
		       StDbBroker::Descriptor *d,
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
		       StDbBroker::Descriptor *d);

extern "C" void *DbRead(unsigned int*,           //&nRows,
		       unsigned int *,         //datetime[4]
		       const char *,  //tableName
		       const char *,  //StructName
		       unsigned int,           //nVar
		       unsigned int,           //sizeOfStruct
		       StDbBroker::Descriptor *d,
               const char*,            //  database Name
               const char*);           // versionName

extern "C" int DbInit(const char *);  		//dbInit

#endif












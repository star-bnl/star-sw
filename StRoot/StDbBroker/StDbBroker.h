//  

#ifndef STAR_StDbBroker
#define STAR_StDbBroker

#include "Rtypes.h"

/* needed for GetComments only */
class St_Table;
/*this is a temporary quick-and-dirty class for db access*/
class StDbBroker  {
  public:
      enum EColumnType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt
                             ,kULong, kUShort, kUChar, kChar };
     struct Descriptor{
     char name[20];		//variable name 
     int firstDimension;		//first dimension, if this is an array
     int secondDimension;	//second dimension
     int offset;			//variable offset
     int size;			//total size of element
     int typeSize;		//unit size
     int dimensions;		//number of dimensions
     EColumnType type;		//type of element
     };

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

  public:
    StDbBroker(){}

    virtual ~StDbBroker(){}
    
    //    int Init(const char *dbname);// return 0 if OK

    void * Use();

    char  **GetComments(St_Table *parentTable);
    void   Fill(void * pArray, const char **ElementComment);

    UInt_t GetNRows()                {return m_nRows;       }
    UInt_t GetBeginDate()            {return m_BeginDate;   }
    UInt_t GetBeginTime()            {return m_BeginTime;   }
    UInt_t GetEndDate()              {return m_EndDate;     }
    UInt_t GetEndTime()              {return m_EndTime;     }

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

    void   SetDateTime(UInt_t date,UInt_t time)
                                   {m_DateTime[0] = date; m_DateTime[1]= time;}

    void   SetDictionary(UInt_t nElements, Descriptor *D)
                                     {m_nElements=nElements; m_descriptor = D;}

    void   SetTableName(const Char_t *table_name)
                                          {m_tableName=strdup(table_name);};
    void   SetStructName(const Char_t *struct_name)
                                          {m_structName=strdup(struct_name);};
    void   SetStructSize(UInt_t size)     {m_sizeOfStruct=size;    };
    void   SetNRows(UInt_t nRows)         {m_nRows = nRows;        }
    void   SetBeginDate(UInt_t BeginDate) {m_BeginDate = BeginDate;}
    void   SetBeginTime(UInt_t BeginTime) {m_BeginTime = BeginTime;}
    void   SetEndDate(UInt_t EndDate)     {m_EndDate = EndDate;    }
    void   SetEndTime(UInt_t EndTime)     {m_EndTime = EndTime;    }
    static int DbInit(const char *);  		//dbInit

    ClassDef(StDbBroker,0)
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

extern "C" int DbInit(const char *);  		//dbInit

#endif

//*-- Author :    Valery Fine   10/05/99  (E-mail: fine@bnl.gov)
// $Id: St_TableElementDescriptor.h,v 1.1 1999/05/18 18:00:43 fine Exp $
// $Log: St_TableElementDescriptor.h,v $
// Revision 1.1  1999/05/18 18:00:43  fine
// The first implementation of the table column descriptor
//  

#ifndef STAR_St_TableElementDescriptor
#define STAR_St_TableElementDescriptor

#include <TNamed.h>

class St_Table;

class St_TableElementDescriptor : public TNamed {
  public:
    enum EColumnType {kNAN, kFloat, kInt, kLong, kShort, kDouble, kUInt
                           ,kULong, kUShort, kUChar, kChar };
  protected:
    UInt_t       m_Offset;      // The first byte in the row this column 
    UInt_t       m_Size;        // The size of the selected column in bytes
    UInt_t       m_Dimensions;  // The number of the dimensions for array 
    EColumnType  m_Type;        // The data type of the selected column
  public:
    St_TableElementDescriptor(St_Table *parentTable=0, const Char_t *columnName="")
     { LearnTable(parentTable,columnName); }
   ~St_TableElementDescriptor(){}
    void   LearnTable(St_Table *parentTable, const Char_t *columnName);
    UInt_t GetOffset()               {return m_Offset;    }
    UInt_t GetSize()                 {return m_Size;      }
    UInt_t GetDimensions()           {return m_Dimensions;}
    EColumnType GetType()            {return m_Type;      }
    void   SetOffset(UInt_t offset)  {m_Offset     = offset;}
    void   SetSize(UInt_t size)      {m_Size       = size;  }
    void   SetDimensions(UInt_t dim) {m_Dimensions = dim;   }
    void   SetType(EColumnType type) {m_Type       = type;  }
};

#endif

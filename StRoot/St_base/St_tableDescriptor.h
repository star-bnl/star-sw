//*-- Author :    Valery Fine   10/05/99  (E-mail: fine@bnl.gov)
// $Id: St_tableDescriptor.h,v 1.4 1999/08/12 18:53:49 fine Exp $
// $Log: St_tableDescriptor.h,v $
// Revision 1.4  1999/08/12 18:53:49  fine
// clash between St_tableDescriptor::GetSize and St_Table::GetSize resolved
//
// Revision 1.3  1999/08/12 02:23:30  fine
//  GetRowDescriptor must be const
//
// Revision 1.2  1999/08/11 14:44:39  fine
// name clash with ROOT over enum resolved
//
// Revision 1.1  1999/08/11 00:40:12  fine
// new class St_tableDescriptor
//

#ifndef STAR_St_tableDescriptor
#define STAR_St_tableDescriptor

#include "St_Table.h"
#include "tableDescriptor.h"                 


class St_tableDescriptor : public St_Table {
  protected:
     static St_tableDescriptor *fgColDescriptors;
     virtual St_tableDescriptor *GetRowDescriptors() const { return fgColDescriptors?fgColDescriptors:(fgColDescriptors=GetTableDescriptors());}
     virtual void  SetRowDescriptors(St_tableDescriptor *list) { fgColDescriptors = list;}  

     St_tableDescriptor(){;}
  public:                                    
 
    St_tableDescriptor(const St_Table *parentTable);
   ~St_tableDescriptor();
    tableDescriptor_st *GetTable(){ return (tableDescriptor_st *)s_Table;}                                            
    virtual  const void *At(Int_t i) const;
             void        LearnTable(const St_Table *parentTable);
             const Char_t *GetColumnName(Int_t columnIndex)        const;
             const Int_t GetColumnByName(const Char_t *columnName=0) const;
             UInt_t      GetNumberOfColumns()                      const;
             UInt_t     *GetIndexArray(Int_t columnIndex)          const;
             UInt_t      GetOffset(Int_t columnIndex)              const;
             Int_t       GetOffset(const Char_t *columnName=0)     const;
             UInt_t      GetColumnSize(Int_t columnIndex)          const;
             Int_t       GetColumnSize(const Char_t *columnName=0) const;
             UInt_t      GetTypeSize(Int_t columnIndex)            const;
             Int_t       GetTypeSize(const Char_t *columnName=0)   const;
             UInt_t      GetDimensions(Int_t columnIndex)          const;
             Int_t       GetDimensions(const Char_t *columnName=0) const;
             EColumnType GetColumnType(Int_t columnIndex)          const;
             EColumnType GetColumnType(const Char_t *columnName=0) const;
             void        SetOffset(UInt_t offset,Int_t column);
             void        SetSize(UInt_t size,Int_t column);
             void        SetTypeSize(UInt_t size,Int_t column);
             void        SetDimensions(UInt_t dim,Int_t column);
             void        SetColumnType(EColumnType type,Int_t column);
    ClassDef(St_tableDescriptor,0)
};

//______________________________________________________________________________
inline const void *St_tableDescriptor::At(Int_t i) const
{
   if (!BoundsOk("St_tableDescriptor::At", i)) return 0;
   return (const void *)(((tableDescriptor_st *)s_Table)+i);
}

//______________________________________________________________________________
inline  const Char_t *St_tableDescriptor::GetColumnName(Int_t column)const{return ((tableDescriptor_st *)At(column))->m_ColumnName;}
inline  UInt_t St_tableDescriptor::GetOffset(Int_t column)          const {return ((tableDescriptor_st *)At(column))->m_Offset;    }
inline  UInt_t *St_tableDescriptor::GetIndexArray(Int_t column)     const {return ((tableDescriptor_st *)At(column))->m_IndexArray;}
inline  UInt_t St_tableDescriptor::GetNumberOfColumns()             const {return GetNRows();                                      }
inline  UInt_t St_tableDescriptor::GetColumnSize(Int_t column)      const {return ((tableDescriptor_st *)At(column))->m_Size;      }
inline  UInt_t St_tableDescriptor::GetTypeSize(Int_t column)        const {return ((tableDescriptor_st *)At(column))->m_TypeSize;  }
inline  UInt_t St_tableDescriptor::GetDimensions(Int_t column)      const {return ((tableDescriptor_st *)At(column))->m_Dimensions;}
inline  St_Table::EColumnType St_tableDescriptor::GetColumnType(Int_t column) const {return EColumnType(((tableDescriptor_st *)At(column))->m_Type);}
inline  void   St_tableDescriptor::SetOffset(UInt_t offset,Int_t column)  {((tableDescriptor_st *)At(column))->m_Offset     = offset;}
inline  void   St_tableDescriptor::SetSize(UInt_t size,Int_t column)      {((tableDescriptor_st *)At(column))->m_Size       = size;  }
inline  void   St_tableDescriptor::SetTypeSize(UInt_t size,Int_t column)  {((tableDescriptor_st *)At(column))->m_TypeSize   = size;  }
inline  void   St_tableDescriptor::SetDimensions(UInt_t dim,Int_t column) {((tableDescriptor_st *)At(column))->m_Dimensions = dim;   }
inline  void   St_tableDescriptor::SetColumnType(EColumnType type,Int_t column) {((tableDescriptor_st *)At(column))->m_Type = type;  }

#endif

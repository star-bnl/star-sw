//*-- Author :    Valery Fine   10/05/99  (E-mail: fine@bnl.gov)
// $Id: St_tableDescriptor.h,v 1.9 2000/03/05 04:11:48 fine Exp $
#ifndef STAR_St_tableDescriptor
#define STAR_St_tableDescriptor

#include "St_Table.h"
#include "tableDescriptor.h"                 

class St_tableDescriptor : public St_Table {
  protected:
     static St_tableDescriptor *fgColDescriptors;
     virtual St_tableDescriptor *GetDescriptorPointer() const { return fgColDescriptors;} 
     virtual void  SetDescriptorPointer(St_tableDescriptor *list) const { fgColDescriptors = list;}  
  public:                                    
    St_tableDescriptor(const St_Table *parentTable=0);
    St_tableDescriptor(TClass *classPtr);
   ~St_tableDescriptor();
    TString CreateLeafList() const;
    tableDescriptor_st *GetTable(){ return (tableDescriptor_st *)s_Table;}                                            
             void        LearnTable(const St_Table *parentTable);
             void        LearnTable(TClass *classPtr);
             const Char_t *ColumnName(Int_t columnIndex)        const;
             const Int_t ColumnByName(const Char_t *columnName=0) const;
             UInt_t      NumberOfColumns()                      const;
             UInt_t     *IndexArray(Int_t columnIndex)          const;
             UInt_t      Offset(Int_t columnIndex)              const;
             Int_t       Offset(const Char_t *columnName=0)     const;
             UInt_t      ColumnSize(Int_t columnIndex)          const;
             Int_t       ColumnSize(const Char_t *columnName=0) const;
             UInt_t      TypeSize(Int_t columnIndex)            const;
             Int_t       TypeSize(const Char_t *columnName=0)   const;
             UInt_t      Dimensions(Int_t columnIndex)          const;
             Int_t       Dimensions(const Char_t *columnName=0) const;
             EColumnType ColumnType(Int_t columnIndex)          const;
             EColumnType ColumnType(const Char_t *columnName=0) const;
             void        SetOffset(UInt_t offset,Int_t column);
             void        SetSize(UInt_t size,Int_t column);
             void        SetTypeSize(UInt_t size,Int_t column);
             void        SetDimensions(UInt_t dim,Int_t column);
             void        SetColumnType(EColumnType type,Int_t column);
    virtual  Int_t       UpdateOffsets(const St_tableDescriptor *newDesciptor);
    ClassDef(St_tableDescriptor,0)
};

//______________________________________________________________________________
inline  const Char_t *St_tableDescriptor::ColumnName(Int_t column)const{return ((tableDescriptor_st *)At(column))->m_ColumnName;}
inline  UInt_t St_tableDescriptor::Offset(Int_t column)          const {return ((tableDescriptor_st *)At(column))->m_Offset;    }
inline  UInt_t *St_tableDescriptor::IndexArray(Int_t column)     const {return ((tableDescriptor_st *)At(column))->m_IndexArray;}
inline  UInt_t St_tableDescriptor::NumberOfColumns()             const {return GetNRows();                                      }
inline  UInt_t St_tableDescriptor::ColumnSize(Int_t column)      const {return ((tableDescriptor_st *)At(column))->m_Size;      }
inline  UInt_t St_tableDescriptor::TypeSize(Int_t column)        const {return ((tableDescriptor_st *)At(column))->m_TypeSize;  }
inline  UInt_t St_tableDescriptor::Dimensions(Int_t column)      const {return ((tableDescriptor_st *)At(column))->m_Dimensions;}
inline  St_Table::EColumnType St_tableDescriptor::ColumnType(Int_t column) const {return EColumnType(((tableDescriptor_st *)At(column))->m_Type);}
inline  void   St_tableDescriptor::SetOffset(UInt_t offset,Int_t column)  {((tableDescriptor_st *)At(column))->m_Offset     = offset;}
inline  void   St_tableDescriptor::SetSize(UInt_t size,Int_t column)      {((tableDescriptor_st *)At(column))->m_Size       = size;  }
inline  void   St_tableDescriptor::SetTypeSize(UInt_t size,Int_t column)  {((tableDescriptor_st *)At(column))->m_TypeSize   = size;  }
inline  void   St_tableDescriptor::SetDimensions(UInt_t dim,Int_t column) {((tableDescriptor_st *)At(column))->m_Dimensions = dim;   }
inline  void   St_tableDescriptor::SetColumnType(EColumnType type,Int_t column) {((tableDescriptor_st *)At(column))->m_Type = type;  }

//______________________________________________________________________________
// $Log: St_tableDescriptor.h,v $
// Revision 1.9  2000/03/05 04:11:48  fine
// Automatic schema evolution for St_Table has been activated
//
// Revision 1.8  2000/02/29 01:54:49  fine
// St_Table -> turn automatic schema evolution for table version 2 and above
//
// Revision 1.7  2000/01/24 03:55:48  fine
// new nethod CreateLeafList() to create text descriptor compatible with TBranch ctor
//
// Revision 1.6  1999/10/28 00:32:55  fine
// method At() has been removed
//
// Revision 1.5  1999/09/07 19:30:29  fine
// table descriptor access has been changed. All tables are affected and must be re-compiled
//
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
//______________________________________________________________________________


#endif

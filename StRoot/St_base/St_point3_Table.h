#ifndef STAF_St_point3_Table
#define STAF_St_point3_Table
// $Id: St_point3_Table.h,v 1.1 1999/12/29 20:44:20 fine Exp $

#include "St_Table.h"

#include "point3.h"
  
class St_point3 : public St_Table
{
protected:
  static St_tableDescriptor *fgColDescriptors;
  virtual St_tableDescriptor *GetDescriptorPointer() const { return fgColDescriptors;}
  virtual void SetDescriptorPointer(St_tableDescriptor *list) const { fgColDescriptors = list;}
public:
  St_point3() : St_Table("point3",sizeof(point3_st)) {SetType("point3");}
  St_point3(Text_t *name) : St_Table(name,sizeof(point3_st)) {SetType("point3");}
  St_point3(Int_t n): St_Table("point3",n,sizeof(point3_st)) {SetType("point3");}
  St_point3(Text_t *name,Int_t n): St_Table(name,n,sizeof(point3_st)) {SetType("point3");}
  point3_st *GetTable(Int_t i=0){ return ((point3_st *)s_Table)+i;}
  point3_st &operator[](Int_t i){ assert(i>=0 && i < GetNRows()); return *GetTable(i); }

  ClassDef(St_point3,0) // class 3D point vector
};
// $Log: St_point3_Table.h,v $
// Revision 1.1  1999/12/29 20:44:20  fine
// New set of classes to provide update detector geometry from Db
//
#endif

//*-- Author :    Valery Fine   10/05/99  (E-mail: fine@bnl.gov)
// $Id: St_Table3Points.cxx,v 1.2 1999/05/24 01:54:02 fine Exp $
// $Log: St_Table3Points.cxx,v $
// Revision 1.2  1999/05/24 01:54:02  fine
// clean up
//
// Revision 1.1  1999/05/18 20:21:24  fine
// New class to 3D table viewer
//  

#include  "St_Table3Points.h"
#include  "St_TableElementDescriptor.h"

ClassImp(St_Table3Points)

//________________________________________________________________________________
St_Table3Points::St_Table3Points():m_ArrayOfColumnDesciptors(0){}

//________________________________________________________________________________
St_Table3Points::St_Table3Points(St_TableSorter *sorter,const void *key,
                       const Char_t *xName, const Char_t *yName, const Char_t *zName
                      ,Option_t *opt)
                : St_TablePoints(sorter,key,opt)

{ 
  m_ArrayOfColumnDesciptors =  new TObjArray(kTotalSize);
  SetXColumn(xName);  SetYColumn(yName);  SetZColumn(zName); 
}
//________________________________________________________________________________
St_Table3Points::~St_Table3Points()
{
  if (m_ArrayOfColumnDesciptors) 
  {
    m_ArrayOfColumnDesciptors->Delete();
    delete m_ArrayOfColumnDesciptors;
    m_ArrayOfColumnDesciptors = 0;
  }
}
//________________________________________________________________________________
St_TableElementDescriptor *St_Table3Points::GetDescriptor(EPointDirection idx) const
{
  TObjArray &refs  = *m_ArrayOfColumnDesciptors;
  return  (St_TableElementDescriptor *)refs[idx];
}
//________________________________________________________________________________
Float_t St_Table3Points::GetAnyPoint(Int_t idx, EPointDirection xAxis) const 
{
 Float_t point    = 0;
 St_Table  *table = 0;
 if (m_TableSorter) table = m_TableSorter->GetTable();
 if (table) {
    const Char_t *tablePtr = (Char_t *)m_Rows;
    tablePtr += Indx(idx)*(table->GetRowSize());
    St_TableElementDescriptor *tableDsc = GetDescriptor(xAxis);
    tablePtr += tableDsc->GetOffset();
    point = *((Float_t *)tablePtr);
 }
  return point;
}
//____________________________________________________________________________
void St_Table3Points::SetAnyColumn(const Char_t *anyName, EPointDirection indx)
{
   St_TableElementDescriptor *dsc = (St_TableElementDescriptor *)(*m_ArrayOfColumnDesciptors)[indx];
   if (dsc) delete dsc;
   dsc = new St_TableElementDescriptor(m_TableSorter->GetTable(), anyName);
   if (dsc->IsZombie()) { delete dsc; dsc = 0; MakeZombie();}
   (*m_ArrayOfColumnDesciptors)[indx] =  dsc;
}

//____________________________________________________________________________
Float_t *St_Table3Points::GetXYZ(Float_t *xyz,Int_t idx, Int_t num) const
{
  if (xyz) {
    Int_t size = TMath::Min(idx+num,Size());
    Int_t j=0;
    for (Int_t i=idx;i<size;i++) {
      xyz[j++] = GetX(i);
      xyz[j++] = GetY(i);
      xyz[j++] = GetZ(i);
    }
  }
  return xyz;
}


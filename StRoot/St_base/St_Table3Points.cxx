//*-- Author :    Valery Fine   10/05/99  (E-mail: fine@bnl.gov)
// $Id: St_Table3Points.cxx,v 1.3 1999/06/01 01:27:53 fine Exp $
// $Log: St_Table3Points.cxx,v $
// Revision 1.3  1999/06/01 01:27:53  fine
// Comments clean ups
//
// Revision 1.2  1999/05/24 01:54:02  fine
// clean up
//
// Revision 1.1  1999/05/18 20:21:24  fine
// New class to 3D table viewer
//  

#include  "St_Table3Points.h"
#include  "St_TableElementDescriptor.h"

///////////////////////////////////////////////////////////////////////////////////
//
//   St_Table3Points class is to create 3D view of any 3 columns of the St_Table objects
//   with one the same "key column value".
//
//   For example all values of the column "x[0]" "x[1]" "x[2]" of the begin_html <a href="http://www.rhic.bnl.gov/STAR/html/comp_l/root/html/g2t_tpc_hit_st.html"> g2t_tpc_hit </a> end_html table
//   from the rows with one and same "track_id" column value will be regarded
//   as an image of one and the same "track".
//   The last means all those points will be painted with one and the same 3D
//   attributes like "color", "size", "style", "light","markers", "connections"  etc.
//
//   The original St_Table object must be pre-sorted by "key column" via St_TableSorter
//   class 
//
// void   CreatePoints(St_g2t_tpc_hit *points)
// {
//   g2t_tpc_hit_st *p = points->GetTable();
//
//  St_Table3Points *track = 0;
//  TString tr;
//  tr = "track_p";
//  St_Table &ttt = *((St_Table *)points);
//  // Track2Line MUST be on heap othwise 3D view will crash just code leaves this
//  // subroutine
//  We will assemble all points by its "track_p" field.
//
//  St_TableSorter *Track2Line = new St_TableSorter (ttt,"track_p");
//
//  Int_t i = 0;
//  Char_t buffer[10];
//  Int_t ntracks = 0;
//  const Int_t maxtracks = 5;
////---------------------------- Fill tracks -------------------
//  long currentId = -1;
//  long newId =  0;
//  g2t_tpc_hit_st *hitPoint = 0;
//  St_Node *thisTrack[7] = {0,0,0,0,0,0,0}; // seven noded for 7 different colors
//  Int_t  MaxRowtoCount = 5000; // 5000;
//  Int_t  MaxTracks = Track2Line->CountKeys();
//  MaxTracks = 100;
//  for (i=0;i<Track2Line->GetNRows() && ntracks <  MaxTracks ;i++) 
//  {
//   hitPoint = p + Track2Line->GetIndex(i);
//   newId =  hitPoint->track_p;
//   if (newId != currentId)  { // The hit for the new track has been found
//      
//     const Char_t *xName = "x[0]";
//     const Char_t *yName = "x[1]";
//     const Char_t *zName = "x[2]";
//
//     track =  new St_Table3Points(Track2Line,(const void *)&newId,xName,yName,zName);
//
//     // Create a shape for this node
//     St_PolyLineShape *trackShape  =  new St_PolyLineShape(track);
//     trackShape->SetVisibility(1);
//     Int_t colorIndx = ntracks%7;
//     trackShape->SetColorAttribute(colorIndx+kGreen);
//     trackShape->SetLineStyle(1);
//     trackShape->SetSizeAttribute(2);
//     // Create a node to hold it
//     if (!thisTrack[colorIndx])  {
//         thisTrack[colorIndx] = new St_Node("hits","hits",trackShape);
//         thisTrack[colorIndx]->Mark();
//         thisTrack[colorIndx]->SetVisibility();
//         St_NodePosition *pp = hall->Add(thisTrack[colorIndx]);
//         if (!pp) printf(" no position %d\n",ntrack);
//     }
//     else 
//       thisTrack[colorIndx]->Add(trackShape);
//     currentId = newId;
//     ntracks++;
//   }  
// }
//
///////////////////////////////////////////////////////////////////////////////////
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


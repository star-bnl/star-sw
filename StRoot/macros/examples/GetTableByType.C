// $Id: GetTableByType.C,v 1.2 1999/12/20 15:27:46 fine Exp $
//=======================================================================
// owner: Valeri Fine (fine@bnl.gov)
// what it does: returns the name('s) of objects byt title (type)
//=======================================================================

St_DataSet *GetTableByType(const Char_t *tableType="dst_point") 
{
  // returns the name('s) of St_Dataset objects by title 
  //                         the St_Table  by its type
  // For example:

  // root4star [2] .x bfc.C(1)
  // root4star [4] .x GetTableByType.C("dst_track")
  //          globtrk  : bfc/.make/dst/.data/dst/globtrk
  //          globtrk2 : bfc/.make/dst/.data/dst/globtrk2
  //          primtrk  : bfc/.make/dst/.data/dst/primtrk
  //  The above means the bfc - bug full chain has 3 different tables
  //  of "dst_track" type. All of them are produced by "dst" maker and
  //  are held by its "dst" dataset.
  //

  St_DataSetIter next(chain,0);  St_DataSet *t = 0;  St_DataSet *lastT = 0;
  while (t = next())  
   if (!strcmp(t->GetTitle(),tableType)){ lastT = t; cout << t->GetName() << " : " <<  t->Path() << endl; }   
  return lastT;
}
// $Log: GetTableByType.C,v $
// Revision 1.2  1999/12/20 15:27:46  fine
// it prints the full path now
//

// $Id: TestSorter.C,v 1.10 2006/08/15 21:43:05 jeromel Exp $
// $Log: TestSorter.C,v $
// Revision 1.10  2006/08/15 21:43:05  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.9  2000/06/05 18:14:13  fine
// Adjuested to ROOT 2.24
//
// Revision 1.8  2000/01/19 21:00:40  kathy
// update macros to use standard default xdf files in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.7  2000/01/12 02:20:19  fine
// test check new St_TableSorter ctors
//
// Revision 1.6  1999/12/01 13:57:42  fine
//  Second test to check operator [] has been introduced
//
// Revision 1.5  1999/05/21 15:33:55  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
//=======================================================================
// owner: Valery Fine
// what it does: 
//=======================================================================
class TTableSorter;
class St_particle;
TTableSorter *sorter = 0;
St_particle *table=0;
void Load() {
    if (gSystem.Load("xdf2root"))     printf(" Loading DLL \"xdf2root\" failed \n");
    if (gSystem.Load("St_Tables"))    printf(" Loading DLL \"St_Tables\" failed \n");
}
void TestSorter(Char_t *xdffilename="/afs/rhic.bnl.gov/star/data/samples/test.xdf",const Char_t *col="phep[3]")
{
 //   Read XDF file
    Load();
    St_XDFFile  xdf(xdffilename);
    TDataSet *event = xdf.NextEventGet();
    if (!event) { printf(" NO events \n"); return;}
    table=0;
    TDataSetIter root(event);
    table = (St_particle *)root["/evgen/particle"]; // [] means we are looking for the "real" table/ not just a St_DataSet
    if (table) {
       TString colName = col;
       sorter = new TTableSorter(table,colName,1,5);  
//       sorter = new TTableSorter(*table,colName,1,5);  
       table->Print(0,6);
       int cols = sorter->GetFirstRow() + sorter->GetNRows() -1;
       cout << " Result of the ordering the table: " << endl 
            << "<" << sorter->GetTableName() <<" : " << sorter->GetTableType() << "[" << sorter->GetFirstRow() << "]> - "	    
            << "<" << sorter->GetTableName() 
	    <<" : " << sorter->GetTableType() 
	    << "[" << cols << "]> "
            << " along: \"" << sorter->GetName() << "\" column" << endl;
       cout << "This table contains " << sorter->CountKeys() << " different keys" << endl;
       int i;

       cout << "Index:";
       for (i=0; i < sorter->GetNRows(); i++) cout << "   [" << sorter->GetIndex(i) << "]  ";
       cout << endl;

       cout << "Value: " ;
       particle_st *particle = table->GetTable();
       for (i=0; i < sorter->GetNRows(); i++) cout << particle[sorter->GetIndex(i)]->phep[3] << "  ";
       cout << endl;

       cout << " Binary Search test:"<< endl;
       for (i=sorter->GetNRows()-1; i >= 0 ; i--) {
          Float_t ph = particle[sorter->GetIndex(i)]->phep[3]; 
          Int_t lastFound = sorter->BinarySearch(ph);
          cout << i << ". " << ph << " == " << lastFound << " : " << sorter->GetLastFound() << endl;
       }
   
//      Int_t key2Count = 1;
//      cout << " Key: " << key2Count << " found " << sorter->CountKey(&key2Count) << " times" << endl;
//      key2Count = 10;
//      cout << " Key: " << key2Count << " found " << sorter->CountKey(&key2Count) << " times" << endl;
    }
  //  second sample:
  //   /afs/rhic.bnl.gov/star/data/samples/set0027_03_49evts_dst.xdf
    cout << " Second pass " << endl;
    St_XDFFile  xd("/afs/rhic.bnl.gov/star/data/samples/gstar.dst.xdf");
    event = xd.NextEventGet();
    if (event) {
      St_dst_vertex *table=0;
      table =  (St_dst_vertex *)event->FindByName("vertex");
      if (table) {
        TString colName = "vtx_id";
          sorter = new TTableSorter(table,colName,1,5);  
//        sorter = new TTableSorter(*table,colName,1,5);  
//        sorter = new TTableSorter(table->GetHeader(),colName,1,5);  
        TTableSorter &sort = *sorter;  
        table->Print(0,6);
        int cols = sorter->GetFirstRow() + sorter->GetNRows() - 1;
        cout << " Result of the ordering the table: " << endl 
             << "<" << sorter->GetTableName() <<" : " << sorter->GetTableType() << "[" << sorter->GetFirstRow() << "]> - "	    
             << "<" << sorter->GetTableName() 
   	     <<" : " << sorter->GetTableType() 
  	     << "[" << cols << "]> "
             << " along: \"" << sorter->GetName() << "\" column" << endl;
        cout << "This table contains " << sorter->CountKeys() << " different keys" << endl;

        cout << "Index:";
        for (i=0; i < sorter->GetNRows(); i++) cout << "   [" << sorter->GetIndex(i) << "]  ";
        cout << endl;

        cout << "Value: " ;
        dst_vertex_st *vertex = table->GetTable();
        for (i=0; i < sorter->GetNRows(); i++) cout << vertex[sorter->GetIndex(i)]->vtx_id << "  ";
        cout << endl;
        for (i=0; i < sorter->GetNRows(); i++) cout << vertex[sorter->GetIndex(i)]->z << "  ";
        cout << endl;

       cout << " Binary Search test:"<< endl;
       Int_t nrows = sorter->GetNRows() - 1;
       for (i=nrows; i >= 0 ; i--) {
          Short_t vtx = vertex[sorter->GetIndex(i)]->vtx_id; 
          Int_t lastFound = sorter->BinarySearch(vtx);
          cout << i << ". " << vtx << " == " << lastFound << " : " << sorter->GetLastFound() << endl;
          if (sort[vtx] != lastFound) 
                cout << " *** Error ****  " << lastFound << " != " << sorter[vtx] << endl;
//                printf(" *** Error ****  %d != %d \n" lastFound,sorter[vtx]);
       }
   
//      Int_t key2Count = 1;
//      cout << " Key: " << key2Count << " found " << sorter->CountKey(&key2Count) << " times" << endl;
//      key2Count = 10;
//      cout << " Key: " << key2Count << " found " << sorter->CountKey(&key2Count) << " times" << endl;
    }
   }
   else  cout << " NO events" << endl;
}

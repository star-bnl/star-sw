class St_TableSorter;
class St_particle;
St_TableSorter *sorter = 0;
St_particle *table=0;
void Load() {
    if (gSystem.Load("St_base"))      printf(" Loading DLL \"St_base\" failed \n");
    if (gSystem.Load("xdf2root"))     printf(" Loading DLL \"xdf2root\" failed \n");
    if (gSystem.Load("St_Tables"))    printf(" Loading DLL \"St_Tables\" failed \n");
}
void TestSorter(Char_t *xdffilename="/afs/rhic/star/data/samples/test.xdf",const Char_t *col="phep[3]")
{
 //   Read XDF file
    Load();
    St_XDFFile  xdf(xdffilename);
    St_DataSet *event = xdf.NextEventGet();
    if (!event) { printf(" NO events \n"); return;}
    table=0;
    St_DataSetIter root(event);
    table = (St_particle *)root["/evgen/particle"]; // [] means we are looking for the "real" table/ not just a St_DataSet
    if (table) {
       TString colName = col;
       sorter = new St_TableSorter(*table,colName,1,5);  
       table->Print(0,6);
       int cols = sorter->GetFirstRow() + sorter->GetNRows() -1;
       cout << " Result of the ordering the table: " << endl 
            << "<" << sorter->GetTableName() <<" : " << sorter->GetTableType() << "[" << sorter->GetFirstRow() << "]> - "	    
            << "<" << sorter->GetTableName() 
	    <<" : " << sorter->GetTableType() 
	    << "[" << cols << "]> "
            << " along: \"" << sorter->GetName() << "\" column" << endl;
       int i;

       cout << "Index:";
       for (i=0; i < sorter->GetNRows(); i++) cout << "   [" << sorter->GetIndex(i) << "]  ";
       cout << endl;

       cout << "Value: " ;
       particle_st *particle = table->GetTable();
       for (i=0; i < sorter->GetNRows(); i++) cout << particle[sorter->GetIndex(i)]->phep[3] << "  ";
       cout << endl;
    }
}

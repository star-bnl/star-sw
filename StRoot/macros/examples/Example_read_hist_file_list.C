// $Id: Example_read_hist_file_list.C,v 1.3 1999/10/07 14:13:10 kathy Exp $
// $Log: Example_read_hist_file_list.C,v $
// Revision 1.3  1999/10/07 14:13:10  kathy
// changes to Example macros to make them work in dev - mostly changes were correcting input file name
//
// Revision 1.2  1999/06/24 20:24:56  kathy
// updated comment lines at top
//
//=======================================================================
// owner: Kathy Turner, 24 June 99
// what it does: reads a flat root histogram file (not star specific) and
//     list all histograms - one can easily add functionality to draw or
//     send to postscript file from here
//
//  - adapted from macro taken from roottalk digest 
//           http://root.cern.ch/root/roottalk/roottalk99/1228.html
//=========================================================================

void Example_read_hist_file_list(const char* filein="Kathy_hist.root")
{
TFile* fin = new TFile(filein) ;

if (!fin->IsOpen()) {
printf("<E> Cannot open input file %s\n",filein) ;
exit(1) ;
}

TList* list = fin->GetListOfKeys() ;
if (!list) { printf("<E> No keys found in file\n") ; exit(1) ; }
TIter next(list) ;
TKey* key ;
TObject* obj ;

while ( key = (TKey*)next() ) {
obj = key->ReadObj() ;
if ( (strcmp(obj->IsA()->GetName(),"TProfile")!=0)
 && (!obj->InheritsFrom("TH2"))
 && (!obj->InheritsFrom("TH1")) ) 
  {
  printf("<W> Object %s is not 1D or 2D histogram : "
  "will not be converted\n",obj->GetName()) ;
  }

 printf("Histo name:%s title:%s\n",obj->GetName(),obj->GetTitle());
 }

 cout << " finished macro " << endl;
}


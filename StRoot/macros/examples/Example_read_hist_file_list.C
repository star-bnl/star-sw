// $Id: Example_read_hist_file_list.C,v 1.2 1999/06/24 20:24:56 kathy Exp $
// $Log: Example_read_hist_file_list.C,v $
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

void draw_hist(const char* filein="/star/u2e/liq/rootc/beam_gas_1.hist.root")
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

 cout << " finish draw_hist " << endl;
}


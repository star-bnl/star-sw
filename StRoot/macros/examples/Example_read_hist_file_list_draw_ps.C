// $Id: Example_read_hist_file_list_draw_ps.C,v 1.6 2006/08/15 21:42:57 jeromel Exp $
// $Log: Example_read_hist_file_list_draw_ps.C,v $
// Revision 1.6  2006/08/15 21:42:57  jeromel
// Fix rhic -> rhic.bnl.gov
//
// Revision 1.5  2000/01/19 15:46:05  kathy
// change default input files to point to ones in /afs/rhic.bnl.gov/star/data/samples
//
// Revision 1.4  1999/11/30 20:04:17  kathy
// fix Example macros so that they work from .dst.root files or .dst.xdf files & update documentation; also had to change which values printed in *read_dst_print_tables* macro since the names have changed in dst tables
//
// Revision 1.3  1999/11/03 19:03:05  kathy
// changes to default input files and output file names - needed by perl script for testing
//
// Revision 1.2  1999/11/02 22:54:36  kathy
// fixing documentation in macro
//
// Revision 1.1  1999/10/11 17:17:58  kathy
// changed names of some macros to make them more standard; changed default input file to MakeHists since previous no longer existed; combined some macros so that the one example will show all functionality
//
//=======================================================================
// owner: Kathy Turner, 24 June 99
// what it does: reads a flat root histogram file (not star specific) and
//     list all histograms - one can easily add functionality to draw or
//     send to postscript file from here
//
//   This macro assumes you don't know what's in the file a priori.
//
//   You can first run  Example_read_dst_makehist.C to create 
//     an output *hist.root file (flat file).  
//   This macro then reads in the histogram file and draws 
//     histogram h1.
//
//   This will not work for *.hist.root files produced from bfc.C since
//    they have a tree directory structure.
//
//  - adapted from macro taken from roottalk digest 
//           http://root.cern.ch/root/roottalk/roottalk99/1228.html
//=========================================================================
// in TPostScript set 
//  *-*     111 ps  Portrait
//  *-*     112 ps  Landscape
//  *-*     113 eps
//  *-*
//

void Example_read_hist_file_list_draw_ps(
    const char* filein="Example_read_dst_makehist.root")
{

// open file
TFile fin(filein);

if (!fin->IsOpen()) {
printf("<E> Cannot open input file %s\n",filein) ;
exit(1) ;
}

// list contents
fin.ls();

// create canvas
TCanvas MyCanvas("CanvasName","Canvas Title",800,600);

// set statistics on
gStyle->SetOptStat(111111);

// set paper size
Int_t width = 21;
Int_t height = 27;
gStyle->SetPaperSize(width, height);

// define output ps file 
TPostScript ps("Example_read_hist_file_list_draw_ps.ps",111);

//range of figures on ps page
ps.Range(20,26);

//..................................................................
// The following code (in ...) is all because you don't know the names
//   of the histograms in the file ahead of time

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

//...............................................................

// Now do the following commands for each histogram 
 obj->Draw();
// do after each histogram (don't have to if at command line)
 gPad->Update();
 }

// close output file
 ps.Close();
 cout << " finished macro " << endl;
}


// $Id: Example_read_hist_file_min_list_draw_ps.C,v 1.4 1999/11/30 20:04:17 kathy Exp $
// $Log: Example_read_hist_file_min_list_draw_ps.C,v $
// Revision 1.4  1999/11/30 20:04:17  kathy
// fix Example macros so that they work from .dst.root files or .dst.xdf files & update documentation; also had to change which values printed in *read_dst_print_tables* macro since the names have changed in dst tables
//
// Revision 1.3  1999/11/03 19:03:06  kathy
// changes to default input files and output file names - needed by perl script for testing
//
// Revision 1.2  1999/11/02 22:54:36  kathy
// fixing documentation in macro
//
// Revision 1.1  1999/10/11 17:17:58  kathy
// changed names of some macros to make them more standard; changed default input file to MakeHists since previous no longer existed; combined some macros so that the one example will show all functionality
//
//
//=======================================================================
// owner: Kathy Turner
// what it does:
//   Minimal commands to read hist file and list,draw & send to ps
//   histogram with known name.
//
//   You must first run  Example_read_dst_makehist.C to create 
//     an output *hist.root file (flat file).  
//   This macro then reads in the histogram file and draws 
//     histogram h1.
//   This will not work for *.hist.root files produced from bfc.C since
//    they have a tree directory structure.
//=======================================================================
//
void Example_read_hist_file_min_list_draw_ps()
{
TFile f1("Example_read_dst_makehist.root");
f1.ls();
TCanvas MyCanvas("CanvasName","Canvas Title",800,600);
gStyle->SetOptStat(111111);
TPostScript ps("Example_read_hist_file_min_list_draw_ps.ps",111);
h1->Draw();
ps.Close();
}

// $Id: Example_read_hist_file_min_list_draw_ps.C,v 1.2 1999/11/02 22:54:36 kathy Exp $
// $Log: Example_read_hist_file_min_list_draw_ps.C,v $
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
//   You must first run  Example_read_xdf_makehist.C to create 
//     an output Kathy_hist.root file (flat file).  
//   This macro then reads in the histogram file and draws 
//     histogram h1.
//   This will not work for *.hist.root files produced from bfc.C since
//    they have a tree directory structure.
//=======================================================================
//
void Example_read_hist_file_min_list_draw_ps()
{
TFile f1("Kathy_hist.root");
f1.ls();
TCanvas MyCanvas("CanvasName","Canvas Title",800,600);
gStyle->SetOptStat(111111);
TPostScript ps("kathy_hist.ps",111);
h1->Draw();
ps.Close();
}

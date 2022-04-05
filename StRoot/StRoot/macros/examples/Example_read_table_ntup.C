// $Id: Example_read_table_ntup.C,v 1.2 2000/04/12 16:13:40 kathy Exp $
// $Log: Example_read_table_ntup.C,v $
// Revision 1.2  2000/04/12 16:13:40  kathy
// have changed so that macro loads only table libraries needed instead of all table libraries
//
// Revision 1.1  2000/01/21 18:20:46  kathy
// now have macro that writes a table-based ntuple and another that reads the ntuple back in and draws from it
//
//
//=======================================================================
// owner: Kathy Turner
// what it does:
//   - read a root table ntuple made from Example_read_dst_write_table_ntup.C
//   - make plots
//
// NOTE: the NtupName is the NAME of the table that's on the ntuple!
//=======================================================================

void Example_read_table_ntup(
  const Char_t *NtupFile="globtrk_ntup.root",
  const Char_t *NtupName="globtrk")
{
 
  cout << " Input ntup file = " << NtupFile << endl;
  cout << " Input ntup name = " << NtupName << endl;

    gSystem->Load("StUtilities");
    gSystem->Load("StAnalysisUtilities");

    f1 = new TFile(NtupFile);

    //cout << "List file" << endl;
    f1.ls();

    //cout << "Print file" << endl;
    f1.Print();

    //cout << "Print ntup" << endl;
    //globtrk->Print();
   
 globtrk->Draw("x_first0:x_first1");

 globtrk->Draw("x_first0:x_first1","n_point<50 && n_fit_point<40");

    f1.Close();
}





// $Id: Example_readdst_makehist.C,v 1.1 1999/09/28 21:59:08 kathy Exp $
// $Log: Example_readdst_makehist.C,v $
// Revision 1.1  1999/09/28 21:59:08  kathy
// add new macro to show how to read .dst.root file, find a value in a table and make a histogram of it and print out
//
//=======================================================================
// owner: Kathy Turner
// what it does:
//    Reads an .dst.root file, finds a table (dst/vertex) and fills
//    histogram with variable "z" from the table.  Draws hist. to
//    canvas and sends to output ps file at the end.
//
//=======================================================================


void Example_readdst_makehist(Int_t nevents=3, const char
*MainFile="/disk00000/star/test/new/tfs_Solaris/year_2a/psc0208_01_40evts.dst.root")
{
 
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StTreeMaker");
    gSystem->Load("StarClassLibrary");


//  Setup top part of chain
    chain = new StChain("bfc");
    chain->SetDebug();
   
//  Input Tree - put in chain
  StTreeMaker *treeMk = new StTreeMaker("treeRead",MainFile);
  treeMk->SetIOMode("r");
  treeMk->SetDebug();
  treeMk->SetBranch("*",0,"0");  		//deactivate all branches
  treeMk->SetBranch("dstBranch",0,"r");	//activate EventBranch

  
// --- now execute chain member functions
  chain->Init();
 
// open output hist file 
 TFile *hist_outfile = 0;
 const Char_t *root_file = "Kathy_hist.root";
 hist_outfile = new TFile(root_file,"RECREATE");

// book histogram
 TH1F *h1 = new TH1F("h1","my first hist",100,-100.,100.);

  for (int iev=0;iev<nevents; iev++)
  {
    chain->Clear();
    int iret = chain->Make();
    if (iret) break;

    cout << " !!!!! Now read  event # " << iev << endl;

    St_DataSet *ds=chain->GetDataSet("dst/vertex");
    St_DataSetIter dsiter(ds);
    St_dst_vertex *vert = (St_dst_vertex *) dsiter.Find("vertex");

  cout << "    vertex table pointer = " << vert << endl;

// if pointer is zero, go back to top and read in next event
// last event is really end-run record, so it doesn't have the
// data on it. After this record is passed, the while loop ends
  if (!vert)  continue;

   // get the table header data using member function of St_Table
  table_head_st *tdt_h = vert->GetHeader();
   cout << " header name   = " << tdt_h->name << endl;
   cout << " header type   = " << tdt_h->type << endl;
   cout << " header maxlen = " << tdt_h->maxlen << endl;
   cout << " header nok    = " << tdt_h->nok << endl;

  dst_vertex_st *sth = vert->GetTable();
  cout << "   prim vtx z : " << sth->z   << endl;
  h1->Fill(sth->z);
}

 cout << " ==> finished loop" << endl;

 TCanvas *c1 = new TCanvas("c1"," from table dst/vertex",200,10,600,880);
 TPostScript ps("MyHist.ps",111);

 h1->Draw();
 c1->Update();

 hist_outfile->Write();
 hist_outfile->ls();

 ps.Close();

}





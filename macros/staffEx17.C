{
//   example of macro to read data from an ascii file and
//   create a root file with an histogram and an ntuple.
//   A'la the famous ROOT/PAW staff data example 
//   ( see PAW - Long write up,example 17, CERN, page33. )

   gROOT->Reset();
   gSystem->Load("libStar");

//  Create ROOT file
   TFileIter file("aptuple.root");
   file = "Staff-data";
   TGenericTable *allStaff = (TGenericTable *)(*file);
   if (allStaff) {
   //  Create ROOT Browser
      new TBrowser("staff",allStaff);

   //  Create couple of the histograms
     TCanvas *canva = new TCanvas("Staff","CERN Population",600,600);
     canva->Divide(2,2);


// one can use 2 meta variable: 
//  n$ - the total number of the rows in the table
//  i$ - stands for the current row index i = [0 -> (n$-1)]

     gStyle->SetHistFillColor(10);
     gStyle->SetHistFillStyle(3013);
     canva->cd(1);
     allStaff->Draw("age");
     canva->Update();

     canva->cd(2);
     TH1F* h11 = new TH1F("h11","Age - User binning",45,20,65);
     allStaff->Draw("age>>h11");
     canva->Update();

     canva->cd(3);
     allStaff->Draw("cost");
     canva->Update();

     canva->cd(4);
     TH1F* h12 = new TH1F("h12","Cost - User binning",50,0,20000);
     allStaff->Draw("cost>>h12");
     canva->Update();
   }
}

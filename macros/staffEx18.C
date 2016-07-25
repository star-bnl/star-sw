{
//   example of macro to read data from an ascii file and
//   create a root file with an histogram and an ntuple.
//   A'la the famous ROOT/PAW staff data example 
//   ( see PAW - Long write up,example 18, CERN, page35. )

   gROOT->Reset();
   gSystem->Load("libStar");

//  Open ROOT file
   TFileIter file("aptuple.root");
// Set the object name we want to get in
   file = "Staff-data";
// Read the object in
   TGenericTable *allStaff = (TGenericTable *)(*file);
   if (allStaff) {
   //  Create ROOT Browser
      new TBrowser("staff",allStaff);

   //  Create couple of the histograms
     TCanvas *canva = new TCanvas("Staff","CERN Population",600,600);

   // one can use 2 meta variable: 
   //  n$ - the total number of the rows in the table
   //  i$ - stands for the current row index i = [0 -> (n$-1)]

     gStyle->SetHistFillColor(10);
     gStyle->SetHistFillStyle(3013);

     TH1F* h200 = new TH1F("h200","Number of years at CERN",35,0,35);
     h200->SetFillStyle(3013);
     allStaff->Draw("service>>h200");
     canva->Update();

     TH1F* h201 = new TH1F("h201","Number of years at CERN",35,0,35);
     h201->SetXTitle("Years at CERN");
     h201->SetYTitle("Number of staff");
     h201->SetFillStyle(3044);
     const int NTFR = 7;
     allStaff->Draw("service>>h201","nation==NTFR","same");
     canva->Update();

     const int DIVEP=5;
     TH1F* h202 = new TH1F("h202","Number of years at CERN",35,0,35);
     h202->SetFillStyle(1044);
     h202->SetFillColor(kBlack);
     allStaff->Draw("service>>h202","(nation==NTFR) && (division==DIVEP)","same");
     canva->Update();
   }
}

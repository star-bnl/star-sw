{
//   example of macro to read data from an ascii file and
//   create a root file with an histogram and an ntuple.
//   A'la the famous ROOT/PAW staff data example 
//   ( see PAW - Long write up, CERN, page33. )

   gROOT->Reset();
   gSystem->Load("libStar");

   struct staff_t {
                Int_t cat;       // catetory
                Int_t division;  // CERN division
                Int_t flag;
                Int_t age;       // age of the person
                Int_t service;  
                Int_t children;  // The number of the children
                Int_t grade;     // the  grade
                Int_t step;
                Int_t nation;    // citizenship
                Int_t hrweek;    // number of working hours per week
                Int_t cost;      // salary
    };

   staff_t staff;

   // open ASCII data file
   TString pathName = "staff.dat";
   gSystem->ExpandPathName(pathName);
   if (gSystem->AccessPathName(pathName.Data())) 
   {
	  printf(" Can not find file %s\n",pathName.Data()); 
      return;
   }
   FILE *fp = fopen(pathName.Data(),"r");

   char line[81];

   // Create the generic table for 1000 rows (it may grow then)
   TGenericTable *allStaff = new TGenericTable("staff_t","Staff-data",1000);

   // Fill the memory resident table 
   while (fgets(&line,80,fp)) {
      sscanf(&line[0] ,"%d%d%d%d", &staff.cat,&staff.division,&staff.flag,&staff.age);
      sscanf(&line[13],"%d%d%d%d", &staff.service,&staff.children,&staff.grade,&staff.step);
      sscanf(&line[24],"%d%d%d",   &staff.nation,&staff.hrweek,&staff.cost);
      printf("staff:%d %d %d %d\n", staff.cat,staff.division,staff.flag,staff.age);
      allStaff->AddAt(&staff);
   }
   fclose(fp);
   // Delete unused space;
   allStaff->Purge();

   allStaff->Print(0,10);

//  Create ROOT file
   TFile *f = new TFile("aptuple.root","RECRETE");
          allStaff->Write();
   f->Write();

   // We should close  TFile otherwise all histograms we create below
   // may be written to the file too occasionaly
   f->Close();

//  Create ROOT Browser
   new TBrowser("staff",allStaff);

//  Create couple of the histograms
   TCanvas *canva = new TCanvas("Staff","CERN Population",600,600);
   canva->Divide(1,2);


// one can use 2 meta variable: 
//  n$ - the total number of the rows in the table
//  i$ - stands for the current row index i = [0 -> (n$-1)]

   gStyle->SetHistFillColor(10);
   gStyle->SetHistFillStyle(3013);
   canva->cd(1);
   allStaff->Draw("age");
   canva->Update();
   canva->cd(2);
   allStaff->Draw("cost");
   canva->Update();
}

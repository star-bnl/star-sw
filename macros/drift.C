{
//   example of macro to read data from an ascii file and
//   create a root file with an histogram and an ntuple.

   gROOT->Reset();

   struct drift_t {
                Float_t date;
                Float_t time;
                Float_t DriftVelocity;
    };

   drift_t drift;

   FILE *fp = fopen("drift.data","r");

   char line[81];

   TFile *f = new TFile("drift.root","RECREATE");
   TNtuple *ntuple = new TNtuple("ntuple","drift data from ascii file",
        "date:time:DriftVelocity");
   char dum[2];
   while (fgets(&line,80,fp)) {
      sscanf(&line[0] ,"%f%f%f", 
	     &drift.date,
	     &drift.time,
	     &drift.DriftVelocity);
      printf("%f %f %f \n",drift.date,drift.time,drift.DriftVelocity);
      ntuple->Fill(&drift.date);
   } 
   ntuple->Print();

   fclose(fp);
   f->Write();
}

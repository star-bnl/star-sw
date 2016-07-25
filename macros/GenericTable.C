 {
   // Copyright(c) 2001 [BNL] Brookhaven National Laboratory, Valeri Fine  (fine@bnl.gov). All right reserved",
   // TGeneric table text macro
    gSystem->Load("libStar");
    struct hit {
     float  energy;     /* energy */
     int    detectorId; /* geometry id */
    };
 
    TGenericTable *allHits = new TGenericTable("hit","hits",1000);
    allHits->Print();
    hit  a;
    int i = 0;
    for (i=0; i<100; i++) {
	   a.energy = sin(i*0.1);
	   a.detectorId++;
	   allHits->AddAt(&a);
    }
    allHits->Print();
    TFile ff("generic.root","RECREATE");
    allHits->Write();
    ff.Write();
    ff.Close();
 }
 

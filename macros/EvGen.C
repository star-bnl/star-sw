THbookTree *T = 0;
THbookFile *f = 0;
struct hepevnt_st {
   Int_t           itrac;
   Int_t           istat;
   Int_t           ipdg;
   Int_t           moth1;
   Int_t           moth2;
   Int_t           idau1;
   Int_t           idau2;
   Float_t         Pxyz[3];
   Float_t         ener;
   Float_t         mass;
   Float_t         Vxyz[3];
   Float_t         Vtime;
};
//________________________________________________________________________________
void EvGen(const Char_t *file = "/star/simu/evgen/cucu200/hijing_382/b0_14/done/evgen.90.nt") {
  f = new THbookFile(file,8100);
  if (! f) return;
  f->ls();
  T = (THbookTree*) f->Get(999);
  hepevnt_st event;
  T->SetBranchAddress("itrac",&event.itrac);
  Long64_t nentries = T->GetEntriesFast();
   Int_t nbytes = 0, nb = 0;
   printf("entry  trk ist   ipdg   m1    m2  d1   d2    Pxyz[3]                    "
	  " ener         mass      Vxyz[3]                       Vtime\n");
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = T->LoadTree(jentry);
      if (ientry < 0) break;
      nb = T->GetEntry(jentry);   nbytes += nb;
      printf("%5i%5i%4i%7i%5i",ientry, event.itrac, event.istat, event.ipdg, event.moth1);
      printf("%5i%5i%5i",event.moth2, event.idau1, event.idau2);
      printf("%10.3f%10.3f%10.3f%10.3f%10.3f%10.3f%10.3f%10.3f%10.3f\n",
	     event.Pxyz[0], event.Pxyz[1], event.Pxyz[2], event.ener, event.mass, 
	     event.Vxyz[0], event.Vxyz[1], event.Vxyz[2], event.Vtime);	     
      if (jentry > 2000) break;
   }
}

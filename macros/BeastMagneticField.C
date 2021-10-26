struct BPoint_t {
  Float_t r, z, br, bz;
};
BPoint_t BP;
void BeastMagneticField(const Char_t *FileName = 
			"/net/l402/data/fisyak/STAR/packages/.DEV2/ATHENA/BeastMagneticField/data/EIC_v.2.0.3_Magnetic_Field_Map_2021_09_28_radial_coords_cm_T.Bmap.txt") {
  FILE *fp = fopen(FileName,"r");
  if (! fp) return;
  TString fName(gSystem->BaseName(FileName));
  fName.ReplaceAll(".txt",".root");
  f = new TFile(fName.Data(),"RECREATE");
  TNtuple *FitP = new TNtuple("Field","r,z,br,bz","r:z:br:bz");//:D_l:D_p:D_u:w:a:b:c");
  TH2F *br = new TH2F("br","br versus r and z", 500, -1., 999., 800, -801, 799.); 
  TH2F *bz = new TH2F("bz","bz versus r and z", 500, -1., 999., 800, -801, 799.); 
  char line[121];
  Int_t i = 0;
  //  fgets(&line[0],120,fp);
  while (fgets(&line[0],120,fp)) {
    sscanf(&line[0],"%f%f%f%f",&BP.r,&BP.z, &BP.br, &BP.bz);
    BP.br *= 1e1;
    BP.bz *= 1e1;
    FitP->Fill(&BP.r);
    br->Fill(BP.r, BP.z, BP.br);
    bz->Fill(BP.r, BP.z, BP.bz);
    i++;
    if (i%1000 == 1) cout << "i:" << i << "\t" << line;
  }
  fclose(fp);
  f->Write();
}

#ifdef __CINT__
class TParticle;
#else
#include "TParticle.h"
#include "THbookFile.h"
#include "THbookTree.h"
#include "TSystem.h"
#endif
struct HepEvent_t {  Int_t itrac, isthep, idhep,  jmohep[2], jdahep[2];   Float_t phep[5], vhep[4];};
struct h998_t {Char_t line;};
h998_t h998;
THbookTree *T999 = 0;
THbookTree *T998 = 0;
THbookFile *f = 0;
static Int_t fDebug = 0;
class Event : public TObject {
 private:
  Int_t   fEvtNum;
  Int_t   fRun;
  Int_t   fDate;
  Int_t   fNparticle;
 public:
  Event() : fEvtNum(0), fRun(0), fDate(0), fNparticle(0) {if (!fgTParticles) fgTParticles = new TClonesArray("TParticle", 1000); fTParticles = fgTParticles;}
  ~Event() {};
  TParticle *AddTParticle(Int_t parent, Int_t pdg,
			  Double_t px, Double_t py, Double_t pz, Double_t e,
			  Double_t vx, Double_t vy, Double_t vz, Double_t tof, Int_t is)    {
    static const Int_t kFirstDaughter = -1, kLastDaughter = -1;
    TClonesArray &particles = *fTParticles;
    TParticle *particle = new(particles[fNparticle++]) TParticle(pdg, is, parent, -1, kFirstDaughter, kLastDaughter,
								 px, py, pz, e, vx, vy, vz, tof );
    return particle;
  }
  Int_t IdGen() {return fNparticle;}
  void Clear(Option_t *option="") {fTParticles->Clear("C"); fNparticle=0;}
  TClonesArray  *fTParticles;            //->array with all particles
  static TClonesArray *fgTParticles;
  ClassDef(Event,1)
};
ClassImp(Event);
TClonesArray *Event::fgTParticles = 0;
void h2p(const Char_t *input="/star/data07/calib/fisyak/evgen/4prong.nt",const Char_t *output = 0) {
  // convert hbook ntuple into particle tree
#if 0
  gSystem->Load("libHbook");
  gSystem->Load("libEG");
#endif
  f = new THbookFile(input,0);
  static HepEvent_t event;
  T999 = (THbookTree *) f->Get(999); if (T999) T999->SetBranchAddress("itrac",&event.itrac);
  T998 = (THbookTree *) f->Get(998); if (T998) T998->SetBranchAddress("line",&h998.line);
  TString Output(output);
  if (Output == "") {
    Output = gSystem->BaseName(input);
    Output.ReplaceAll(".nt","");
    Output += ".root";
  }
  TFile *fOut = new TFile(Output,"recreate");
  Event *e = new Event;
  TTree *tree = new TTree("HepEvent","Generated Events");
  tree->Branch("Event",&e, 16000, 99);

  static Int_t   &itrac = *(&event.itrac);
  static Int_t   &ip    = *(&event.itrac);
  static Int_t   &istat = *(&event.isthep);
  static Int_t   &ipdg  = *(&event.idhep);
  static Int_t   &moth1 = *(&event.jmohep[0]);
  static Int_t   &moth2 = *(&event.jmohep[1]);
  static Int_t   &idau1 = *(&event.jdahep[0]);
  static Int_t   &idau2 = *(&event.jdahep[1]);
  static Float_t *Pxyz  =   &event.phep[0];
  static Float_t &ener  = *(&event.phep[3]);
  static Float_t &mass  = *(&event.phep[4]);
  static Float_t *Vxyz  =   &event.vhep[0];
  static Float_t &Vtime = *(&event.vhep[3]);
  static Double_t ct = TMath::Ccgs()/0.1; // mm s^-1
  Long64_t fnEntries = T999->GetEntriesFast();
  Long64_t fEntry = 0;
  Int_t nbytes = 0;
  Int_t nb;
  Int_t IdGen = 0;
  Int_t IdEvHep[4], NpHEP; 
  Double_t Mass, Hpar[4], Comp[4];
  e->Clear();
  for (; fEntry<fnEntries;fEntry++) {
    Long64_t ientry = T999->LoadTree(fEntry);
    if (ientry < 0) break;
    nb = T999->GetEntry(fEntry);   nbytes += nb;
    if (fDebug) {
      printf("%5i%5i%4i%7i%5i%5i%5i%5i",
	     (int) ientry, itrac, istat, ipdg, 
	     moth1,moth2, 
	     idau1,idau2);
      printf("%10.3f%10.3f%10.3f%10.3f%10.3f%10.3f%10.3f%10.3f%10.3f\n",
	     Pxyz[0], Pxyz[1], Pxyz[2], ener, mass, 
	     Vxyz[0], Vxyz[1], Vxyz[2], Vtime);	     
    }
    if (istat >= 10) {
      if (IdGen) {
	fEntry++; 
	tree->Fill(); 
	IdGen = 0; e->Clear();
      }
      if (ipdg > 999990) {
	if (ipdg == 999999) {
	  for (Int_t i = 0; i < 4; i++) IdEvHep[i] = (int) Pxyz[i]; Mass = Pxyz[4];
	  NpHEP=ip;
	  if (fDebug) 
	    printf("NpHEP%10i%10i%10i%10i%10i%10.3f\n",
		   NpHEP, IdEvHep[0], IdEvHep[1], IdEvHep[2], IdEvHep[3], Mass); 
	} else {
	  if ( ipdg == 999998) {
	    for (Int_t i = 0; i < 4; i++) Hpar[i] = Pxyz[i];
	    if (fDebug) printf("Hpar%10.3f%10.3f%10.3f%10.3f\n",
			       Hpar[0], Hpar[1], Hpar[2], Hpar[3]); 
	  }
	  else {
	    if ( ipdg== 999997)  {
	      for (Int_t i = 0; i < 4; i++) Comp[i] = Pxyz[i];
	      if (fDebug) printf("Comp%10.3f%10.3f%10.3f%10.3f\n",
				 Comp[0], Comp[1], Comp[2], Comp[3]); 
	    }
	  }
	}
      }
      continue;
    }
    IdGen++;
    Int_t parent = event.jmohep[0] - 1;
    e->AddTParticle(parent, event.idhep, 
		    event.phep[0], event.phep[1], 
		    event.phep[2], event.phep[3],// px, py, pz, e, 
		    event.vhep[0]/10, 
		    event.vhep[1]/10, 
		    event.vhep[2]/10, 
		    event.vhep[3]/ct,//vx, vy, vz, tof, (mm->cm) 
		    IdGen); 
  }
  if (event.itrac == -1 || event.itrac == 65535 || IdGen == NpHEP) {
    fEntry++; 
    tree->Fill(); 
    IdGen = 0; e->Clear();
  }
  fOut->Write();
}

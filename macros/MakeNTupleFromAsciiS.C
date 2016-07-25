#ifndef __CINT__
#include "Riostream.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TFileSet.h"
#include "TDataSetIter.h"
#include "TFile.h"
#include "TTree.h"
#include "TString.h"
#include "TRegexp.h"
#endif
struct Var_t {
  Float_t    sec;
  Float_t    row; // 
  Float_t  Npads; // npads or ntmbks
  Float_t Ntmbks; // npads or ntmbks
  Float_t   phiL;
  Float_t   dipL;
  Float_t     zL;
  Float_t   AdcL;
  Float_t   xPad; // xRC hit position within pad
  Float_t   zTbk; // zRC hit position within time bucket
  Float_t A; 
  Float_t dA;
  Float_t mu; 
  Float_t dmu;
  Float_t sigma; 
  Float_t dsigma;
  Float_t chi2; 
  Int_t   iXZ;
};
const Char_t *vars = "sec/F:row/F:Npads/F:Ntmbks/F:phiL/F:dipL/F:zL/F:AdcL/F:xPad/F:zTbk/F:A/F:dA/F:mu/F:dmu/F:sigma/F:dsigma/F:chi2/F:iXZ/I";
void MakeNTupleFromAsciiS(const Char_t* dir = ".", const Char_t *pattern="Sp", const Char_t *out="") { 
  TFileSet *fs = new TFileSet(dir);
  TString Path();
  TString Pattern(pattern); Pattern += "._.*log$";
  TDataSetIter next(fs,1);
  TRegexp reg(Pattern.Data());
  TString Out(out);
  if (Out == "") {
    Out = pattern; Out += "Out.root";
  }
  TFile *f = new TFile(Out.Data(),"RECREATE");
  TTree *FitP = new TTree("FitP","Fit parameters");
  Var_t B;
  FitP->Branch("Fit", &B.sec, vars);
  TDataSet *set = 0;
  while ((set = next())) { //loop over DIR 
    TString Path(set->Path());
    if (! Path.Contains(reg)) continue;
    FILE *fp = fopen(Path.Data(),"r");
    if (! fp) {
      cout << "Can't open" << Path.Data() << endl;
      return;
    }
    cout << "Open " << Path.Data() << endl;
    B.iXZ = 0;
    if (Path.Contains("SpZ")) B.iXZ = 1;
    Char_t line[320];
    Int_t i = 0;
    Char_t N[6];
    Int_t  b;
    while (fgets(&line[0],320,fp)) {
      TString Line(line);
      if (! Line.Contains("+/-")) continue;
    /*
  sec  2  18.50   row  2  20.00 Npads  7   7.00 Ntmbks  6  17.00  phiL 19   0.75  dipL  5  -0.65    zL 13 120.00  AdcL 10   7.75  xPad  9  -0.23  zTbk  0  -0.53        A =    1.182 +/-   1.713 mu =    1.330 +/-   8.055 sigma =    2.730 +/-   1.798      chi2 = 0.0501934
	for (Int_t k = 0; k < Ndim - 1; k++) {
	  cout << Form("%5s", Names[k]);
	  if (k < Ndim - 3) cout << Form(" %2i",bins[k]);
	  else {
	    if (k == Ndim - 3) cout << Form(" %2i",i);
	    if (k == Ndim - 2) cout << Form(" %2i",j);
	  }
	  cout << Form(" %6.2f ",XC[k]);
	}
	cout << Form("\tA = %8.3f +/- %7.3f",fit.A,fit.dA)
	     << Form(" mu = %8.3f +/- %7.3f",fit.mu,fit.dmu)
	     << Form(" sigma = %8.3f +/- %7.3f",fit.sigma,fit.dsigma) 
	     << "\tchi2 = " << fit.chi2 << endl;
    */
      Char_t *fmt =
	"%s %i %f " // sec 
	"%s %i %f " // row
	"%s %i %f " // Npads
	"%s %i %f " // Ntmbks
	"%s %i %f " // phiL
	"%s %i %f " // dipL
	"%s %i %f " // zL
	"%s %i %f " // AdcL
	"%s %i %f " // xPad
	"%s %i %f " // zTbk
	"A = %f +/- %f mu = %f +/- %f sigma = %f +/- %f chi2 = %f";
      Int_t n = sscanf(Line.Data(),fmt
		       ,&N[0],&b,&B.sec 
		       ,&N[0],&b,&B.row
		       ,&N[0],&b,&B.Npads
		       ,&N[0],&b,&B.Ntmbks
		       ,&N[0],&b,&B.phiL
		       ,&N[0],&b,&B.dipL
		       ,&N[0],&b,&B.zL
		       ,&N[0],&b,&B.AdcL
		       ,&N[0],&b,&B.xPad
		       ,&N[0],&b,&B.zTbk
		       ,&B.A
		       ,&B.dA
		       ,&B.mu
		       ,&B.dmu
		       ,&B.sigma
		       ,&B.dsigma
		       ,&B.chi2
		       );
      if (B.xPad > 0) continue;
      B.xPad = 2*B.xPad + 0.5;
      if (B.A < 3*B.dA) continue;
      FitP->Fill();
      i++;
    }
    fclose(fp);
  }
  f->Write();
}

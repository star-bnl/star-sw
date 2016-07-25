/*
 gSystem->Load("libStDb_Tables.so");
 .x MakeTpcPedestal.C
*/
#if !defined(__CINT__)
// code that should be seen ONLY by the compiler
#else
#if !defined(__CINT__) || defined(__MAKECINT__)
// code that should be seen by the compiler AND rootcint
#else
// code that should always be seen
#endif
#endif

#include <stdio.h>
#if !defined(__CINT__)
#include "tables/St_tpcPedestal_Table.h"
#endif
#include "TFile.h"
#include "Riostream.h"
using namespace std;
//________________________________________________________________________________
void MakeTpcPedestal() {
  St_tpcPedestal *tpcPed = new St_tpcPedestal("tpcPedestal",24);
  tpcPedestal_st Ped;
  Char_t line[121];
  Int_t row, pad, tmbk;
  Float_t ped, rms;
  Int_t i = 0;
  Int_t rowOld = -1;
  for (Int_t sec = 1; sec <= 24; sec++) { 
    memset (&Ped, 0, sizeof(tpcPedestal_st));
    for (Int_t rdo = 1; rdo <= 6; rdo++) {
#if 0
      TString FileName(Form("./%i/pedestals_r%i.txt",sec,rdo));
#else
      TString FileName(Form("./pedestals_s%02i_r%i.txt",sec,rdo));
#endif
      FILE *fp = fopen(FileName.Data(),"r");
      if (! fp) {
	cout << "Can't open\t" << FileName << endl;
	continue;
      } else {
	cout << "open\t" << FileName << endl;
      }
      while (fgets(&line[0],120,fp)) {
	Int_t n = sscanf(&line[0],"%d %d %d %f %f",&row,&pad,&tmbk,&ped,&rms);
	if (n != 5) continue; 
	i++;
	if (rowOld != row) {
	  //	  printf("%i %s",n,line);
	  printf("%d %d %d %f %f\n",row,pad,tmbk,ped,rms);
	}
	rowOld = row;
	if (row <= 0 || row > 100 || pad <= 0 || pad > 182 || tmbk < 0 || tmbk > 512) continue;
	Ped.Pedestal[row-1][pad-1][tmbk] = ped;
	/*	Ped.Rms[row-1][pad-1][tmbk] = rms; */
      }
      fclose(fp);
    }
    tpcPed->AddAt(&Ped);
  }
  //  TFile *fOut = new TFile("tpcPedestal.20130214.021326.root","recreate");
  TFile *fOut = new TFile("tpcPedestal.20130215.162617.root","recreate");
  tpcPed->Write();
  delete fOut;
}



#include "TH1.h"
#include "TH2.h"
#include "TFile.h"

void create_hist()
{
  TH1F *hist = new TH1F("testname","testtitle",50,0,100);

  for(int i=0;i<100;i++) {
    for(int j=0;j<i;j++) {
      hist->Fill(i);
    }
  }

  TFile f("test.root","recreate");
  hist->Write();
  f.Close();
  
}

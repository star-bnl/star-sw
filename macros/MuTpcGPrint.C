//  foreach f (`ls -1d */*/*GeV*/MuTpcG.root | tail -2`)
//    root.exe ${f} MuTpcGPrint.C
//  end

#define __SUMMARY_ONLY__
#include "PrintTH1.C"
#include "TCanvas.h"
void MuTpcGPrint() {
  cout << gDirectory->GetName() << endl;
TCanvas *c1 = new TCanvas(); c1->Divide(1,3);
c1->cd(1)->SetLogz(1);  dXS->Project3D("zx")->Draw("colz"); dXS_zx->FitSlicesY(); dXS_zx_1->Draw("same"); PrintTH1(dXS_zx_1);
c1->cd(2)->SetLogz(1);  dYS->Project3D("zx")->Draw("colz"); dYS_zx->FitSlicesY(); dYS_zx_1->Draw("same"); PrintTH1(dYS_zx_1); 
c1->cd(3)->SetLogz(1);  dZS->Project3D("zx")->Draw("colz"); dZS_zx->FitSlicesY(); dZS_zx_1->Draw("same"); PrintTH1(dZS_zx_1);
}

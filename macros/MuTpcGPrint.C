//  foreach f (`ls -1d */*/*GeV*/MuTpcG.root | tail -2`)
//    root.exe ${f} MuTpcGPrint.C
//  end

#define __SUMMARY_ONLY__
#include "PrintTH1.C"
#include "TCanvas.h"
void MuTpcGPrint() {
#if 1
  cout << gDirectory->GetName() << endl;
  TCanvas *c1 = new TCanvas("c1","c1",10,10,700,900); c1->Divide(1,3);
c1->cd(1)->SetLogz(1);  dXS->Project3D("zx")->Draw("colz"); dXS_zx->FitSlicesY(); dXS_zx_1->Draw("same"); PrintTH1(dXS_zx_1);
c1->cd(2)->SetLogz(1);  dYS->Project3D("zx")->Draw("colz"); dYS_zx->FitSlicesY(); dYS_zx_1->Draw("same"); PrintTH1(dYS_zx_1); 
dYS_zx_1->Fit("pol0","er","same", 0.5,12.5); cout << Form("dYW = %8.4f +/- %8.4f",pol0->GetParameter(0),pol0->GetParError(0)) << endl;
dYS_zx_1->Fit("pol0","er+","same",12.5,24.5); cout << Form("dYE = %8.4f +/- %8.4f",pol0->GetParameter(0),pol0->GetParError(0)) << endl;
c1->cd(3)->SetLogz(1);  dZS->Project3D("zx")->Draw("colz"); dZS_zx->FitSlicesY(); dZS_zx_1->Draw("same"); PrintTH1(dZS_zx_1);


TCanvas *c2 = new TCanvas("c2","c2",10,910,700,900); c2->Divide(1,4);
c2->cd(1);  dT->ProjectionY()->Fit("gaus"); cout << Form(" dT: %6.4f +/- %6.4f",gaus->GetParameter(1), gaus->GetParameter(2)) << endl;
c2->cd(2);  dZ->ProjectionY()->Fit("gaus"); cout << Form(" dZ: %6.4f +/- %6.4f",gaus->GetParameter(1), gaus->GetParameter(2)) << endl;
c2->cd(3);  dX->ProjectionY()->Fit("gaus"); cout << Form(" dX: %6.4f +/- %6.4f",gaus->GetParameter(1), gaus->GetParameter(2)) << endl;
c2->cd(4);  dY->ProjectionY()->Fit("gaus"); cout << Form(" dY: %6.4f +/- %6.4f",gaus->GetParameter(1), gaus->GetParameter(2)) << endl;
#else
 TH1D *dTpy = (TH1D *) dT->ProjectionY(); dTpy->Fit("gaus","q"); TF1 *gaus = (TF1 *) dTpy->GetListOfFunctions()->FindObject("gaus"); 
 dT->FitSlicesY(); dT_1->Fit("pol2","q");  TF1 *pol2 = (TF1 *) dT_1->GetListOfFunctions()->FindObject("pol2"); 
 cout << gDirectory->GetName() 		    
      << Form("\t dT: %8.6f +/- %8.6f",gaus->GetParameter(1), gaus->GetParameter(2)) 
      << Form("\t dT: %8.6f +/- %8.6f",pol2->GetParameter(0), pol2->GetParError(0)) << endl;
#endif
}
//________________________________________________________________________________

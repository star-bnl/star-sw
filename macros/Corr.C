{
TCanvas *Corr = new TCanvas("Corr","Correlations",10,40,800,800);
Corr->Range(0,0,25,18);
Corr->Divide(2,3);
Corr->cd(1); pKpTEta->Draw("colz"); pKpTEta->SetMaximum(1); pKpTEta->SetMinimum(-1); pKpTEta->SetStats(0);
Corr->cd(2); ppipTEta->Draw("colz"); ppipTEta->SetMaximum(1); ppipTEta->SetMinimum(-1); ppipTEta->SetStats(0);
Corr->cd(3); KpipTEta->Draw("colz"); KpipTEta->SetMaximum(1); KpipTEta->SetMinimum(-1); KpipTEta->SetStats(0);
Corr->cd(4); pepTEta->Draw("colz"); pepTEta->SetMaximum(1); pepTEta->SetMinimum(-1); pepTEta->SetStats(0);
Corr->cd(5); KepTEta->Draw("colz"); KepTEta->SetMaximum(1); KepTEta->SetMinimum(-1); KepTEta->SetStats(0);
Corr->cd(6); piepTEta->Draw("colz"); piepTEta->SetMaximum(1); piepTEta->SetMinimum(-1); piepTEta->SetStats(0);
}

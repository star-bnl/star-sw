void DrawdX() {
}
/*
gStyle->SetOptStat(0)
rcd("dX3CGFdEdx330")
c1->cd()
FitP->SetMarkerColor(1);
FitP->Draw("mu:y>>i0","i&&j&&i<=13","prof")
i0->SetXTitle("dX(cm)")
TLegend *l = new TLegend(0.6,0.6,0.8,0.8)
l->AddEntry(i0,"Adc thr = 3, Inner TPC")
l->Draw()
i0->SetMaximum(0.12)
i0->SetTitle("#mu versus dX")
rcd("dX3CGFdEdx331")
FitP->SetMarkerColor(2);
FitP->Draw("mu:y>>i1","i&&j&&i<=13","profsame")
FitP->SetMarkerColor(2)
FitP->Draw("mu:y>>i1","i&&j&&i<=13","profsame")
i1->SetMarkerColor(2)
l->AddEntry(i1,"Adc thr = 4, Inner TPC")
rcd("dX3iTPCCGFdEdx330")
FitP->SetMarkerColor(3);
FitP->Draw("mu:y>>x0","i&&j&&i<=40","profsame")
l->AddEntry(x0,"Adc thr = 3, Inner iTPC")
rcd("dX3iTPCCGFdEdx331")
FitP->SetMarkerColor(4);
FitP->Draw("mu:y>>x1","i&&j&&i<=40","profsame")
l->AddEntry(x1,"Adc thr = 4, Inner iTPC")

rcd("dX3CGFdEdx330")
c1->cd()
FitP->Draw("mu:y>>o0","i&&j&&i>13","prof")
o0->SetXTotle("dX(cm)")
TLegend *L = new TLegend(0.6,0.6,0.8,0.8)
L->AddEntry(o0,"Adc thr = 3, Outer TPC")
L->Draw()
o0->SetMaximum(0.12)
o0->SetTitle("#mu versus dX")
rcd("dX3CGFdEdx331")
FitP->SetMarkerColor(2);
FitP->Draw("mu:y>>o1","i&&j&&i>13","profsame")
FitP->SetMarkerColor(2)
FitP->Draw("mu:y>>o1","i&&j&&i>13","profsame")
L->AddEntry(o1,"Adc thr = 4, Outer TPC")
rcd("dX3iTPCCGFdEdx330")
FitP->SetMarkerColor(3)
FitP->Draw("mu:y>>X0","i&&j&&i>40","profsame")
L->AddEntry(X0,"Adc thr = 3, Outer iTPC")
rcd("dX3iTPCCGFdEdx331")
FitP->SetMarkerColor(4)
FitP->Draw("mu:y>>X1","i&&j&&i>40","profsame")
L->AddEntry(X1,"Adc thr = 4, Outer iTPC")
*/

void Drift(TH2 *h2) {
  if (! h2) return;
  TProfile * pfx = h2->ProfileX(); 
  pfx->SetMarkerStyle(20); 
  pfx->SetMarkerColor(6); 
  pfx->Fit("pol1","er","",-2.9,0); 
  pfx->Fit("pol1","+er","",0,2.9);  
  h2->Draw("colz"); 
  pfx->Draw("same");
}

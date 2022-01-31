void plotMip(int plt=0, int run=1){
  char file[100];
  sprintf(file,"%d.mip.root",run);
  printf("Reading %s\n",file);
  TFile *F = new TFile(file,"old");
  
  c1 = new TCanvas("c1","FCS MIP",50,0,1500,1200);
  gStyle->SetLabelSize(0.1,"xy");
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);
  
  if(plt==0 || plt==1) {
    c1->Clear();
    c1->Divide(3,2);
    c1->cd(1); NCluster_Ecal->Draw();
    c1->cd(2); NTowerCluster_Ecal->Draw();
    c1->cd(3); NNeiCluster_Ecal->Draw();
    c1->cd(4); NCluster_Hcal->Draw();
    c1->cd(5); NTowerCluster_Hcal->Draw();
    c1->cd(6); NNeiCluster_Hcal->Draw();
    c1->SaveAs("mip_clu.png");

    TH1D* h;
    c1->Clear();
    c1->Divide(1,2);
    c1->cd(1); 
    ADC_EcalN->ProjectionY()->Draw(); 
    h = ADCSingle_EcalN->ProjectionY("ES");
    h->SetLineColor(kBlue); h->Draw("same"); 
    c1->cd(2); 
    ADC_HcalN->ProjectionY()->Draw(); 
    h = ADCSingle_HcalN->ProjectionY("HS");
    h->SetLineColor(kBlue); h->Draw("same");
    h = ADCEcal_HcalN->ProjectionY("HE");
    h->SetLineColor(kRed); h->Draw("same");
    c1->SaveAs("mip_spec.png");
    
    c1->Clear();
    c1->Divide(1,2);
    c1->cd(1); X->Draw("colz");
    c1->cd(2); Y->Draw("colz");
    c1->SaveAs("mip_xy.png");

    c1->Clear();
    c1->Divide(2,2);
    c1->cd(1); DXX->Draw("colz");
    c1->cd(2); DXY->Draw("colz");
    c1->cd(3); DXX->ProjectionY()->Draw();
    c1->SaveAs("mip_dx.png");

    c1->Clear();
    c1->Divide(2,2);
    c1->cd(1); DYX->Draw("colz");
    c1->cd(2); DYY->Draw("colz");
    c1->cd(3); DYX->ProjectionY()->Draw();
    c1->SaveAs("mip_dy.png");

    c1->Clear();
    c1->Divide(1,2);
    c1->cd(1); DR->Draw("colz");
    c1->cd(2); DR->ProjectionY()->Draw();
    c1->SaveAs("mip_dr.png");
  }
}

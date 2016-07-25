TH1D *MakedNdE() {
  const Char_t *FileName="dNdE_Bichsel.data";
  TString fName(FileName);
  fName.ReplaceAll(".data",".root");
  f = new TFile(fName.Data(),"RECREATE");
  FILE *fp = fopen(FileName,"r");
  const Int_t N = 1000;
  Int_t     i;
  Double_t  x[N];
  Double_t  y[N];
  Double_t  z[N];
  Float_t  X,Y,Z,V;
  Int_t npoints = 0;
  char line[121];
  Int_t i = 0;
  fgets(&line[0],120,fp);
  Double_t dXavL10 = 0;
  Double_t dXavLn  = 0;
  while (fgets(&line[0],120,fp)) {
    sscanf(&line[0],"%i %f %f %f %f",&i,&X,&V,&Y,&Z);
    //    printf("%s",line);
    x[npoints] = X;
    y[npoints] = Y;
    z[npoints] = Z;
    Double_t dX = 0;
    if (npoints > 0) {dX = TMath::Log10(x[npoints]/x[npoints-1]); dXavL10 += dX;}
    if (npoints > 0) {dX = TMath::Log(x[npoints]/x[npoints-1]); dXavLn += dX;}
    if (i%100 == 0) printf("%i\t%f\t%f\t%f\t%f\n",i,x[npoints],y[npoints],z[npoints],dX);
    npoints++;
    if (npoints > N) break;
  }
  fclose(fp);
  dXavL10 /= (npoints-1);
  dXavLn /= (npoints-1);
  printf("dX (Log10) = %f (Ln) %f\n",dXavL10,dXavLn);
  Double_t *xBins = new Double_t [npoints+1];
  for (i = 0; i <= npoints; i++) {
    if (i == 0)            xBins[i] = x[i]   - 0.5*(x[i+1] - x[i]); 
    else if (i == npoints) xBins[i] = x[i-1] + 0.5*(x[i-1] - x[i-2]);
      else                 xBins[i] = 0.5*(x[i] + x[i-1]);
    //    cout << i << "\t" << x[i] << "\t" << xBins[i] << endl;
  }
  TH1D *dNdE = new TH1D("dNdE","sigma dNdE",npoints,xBins);
  dNdE->SetLineColor(2);
  dNdE->SetLineWidth(4);
  dNdE->SetMarkerColor(2);
  dNdE->SetMarkerStyle(21);
  dNdE->GetXaxis()->SetTitle("E(eV)");
  dNdE->GetYaxis()->SetTitle("dN/dE (1/eV)");
  TH1D *dNdEL10 = new TH1D("dNdEL10","sigma dN/dlog_{10}(E)",npoints,TMath::Log10(x[0])-dXavL10/2,TMath::Log10(x[npoints-1])+dXavL10/2);
  dNdEL10->SetLineColor(6);
  dNdEL10->SetLineWidth(4);
  dNdEL10->SetMarkerColor(6);
  dNdEL10->SetMarkerStyle(21);
  dNdEL10->GetXaxis()->SetTitle("Log _{10} (E(eV))");
  dNdEL10->GetYaxis()->SetTitle("dN/d(Log _{10} (E [eV])");
  TH1D *dNdELn = new TH1D("dNdELn","sigma dN/dlog(E)",npoints,TMath::Log(x[0])-dXavLn/2,TMath::Log(x[npoints-1])+dXavLn/2);
  dNdELn->SetLineColor(7);
  dNdELn->SetLineWidth(4);
  dNdELn->SetMarkerColor(7);
  dNdELn->SetMarkerStyle(21);
  dNdELn->GetXaxis()->SetTitle("Log (E(eV))");
  dNdELn->GetYaxis()->SetTitle("dN/d(Log _{10} (E [eV])");
  TH1D *dNdEi = new TH1D("dNdEi","Integral dNdE",npoints,xBins);
  dNdEi->SetLineColor(1);
  dNdEi->SetLineWidth(4);
  dNdEi->SetMarkerColor(1);
  dNdEi->SetMarkerStyle(21);
  dNdEi->GetXaxis()->SetTitle("E(eV)");
  dNdEi->GetYaxis()->SetTitle("Integral dN/dE (1/eV)");
  TH1D *dNdEI = new TH1D("dNdEI","Integral dNdE made from dNdE*dE",npoints,xBins);
  dNdEI->SetLineColor(3);
  dNdEI->SetLineWidth(4);
  dNdEI->SetMarkerColor(3);
  dNdEI->SetMarkerStyle(21);
  dNdEI->GetXaxis()->SetTitle("E(eV)");
  dNdEI->GetYaxis()->SetTitle("Integral dN/dE (1/eV)");
  Double_t dZ = 0;
  Double_t log10 = TMath::Log(10.);
  for (i = 0; i < npoints; i++) {
    dNdE->SetBinContent(i+1,y[i]);
    Double_t E = TMath::Power(10.,dNdEL10->GetBinCenter(i+1));
    dNdEL10->SetBinContent(i+1,y[i]*log10*E);
    E = TMath::Exp(dNdELn->GetBinCenter(i+1));
    dNdELn->SetBinContent(i+1,y[i]*E);
    if (i < npoints - 1) dZ = z[i+1] - z[i];
    else                 dZ = 1.     - z[i];
    dNdEi->SetBinContent(i+2,dZ);
    dNdEI->SetBinContent(i+1,y[i]*(x[i+1]-x[i]));
  }
  Double_t scale = dNdEI->Integral();
  dNdEI->Scale(1./scale);
  TLegend *l = new TLegend(0.2,0.2,0.5,0.5);
  dNdE->SetStats(0);
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (c1) c1->Clear();
  else    c1  = new TCanvas("c1","c1");
  c1->SetLogx(1);
  c1->SetLogy(1);
  dNdE->Draw("");      l->AddEntry(dNdE,dNdE->GetName());
  dNdEi->Draw("same"); l->AddEntry(dNdEi,dNdEi->GetName());
  dNdEI->Draw("same"); l->AddEntry(dNdEI,dNdEI->GetName());
  l->Draw();
  f->Write();
  return dNdE;
}

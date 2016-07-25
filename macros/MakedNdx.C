TH1D *MakedNdx() {
  const Char_t *FileNames[2] = {"/afs/rhic.bnl.gov/star/users/fisyak/WWW/star/PiD/Bichsel/P10sglYu11bsmu.mom",
				"/afs/rhic.bnl.gov/star/users/fisyak/WWW/star/PiD/Bichsel/P10sglYu11bgmu.mom"};
  const Char_t *Tags[2] = {"","bg"};
  //  const Char_t *FileName="dNdx_Bichsel.data";
  f = new TFile("dNdx_Bichsel.root","RECREATE");
  for (Int_t m = 0; m < 2; m++) {
    TString fName(FileNames[m]);
    //  fName.ReplaceAll(".data",".root");
    FILE *fp = fopen(fName.Data(),"r");
    if (! fp) {
      cout << "Cannot open file " << fName.Data() << endl;
      return 0;
    } else {cout << "open file " << fName.Data() << endl;}
    const Int_t N = 500;
    Double_t  x[N];
    Double_t  y[N];
    Double_t  z[N];
    Float_t  X,Y,T;
    Int_t npoints = 0;
    char line[121];
    Int_t i = 0;
    Double_t dXav = 0;
    fgets(&line[0],120,fp);
    while (fgets(&line[0],120,fp)) {
      //    printf("%s",line);
      TString Line(line);
      Line.Strip();
      if (Line.Contains("(")) continue;
      if (Line == "") continue;
      //    printf("%s\n",Line.Data());
      Int_t nr = sscanf(&line[0],"%i %f %f %f",&i,&X,&T,&Y);
      if (nr != 4) continue;
      //    printf("%s",line);
      x[npoints] = X;
      y[npoints] = Y;
      Double_t dX = 0;
      if (npoints > 0) {dX = TMath::Log10(x[npoints]/x[npoints-1]); dXav += dX;}
      //      printf("%i\t%i\t%f\t%f\t%f\t%f\n",nr,i,x[npoints],T,y[npoints],dX);
      npoints++;
      if (npoints > N) break;
    }
    dXav /= (npoints-1);
    printf("dX = %f\n",dXav);
    fclose(fp);
    Double_t *xBins = new Double_t [npoints+1];
    for (i = 0; i <= npoints; i++) {
      if (i == 0)            xBins[i] = x[i]   - 0.5*(x[i+1] - x[i]); 
      else if (i == npoints) xBins[i] = x[i-1] + 0.5*(x[i-1] - x[i-2]);
      else                   xBins[i] = 0.5*(x[i] + x[i-1]);
      cout << i << "\t" << x[i] << "\t" << xBins[i] << endl;
    }
    TH1D *hist = new TH1D(Form("dNdx_%s",Tags[m]),"dNdx versus #beta#gamma",npoints,xBins);
    Double_t xmin = TMath::Log10(x[0]);
    Double_t xmax = TMath::Log10(x[npoints-1]);
    //  Double_t dx   = (xmax - xmin)/(npoints-1);
    TH1D *histB = new TH1D(Form("dNdxL10_%s",Tags[m]),"dNdx versus Log _{10} (#beta#gamma)",npoints,xmin-dXav/2,xmax+dXav/2);
    cout << "Create hitogram " << histB->GetName() << endl;
    for (i = 0; i < npoints; i++) {hist->SetBinContent(i+1,y[i]); histB->SetBinContent(i+1,y[i]);}
    hist->SetLineColor(2+2*m);
    hist->SetLineWidth(4);
    hist->SetMarkerColor(4);
    hist->SetMarkerStyle(21);
    hist->GetXaxis()->SetTitle("#beta#gamma");
    hist->GetYaxis()->SetTitle("dN/dx (collision/cm)");
    hist->Draw("");
    histB->SetLineColor(3+2*m);
    histB->SetLineWidth(4);
    histB->SetMarkerColor(4);
    histB->SetMarkerStyle(21);
    histB->GetXaxis()->SetTitle("Log_{10} ( #beta#gamma )");
    histB->GetYaxis()->SetTitle("dN/dx (collision/cm)");
  }
  f->Write();
  return hist;
}

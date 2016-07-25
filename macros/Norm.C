void Norm(TH2F *hxz, TH2F *newh=0)
{
  Int_t nx = hxz->GetNbinsX();
  Int_t ny = hxz->GetNbinsY();
  Double_t Sum;
  Int_t i,j;
  for (i=1;i<=nx;i++) {
    Sum = 0.;
    for (j=1;j<=ny;j++) {
      Sum += hxz->GetCellContent(i,j); printf("%i %i %f\n",i,j,Sum);
    }
    if (Sum > 10.0) {
      for (j=1;j<=ny;j++) {
	Double_t cont = hxz->GetCellContent(i,j)/Sum;
	Double_t err  = sqrt(cont*(1.-cont)/Sum);
	 printf("%i %i %f +/- %f\n",i,j,cont,err);
	if (newh) {
	  newh->SetCellContent(i,j,cont);
	  newh->SetCellError(i,j,err);
	}
      }
    }
  }
}
//________________________________________________________________________________
void Dump(TH1D *hist, const Char_t *file="Dump.dat")
{
  //  ofstream *out = new ofstream(file);
  //  ofstream &Out = *out;
  FILE *File = fopen(file,"w");
  Int_t nx = hist->GetNbinsX();
  Int_t jb;
  for (jb=1;jb<=nx;jb++) {
    Double_t x1 = hist->GetXaxis()->GetBinLowEdge(jb);
    Double_t x2 = hist->GetXaxis()->GetBinLowEdge(jb+1);
    Double_t y  = hist->GetCellContent(jb,0); 
    Double_t dy = hist->GetCellError(jb,0); 
//     printf ("jb = %i x1 = %f x2 = %f y = %f dy = %f\n",
// 	    jb,x1,x2,y,dy);
    //     fprintf (File,"jb = %i x1 = %f x2 = %f y = %f dy = %f\n",
    // 	    jb,x1,x2,y,dy);
     fprintf (File,"\t%i\t%f\t%f\t%f\t%f\n",
 	    jb,x1,x2,y,dy);
    //    Out << jb << x1 << x2 << y << dy << endl;
     //    cout << jb << x1 << x2 << y << dy << endl;
  }
  //  delete out;
  fclose(File);
}
//____________________________________________________________
TH1D *ProjectY(TH2F *hist) {
  Int_t ny = hist->GetNbinsY();
  return hist->ProjectionY("projY",1,ny);
}
//____________________________________________________________
void Fit(TH1D *hist) {
  Double_t params[12];
  memset (params,0, 12*sizeof(Double_t));
  params[2] = params[5] = params[8] = params[11] = 1;
  TF1 *g = new TF1("g","gaus");
  TF1 *g2 = new TF1("g2","gaus(0)+gaus(3)");
  TF1 *g3 = new TF1("g3","gaus(0)+gaus(3)+gaus(6)");
  TF1 *g4 = new TF1("g4","gaus(0)+gaus(3)+gaus(6)+gaus(9)");
  Int_t i;
  hist->Fit("g");
  hist->Draw("e");
  cin >> i;  
  g->GetParameters(params);
  g2->SetParameters(params);
  hist->Fit("g2");
  hist->Draw("e");
  cin >> i;
  g2->GetParameters(params);
  g3->SetParameters(params);
  hist->Fit("g3");
  hist->Draw("e");
  cin >> i;
  g3->GetParameters(params);
  g4->SetParameters(params);
  hist->Fit("g4");
  hist->Draw("e");
}
//________________________________________________________________________________
void PrintPars(TF1 *fit, Int_t Npar=12) {
  Int_t i;
  printf ("Double_t params[] = {");
  Double_t a = fit->GetXmin();
  Double_t b = fit->GetXmax();
  Double_t c = fit->Integral(a,b);   
  for (i=0; i< Npar; i++) {
    Double_t par = fit->GetParameter(i);
    if (i%3 == 0) par /=c;
    printf ("%f,",par);
  }
  printf("};\n");
  printf ("Double_t perrs[] = {");
  for (i=0; i< Npar; i++) {
    Double_t par = fit->GetParError(i);
    if (i%3 == 0) par /= c;
    printf ("%f,",par);
  }
  printf("};\n");
}

//#define INNER
#if 0
static TF2* QxyShape = 0;
//________________________________________________________________________________
Double_t QxyFunc(Double_t *x, Double_t *par) {
  Double_t D = par[0]; // anode - cathode spacing
  Double_t X = x[0];   // pad direction
  Double_t Y = x[1];   // row direction
  Double_t R = TMath::Sqrt(X*X + Y*Y + D*D);
  Double_t Q = D/(2*TMath::Pi())/TMath::Sqrt(R*R*R);

}
//________________________________________________________________________________
Double_t QxyFuncI(Double_t *x, Double_t *par) {
  Double_t D = par[0];  // anode - cathode spacing
  if (! QxyShape) {
    QxyShape = new TF2("QxyShape",QxyFunc,-10,10,-10,10, 1);
    QxyShape->SetParameter(0,D);
  }
  else {
    QxyShape->SetParameter(0,D);
  }
  Double_t widthX = 0.5*par[1]; // x
  Double_t widthY = 0.5*par[2]; // y
  Double_t X = x[0];   // pad direction X-X0
  Double_t Y = x[1];   // row direction Y-Y0
  return QxyShape->Integral(X-widthX,X+widthX,Y-widthY,Y+widthY);
}
void Q(Int_t opt = 1) {
  //                          w       h         s      a       l   i
  Double_t paramsI[6] = {0.2850, 0.2000,  0.4000, 0.0010, 1.1500, 0};
  Double_t paramsO[6] = {0.6200, 0.4000,  0.4000, 0.0010, 1.1500, 0};
  

}
#endif
//________________________________________________________________________________
Double_t GFunc(Double_t t, Double_t K1, Double_t K2, Double_t K3) {
  Double_t th = TMath::TanH(K2*t); th *= th;
  return K1*(1. - th)/(1. + K3*th);
}
//________________________________________________________________________________
Double_t GattiK3(Double_t h2s, Double_t a2s) {
  // Calculation of K_3 coefficient for E.Gatti formula
  // It is used parametrization from Fig.2 from E.Mathieson, J.S.Gordon, 
  // "Cathode charge distributions in multiwire chambers", NIM 227 (1984) 277-282
  //  h2s - ratio Anode-Cathode gap to wire spacing
  //  a2s - ratio Anode wire radius to wire spacing 
  static Double_t params[5] = {.1989337e-02, -.6901542e-04,  .8665786, 154.6177, -.6801630e-03};
  return  (params[0]/ h2s + params[1])*(params[2]/a2s + params[3] + params[4]/(a2s*a2s));
}
//________________________________________________________________________________
Double_t K3(Double_t *x, Double_t *p) {
  return GattiK3(x[0],p[0]);
} 
//________________________________________________________________________________
Double_t GattiF(Double_t *x, Double_t *par) {
  /************************************************************************
   *  Function    : generates the cathode signal using                    *
   *                the single-parameter Gatti formula:                   *
   *                              1 - tanh(K2 * lambda)**2                *
   *     GFunc(lambda) = K1 * -------------------------------             *
   *                           1 + K3 * tanh (K2 *lambda)**2              *
   *     lambda = x/h, h is anode cathode spacing                         *
   *                                                                      *
   *     K2 = pi/2*(1 - 0.5*sqrt(K3))                                     *
   *                                                                      *
   *              K2*sqrt(K3)                                             *
   *     K1 = -------------------                                         *
   *            4 * atan(sqrt(K3))                                        *
   *                                                                      *
   *  References  : E.Gatti, A.Longoni, NIM 163 (1979) 82-93.             *
   *  Authors : V.Balagura,V.Cherniatin,A.Chikanian                       *
   ************************************************************************/
  Double_t w = par[0]; // w = width of pad
  Double_t h = par[1]; // h = Anode-Cathode gap
  Double_t s = par[2]; // s = wire spacing 
  Double_t a = par[3]; // a = Anode wire radius
  Double_t l = par[4]; // l = length of pad
  Int_t    i = par[5]; // switch between int and shape
  if (i == 2) w = l;
  Double_t y = x[0]*w; 
  //  cout << "y " << y << " w " << w << " h " << h << " s " << s << " a " << a << " l " << l << endl;
  Double_t K3 = 0;
  if (i <= 2) 
    K3 = GattiK3 (h/s,a/s);//   cout << "  K3 " <<  K3 <<endl;      // K_3
  else
    K3 = par[6];
  Double_t K2 = TMath::PiOver2()*(1. - 0.5*TMath::Sqrt(K3));                 // K_2
  //  Double_t K1 = 1.;
  Double_t K1 = K2*TMath::Sqrt(K3)/(2*TMath::ATan(TMath::Sqrt(K3)));//h;// K_1
  //  Double_t K1 = K2*TMath::Sqrt(K3)/(TMath::Pi()*h);
  //  cout << "K1 " << K1 << " K2 " << K2 << "  K3 " <<  K3 <<endl; 
  Double_t lambda = y/h;
  if (! i) {
#if 1
    Double_t z = TMath::TanH(K2*lambda); z *= z;
    Double_t val = K1*(1 - z)/(1 + K3*z);
    //    cout << "y " << y << " z " << z << " val " << val << endl;
    return val;
#else
    return GFunc(lambda, K1, K2, K3);
#endif
  }
  else {
    //    if (i == 2) w = l;
    Double_t SQK3 = TMath::Sqrt(K3);
    //    Double_t ATSQK3_2 = K1/K2/TMath::ATan(SQK3);
    Double_t ATSQK3_2 = 0.5/TMath::ATan(SQK3);
    Double_t Y1 = lambda - w/h/2;
    Double_t Y2 = Y1 + w/h;
    Double_t X1 = K2*Y1;
    Double_t X2 = K2*Y2;
    Double_t Z1 = SQK3*TMath::TanH(X1);
    Double_t Z2 = SQK3*TMath::TanH(X2);
    return ATSQK3_2*(TMath::ATan(Z2) - TMath::ATan(Z1));
  }
}
//________________________________________________________________________________
void Gatti() {
  //                          w       h         s      a         l    i    K3
  Double_t paramsI[28] = {0.2850, 0.2000,  0.4000, 0.0010, 1.1500, 0, 0.40,
			  0.2850, 0.2000,  0.4000, 0.0010, 1.1500, 0, 0.50,
			  0.2850, 0.2000,  0.4000, 0.0010, 1.1500, 0, 0.68,
			  0.2850, 0.2000,  0.4000, 0.0010, 1.1500, 0, 0.89};
  Double_t paramsO[7] =  {0.6200, 0.4000,  0.4000, 0.0010, 1.1500, 0, 0.55};
  Double_t xmin =  0;
  Double_t xmax =  5;
  TLegend *leg = new TLegend(0.65,0.7,0.9,0.9,"");
  TString opt("");
  for (int i = 0; i < 4; i++) {
    TF1 *GatFI = new TF1(Form("GatFI_%i",i),GattiF,xmin,xmax,7);
    GatFI->SetLineColor(i+2);
    GatFI->SetParameters(&paramsI[7*i]);
    GatFI->SetParameter(5, 3);
    leg->AddEntry(GatFI,Form("GatFI inner pad sharing(Integral) K3=%f",paramsI[7*i+6]));
    GatFI->Draw(opt.Data()); opt = "same";
  }
  TF1 *GatOFI = new TF1("GatOFI",GattiF,xmin,xmax,7);
  
  GatOFI->SetLineColor(1);
  GatOFI->SetParameters(paramsO);
  GatOFI->SetParameter(5, 3);
  leg->AddEntry(GatOFI,"GatOFI outer pad sharing (integral)");
  GatOFI->Draw(opt.Data()); opt = "same";
#if 0
  TF1 *GatFL = new TF1("GatFL",GattiF,xmin,xmax,7);
  GatFL->SetLineColor(6);
  GatFL->SetParameters(paramsI);
  GatFL->SetParameter(5, 2);
  leg->AddEntry(GatFL,"GatFL Integral Inner Charger sharing between rows");
  GatFL->Draw(opt.Data()); opt = "same";
  TF1 *GatOFL = new TF1("GatOFL",GattiF,xmin,xmax,7);
  GatOFL->SetLineColor(7);
  GatOFL->SetParameters(paramsO);
  GatOFL->SetParameter(5, 2);
  leg->AddEntry(GatOFL,"GatOFL Integral Outer Charge sharing between rows");
  GatOFL->Draw(opt.Data()); opt = "same";
  Double_t sigmaO = 0.3913/0.6200;
  TF1 *TrsO = new TF1("TrsO","gaus(0)",xmin,xmax);
  Double_t norm = 1./sigmaO/TMath::Sqrt(2*TMath::Pi());///0.6200;
  TrsO->SetParameters(norm,0,sigmaO);
  TrsO->SetLineColor(9);
  TrsO->SetLineWidth(4);
  leg->AddEntry(TrsO,"TrsO Outer");
  TrsO->Draw(opt.Data()); opt = "same";
  Double_t sigmaI = 0.1964/0.2850;
  TF1 *TrsI = new TF1("TrsI","gaus(0)",xmin,xmax);
  Double_t norm = 1./sigmaI/TMath::Sqrt(2*TMath::Pi());///0.2850;
  TrsI->SetParameters(norm,0,sigmaI);
  TrsI->SetLineColor(8);
  TrsI->SetLineWidth(4);
  leg->AddEntry(TrsI,"TrsI Inner");
  TrsI->Draw(opt.Data()); opt = "same";
  TF1 *GatF = new TF1("GatF",GattiF,xmin,xmax,7);
  GatF->SetLineColor(1);
  GatF->SetParameters(paramsI); 
  leg->AddEntry(GatF,"GatF");
  GatF->Draw(opt.Data()); opt = "same";
  TF1 *GatOF = new TF1("GatOF",GattiF,xmin,xmax,7);
  GatOF->SetParameters(paramsO);
  GatOF->SetLineColor(3);
  leg->AddEntry(GatOF,"GatOF outer");
  GatOF->Draw(opt.Data()); opt = "same";
#endif
  leg->Draw();
}
//________________________________________________________________________________
void Rows() {
  TF1 *GatFL = (TF1*) gROOT->GetFunction("GatFL");
  TF1 *GatOFL = (TF1*) gROOT->GetFunction("GatOFL");
  Double_t xmin = -2.;
  Double_t xmax =  2.;
  
  TH1D *rowsI = new TH1D("rowsI","Effective Inner pad length",100,xmin,xmax);
  rowsI->SetLineColor(2);
  TH1D *rowsO = new TH1D("rowsO","Effective Outer pad length",100,xmin,xmax);
  rowsO->SetLineColor(3);
  TH1D *rows  = new TH1D("rows","Generated",100,xmin,xmax);
  for (Int_t i = 0; i < 10000; i++) {
    Double_t x = xmin + gRandom->Rndm()*(xmax - xmin);
    rows->Fill(x);
    Double_t y = GatFL->Eval(x);
    rowsI->Fill(x,y);
    y =  GatOFL->Eval(x);
    rowsO->Fill(x,y);
  }
  //  rows->Draw();
  rowsI->Divide(rows);
  rowsO->Divide(rows);
  rowsI->Draw("l");
  rowsO->Draw("lsame");
}

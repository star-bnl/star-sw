void SigmaVSVoltage() {
  struct Data_t {
    Double_t V_I;
    Double_t V_O;
    Double_t sigma_I;
    Double_t sigma_O;
  };
  const Int_t N = 15;
  Data_t Data[N] = {
    {1000, 1390, 5.78008e-01, 2.95017e-01},
    {1020, 1390, 3.19960e-01, 2.96802e-01},
    {1040, 1390, 2.92488e-01, 2.94467e-01},
    {1060, 1390, 2.83756e-01, 2.88717e-01},
    {1080, 1390, 3.05371e-01, 2.94821e-01},
    {1100, 1390, 3.30971e-01, 2.97934e-01},
    {1120, 1390, 3.43941e-01, 2.97446e-01},
    {1135, 1195, 3.95919e-01, 6.27957e-01},
    {1135, 1220, 3.30980e-01, 3.67140e-01},
    {1135, 1240, 3.71617e-01, 2.84972e-01},
    {1135, 1270, 3.98048e-01, 2.76000e-01},
    {1135, 1300, 3.27050e-01, 2.50549e-01},
    {1135, 1325, 3.27866e-01, 2.76542e-01},
    {1135, 1345, 3.42106e-01, 3.07177e-01},
    {1170, 1390, 3.52488e-01, 3.02773e-01}
  };
  Double_t x[2][N], y[2][N];
  for (Int_t i = 0; i < N; i++) {
    x[0][i] = Data[i].V_I;  y[0][i] = Data[i].sigma_I;
    x[1][i] = Data[i].V_O;  y[1][i] = Data[i].sigma_O;
  }
  TGraph *gr0 = new TGraph(N, x[0], y[0]);
  TGraph *gr1 = new TGraph(N, x[1], y[1]); gr1->SetMarkerColor(2);
  TCanvas *c1 = new TCanvas("RMS");
  TH1F *frame = c1->DrawFrame(950,0.2,1400,0.65);
  frame->SetTitle("#sigma(dE/dx)/(dE/dx) versus Voltage for Inner (black) and Outer (red) Sectors");
  frame->SetXTitle("Voltage");
  frame->SetYTitle("#sigma");
  gr0->Draw("p");
  gr1->Draw("p");
}

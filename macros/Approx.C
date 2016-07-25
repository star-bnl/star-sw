TCanvas *vC1;
TGraph *grxy, *grin, *grout;

void Approx()
{
//**********************************************
// Macro to test interpolation function Approx
// Author: Christian Stratowa, Vienna, Austria.
// Created: 26 Aug 2001                           
//**********************************************


// test data (square)
   Int_t n = 43;
   Double_t x[] = {11., 12., 13., 14., 15., 17., 18., 19., 20., 22.,
		   23., 24., 25., 26., 27., 28., 29., 30., 32., 33.,
		   34., 35., 38., 40., 41., 42., 43., 46., 47., 48.,
		   49., 50., 51., 53., 57., 66., 73., 74., 78., 79.,
		   80., 82., 83.}; // Z
   Double_t y[] = {8.8033795E-02, 0.1175197    , 0.1043398    , 8.3658338E-02,
		   7.6843806E-02, 5.7563554E-02, 4.0977564E-02, 4.6153713E-02,
		   4.0977564E-02, 0.1257856    , 0.1797267    , 0.1679161    ,
		   0.1650867    , 0.1828069    , 0.1828069    , 0.1956649    ,
		   0.2094273    , 0.1923680    , 0.1923680    , 0.2612006    ,
		   0.2795725    , 0.3095814    , 0.4499212    , 0.6880791    ,
		   0.7240669    , 0.6213810    , 0.5          , 0.5          ,
		   0.5          , 0.3546627    , 0.3796084    , 0.3428115    ,
		   0.2941946    , 0.3370352    , 0.3          , 0.2702305    ,
		   0.2941946    , 0.2567994    , 0.3607410    , 0.3607410    ,
		   0.3428115    , 0.3          , 0.4}; // Dose

   grxy = new TGraph(n,x,y);

// x values, for which y values should be interpolated
   Int_t nout = 100;
   Double_t xout[100];
   for (Int_t i = 0; i < nout; i++) xout[i] = i+1;

// create Canvas
   vC1 = new TCanvas("vC1","square",200,10,700,700);
   vC1->Divide(2,2);

// initialize graph with data
   grin = new TGraph(n,x,y);
// interpolate at equidistant points (use mean for tied x-values)
   TGraphSmooth *gs = new TGraphSmooth("normal");
   grout = gs->Approx(grin,"linear");
   DrawSmooth(1,"Approx: ties = mean","X-axis","Y-axis");

// re-initialize graph with data 
// (since graph points were set to unique vales)
   grin = new TGraph(n,x,y);
// interpolate at given points xout
   grout = gs->Approx(grin,"linear", nout, xout, 0, 1.);
   DrawSmooth(2,"Approx: ties = mean","","");

   // print output variables for given values xout
   Int_t vNout = grout->GetN();
   Double_t vXout, vYout;
   for (Int_t k=0;k<vNout;k++) {
      grout->GetPoint(k, vXout, vYout);
      cout << "k= " << k << "  vXout[k]= " << vXout
           << "  vYout[k]= " << vYout << " interpolated " << grin->Eval(vXout) << endl;
   }

// re-initialize graph with data
   grin = new TGraph(n,x,y);
// interpolate at equidistant points (use min for tied x-values)
//   grout = gs->Approx(grin,"linear", 50, 0, 0, 0, 1, 0, "min");
   grout = gs->Approx(grin,"constant", nout, 0, 0, 0, 1, 0.5, "min");
   DrawSmooth(3,"Approx: ties = min","","");

// re-initialize graph with data
   grin = new TGraph(n,x,y);
// interpolate at equidistant points (use max for tied x-values)
   grout = gs->Approx(grin,"linear", nout, xout, 0, 0, 2, 0, "max");
   DrawSmooth(4,"Approx: ties = max","","");
// print output variables for given values xout
//    Int_t vNout = nout;
//    Double_t vXout, vYout;
//    for (Int_t k=0;k<vNout;k++) {
//      grout = gs->Approx(grin,"linear", 1, &xout[k], 0, 0, 2, 0, "max");
//      grout->GetPoint(1, vXout, vYout);
//      cout << "k= " << k << "  vXout[k]= " << vXout
// 	  << "  vYout[k]= " << vYout << endl;
//    }

// cleanup
   delete gs;
}

void DrawSmooth(Int_t pad, const char *title, const char *xt, 
   const char *yt)
{
   vC1->cd(pad);
   TH1F *vFrame = gPad->DrawFrame(0,0,100,1);
   vFrame->SetTitle(title);
   vFrame->SetTitleSize(0.2);
   vFrame->SetXTitle(xt);
   vFrame->SetYTitle(yt);
   grxy->SetMarkerColor(kBlue);
   grxy->SetMarkerStyle(21);
   grxy->SetMarkerSize(0.5);
   grxy->Draw("P");
   grin->SetMarkerColor(kRed);
   grin->SetMarkerStyle(5);
   grin->SetMarkerSize(0.7);
   grin->Draw("P");
   grout->DrawClone("LP");
}


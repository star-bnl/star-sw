// histograms filled and drawn in a loop
void hsum() {
//
// To see the output of this macro, click begin_html <a href="gif/hsum.gif" >here</a> end_html
//    Simple example illustrating how to use the C++ interpreter	
//    to fill histograms in a loop and show the graphics results
//Author: Rene Brun

  TCanvas *c1 = new TCanvas("c1","The HSUM example",200,10,600,400);
  c1->SetGrid();

  gBenchmark->Start("hsum");

// Create some histograms.
  TColor *t = new TColor(253,0.,0.,1.,"bluet",0.2);
  gStyle->SetStatColor(253);
  TColor *t = new TColor(254,0.,0.,0.,"grayt",0.2);
  gStyle->SetStatTextColor(254);
  total  = new TH1F("total","This is the total distribution",100,-4,4);
  main   = new TH1F("main","Main contributor",100,-4,4);
  s1     = new TH1F("s1","This is the first signal",100,-4,4);
  s2     = new TH1F("s2","This is the second signal",100,-4,4);
  total->Sumw2();  // store the sum of squares of weights
  total->SetMarkerStyle(21);
  total->SetMarkerSize(0.7);
  total->SetFillStyle(4050);
  main->SetFillStyle(4050);
  main->SetFillColor(16);
  s1->SetFillColor(42);
  s2->SetFillStyle(4050);
  s2->SetFillColor(46);
  TSlider *slider = 0;

// Fill histograms randomly
  gRandom->SetSeed();
  const Int_t kUPDATE = 500;
  Float_t xs1, xs2, xmain;
  for ( Int_t i=0; i<10000; i++) {
     xmain = gRandom->Gaus(-1,1.5);
     xs1   = gRandom->Gaus(-0.5,0.5);
     xs2   = gRandom->Landau(1,0.15);
     main->Fill(xmain);
     s1->Fill(xs1,0.3);
     s2->Fill(xs2,0.2);
     total->Fill(xmain);
     total->Fill(xs1,0.3);
     total->Fill(xs2,0.2);
     if (i && (i%kUPDATE) == 0) {
        if (i == kUPDATE) {
           total->Draw("e1p");
           main->Draw("same");
           s1->Draw("same");
           s2->Draw("same");
           c1->Update();
           slider = new TSlider("slider",
              "test",4.2,0,4.6,total->GetMaximum(),38);
           slider->SetFillColor(46);
        }
        if (slider) slider->SetRange(0,Float_t(i)/10000.);
        c1->Modified();
        c1->Update();
     }
  }
  slider->SetRange(0,1);
  // total->Draw("sameaxis"); // to redraw axis hidden by the fill area
   TLegend *leg = new TLegend(0.1,0.7,0.48,0.9);
   leg->SetFillStyle(4050);
   leg->SetHeader("The Legend Title");
   leg->AddEntry(total);
   leg->AddEntry(main);
   leg->AddEntry(s1);
   leg->AddEntry(s2);
   leg->Draw();
  c1->Modified();
  gBenchmark->Show("hsum");
  cout << "Create the clickable TCanvas  HTML image map:" << endl;
  c1->Update();
  TQtCanvas2Html WebSite(c1,1.8,"html");
}

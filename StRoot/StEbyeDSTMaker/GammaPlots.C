{
  #include<fstream.h>
  gROOT->Reset();

  //
  // a macro to plot data vs. gamma reference for pt fluctuation 
  //  analysis (depends on loading Gammas.C first [.L Gammas.C]
  // 
  // by Jeff Reid
  // with substantial input from Tom Trainor and Lanny Ray
  //

  /////////////////////////////////////////////////////////////
  //  
  // first calculate width excess numerically...
  //

  int i,j;
  int Index = 0;

  double Np,ptp,pt2p;
  double Nm,ptm,pt2m;

  int eventPool = 300000;

  double N[eventPool];
  double pt[eventPool];
  double pt2[eventPool];

  double OverallMeanPt = 0;
  double OverallMeanPtSquared = 0;
  double OverallMeanN = 0;
  double OverallMeanNSquared = 0;
  double OverallMeanNFourth = 0;

  double psi = 0;
  double Z, Zsquare;
  double OverallPtVariance;
  double OverallNVariance;
  double Phi;

  double term1a, term2a, term3;
  double term1b, term2b;
  double term4, term5, term6;

  double terma, termb, termc;
  int eid, rid;

  double MeanNptSq = 0;
  double Nbar2; 
  double lcc0,lcc,lcc2,lcc3;
  double ptNsum = 0;
  double ptsum = 0;
  double pt2sum = 0;
  
  double ptpmean = 0;
  double ptmmean = 0;
 
  TH1F *mult = new TH1F("N","N",100,0,1000);

  // input file names
  char *FileP = "C:/root/data/STAR/qm02.nochi.+";
  char *FileM = "C:/root/data/STAR/qm02.nochi.-";

  ifstream InFilePhiP(FileP,ios::in);
  ifstream InFilePhiM(FileM,ios::in);

  // loop over input data and calculate statistics
  while (InFilePhiP >> Np >> ptp >> pt2p) {
    InFilePhiM >> Nm >> ptm >> pt2m;
    N[Index] = Np+Nm;
    mult->Fill(N[Index]); 

    if ((Np >= 1) && (Nm >= 1)) {
      ptpmean += ptp; ptmmean += ptm;
      pt[Index] = ((ptp*Np)+(ptm*Nm))/(Np+Nm);
      pt2[Index] = ((pt2p*Np)+(pt2m*Nm))/(Np+Nm);
      OverallMeanPt += pt[Index]*N[Index];
      MeanNptSq += (N[Index]*pt[Index])*(N[Index]*pt[Index]);
      OverallMeanPtSquared += pt2[Index]*N[Index];
      OverallMeanN += N[Index];
      OverallMeanNSquared += N[Index]*N[Index];
      OverallMeanNFourth += N[Index]*N[Index]*N[Index]*N[Index];
      ptNsum += N[Index]*pt[Index]*N[Index];
      ptsum += pt[Index];
      pt2sum += pt[Index]*pt[Index];
      Index++;
    } 

  }

  OverallMeanPt /= OverallMeanN;
  OverallMeanPtSquared /= OverallMeanN;

  ptpmean /= Index; ptmmean /= Index;

  for (i = 0 ; i < Index ; i++) {
    Z = N[i]*(pt[i]-OverallMeanPt); 
    psi += N[i]*(pt[i]-OverallMeanPt)*(pt[i]-OverallMeanPt);
    Zsquare += Z*Z;   
  }

  psi /= Index;
  Zsquare /= Index;
  OverallMeanN /= Index;
  OverallMeanNSquared /= Index;
  MeanNptSq /= Index;

  ptNsum /= Index;
  ptsum /= Index;
  Nbar2 = OverallMeanN*OverallMeanN;

  OverallNVariance = sqrt(OverallMeanNSquared-(OverallMeanN*OverallMeanN));
  OverallPtVariance = sqrt(OverallMeanPtSquared-(OverallMeanPt*OverallMeanPt));

  // get Phi_pt in units of MeV; a measure of width excess
  //  (approximately equal to Delta sigma in this regime)
  Phi = 1000*(sqrt(Zsquare/OverallMeanN)-OverallPtVariance);

//---------- end of phi calc ---------------

//////////////////////////////////////////////////////////////////////
//
// Now that we have the numerical results, we can make the plots...

  Float_t N0 = 100;

  Float_t mpt = OverallMeanPt;
  Float_t spt = OverallPtVariance;
  Float_t Nbar = OverallMeanN;
  Float_t phi = Phi/1000;
  Float_t ptpbar = ptpmean;
  Float_t ptmbar = ptmmean;

  Float_t rms = (spt/sqrt(Nbar));

  Float_t v,x,y,z,dif,difG;
 
  // alpha is Dsig_pt^2/sig_pt^2
  // need a factor of two because we want dsig^2/sig^2=dn/n
  // which comes in from the derivative
  Float_t alphaT = phi/spt;
  Float_t alpha = 2*alphaT;
 
  // hardwired histogram numbers
  Float_t titoff = 1.25;
  Float_t Bins = 64;

  Float_t LowLimit = -5;
  Float_t HighLimit = 5;

  // define your histograms
 
  TH1F *odHist = new TH1F("data","",Bins,LowLimit,HighLimit);
  TH1F *ofHist = new TH1F("fit","",Bins,LowLimit,HighLimit);
  TH1F *osHist = new TH1F("smiley","",Bins,LowLimit,HighLimit);

  odHist->GetXaxis()->SetTitle("#sqrt{n}#bullet(#LTp_{t}#GT-#hat{p_{t}})/#sigma_{p_{t}}");  
  odHist->GetYaxis()->SetTitle("number of events N");

  osHist->GetXaxis()->SetTitle("#sqrt{n}#bullet(#LTp_{t}#GT-#hat{p_{t}})/#sigma_{p_{t}}");
  osHist->GetYaxis()->SetTitle("#deltaN / #sqrt{N}");

  ifstream InFilePlus(FileP,ios::in);
  ifstream InFileMinus(FileM,ios::in);

  float value_mean = 0;
  float value;

  while (InFilePlus >> Np >> ptp >> pt2p) {
    InFileMinus >> Nm >> ptm >> pt2m;
    if ((Np >= 1) && (Nm >= 1)) {
      value = sqrt(Np+Nm)*(((Np*ptp+Nm*ptm)/(Np+Nm))-mpt)/(rms*sqrt(Nbar));
      // subtract value_mean to make a zero-centered distribution
      odHist->Fill(value-(-0.007119));
      value_mean += value;        
    }
  }

  printf("%f = value_mean\n",(value_mean/Index));

  Int_t Params = 5;

  // Creates a Root function based on function tomGamma above
  TF1 *myOneDGamma = new TF1("myOneDGamma",tomGamma,LowLimit,HighLimit,Params);
  myOneDGamma->SetParameters(N0,mpt,spt,Nbar,1);

  // peak height is a free parameter so choose it to look good...
  float dMax = 10000;

  N0 *= (dMax/(myOneDGamma->Eval(0)));
  myOneDGamma->SetParameters(N0,mpt,spt,Nbar,1);
  Float_t myTop=myOneDGamma->Eval(0);

  TF1 *myOthOneDGamma = new TF1("myOthOneDGamma",tomGamma,LowLimit,HighLimit,Params);
  myOthOneDGamma->SetParameters(N0,mpt,spt,Nbar,(1-alpha));
  Float_t myOtherTop=myOthOneDGamma->Eval(0);

  myOthOneDGamma->SetParameters(((myTop/myOtherTop)*N0),mpt,spt,Nbar,(1-alpha));
  
  //
  // read data from Draper's MC
  //
  ifstream jimFile("C:/root/data/STAR/DraperFix.txt",ios::in);

  float val;
  int cnt = 0;

  TH1F *jdHist = new TH1F("jd","",661,LowLimit,HighLimit);
  TH1F *jd2Hist = new TH1F("jd2","",661,LowLimit,HighLimit);
  while (jimFile >> val) { cnt++; jdHist->SetBinContent(cnt,val); }

  // create a new canvas
  myC = new TCanvas("myC","The Ntuple canvas",10,10,510,410);
  gPad->SetTicks();
  myC->SetLogy(1);

  gStyle->SetOptStat(0);
  gStyle->SetTitleOffset(titoff,"xy");

  odHist->SetLineWidth(4);
  odHist->SetMaximum(20000);
  odHist->SetMinimum(1);

  odHist->Draw();
  //jdHist->Draw("csame");

  myOneDGamma->SetLineWidth(8);
  myOneDGamma->SetLineColor(3); 
  myOneDGamma->SetLineStyle(kDashDotted);
  myOneDGamma->Draw("same");
 
  myOthOneDGamma->SetLineColor(4);
  myOthOneDGamma->Draw("same");

  jdHist->Draw("csame");

  // data/gamma difference
  for (int i=0;i<=Bins;i++)
  {
    x=osHist->GetBinCenter(i);
    y=myOneDGamma->Eval(x);
    z=odHist->GetBinContent(i);
   
    dif = (z-y)/sqrt(y);    
   
    osHist->Fill(x,dif);
  }

  // get gamma/gamma difference   
  for (int i=0;i<=Bins;i++)
  {
    x=ofHist->GetBinCenter(i);
    y=myOneDGamma->Eval(x);
    v=myOthOneDGamma->Eval(x);

    difG = (v-y)/sqrt(y);

    ofHist->Fill(x,difG);
  }
  
  // get DraperMC/gamma difference
  for (int i=1;i<=661;i++)
  {
    x=jdHist->GetBinCenter(i);
    y=myOneDGamma->Eval(x);
    v=jdHist->GetBinContent(i);

    difG = (v-y)/sqrt(y);

    jd2Hist->SetBinContent(i,difG);
  }

  myC2 = new TCanvas("myC2","Smile!",10,10,510,410);
  gPad->SetTicks();

  gStyle->SetOptStat(0);
  gStyle->SetTitleOffset(titoff,"xy");

  ofHist->SetMaximum(30);
  ofHist->SetMinimum(-5);

  osHist->SetMaximum(30);
  osHist->SetMinimum(-5);
 
  osHist->SetLineWidth(4);
  ofHist->SetLineWidth(6);

  osHist->Draw();
  ofHist->Draw("csame");
 
  TF1 *zeroLine = new TF1("zeroLine","0",LowLimit,HighLimit);

  zeroLine->SetLineWidth(8);
  zeroLine->SetLineColor(kRed);
  zeroLine->SetLineStyle(kDashed);
  
  zeroLine->Draw("same");  

  //jd2Hist->Draw("csame");

  //
  //mult->Draw();

  //TH1F GammaCheck("Gamma Check","histo from Gamma",Bins,LowLimit,HighLimit);
  //GammaCheck.FillRandom("myOneDGamma",200000);

  //myC3 = new TCanvas("myC3","GCheck",10,10,510,410);
  //gStyle->SetOptStat(1);
  
  //GammaCheck.Draw();

}


// This macro calculates the background ALL based on the side-band analysis.
// Author: Weihong He

const int mx=4;
const int mt=1;
TFile *fd[mx];
TCanvas *c1=0;
int bg1[7][4],bg2[7][4];
float cut1[7]={6,5,4,5,5,6,7};

BackgroundLL(){
  gStyle->SetPadGridX(0);
  gStyle->SetPadGridY(0);
  gStyle->SetCanvasColor(0);
  gStyle->SetOptStat(0);
  c=new TCanvas("tmp","tmp",700,500);
  float fptmin[7]={4.0,5.0,6.0,7.0,8.0,9.0,10.0};
  float fptmax[7]={5.0,6.0,7.0,8.0,9.0,10.0,25.0};
  TH1F* h5=new TH1F("A_LL_back","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.0#leq#eta#leq2.0;pT[Gev/c];A_{LL}_back",22,1.0,12.0);
  TH1F* h6=new TH1F("A_LL","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.089#leq#eta#leq2.0,P=60%;[Gev/c];A_{LL}",22,1.0,12.0);
  TH1F* h7=new TH1F("A_LL","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.089#leq#eta#leq2.0,P=60%;[Gev/c];A_{LL}",22,1.0,12.0);
  TH1F* h8=new TH1F("A_LL","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.089#leq#eta#leq2.0,P=60%;[Gev/c];A_{LL}",22,1.0,12.0);
  const int nBins = 7;
  float bins[nBins+1] = {4,5,6,7,8,9,10,12};
  TH1F* h4 = new TH1F("A_LL_back","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.0#leq#eta#leq2.0;pT[Gev/c];A_{LL};",nBins,bins);
  h4->SetMarkerStyle(8);
#endif
  TH1F* hSys = new TH1F("A_LL_back","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.0#leq#eta#leq2.0;pT[Gev/c];A_{LL};",nBins,bins);
  TH1F* hB = new TH1F("A_LL_back","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.0#leq#eta#leq2.0;pT[Gev/c];A_{LL};",nBins,bins);
  float baseLine=-0.14;
  float sysErr[7] = {0.00239,0.00521,0.00485,0.00405,0.012,0.0108,0.0104};


  psquare=0.3;

  for(int i=0;i<7;i++){
    float bgll=0.0,bgll_error=0.0,bgll1=0.0,bgll1_error=0.0,bgll2=0.0,bgll2_error=0.0;
    Background(i, fptmin[i], fptmax[i]);
    int Nsame1=bg1[i][0]+bg1[i][3];
    int Nanti1=bg1[i][1]+bg1[i][2];
    int Nsame2=bg2[i][0]+bg2[i][3];
    int Nanti2=bg2[i][1]+bg2[i][2];
    //int Nsame=Nsame1+Nsame2;
    //int Nanti=Nanti1+Nanti2;
    bgll1=1.0/psquare*(Nsame1-Nanti1)/(Nsame1+Nanti1);
    bgll1_error=1.0/psquare*1.0/sqrt(Nsame1+Nanti1);
    bgll2=1.0/psquare*(Nsame2-Nanti2)/(Nsame2+Nanti2);
    bgll2_error=1.0/psquare*1.0/sqrt(Nsame2+Nanti2);
    bgll=0.5*(bgll1+bgll2);
    bgll_error=0.5*(bgll1_error+bgll2_error);
    //bgll_error=sqrt(1.0/(1.0/pow(bgll1_error,2.0)+1.0/pow(bgll2_error,2.0)));
    cout<<"bgll="<<bgll<<" error1="<<bgll1_error<<" error2="<<bgll2_error<<" error="<<bgll_error<<endl;
    h4->Fill(fptmin[i]+0.5,bgll);
    int nbin0=h4->FindBin(fptmin[i]+0.5);
    h4->SetBinError(nbin0,bgll_error);
    //cout<<"point1 error="<<e3[0]<<" all="<<a3[0]<<endl;
    hSys->SetBinContent(nbin0,baseLine + sysErr[i]);
    hB->SetBinContent(nbin0,baseLine);

  }

  //open theory curve
  ifstream unp("theorycurve/pion-unp-cteq6-rap1to2.dat");
  ifstream g0("theorycurve/pion-pol-g0-rap1to2.dat");
  ifstream plusg("theorycurve/pion-pol-max-rap1to2.dat");
  ifstream minusg("theorycurve/pion-pol-maxminus-rap1to2.dat");
  ifstream stdg("theorycurve/pion-pol-std-rap1to2.dat");
  double x1=0.0,x2=0.0,x3=0.0,x4=0.0,x5=0.0;
  double d1=0.0,d2=0.0,d3=0.0,d4=0.0,d5=0.0;
  double b1=0.0,b2=0.0,b3=0.0,b4=0.0,b5=0.0;
  double c1=0.0,c2=0.0,c3=0.0,c4=0.0,c5=0.0;
  double y1=0.0,y2=0.0,y3=0.0,y4=0.0,y5=0.0;
  int ccount=0;
  double theoryd[18],theoryb[18],theoryc[18],theoryg[18];;
  while(1){
    unp>>y1>>y2>>y3>>y4>>y5;
    g0>>x1>>x2>>x3>>x4>>x5;
    plusg>>d1>>d2>>d3>>d4>>d5;
    minusg>>b1>>b2>>b3>>b4>>b5;
    stdg>>c1>>c2>>c3>>c4>>c5;
    theoryd[ccount]=d5/y5;
    theoryb[ccount]=b5/y5;
    theoryc[ccount]=c5/y5;
    theoryg[ccount]=x5/y5;
    //cout<<"x3="<<d3<<" theoryd="<<theoryg[ccount]<<endl;
    h5->Fill(d3,theoryd[ccount]);
    h6->Fill(b3,theoryb[ccount]);
    h7->Fill(c3,theoryc[ccount]);
    h8->Fill(x3,theoryg[ccount]);
    ccount++;
    if(ccount>17) break;
  }
  h5->Fill(8.0,0.03);
  h5->SetAxisRange(0,25,"X");
  h5->Fill(9.0,0.0305);
  h5->Fill(10.0,0.031);
  h5->Fill(10.5,0.0312);
  h5->Fill(11.5,0.031);
  h6->Fill(8.0,-0.001);
  h6->Fill(9.0,-0.0035);
  h6->Fill(10.0,-0.0065);
  h6->Fill(10.5,-0.0075);
  h6->Fill(11.5,-0.01);
  h7->Fill(8.0,0.0072);
  h7->Fill(9.0,0.0079);
  h7->Fill(10.0,0.0087);
  h7->Fill(10.5,0.0091);
  h7->Fill(11.5,0.0099);
  h8->Fill(8.0,0.00152);
  h8->Fill(9.0,0.00183);
  h8->Fill(10.0,0.00204);
  h8->Fill(10.5,0.0022);
  h8->Fill(11.5,0.0026);
#if 1
  //A_LL plotting
  c->cd(1);
  h5->SetLineColor(kRed);
  h5->Draw("C");
  h5->SetMinimum(-0.15);
  h5->SetMaximum(0.15);
  hB->SetFillColor(11);
  hB->Draw("same");
  hSys->SetFillColor(10);
  hSys->Draw("same");
  h6->SetLineColor(kGreen);
  h6->Draw("Csame");
  h7->Draw("Csame");
  h8->SetLineColor(kBlue);
  h8->Draw("Csame");

  h4->Draw("E1same,p");

  TLegend* leg = new TLegend(0.15,0.6,0.4,0.8);
  leg->AddEntry(h5,"#Delta{G}=G","L");
  leg->AddEntry(h6,"#Delta{G}=-G","L");
  leg->AddEntry(h7,"#Delta{G}=std","L");
  leg->AddEntry(h8,"#Delta{G}=0","L");
  leg->SetHeader("Vogelsang Prediction(GRSV)");
  leg->SetBorderSize(0.0);
  leg->SetFillColor(0.0);
  leg->Draw();
  TLegend* leg2 = new TLegend(0.6,0.75,0.9,0.85);
  leg2->SetHeader("STAR 2006 Preliminary #pi^{0}");
  leg2->SetBorderSize(0.0);
  leg2->SetFillColor(0.0);
  leg2->Draw();

#endif
  c->Print("tmp.gif");
}

void Background(int key, float ptmin, float ptmax) {

  int Realpi=0,backpi=0,totpi=0;

  char *PlotName[mx]={"hMassPtUU","hMassPtUD","hMassPtDU","hMassPtDD"};
  char *fName[mt]={"allfill.hist"};
  TString inPath="";
  
  gStyle->SetPalette(1,0); 
  int i;
  for(i=0;i<mt;i++) {
    TString   hFile=inPath+fName[i];
    hFile+=".root";
    fd0=new TFile(hFile); assert(fd0->IsOpen());
    fd[i]=fd0;
  }
  //int ploti;

  for(int ploti=0;ploti<mx;ploti++) 
    { 
      int ent=0;
      int minbin,maxbin;
      TH1F*  h1= new TH1F("hMassAny","diphoton invariant mass",120,0.,1.2 );
      TString hname=PlotName[ploti];
      printf("i= %d =%s=\n",ploti,hname.Data());
      h0=(TH2F*)fd[0]->Get(hname); assert(h0);
      minbin=h0->GetYaxis()->FindBin(ptmin);
      maxbin=h0->GetYaxis()->FindBin(ptmax)-1;
      h1->Add(h0->ProjectionX("htemp",minbin,maxbin,""));
      ent=h1->Integral();
      bg1[key][ploti]=h1->Integral(5,9);
      bg2[key][ploti]=h1->Integral(20,41);
      cout<<"entry="<<ent<<" bg1="<<bg1[key][ploti]<<" bg2="<<bg2[key][ploti]<<endl;
      h1->SetEntries(ent);
      
    }//end of mx
  return;
  
}


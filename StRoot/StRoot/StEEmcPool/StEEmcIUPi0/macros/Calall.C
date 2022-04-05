//this macro is designed to calculate the single spin asymmetries from yellow and blue beams, and double spin asymmetry from pi0 study, based on the generated/fitted/normalized pi0 yield from FitPt2Mass.C in our fitting philosopy. 
//author: Weihong He

#include <iostream>
#include <string>
#include <fstream>
  using namespace std;
//X:yellow Y:blue
//background_LL is calculated from BackgroundLL.C because we want to use another method: side-band analysis to background study, although you can also calculate background all here by simply replacing the all array to back array.
double epsilon_ll[7],ep_error[7],ptmean[7],epsilon_real[7],epsilon_back[7],ds[7],dr[7],dw[7],db[7],epsilon_X[7],epsilon_Y[7],dx[7],dy[7],ep_error_rb[7];
Calall(){
  //x*[*] save epsilon from yellow beam, y*[*] save epsilon from blue beam, all*[*] save all values.
  double x1[7],x2[7],x3[7],y1[7],y2[7],y3[7],all1[7],all2[7],all3[7];
  //corresponding errors
  double ex1[7],ex2[7],ex3[7],ey1[7],ey2[7],ey3[7],eall1[7],eall2[7],eall3[7];
  double pt1[7],pt2[7],pt3[7];
  gStyle->SetPadGridX(0);
  gStyle->SetPadGridY(0);
  gStyle->SetCanvasColor(0);
  gStyle->SetOptStat(0);
  c0=new TCanvas("epsilony","epsilony",700,500);
  c0->Divide(1,1);
  c1=new TCanvas("epsilonx","epsilonx",700,500);
  c1->Divide(1,1);
  c2=new TCanvas("all","all",700,500);
  c2->Divide(1,1);
  TH1F* h1=new TH1F("epsilon_LB","#epsilon_{LB} vs pT",60,4.,14.);
  TH1F* h2=new TH1F("epsilon_LY","#epsilon_{LY} vs pT",60,4.,14.);
  //theory curve histograms
  TH1F* h5=new TH1F("A_LL","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.0#leq#eta#leq2.0;pT[Gev/c];A_{LL}",22,1.0,12.0);
  TH1F* h6=new TH1F("A_LL","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.089#leq#eta#leq2.0,P=60%;[Gev/c];A_{LL}",22,1.0,12.0);
  TH1F* h7=new TH1F("A_LL","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.089#leq#eta#leq2.0,P=60%;[Gev/c];A_{LL}",22,1.0,12.0);
  TH1F* h8=new TH1F("A_LL","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.089#leq#eta#leq2.0,P=60%;[Gev/c];A_{LL}",22,1.0,12.0);


  gStyle->SetOptStat(0); 

  //dataset 1
  ifstream yield1("yield3335557.txt");  
  doCalall(yield1);
  for(int j=0;j<7;j++){
    x1[j]=epsilon_X[j];
    ex1[j]=dx[j];
    y1[j]=epsilon_Y[j];
    ey1[j]=dy[j];
    all1[j]=epsilon_ll[j];
    eall1[j]=ep_error_rb[j];
    pt1[j]=ptmean[j];

  }

  //dataset 2
  ifstream yield2("yield4444446.txt");  
  doCalall(yield2);
  for(int j=0;j<7;j++){
    x2[j]=epsilon_X[j];
    ex2[j]=dx[j];
    y2[j]=epsilon_Y[j];
    ey2[j]=dy[j];
    all2[j]=epsilon_ll[j];
    eall2[j]=ep_error_rb[j];
    pt2[j]=ptmean[j];

  }

  //dataset3
  ifstream yield3("yield5556668.txt");  
  doCalall(yield3);
  for(int j=0;j<7;j++){
    x3[j]=epsilon_X[j];
    ex3[j]=dx[j];
    y3[j]=epsilon_Y[j];
    ey3[j]=dy[j];
    all3[j]=epsilon_ll[j];
    eall3[j]=ep_error_rb[j];
    pt3[j]=ptmean[j];

  }
  double big[7],small[7];
  for(int k=0;k<7;k++){
    if(all1[k]>all2[k]) big[k]=all1[k];
    else big[k]=all2[k];
    if(all3[k]>big[k]) big[k]=all3[k];

    if(all1[k]<all2[k]) small[k]=all1[k];
    else small[k]=all2[k];
    if(all3[k]<small[k]) small[k]=all3[k];
  }
  double sys_error[7];
  for(int m=0;m<7;m++){
    sys_error[m]=big[m]-small[m];
    //cout<<"syserror="<<sys_error[m]<<endl;
  }
  //calculate All

  const int nBins = 7;
  float bins[nBins+1] = {4,5,6,7,8,9,10,12};
#if 1
  TH1F* h4 = new TH1F("A_LL","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.0#leq#eta#leq2.0;pT[Gev/c];A_{LL};",nBins,bins);
  h4->SetMarkerStyle(8);
#endif
  TH1F* hSys = new TH1F("A_LL","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.0#leq#eta#leq2.0;pT[Gev/c];A_{LL};",nBins,bins);
  TH1F* hB = new TH1F("A_LL","#vec{p}+#vec{p}->#pi^{0}X, #sqrt{s}=200Gev, 1.0#leq#eta#leq2.0;pT[Gev/c];A_{LL};",nBins,bins);
  float baseLine=-0.17;
  float sysErr[7] = {0.00239,0.00521,0.00485,0.00405,0.012,0.0108,0.0104};

  //point 1
  h4->Fill(pt3[0],all3[0]);
  int nbin0=h4->FindBin(pt3[0]);
  h4->SetBinError(nbin0,eall3[0]);//+sys_error[0]);//+0.0005315);
  h1->Fill(pt3[0],y3[0]);
  int nbbin0=h1->FindBin(pt3[0]);
  h1->SetBinError(nbbin0,ey3[0]);
  h2->Fill(pt3[0],x3[0]);
  int nbinn0=h2->FindBin(pt3[0]);
  h2->SetBinError(nbinn0,ex3[0]);
  cout<<"point1 error="<<eall3[0]<<" all="<<all3[0]<<endl;
  hSys->SetBinContent(nbin0,baseLine + sysErr[0]);
  hB->SetBinContent(nbin0,baseLine);
  //hSys->SetBinError(nbin0,sysErr[0]/2.);
  //point 2
  h4->Fill(pt2[1],all2[1]);
  int nbin1=h4->FindBin(pt2[1]);
  h4->SetBinError(nbin1,eall2[1]);//+sys_error[1]);//+0.0002291);
  h1->Fill(pt2[1],y2[1]);
  int nbbin1=h1->FindBin(pt2[1]);
  h1->SetBinError(nbbin1,ey2[1]);
  h2->Fill(pt2[1],x2[1]);
  int nbinn1=h2->FindBin(pt2[1]);
  h2->SetBinError(nbinn1,ex2[1]);
  cout<<"point2 error="<<eall2[1]<<" all="<<all2[1]<<endl;
  hSys->SetBinContent(nbin1,baseLine + sysErr[1]);
  //hSys->SetBinError(nbin1,sysErr[1]/2.);
  hB->SetBinContent(nbin1,baseLine);
  //point 3
  h4->Fill(pt1[2],all1[2]);
  int nbin2=h4->FindBin(pt1[2]);
  h4->SetBinError(nbin2,eall1[2]);//+sys_error[2]);//+0.0005046);
  h1->Fill(pt1[2],y1[2]);
  int nbbin2=h1->FindBin(pt1[2]);
  h1->SetBinError(nbbin2,ey1[2]);
  h2->Fill(pt1[2],x1[2]);
  int nbinn2=h2->FindBin(pt1[2]);
  h2->SetBinError(nbinn2,ex1[2]);
  cout<<"point3 error="<<eall1[2]<<" all="<<all1[2]<<endl;
  hSys->SetBinContent(nbin2,baseLine + sysErr[2]);
  //hSys->SetBinError(nbin2,sysErr[2]/2.);
  hB->SetBinContent(nbin2,baseLine);
  //point 4
  h4->Fill(pt2[3],all2[3]);
  int nbin3=h4->FindBin(pt2[3]);
  h4->SetBinError(nbin3,eall2[3]);//+sys_error[3]);//+0.0004908);
  h1->Fill(pt2[3],y2[3]);
  int nbbin3=h1->FindBin(pt2[3]);
  h1->SetBinError(nbbin3,ey2[3]);
  h2->Fill(pt2[3],x2[3]);
  int nbinn3=h2->FindBin(pt2[3]);
  h2->SetBinError(nbinn3,ex2[3]);
  cout<<"point4 error="<<eall2[3]<<" all="<<all2[3]<<endl;
  hSys->SetBinContent(nbin3,baseLine + sysErr[3]);
  //hSys->SetBinError(nbin3,sysErr[3]/2.);
  hB->SetBinContent(nbin3,baseLine);
  //point 5
  h4->Fill(pt2[4],all2[4]);
  int nbin4=h4->FindBin(pt2[4]);
  h4->SetBinError(nbin4,eall2[4]);//+sys_error[4]);//+0.0002051);
  h1->Fill(pt2[4],y2[4]);
  int nbbin4=h1->FindBin(pt2[4]);
  h1->SetBinError(nbbin4,ey2[4]);
  h2->Fill(pt2[4],x2[4]);
  int nbinn4=h2->FindBin(pt2[4]);
  h2->SetBinError(nbinn4,ex2[4]);
  cout<<"point5 error="<<eall2[4]<<" all="<<all2[4]<<endl;
  hSys->SetBinContent(nbin4,baseLine + sysErr[4]);
  //hSys->SetBinError(nbin4,sysErr[4]/2.);
  hB->SetBinContent(nbin4,baseLine);
  //point 6
  h4->Fill(pt1[5],all1[5]);
  int nbin5=h4->FindBin(pt1[5]);
  h4->SetBinError(nbin5,eall1[5]);//+sys_error[5]);//+0.00055);
  h1->Fill(pt1[5],y1[5]);
  int nbbin5=h1->FindBin(pt1[5]);
  h1->SetBinError(nbbin5,ey1[5]);
  h2->Fill(pt1[5],x1[5]);
  int nbinn5=h2->FindBin(pt1[5]);
  h2->SetBinError(nbbin5,ex1[5]);
  cout<<"point6 error="<<eall1[5]<<" all="<<all1[5]<<endl;
  hSys->SetBinContent(nbin5,baseLine + sysErr[5]);
  //hSys->SetBinError(nbin5,sysErr[5]/2.);
  hB->SetBinContent(nbin5,baseLine);
  //point 7
  h4->Fill(pt2[6],all2[6]);
  int nbin6=h4->FindBin(pt2[6]);
  h4->SetBinError(nbin6,eall2[6]);//+sys_error[6]);//+0.001953);
  h1->Fill(pt2[6],y2[6]);
  int nbbin6=h1->FindBin(pt2[6]);
  h1->SetBinError(nbbin6,ey2[6]);
  h2->Fill(pt2[6],x2[6]);
  int nbinn6=h2->FindBin(pt2[6]);
  h2->SetBinError(nbinn6,ex2[6]);
  cout<<"point7 error="<<eall2[6]<<" all="<<all2[6]<<endl;
  hSys->SetBinContent(nbin6,baseLine + sysErr[6]);
  //hSys->SetBinError(nbin6,sysErr[6]/2.);
  hB->SetBinContent(nbin6,baseLine);
  //open theory curve
  ifstream unp("theorycurve/pion-unp-cteq6-rap1to2.dat");
  ifstream g0("theorycurve/pion-pol-g0-rap1to2.dat");
  ifstream plusg("theorycurve/pion-pol-max-rap1to2.dat");
  ifstream minusg("theorycurve/pion-pol-maxminus-rap1to2.dat");
  ifstream stdg("theorycurve/pion-pol-std-rap1to2.dat");
  double tx1=0.0,tx2=0.0,tx3=0.0,tx4=0.0,tx5=0.0;
  double td1=0.0,td2=0.0,td3=0.0,td4=0.0,td5=0.0;
  double tb1=0.0,tb2=0.0,tb3=0.0,tb4=0.0,tb5=0.0;
  double tc1=0.0,tc2=0.0,tc3=0.0,tc4=0.0,tc5=0.0;
  double ty1=0.0,ty2=0.0,ty3=0.0,ty4=0.0,ty5=0.0;
  int ccount=0;
  double theoryd[18],theoryb[18],theoryc[18],theoryg[18];;
  while(1){
    unp>>ty1>>ty2>>ty3>>ty4>>ty5;
    g0>>tx1>>tx2>>tx3>>tx4>>tx5;
    plusg>>td1>>td2>>td3>>td4>>td5;
    minusg>>tb1>>tb2>>tb3>>tb4>>tb5;
    stdg>>tc1>>tc2>>tc3>>tc4>>tc5;
    theoryd[ccount]=td5/ty5;
    theoryb[ccount]=tb5/ty5;
    theoryc[ccount]=tc5/ty5;
    theoryg[ccount]=tx5/ty5;
    //cout<<"x3="<<d3<<" theoryd="<<theoryg[ccount]<<endl;
    h5->Fill(td3,theoryd[ccount]);
    h6->Fill(tb3,theoryb[ccount]);
    h7->Fill(tc3,theoryc[ccount]);
    h8->Fill(tx3,theoryg[ccount]);
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
  //plotting epsilon_ll, h1 for blue beam, h2 for yellow beam
#if 1
  c0->cd(1);
  h1->Draw();
  h1->Fit("pol0","R","",4.0,12.0);
  h1->SetMinimum(-0.05);
  h1->SetMaximum(0.05);
  TLegend* leg1 = new TLegend(0.15,0.65,0.5,0.85);
  leg1->SetHeader("STAR 2006 Preliminary #pi^{0}");
  leg1->SetBorderSize(0.0);
  leg1->SetFillColor(0.0);
  leg1->Draw();
  c0->Print("epsilony.gif");
  c1->cd(1);
  h2->Draw();
  h2->Fit("pol0","R","",4.0,12.0);
  h2->SetMinimum(-0.05);
  h2->SetMaximum(0.05);
  TLegend* leg2 = new TLegend(0.15,0.65,0.5,0.85);
  leg2->SetHeader("STAR 2006 Preliminary #pi^{0}");
  leg2->SetBorderSize(0.0);
  leg2->SetFillColor(0.0);
  leg2->Draw();
  c1->Print("epsilonx.gif");
#endif

#if 1
  //A_LL plotting
  c2->cd(1);
  h5->SetLineColor(kRed);
  h5->Draw("C");
  h5->SetMinimum(-0.18);
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
  TLegend* leg3 = new TLegend(0.6,0.75,0.9,0.85);
  leg3->SetHeader("STAR 2006 Preliminary #pi^{0}");
  leg3->SetBorderSize(0.0);
  leg3->SetFillColor(0.0);
  leg3->Draw();
  c2->Print("all.gif");
#endif
  
}

void doCalall(ifstream& yield){
  double epsilon_raw[7],weight[7],eps_lx_raw[7],eps_ly_raw[7],eps_lx_back[7],eps_ly_back[7];
  string s1,s2,s3,s4,s5;
  yield>>s1>>s2>>s3>>s4>>s5;
  double totsum=0,realsum=0,backsum=0;
  double psquare=0.3;
  //cout<<s1<<endl;
  for(int i=0;i<7;i++){
    int flag=0;
    int spin=0;
    double totpi=0,realpi=0,backpi=0;
    double pt=0.0;
    double Nuu_sig=0,Nuu_real=0,Nuu_back=0,Nud_sig=0,Nud_real=0,Nud_back=0,Ndu_sig=0,Ndu_real=0,Ndu_back=0,Ndd_sig=0,Ndd_real=0,Ndd_back=0;
    double sumtot=0,sumreal=0,sumback=0;
    double sumpt=0.0;
    double xx1=0.0,xx2=0.0,yy1=0.0,yy2=0.0;
    double des=0.0,deb=0.0,der=0.0,dew=0.0,eepsilon_raw=0.0,eepsilon_real=0.0,eeps_lx_raw=0.0,eeps_ly_raw=0.0,eeps_lx_back=0.0,eeps_ly_back=0.0;
    while(1)
      {
	yield>>spin>>pt>>totpi>>realpi>>backpi;
	//cout<<"spin="<<spin<<" pt="<<pt<<" totpi="<<totpi<<" realpi="<<realpi<<" backpi="<<backpi<<endl;
	sumtot+=totpi;
	sumreal+=realpi;
	sumback+=backpi;
	sumpt+=pt*totpi;
	totsum+=totpi;
	realsum+=realpi;
	backsum+=backpi;
	//cout<<"sumpt="<<sumpt<<endl;
	if(spin==0)
	  {
	    Nuu_sig+=totpi;
	    Nuu_real+=realpi;
	    Nuu_back+=backpi;
	  }
	if(spin==1)
	  {
	    Nud_sig+=totpi;
	    Nud_real+=realpi;
	    Nud_back+=backpi;
	  }
	if(spin==2)
	  {
	    Ndu_sig+=totpi;
	    Ndu_real+=realpi;
	    Ndu_back+=backpi;
	  }
	if(spin==3)
	  {
	    Ndd_sig+=totpi;
	    Ndd_real+=realpi;
	    Ndd_back+=backpi;
	  }
	flag++;
	if(flag==4) break;
	//if(flag==8) break;
      }//end of while
    xx1=Ndd_real+Ndu_real;
    xx2=Nud_real+Nuu_real;
    yy1=Ndd_real+Nud_real;
    yy2=Ndu_real+Nuu_real;
    epsilon_X[i]=-(xx1-xx2)/(xx1+xx2);
    epsilon_Y[i]=-(yy1-yy2)/(yy1+yy2);
    ptmean[i]=sumpt/sumtot;

    eepsilon_raw=(Nuu_sig-Nud_sig-Ndu_sig+Ndd_sig)/sumtot;
    eepsilon_real=(Nuu_real-Nud_real-Ndu_real+Ndd_real)/sumreal;
    eeps_lx_raw=(Ndd_sig-Nud_sig-Nuu_sig+Ndu_sig)/sumback;
    eeps_ly_raw=(Ndd_sig+Nud_sig-Nuu_sig-Ndu_sig)/sumback;
    eeps_lx_back=(Ndd_back-Nud_back-Nuu_back+Ndu_back)/sumback;
    eeps_ly_back=(Ndd_back+Nud_back-Nuu_back-Ndu_back)/sumback; 
    epsilon_raw[i]=1.0/psquare*eepsilon_raw;
    //all calculation from pure pi0 yield after background subtraction.
    epsilon_real[i]=1.0/psquare*eepsilon_real;
    eps_lx_raw[i]=1.0/psquare*eeps_lx_raw;
    eps_ly_raw[i]=1.0/psquare*eeps_ly_raw;
    eps_lx_back[i]=1.0/psquare*eeps_lx_back;
    eps_ly_back[i]=1.0/psquare*eeps_ly_back;
    weight[i]=sumback/sumtot;

    epsilon_back[i]=1.0/psquare*(Nuu_back-Nud_back-Ndu_back+Ndd_back)/sumback;
    //all calculation
    epsilon_ll[i]=(epsilon_raw[i]-weight[i]*epsilon_back[i])/(1.0-weight[i]);
    //stat. error calculation
    des=sqrt(sumtot)/sumtot;
    der=sqrt(sumreal)/sumreal;
    deb=sqrt(sumback)/sumback;
    dew=sqrt(sumback)/sumtot; 
    ds[i]=1.0/psquare*des;
    dr[i]=1.0/psquare*der;
    db[i]=1.0/psquare*deb;
    dw[i]=1.0/psquare*dew;


    ep_error[i]=sqrt(pow(ds[i]/(1-weight[i]),2)+pow((weight[i]*db[i])/(1-weight[i]),2)+pow(dw[i]*(epsilon_raw[i]-epsilon_back[i])/pow(1-weight[i],2),2));
    dx[i]=sqrt(pow(des/(1-weight[i]),2)+pow((weight[i]*deb)/(1-weight[i]),2)+pow(dew*(eeps_lx_raw-eeps_lx_back)/pow(1-weight[i],2),2));
    dy[i]=sqrt(pow(des/(1-weight[i]),2)+pow((weight[i]*deb)/(1-weight[i]),2)+pow(dew*(eeps_ly_raw-eeps_ly_back)/pow(1-weight[i],2),2));
    //the second way to calculate error of all
    ep_error_rb[i]=sqrt(pow(dr[i],2)+pow(db[i],2));
  

  }//end of for looping pt bins
 

}

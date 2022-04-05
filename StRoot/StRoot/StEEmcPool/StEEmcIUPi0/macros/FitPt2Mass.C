// After we test the parameter values and fit function from FitPtMass.C, we fix some parameters and start generating four spin-depdendent pi0 yields.
// The default input file is the root file after normalization from normalizemass.C. The default file name is 'allfill.hist.root'.
// We are looping over four spin-dependent plot from the root file "hMassPtUU","hMassPtUD","hMassPtDU","hMassPtDD" to generate yield.
// The output will be 4*7 fitted histograms and a pi0 yield txt file. By changing the fit ranges three times, we get three yield txts, which will be used by Calall.C to calculate the double spin asymmetry.
// Author: Weihong He.

const int mx=4;
const int mt=1;
TFile *fd[mx];
TCanvas *c1=0;
FILE* fout=fopen("yield.txt","w");assert(fout);
FILE* fmass=fopen("mass.txt","w");assert(fout);
TF1* f1[7];

FitPt2Mass(){
  fprintf(fout,"ss pt_mean totpi realpi background\n");
  float fptmin[7]={4.0,5.0,6.0,7.0,8.0,9.0,10.0};
  float fptmax[7]={5.0,6.0,7.0,8.0,9.0,10.0,25.0};
  float ffit_x1[7]={0.03,0.03,0.03,0.05,0.05,0.05,0.07};
  float ffit_x2=0.4;
  float fpar0[7]={8.0,7.8,7.2,6.4,5.6,4.9,4.9};
  float fpar1[7]={-8.33,-6.82,-5.15,-4.14,-3.52,-3.0,-2.9};
  float fpar2[7]={3000,3000,2000,1000,500,400,400};
  float fpar3[7]={0.1308,0.1356,0.136,0.137,0.138,0.1375,0.1385};
  float fpar4[7]={0.0208,0.02166,0.0231,0.025,0.027,0.027,0.029};
  f1[0]=new TF1("ffit","exp([0]-8.262*x)+[1]*exp(-0.5*((x-0.1305)/(0.02073*(1.+6.11*(x-0.1305))))**2)",ffit_x1[0],ffit_x2);
  f1[1]=new TF1("ffit","exp([0]-6.752*x)+[1]*exp(-0.5*((x-0.1353)/(0.02164*(1.+6.11*(x-0.1353))))**2)",ffit_x1[1],ffit_x2);
  f1[2]=new TF1("ffit","exp([0]-5.095*x)+[1]*exp(-0.5*((x-0.1359)/(0.02307*(1.+6.11*(x-0.1359))))**2)",ffit_x1[2],ffit_x2);
  f1[3]=new TF1("ffit","exp([0]-4.041*x)+[1]*exp(-0.5*((x-0.1375)/(0.02498*(1.+6.11*(x-0.1375))))**2)",ffit_x1[3],ffit_x2);
  f1[4]=new TF1("ffit","exp([0]-3.37*x)+[1]*exp(-0.5*((x-0.1374)/(0.02666*(1.+6.11*(x-0.1374))))**2)",ffit_x1[4],ffit_x2);
  f1[5]=new TF1("ffit","exp([0]-2.942*x)+[1]*exp(-0.5*((x-0.137)/(0.0266*(1.+6.11*(x-0.137))))**2)",ffit_x1[5],ffit_x2);
  f1[6]=new TF1("ffit","exp([0]-2.668*x)+[1]*exp(-0.5*((x-0.1392)/(0.02792*(1.+6.11*(x-0.1392))))**2)",ffit_x1[6],ffit_x2);
  for(int i=0;i<7;i++){
    if(i<3){
      const int fnend=int(ffit_x1[i]*100)+1;
    }
    else{
      const int fnend=int(ffit_x1[i]*100);
    }
    const int fnstart=int(ffit_x2*100);
    cout<<"i="<<i<<" nend="<<fnend<<" nstart="<<fnstart<<endl;
    
    FitPtMass(i, fptmin[i], fptmax[i], ffit_x1[i], ffit_x2, fpar0[i], fpar1[i], fpar2[i],  fnend, fnstart);
  }
}

void FitPtMass(int key, float ptmin, float ptmax, float fit_x1, float fit_x2, float par0, float par1, float par2, int nend, int nstart) {

  int Realpi=0,backpi=0,totpi=0;
  //int minbin,maxbin;
  //TH1F*  h1= new TH1F("hMassAny","diphoton invariant mass",120,0.,1.2 );
  //TH1F*  h2= new TH1F("hMassAny","diphoton invariant mass",120,0.,1.2 );
  //TH1F*  hResidual= new TH1F("hMassAny","diphoton invariant mass",120,0.,1.2 );
  
  //fprintf(fout,"Bin# Entry\n");
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
  int channel_yield[mx][40];
  int channel_pi_yield[mx][40];
  int channel_bg_yield[mx][40];
  for(int cmx=0;cmx<mx;cmx++)
    {
      for(int i=0;i<40;i++)
	{
	  channel_yield[cmx][i]=0;
	  channel_pi_yield[cmx][i]=0;
	  channel_bg_yield[cmx][i]=0;
	}
    }
  for(int ploti=0;ploti<mx;ploti++) 
    { 
      if(c1) delete c1;
      c1=new TCanvas("c1","c1",500,500);
      c1->Divide(1,1);
      c1->cd(1);
      //gStyle->SetPalette(1,0);
      // .... inp eve
      int minbin,maxbin;
      TH1F*  h1= new TH1F("hMassAny","diphoton invariant mass",120,0.,1.2 );
      TH1F*  h2= new TH1F("hMassAny","diphoton invariant mass",120,0.,1.2 );
      TH1F*  hResidual= new TH1F("hMassAny","diphoton invariant mass",120,0.,1.2 );
      TString hname=PlotName[ploti];
      printf("i= %d =%s=\n",ploti,hname.Data());
      h0=(TH2F*)fd[0]->Get(hname); assert(h0);
      minbin=h0->GetYaxis()->FindBin(ptmin);
      maxbin=h0->GetYaxis()->FindBin(ptmax)-1;
      h1->Add(h0->ProjectionX("htemp",minbin,maxbin,""));
      int ent=h1->Integral();
      cout<<"entry="<<ent<<endl;
      h1->SetEntries(ent);
      int ccount=0;
      float sum=0;
      float tpt=ptmin;
      for(int i=minbin;i<=maxbin;i++){
	ccount+=h0->ProjectionX("",i,i,"")->Integral();
	sum+=(h0->ProjectionX("",i,i,"")->Integral())*tpt;
	tpt+=(ptmax-ptmin)/(maxbin+1-minbin);
      }
      float ptmean=sum/ccount;
      cout<<"raw="<<int(h1->Integral(11,19))<<" ptmean="<<ptmean<<endl;
      h1->Draw();
      h1->SetMinimum(-ent/100);
      //TF1* f1=new TF1("ffit","exp([0]+par1*x)+[2]*exp(-0.5*((x-par3)/(par4*(1.+6.11*(x-par3))))**2)",fit_x1,fit_x2);
      f1[key]->SetParameters(par0,par2);
      h1->Fit(f1[key],"R+","",fit_x1,fit_x2);
      int nx=h1->GetNbinsX();
      cout<<"nx="<<nx<<endl;
      float sumx=0,sum2;
      for(int ix=1;ix<=40;ix++)
	{
	  channel_yield[ploti][ix-1]+=h1->GetBinContent(ix);
	    
	  sumx+=h1->GetBinContent(ix);
	}
      //cout<<"sumx="<<sumx<<endl;
      TF1* f2=new TF1("fit2","expo",0.,1.2);
      //TF1* f2=new TF1("fit2","exp(5.7-6.4*x)",0.,1.2);
      h2->Add(h1);
      hResidual->Add(h1);
      hResidual->Add(f1[key],-1);
      for (int ix= 1; ix <= nend; ix++) {
	hResidual->SetBinContent(ix, 0);
      }  
      for (int ix= nstart; ix <= nx; ix++) {
	hResidual->SetBinContent(ix, 0);
      }
      hResidual->SetLineColor(11);
      hResidual->Draw("same");
      f2->SetParameters(f1[key]->GetParameter(0),par1);
      h2->Add(f2,-1);
      for (int ix= 1; ix <= 8; ix++) {
	//if (h2->GetBinContent(ix) < 0) h2->SetBinContent(ix, 0);
	h2->SetBinContent(ix, 0);
      }  
      //for(int ix=9;ix<=19;ix++){
      //sum2+=h2->GetBinContent(ix);
      //}
      h2->SetLineColor(kBlue);
      TF1* f3=new TF1("f3","expo(0)",fit_x1,fit_x2);
      f3->SetParameters(f1[key]->GetParameter(0),par1);
      cout<<"p0="<<f1[key]->GetParameter(0)<<endl;
      //h2->Fit("gaus","","",0.42,0.66);
      Realpi=h2->Integral(11,19);
      backpi=h1->Integral(11,19)-h2->Integral(11,19);
      totpi=h1->Integral(11,19);
      
      for(int ix=1;ix<=40;ix++)
	{
	  channel_pi_yield[ploti][ix-1]+=h2->Integral(ix,ix);
	  //cout<<"channel_pi_yield[ploti][ix-1]="<<channel_pi_yield[ploti][ix-1]<<endl;
	  channel_bg_yield[ploti][ix-1]+=h1->Integral(ix,ix)-h2->Integral(ix,ix);  
	  //cout<<"channel_bg_yield[ploti][ix-1]="<<channel_bg_yield[ploti][ix-1]<<endl;
	}
      //ttotpi=h1->GetBinContent(11)+h1->GetBinContent(12)+h1->GetBinContent(13)+h1->GetBinContent(14)+h1->GetBinContent(15)+h1->GetBinContent(16)+h1->GetBinContent(17)+h1->GetBinContent(18)+h1->GetBinContent(19);
      cout<<"total pi="<<totpi<<" Real Pi0 integral="<<Realpi<<" backpi="<<backpi<<endl;
      //cout<<"check tot pi="<<ttotpi<<endl;
      fprintf(fout,"%d %6f %d %d %d\n",ploti,ptmean,totpi,Realpi,backpi);
      h2->Draw("same");
      f3->SetLineColor(6);
      f3->Draw("same");
      //h2->Draw("error");
      l1=new TLine(0.135,0.,0.135,7000);
      l1->SetLineColor(kRed);
      l1->Draw();
      l2=new TLine(0.18,0.,0.18,7000);
      l2->SetLineColor(kGreen);
      l2->Draw();
      l3=new TLine(0.1,0.,0.1,7000);
      l3->SetLineColor(kGreen);
      l3->Draw();
      //h2->SetStats(kFALSE);
      TString hMassAny="mass";
      hMassAny+=key;
      hMassAny+=ploti;
      c1->Print(hMassAny+".gif");
      
    }//end of mx
  if(key==5)
    {
      for(int j=0;j<40;j++)
	{
	  int ySame=channel_yield[0][j]+channel_yield[3][j];
	  int yAnti=channel_yield[1][j]+channel_yield[2][j];
	  float ypi=channel_pi_yield[0][j]+channel_pi_yield[1][j]+channel_pi_yield[2][j]+channel_pi_yield[3][j];
	  if(ypi<0.0) ypi=0.0;
	  float ybg=channel_bg_yield[0][j]+channel_bg_yield[1][j]+channel_bg_yield[2][j]+channel_bg_yield[3][j];
	  float ratio=0.0;
	  if(ybg==0)
	    {
	      ratio=0;
	    }
	  else{
	    ratio=ypi/ybg;
	  }
	  fprintf(fmass,"%d %d %d %f\n",j+1,ySame,yAnti,ratio);
	}
    }
  return;
  
}


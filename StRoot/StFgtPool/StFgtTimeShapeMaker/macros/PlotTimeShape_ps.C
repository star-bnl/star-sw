PlotTimeShape_ps(Int_t runnum=0)
{
  //macro to generate the time shape analysis plots from the TTree output of the StFgtTimeShapeMaker
  gStyle->SetPalette(1);
  gStyle->SetOptFit(1111);
  gStyle->SetCanvasColor(0);
  gStyle->SetPadColor(0);
  gStyle->SetFillColor(0);
  gStyle->SetStatBorderSize(0); 

  Int_t adcth=600;
  Int_t intth=1000;
  Float_t t0min=1.5;
  Float_t t0max=2.5;
  Float_t ch2th=2.;
  
  char tname[100];
  sprintf(tname,"run%d.*tree.root",runnum);

  TChain* tFgt=new TChain("tFgt");
  tFgt->Add(tname);

  Int_t iEvt;
  Int_t rdo;
  Int_t arm;
  Int_t apv;
  Int_t chn;
  Short_t disk;
  Short_t quad;
  Short_t strip;
  Short_t stat;
  Double_t ordinate;
  Double_t lowerSpan;
  Double_t upperSpan;
  Char_t layer;
  Double_t ped;
  Double_t pedSig;
  Int_t adc[7];
  Int_t adcmax;
  Int_t mmax;
  Int_t mmin;
  Float_t chi2;
  Float_t fmax;
  Float_t norm;
  Float_t tau;
  Float_t t0;
  Float_t offset;
  Int_t errCode;
  
  tFgt->SetBranchAddress("iEvt",&iEvt);
  tFgt->SetBranchAddress("rdo",&rdo);
  tFgt->SetBranchAddress("arm",&arm);
  tFgt->SetBranchAddress("apv",&apv);
  tFgt->SetBranchAddress("chn",&chn);
  tFgt->SetBranchAddress("disk",&disk);
  tFgt->SetBranchAddress("quad",&quad);
  tFgt->SetBranchAddress("strip",&strip);
  tFgt->SetBranchAddress("stat",&stat);
  tFgt->SetBranchAddress("ordinate",&ordinate);
  tFgt->SetBranchAddress("lowerSpan",&lowerSpan);
  tFgt->SetBranchAddress("upperSpan",&upperSpan);
  tFgt->SetBranchAddress("layer",&layer);
  tFgt->SetBranchAddress("adc",adc);
  tFgt->SetBranchAddress("ped",&ped);
  tFgt->SetBranchAddress("pedSig",&pedSig);
  tFgt->SetBranchAddress("adcmax",&adcmax);
  tFgt->SetBranchAddress("mmin",&mmin);
  tFgt->SetBranchAddress("mmax",&mmax);
  tFgt->SetBranchAddress("chi2",&chi2);
  tFgt->SetBranchAddress("fmax",&fmax);
  tFgt->SetBranchAddress("norm",&norm);
  tFgt->SetBranchAddress("tau",&tau);
  tFgt->SetBranchAddress("t0",&t0);
  tFgt->SetBranchAddress("offset",&offset);
  tFgt->SetBranchAddress("errCode",&errCode);
   
  TH2F htauapv("htauapv","tau (width) Parameter per APV;binAPV;tau (width)",240,0,240,100,0,5);
  TH2F ht0apv("ht0apv","t0 (rise point) Parameter per APV;binAPV;t0 (time offset)",240,0,240,120,-5,7);
  TH2F ht0apv2("ht0apv2","t0 (rise point) Parameter per APV;iapv;t0 (time offset)",288,0,288,120,-5,7);
  TH2F htmapv2("htmapv2","t-mean per APV;iapv;t-mean",288,0,288,120,-5,7);
  TH2F hoffapv("hoffapv","Y-offset per APV;binAPV;Y-offset",240,0,240,200,-1000,1000);

  TH2F hch2apv("hch2apv","Chi2/dof per APV;binAPV;chi2/dof",240,0,240,200,0,40);
  TH2F hfmxapv("hfmxapv","Time of Maximum per APV;binAPV;fmax (time of  maximum)",240,0,240,120,-2,10);
  TH2F hintapv("hintapv","Integral of Fit Funtion per APV;binAPV;Integral of Fit Function",240,0,240,200,0,20000);  

  TH2F hamxapv("hamxapv","ADCMAX per APV;binAPV;ADCMAX",240,0,240,400,0,4000);
  TH2F hpedapv("hpedapv","Pedestal per APV;binAPV;Pedestal",240,0,240,200,0,1000);  
  TH2F hsigapv("hsigapv","Pedestal Sigma per APV;binAPV;Pedestal Sigma",240,0,240,200,0,200);  
  TH2F herrapv("herrapv","Error Code Fraction per APV;binAPV;Error Code Type",240,0,240,5,0,5); 

  TH2F hmaxapv("hmaxapv","Max time bin per APV;binAPV;Max time bin",240,0,240,7,0,7); 
  TH2F hminapv("hminapv","Min time bin per APV;binAPV;Min time bin",240,0,240,7,0,7); 

  TH2F hxbquad("hxbquad","Quadrant diagnostic XBin;binQUAD;Xbin",24,0,24,5,0,5);
  TH2F hamxquad("hamxquad","Maximum ADC per Quadrant;binQUAD;adcmax",24,0,24,400,0,4000);
  TH2F hch2quad("hch2quad","Chi2/dof per Quadrant;binQUAD;chi2/dof",24,0,24,200,0,40);
  TH2F hfmxquad("hfmxquad","Time of Maximum per Quadrant;binQUAD;fmax (time of maximum)",24,0,24,120,-2,10);
  TH2F ht0quad("ht0quad","t0 (time offset) per Quadrant;binQUAD;t0 (time offset)",24,0,24,120,-5,7);
  TH2F hintquad("hintquad","Integral of Fit Funtion per Quadrant;binQUAD;integral",24,0,24,200,0,20000);  

  TString quadname[48];
  TString quadname2[24];

  for(Int_t di=0;di<6;di++)
    {
      for(Int_t qu=0;qu<4;qu++)
	{
	  char qc=65+qu;
	  quadname[2*(di*4 + qu)]+=di+1;
	  quadname[2*(di*4 + qu)]+=qc;
	  quadname[2*(di*4 + qu)]+='L';
	  quadname[2*(di*4 + qu)+1]+=di+1;
	  quadname[2*(di*4 + qu)+1]+=qc;
	  quadname[2*(di*4 + qu)+1]+='S';
	  quadname2[di*4 + qu]+=di+1;
	  quadname2[di*4 + qu]+=qc;
	}
    }

  htauapv.GetXaxis()->SetTitleOffset(1.2);
  ht0apv.GetXaxis()->SetTitleOffset(1.2);
  hoffapv.GetXaxis()->SetTitleOffset(1.2);

  hch2apv.GetXaxis()->SetTitleOffset(1.2);
  hfmxapv.GetXaxis()->SetTitleOffset(1.2);
  hintapv.GetXaxis()->SetTitleOffset(1.2);

  hamxapv.GetXaxis()->SetTitleOffset(1.2);
  hpedapv.GetXaxis()->SetTitleOffset(1.2);
  hsigapv.GetXaxis()->SetTitleOffset(1.2);
  herrapv.GetXaxis()->SetTitleOffset(1.2);

  hmaxapv.GetXaxis()->SetTitleOffset(1.2);
  hminapv.GetXaxis()->SetTitleOffset(1.2);

  htauapv.SetStats(0);
  ht0apv.SetStats(0);
  hoffapv.SetStats(0);

  hch2apv.SetStats(0);
  hfmxapv.SetStats(0);
  hintapv.SetStats(0);

  hamxapv.SetStats(0);
  hpedapv.SetStats(0);
  hsigapv.SetStats(0);
  herrapv.SetStats(0);

  hmaxapv.SetStats(0);
  hminapv.SetStats(0);

  Int_t bcnt=0;
  for(Int_t ib=0;ib<240;ib++)
    {
      if(ib%5==2)
	{
	  htauapv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);
	  ht0apv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);
	  hoffapv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);

	  hch2apv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);
	  hfmxapv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);
	  hintapv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);

	  hamxapv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);
	  hpedapv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);
	  hsigapv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);
	  herrapv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);

	  hmaxapv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);
	  hminapv.GetXaxis()->SetBinLabel(ib+1,quadname[bcnt]);
	  bcnt++;
	};
    };
  
  herrapv.GetYaxis()->SetBinLabel(1,"Good events");
  herrapv.GetYaxis()->SetBinLabel(2,"Oscillation");
  herrapv.GetYaxis()->SetBinLabel(3,"Saturation");

  hxbquad.GetXaxis()->SetTitleOffset(1.);
  hamxquad.GetXaxis()->SetTitleOffset(1.);
  hch2quad.GetXaxis()->SetTitleOffset(1.);
  hfmxquad.GetXaxis()->SetTitleOffset(1.);
  hintquad.GetXaxis()->SetTitleOffset(1.);
  hxbquad.SetStats(0);
  //hamxquad.SetStats(0);
  //hch2quad.SetStats(0);
  //hfmxquad.SetStats(0);
  //hintquad.SetStats(0);
  for(Int_t ib=0;ib<24;ib++)
    {
      hxbquad.GetXaxis()->SetBinLabel(ib+1,quadname2[ib]);	
      hamxquad.GetXaxis()->SetBinLabel(ib+1,quadname2[ib]);       
      hch2quad.GetXaxis()->SetBinLabel(ib+1,quadname2[ib]);	
      hfmxquad.GetXaxis()->SetBinLabel(ib+1,quadname2[ib]);	
      hintquad.GetXaxis()->SetBinLabel(ib+1,quadname2[ib]);       
    };
  
  c1=new TCanvas("c1","c1",850,1100);
  c1->SetGridx();
  c1->SetLogz();


  c10=new TCanvas("c10","c10",850,1100);
  c10->Divide(5,10);
  c11=new TCanvas("c11","c11",850,1100);
  c11->Divide(5,10);
  
  TH1F* h;
  char hname[100];
  char fname[100];
  Int_t nevents=tFgt->GetEntries();
  printf("nevents=%d \n",nevents);
  Int_t evtCnt[10]={0,0,0,0,0,0,0,0,0,0};
  Int_t jCnt=0;
  Int_t plotCnt=0;
  Int_t plotCnt2=0;
  sprintf(fname,"runpdfs/pulses_run%d.ps(",runnum);
  c10->Print(fname);
  sprintf(fname,"runpdfs/pulseschi2_run%d.ps(",runnum);
  c11->Print(fname);
  for(Int_t i=0;i<nevents;i++)
    {
      tFgt->GetEntry(i);
      if(iEvt<evtCnt[jCnt])
	{	  
	  jCnt++;
	};
      evtCnt[jCnt]=iEvt;
      if(i%10000==0)printf("evt=%d %d--------------------\n",iEvt,evtCnt[jCnt]);
      Int_t binAPV = disk*40 + quad*10 + (apv%12);
      Int_t binQUAD = disk*4 + quad;
      //printf("%d %d %d %d\n",binAPV,binQUAD,disk,quad);
      if(adcmax>adcth && errCode==0 && stat==0 && plotCnt<1000)
	{
	  sprintf(hname,"hpulse_rdo%d_arm%d_apv%d_chn%d_%d",rdo,arm,apv,chn,plotCnt);
	  h=new TH1F(hname,hname,7,0,7);
	  Float_t tmean=0;
	  Float_t wsum=0;
	  for(Int_t j=0;j<7;j++)
	    {
	      h->SetBinContent(j+1,adc[j]);
	      h->SetBinError(j+1,pedSig);
	      tmean+=adc[j]*j;
	      wsum+=adc[j];
	    };
	  tmean=tmean/wsum;
	  c10->cd(plotCnt%50+1);
	  h->Draw();
	  c10->Update();
	  if(plotCnt%50==49)
	    {
	      sprintf(fname,"runpdfs/pulses_run%d.ps",runnum);
	      c10->Print(fname);
	    };
	  plotCnt++;
	  if(chi2<ch2th)
	    {
	      c11->cd(plotCnt2%50+1);
	      h->Draw();
	      if(plotCnt2%50==49)
		{
		  sprintf(fname,"runpdfs/pulseschi2_run%d.ps",runnum);
		  c11->Print(fname);
		};
	      plotCnt2++;
	    };
	};
      if(stat==0)
	{
	  herrapv.Fill(binAPV,errCode);	  	  
	  if(errCode==0)
	    {
	      hamxquad.Fill(binQUAD,adcmax);
	      hamxapv.Fill(binAPV,adcmax);	  	  
	      hmaxapv.Fill(binAPV,mmax);	  	  
	      hminapv.Fill(binAPV,mmin);	  	  
	      hpedapv.Fill(binAPV,ped);	  	  
	      hsigapv.Fill(binAPV,pedSig);	  	  
	      if(adcmax>adcth && chi2>0.)
		{
		  hch2apv.Fill(binAPV,chi2);
		  hch2quad.Fill(binQUAD,chi2);
		  hxbquad.Fill(binQUAD,1);
		  if(chi2<ch2th)
		    {
		      Int_t irdo=rdo-1;
		      Int_t iarm=arm+6*irdo;
		      Int_t iapv=apv+24*iarm;
		      htauapv.Fill(binAPV,tau);
		      ht0apv.Fill(binAPV,t0);
		      ht0apv2.Fill(iapv,t0);
		      htmapv2.Fill(iapv,tmean);
		      hoffapv.Fill(binAPV,offset);
		      hfmxapv.Fill(binAPV,fmax);	  	  
		      TF1 fs("fs","[0]*(x-[4])**[2]*exp(-(x-[4])/[1])+[3]",-100,100);
		      fs.SetParameter(0,norm);
		      fs.SetParameter(1,tau);
		      fs.SetParameter(2,2.);
		      fs.SetParameter(3,offset);		      
		      fs.SetParameter(4,t0);		      
		      Float_t integral=fs.Integral(t0,t0+10);			  	
		      hintapv.Fill(binAPV,integral);		      

		      hfmxquad.Fill(binQUAD,fmax);
		      ht0quad.Fill(binQUAD,t0);
		      hxbquad.Fill(binQUAD,2);
		      if(t0>t0min && t0<t0max)
			{
			  hintquad.Fill(binQUAD,integral);
			  hxbquad.Fill(binQUAD,3);
			  if(integral>intth)
			    {
			      hxbquad.Fill(binQUAD,4);
			    };
			};
		    };
		};	      
	    };
	};

    };
  sprintf(fname,"runpdfs/pulses_run%d.ps)",runnum);
  c10->Print(fname);
  sprintf(fname,"runpdfs/pulseschi2_run%d.ps)",runnum);
  c11->Print(fname);

  Int_t totCnt=0;
  for(Int_t i=0;i<10;i++){printf("evtCnt = %d \n",evtCnt[i]);totCnt+=evtCnt[i];};

  for(Int_t i=0;i<24;i++)
    {
      hxbquad.SetBinContent(i+1,1,totCnt);
    };  

  for(Int_t i=0;i<240;i++)
    {
      Float_t Ntot=herrapv.Integral(i+1,i+1,1,5);
      for(Int_t k=0;k<5;k++)
	{
	  Float_t nn=herrapv.GetBinContent(i+1,k+1);
	  if(Ntot>0)herrapv.SetBinContent(i+1,k+1,nn/Ntot);
	};
    };
  

  c1->Clear();
  TPad *c=makeTitle(c1,"APV Fit Parameters",runnum,0); c->cd();  
  c->Divide(1,3);
  c->cd(1);
  c->GetPad(1)->SetLogz();
  c->GetPad(1)->SetGridx();
  htauapv.Draw("colz");
  c->cd(2);
  c->GetPad(2)->SetLogz();
  c->GetPad(2)->SetGridx();
  ht0apv.Draw("colz");
  c->cd(3);
  c->GetPad(3)->SetLogz();
  c->GetPad(3)->SetGridx();
  hoffapv.Draw("colz"); 
  sprintf(fname,"runpdfs/apv_run%d.ps(",runnum);
  c1->Print(fname);
  c1->Update();

  c1->Clear();
  TPad *c=makeTitle(c1,"APV Fit Results",runnum,1); c->cd();  
  c->Divide(1,3);
  c->cd(1);
  c->GetPad(1)->SetLogz();
  c->GetPad(1)->SetGridx();
  hch2apv.Draw("colz");
  c->cd(2);
  c->GetPad(2)->SetLogz();
  c->GetPad(2)->SetGridx();
  hfmxapv.Draw("colz"); 
  c->cd(3);
  c->GetPad(3)->SetLogz();
  c->GetPad(3)->SetGridx();
  hintapv.Draw("colz"); 
  sprintf(fname,"runpdfs/apv_run%d.ps",runnum);
  c1->Print(fname);
  c1->Update();

  c1->Clear();
  TPad *c=makeTitle(c1,"APV Diagnostics",runnum,1); c->cd();  
  c->Divide(1,3);
  c->cd(1);
  c->GetPad(1)->SetLogz();
  c->GetPad(1)->SetGridx();
  hamxapv.Draw("colz");
  c->cd(2);
  c->GetPad(2)->SetLogz();
  c->GetPad(2)->SetGridx();
  hpedapv.Draw("colz"); 
  c->cd(3);
  c->GetPad(3)->SetLogz();
  c->GetPad(3)->SetGridx();
  hsigapv.Draw("colz");
  sprintf(fname,"runpdfs/apv_run%d.ps",runnum);
  c1->Print(fname);

  c1->Clear();
  TPad *c=makeTitle(c1,"APV Diagnostics 2",runnum,1); c->cd();  
  c->Divide(1,3);
  c->cd(1);
  c->GetPad(1)->SetLogz();
  c->GetPad(1)->SetGridx();
  hmaxapv.Draw("colz");
  c->cd(2);
  c->GetPad(2)->SetLogz();
  c->GetPad(2)->SetGridx();
  hminapv.Draw("colz"); 
  c->cd(3);
  c->GetPad(3)->SetLogz();
  c->GetPad(3)->SetGridx();
  herrapv.Draw("colz"); 
  sprintf(fname,"runpdfs/apv_run%d.ps)",runnum);
  c1->Print(fname);


  c2=new TCanvas("c2","c2",850,1100);
  sprintf(fname,"runpdfs/quad_run%d.ps[",runnum);
  c2->Print(fname);
  TH1D* htemp;
  TLine* line;
  char label[100];
  for(Int_t i=0;i<24;i++)
    {
      TString pagetitle;
      pagetitle+="Quadrant ";
      pagetitle+=quadname2[i];
      TPad *c=makeTitle(c2,pagetitle,runnum,i+1); c->cd();  
      c->Divide(1,5); 

      c->cd(1);
      htemp=hxbquad.ProjectionY(quadname2[i],i+1,i+1);htemp->Draw();
      htemp->GetXaxis()->SetBinLabel(1,"Nevent");
      sprintf(label,"ADCMAX > %d",adcth);
      htemp->GetXaxis()->SetBinLabel(2,label);
      sprintf(label,"chi2/dof < %d",ch2th);
      htemp->GetXaxis()->SetBinLabel(3,label);
      sprintf(label,"%.1f < t0 < %.1f ",t0min,t0max);
      htemp->GetXaxis()->SetBinLabel(4,label);
      sprintf(label,"Integral > %d",intth);
      htemp->GetXaxis()->SetBinLabel(5,label);
      htemp->GetXaxis()->SetLabelSize(.1);
      if(htemp->GetEntries()>0)c->GetPad(1)->SetLogy();

      c->cd(2);      
      htemp=hamxquad.ProjectionY(quadname2[i]+=' ',i+1,i+1);htemp->Draw();
      if(htemp->GetEntries()>0)c->GetPad(2)->SetLogy();
      line = new TLine(adcth,0.,adcth,2.*htemp->GetMaximum());
      line->SetLineColor(2);
      line->Draw();

      c->cd(3);
      htemp=hch2quad.ProjectionY(quadname2[i]+=' ',i+1,i+1);htemp->Draw();
      //if(htemp->GetEntries()>0)c->GetPad(3)->SetLogy();
      line = new TLine(ch2th,0.,ch2th,2.*htemp->GetMaximum());
      line->SetLineColor(2);
      line->Draw();

      c->cd(4);
      htemp=ht0quad.ProjectionY(quadname2[i]+=' ',i+1,i+1);htemp->Draw();
      if(htemp->GetEntries()>0)c->GetPad(4)->SetLogy();
      line = new TLine(t0min,0.,t0min,2.*htemp->GetMaximum());
      line->SetLineColor(2);
      line->Draw();
      line = new TLine(t0max,0.,t0max,2.*htemp->GetMaximum());
      line->SetLineColor(2);
      line->Draw();

      c->cd(5);  
      htemp=hintquad.ProjectionY(quadname2[i]+=' ',i+1,i+1);htemp->Draw();
      if(htemp->GetEntries()>0)c->GetPad(5)->SetLogy();
      line = new TLine(intth,0.,intth,2.*htemp->GetMaximum());
      line->SetLineColor(2);
      line->Draw();

      sprintf(fname,"runpdfs/quad_run%d.ps",runnum);
      c2->Print(fname);
    }
  sprintf(fname,"runpdfs/quad_run%d.ps]",runnum);
  c2->Print(fname);

  char command[500];
  sprintf(command,"ps2pdf runpdfs/apv_run%d.ps runpdfs/pdfs/apv_run%d.pdf",runnum,runnum);
  system(command);
  sprintf(command,"ps2pdf runpdfs/quad_run%d.ps runpdfs/pdfs/quad_run%d.pdf",runnum,runnum);
  system(command);
  sprintf(command,"ps2pdf runpdfs/pulses_run%d.ps runpdfs/pdfs/pulses_run%d.pdf",runnum,runnum);
  system(command);
  sprintf(command,"ps2pdf runpdfs/pulseschi2_run%d.ps runpdfs/pdfs/pulseschi2_run%d.pdf",runnum,runnum);
  system(command);

  sprintf(fname,"hfgt_run%d.root",runnum);
  TFile out(fname,"recreate");
  htauapv.Write();
  ht0apv.Write();
  ht0apv2.Write();
  htmapv2.Write();
  hoffapv.Write();

  hch2apv.Write();
  hfmxapv.Write();
  hintapv.Write();

  hamxapv.Write();
  hpedapv.Write();
  hsigapv.Write();
  herrapv.Write();

  hmaxapv.Write();
  hminapv.Write();

  hxbquad.Write();
  hamxquad.Write();
  hch2quad.Write();
  hfmxquad.Write();
  ht0quad.Write();
  hintquad.Write();

}

TPad *makeTitle(TCanvas *c,TString core, int runnum, int page) {  
  c->Clear();
  c->Range(0,0,1,1);
  char pname[100];
  sprintf(pname,"pad0_%d",page);
  TPad *pad0 = new TPad(pname,pname,0.0,0.96,1.,1.);
  pad0->Draw();
  pad0->cd();

  TPaveText *pt = new TPaveText(0,0.,1,1,"br");
  pt->Draw();
  TDatime dt;
  TString txt2=core;
  txt2+=",  ";
  if(runnum>0)
    {
      txt2+="run";
      txt2+=runnum;
      txt2+=",  ";
    };
  txt2+=dt.AsString();
  pt->AddText(txt2);
  txt2=" -- ";
  pt->AddText(txt2);  
  pt->Draw();

  c->cd();
  char pname[100];
  sprintf(pname,"pad1_%d",page);
  pad = new TPad(pname, pname,0.0,0.0,1,.95);
  pad->Draw();  
  return pad;
}

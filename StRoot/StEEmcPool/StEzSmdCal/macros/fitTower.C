TCanvas *c;
double par[6];  
double *epar;  
TFile *fdA[12];
FILE *wfd=stdout; // output web page
FILE *gfd;


//========================
void openAll(char cT) {
  int i;
  char txt[200];
  for(i=0;i<12;i++) {
    sprintf(txt,"iter5-pp/sect%02d/sum-sect%d.hist.root",i+1,i+1);
    fdA[i]=new TFile(txt);
    assert(fdA[i]->IsOpen());
  }
  sprintf(txt,"/star/u/wissink/cal2006/iter5-pp/gains%c-allSect.dat",cT); 
  gfd=fopen(txt,"w");
  assert(gfd);
  fprintf(gfd,"# final %c-gains from MIP using UxV\n",cT);
  fprintf(gfd,"# format: gain (ch/GeV) & errGain, GausLandauFit: MPV(ADC) & errMPV, pedFit: means(ADC) & err \n");

  
  sprintf(txt,"/star/u/wissink/cal2006/iter5-pp/gains%c.html",cT);
  
  wfd=fopen(txt,"w");
  assert(wfd);
  
  fprintf(wfd,"MIP -- > <a href=\"gains%c-allSect.dat\"> final %c-gains</a> table \n",cT,cT);
  fprintf(wfd,"<table border=1>\n");
  fprintf(wfd,"<tr> <th> etaBin <th> problems <th> # of good<th> # of errors <br> +warn <th> spectra <br> (60 tiles)<th> MPV range <br>(ADC) <th> summary <br> plot\n");
  
  
}


//======================================
fitTower() {
  gStyle->SetStatW(0.22);
  gStyle->SetStatH(0.22);
  
  // f=new TFile("/star/data05/scratch/balewski/2005-eemcCal/day49-hist/iter2-out/sum-sect5.hist.root");

  TH1F *h1=new TH1F("mpv","MPV gated w/ MIP ; MPV of ADC-ped",40,-5,35); 
  TH1F *h2=new TH1F("mpvE","relative error of MPV , MIP gated; err(MPV)/MPV ",50,0,0.3); 


 // stupid root tricks:
 hDum=new TH1F("aa","bb",10,1,9);
 hDum->Fill(5);

 
  char cT='T';
  openAll(cT);
  
  int sec=5;
  char core[100];
  
  int eta;
  char sub='C';
  
  for(eta=1;eta<=12;eta++) {
    int nErr=0, nOK=0;
    float mpvL=999, mpvH=0; 
    h1->Reset(); h2->Reset(); 
    fprintf(wfd," <tr> <th> %d <td> \n",eta); 
    gStyle->SetOptStat(1001111);

    for(sec=1; sec<=12;sec++) {
      TFile *f=fdA[sec-1];
      for(sub='A';sub<='E';sub++)     {
	sprintf(core,"%02d%c%c%02d",sec,cT,sub,eta);
	TString coreT=core;
	ha=(TH1F*)f->Get("a"+coreT);
	hd=(TH1F*)f->Get("d"+coreT);

	// special case of stuck low bits - sww - modified to include 06TA07
	if( (eta==7 && sub=='B' && (sec==4 || sec==8)) || (eta==7 && sub=='A' && sec==6)) {
	  ha->Rebin(4);
	  hd->Rebin(4);
	  printf("tower=%s rebinned\n",core);
	}

	c=new TCanvas("aa","aa",400,400);
	c->Divide(1,2);  c->cd(1);
	hDum->Draw();  gPad->SetLogy();
	float mpv, mpvEr;
	TString errS=plotOne(ha,hd, mpv, mpvEr);
	printf("errS=%s=\n",errS.Data());

	bool isBad=errS.Sizeof()>1;
	if(isBad) { // report error channel
	  fprintf(wfd,"     %s ,\n",(coreT+"-"+errS).Data());
	  nErr++;
	}
	if(errS.Contains("mask")) continue;
	nOK++;
       	//return;
  	
	if(mpvL>mpv) mpvL=mpv;
	if(mpvH<mpv) mpvH=mpv;

	h1->Fill(mpv);
	if(mpv>0) {
	  h2->Fill(mpvEr/mpv);
	}
	
      } // end of eta bin
      
    }// end of sector loop
    fprintf(wfd," <td> %d <td> %d\n",nOK,nErr);

    char txt[100],sumN[100], pdfN[100];
    sprintf(pdfN,"%cfitEta%02d.pdf",cT,eta);
    sprintf(txt,"cat *%02d.ps | ps2pdf - %s",eta,pdfN);
    printf("%s\n",txt);
    system(txt);
    sprintf(txt,"mv %s /star/u/wissink/cal2006/tmp/",pdfN);
    printf("%s\n",txt);
    system(txt);
    fprintf(wfd,"     <td> <a href=\"%s\"> PDF </a>\n",pdfN);    
    fprintf(wfd,"     <td> %.1f  to %.1f \n",mpvL,mpvH);
    
    gStyle->SetOptStat(1111111);

    sprintf(sumN,"mpv%c-eta%02d",cT,eta);
    c=new TCanvas(sumN,sumN,600,600);
    c->Divide(1,3);
    c->cd(1); h1->Draw();
    c->cd(2); h2->Draw();
    c->Print();
    
    sprintf(txt,"ps2pdf %s.ps %s.pdf",sumN,sumN);
    printf("%s\n",txt);
    system(txt);
    sprintf(txt,"mv %s.pdf /star/u/wissink/cal2006/tmp/",sumN);
    printf("%s\n",txt);
    system(txt);
    fprintf(wfd,"     <td> <a href=\"%s.pdf\"> PDF </a>\n",sumN);
      
    fflush(wfd);
  }// end of loop over eta bins
  
  if(wfd!=stdout)fclose(wfd);
  fclose(gfd);
  return;
}

//=================================
TString plotOne(TH1F *ha, TH1F *hd, float &MPV, float &MPVerr) {
  assert(ha);
  assert(hd);
  const float feta[]=
    {1.95,1.855,1.765,1.675,1.59,1.51,1.435,1.365,1.3,1.235,1.17,1.115};
  
  char *core =ha->GetName()+1;
  TString coreT=core;
  float xMax=120;
  ha->SetAxisRange(-10,xMax); 
  hd->SetAxisRange(-10,xMax/2.);
  
  
  int maxbina=ha->GetMaximumBin();
  float xcenta=ha->GetBinCenter(maxbina);
  printf("\n============================\n working on %s\n",ha->GetName());
  
  c->cd(1);
  ha->SetLineColor(4);
  ha->SetMinimum(0.9);
  char *func="gaus";
  ha->Fit(func,"RQI","",xcenta-5,xcenta+5);
  gPad->SetLogy();
  TF1* gausa=ha->GetFunction(func);
  gausa->SetLineWidth(1);
  gausa->SetLineColor(2);
  float meanA=gausa->GetParameter(1);
  float errorA=gausa->GetParError(1);
  
  hx=hd->Clone(); // memory leak
  hx->Draw("same");

  c->cd(2);
  
  TF1 *f1 = new TF1("myfunc",myfunction,-10,100,5);
  f1->SetParNames("x0","aL","aG","sigL","sigG");
  f1->SetLineColor(kRed);
  f1->SetLineWidth(1);
  hd->Fit("gaus","RQ","",0,30);
  TF1 *ff=hd->GetFunction("gaus");
  f1->SetParameter(0,ff->GetParameter(1));
  f1->SetParameter(1,ff->GetParameter(0));
  f1->FixParameter(2,0);

  fitGausLand(hd); // do fitting
  c->Print(coreT+".ps"); // draw before QA on results changing axis limits

  //....... retrieve components
  f1->GetParameters(par);
  epar= f1->GetParErrors();
  
  ha->SetAxisRange(-3,3);
  // printf("RR=%f\n",ha->Integral()/ha->GetEntries());
  if(ha->Integral()/ha->GetEntries()>0.999) return "mask";

  float meanD=par[0];
  float errorD=epar[0]; 
  //printf("ss=%s=\n",ha->GetName()+5);
  int ieta=atoi(core+4) -1;
  // printf("ssieta=%d\n",ieta);
  
  MPV=(meanD-meanA);
  MPVerr=sqrt(errorA*errorA+errorD*errorD+0.09);
  float gain=2.89*TMath::TanH(feta[ieta])*MPV;
  float sig=2.89*TMath::TanH(feta[ieta])*MPVerr;
  fprintf(gfd,"%s %.2f %.2f    %5.1f %5.2f    %5.2f %5.2f\n",core,gain,sig,meanD,errorD,meanA,errorA);
  
  if(fabs(meanA)>1.) return "pedOff";
  if(ha->GetEntries() <=5) return "noPed";
  ha->SetAxisRange(-3.,3.);
  if(ha->Integral()<0.9*ha->GetEntries()) return "Multped";

  if(hd->GetEntries() <=5) return "noMip";
 
  // deviation of gain from the goal
  TListIter it(hd->GetListOfFunctions()); 
  TLine *w=(TLine*)it.Next() ;
  w->Print();
  float del=MPV-w->GetX1();
  printf(" %s MPV=%f, del=%f goal=%f\n",hd->GetName(),MPV,del,w->GetX1());
  float eps=0.30;
  if(MPV<(1-eps)*w->GetX1())  return "lowG";
  if(MPV>(1+eps)*w->GetX1())  return "highG";

  if(par[1]<0 || par[2]<0)  return "amplNeg";
  if(gain<10.)  return "gainNeg";
 
  if(fabs(epar[0])<0.05)  return "singF"; // single bin pathology
  if(par[4]/par[0]>0.60)  return "wideG";
  hd->SetAxisRange(3,50);
  float sum=hd->Integral();
  if(sum<20)  return "lowS"; // was 150 for CuCu


   return "";
}



//---------------------------------------
Double_t myfunction(Double_t *x, Double_t *par)
{
  Float_t xx =x[0];
  float mpv=par[0];
  float aL=par[1]*fabs(par[3]);
  float aG=par[1];
  float sigL=2*par[3];
  float sigG=par[4];
  Double_t fland = TMath::Landau(xx,mpv,sigL);
  if(sigL!=0) fland/=sigL;
  Double_t fgaus = TMath::Gaus(xx,mpv,sigG);
  double f=aL*fland + aG*fgaus;
  return f;
}
//
float fitGausLand(TH1F *h) {
  char *funcd="myfunc";
  int maxbind=h->GetMaximumBin();
  float sig=h->GetRMS();
  float sum=h->Integral();
  float xcentd=h->GetBinCenter(maxbind);
  float eta=atoi(h->GetName()+5);
  //printf("eta=%f sum=%f\n",eta,sum);
  TF1 *f1=(TF1 *)gROOT->GetFunction(funcd);
  f1->SetParameters(xcentd,0,0,sig,sig);
  float x1=xcentd-10;
  float x2=xcentd*2.;
  if(x1<0) x1=0;
  if(x2<xcentd) x2=xcentd+10; 

  h->Fit(funcd,"R","",x1,x2);
  //h->Fit(funcd,"R","",20,70);

}

TCanvas *c;
double par[6];  
double *epar;  
TFile *fdA[12];
FILE *wfd=stdout; // output web page
FILE *gfd;

//======================================
fitPrePost() {
  gStyle->SetStatW(0.22);
  gStyle->SetStatH(0.22);
  
  //  f=new TFile("iter4-pp/sect05/sum-sect5.hist.root");

  TH1F *h1=new TH1F("mpv","MPV  MIP gated; MPV of ADC-ped",35,-5,65); 
  TH1F *h2=new TH1F("mpvE","relative error of MPV , MIP gated; err(MPV)/MPV ",50,0,0.5); 
  TH1F *h3=new TH1F("mpvS","relative width of L-peak,  MIP gated; sigma/MPV",25,0.,1.); 
 TH2F *h4=new TH2F("mpv2","MPV from ; gated w/ MIP ; inclusive spectrum;",25,0,50,25,0,25); 

//===sww===Set layer to analyze in next line
  char cT='R';
  openAll(cT);
  
  const float feta[]=
    {1.95,1.855,1.765,1.675,1.59,1.51,1.435,1.365,1.3,1.235,1.17,1.115};
  
  int sec=5;
  char core[100];
  
  int eta;
  char sub='C';
  
//===sww===Set eta range and sector range in next two "for" statements
  for(eta=1;eta<=12;eta++) {
    int nErr=0, nOK=0;
    float mpvL=999, mpvH=0; 
    h1->Reset(); h2->Reset(); h3->Reset(); h4->Reset(); 
    fprintf(wfd," <tr> <th> %d <td> \n",eta); 
    gStyle->SetOptStat(1001111);

    for(sec=1; sec<=12;sec++) {
      TFile *f=fdA[sec-1];
      for(sub='A';sub<='E';sub++)     {
	sprintf(core,"%02d%c%c%02d",sec,cT,sub,eta);
	TString coreT=core;
	ha=(TH1F*)f->Get("a"+coreT);
	hd=(TH1F*)f->Get("d"+coreT);
	c=new TCanvas("aa","aa",400,400);
	plotOne(ha,hd);
	//return;
	c->Print(coreT+".ps");
	TString errS=QaOne(ha,hd,cT);
	printf("errS=%s=%d\n",errS.Data(),errS.Sizeof());
	bool isBad=errS.Sizeof()>1;
	if(isBad) { // report error channel
	  fprintf(wfd,"     %s ,\n",(coreT+"-"+errS).Data());
	  nErr++;
	  //	continue;
	}
	if(errS.Contains("mask")) continue;
	nOK++;
	//    return;
	//  
	//  c->Print(coreT+".gif");
  	float mpv=par[4];
	float mpvEr=epar[4];
	if(mpvL>mpv) mpvL=mpv;
	if(mpvH<mpv) mpvH=mpv;
	
        int ieta=eta-1; 

        float fac=TMath::TanH(feta[ieta])/0.0009; // assumed 0.9 MeV per plastic
        float err=sqrt(mpvEr*mpvEr+1);
        float gain=mpv*fac; 
        float sig=err*fac;
        fprintf(gfd,"%s %.0f %.0f %.1f %.1f \n",core,gain,sig,mpv,mpvEr);
      
	h1->Fill(mpv);
	if(mpv>0) {
	  h2->Fill(mpvEr/mpv);
	  h3->Fill(par[5]/mpv);
	}
	//  h4->Fill(mpv,mpvInc);
	
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
    fprintf(wfd,"     <td> %.1f to %.1f \n",mpvL,mpvH);
    
    gStyle->SetOptStat(1111111);

    sprintf(sumN,"mpv%c-eta%02d",cT,eta);
    c=new TCanvas(sumN,sumN,600,600);
    c->Divide(1,3);
    c->cd(1); h1->Draw();
    c->cd(2); h2->Draw();
    c->cd(3); h3->Draw();
    // c->cd(4); h4->Draw("box");
    c->Print();
    
    sprintf(txt,"ps2pdf %s.ps %s.pdf",sumN,sumN);
    printf("%s\n",txt);
    system(txt);
    sprintf(txt,"mv %s.pdf /star/u/wissink/cal2006/tmp/",sumN);
    printf("%s\n",txt);
    system(txt);
    fprintf(wfd,"     <td> <a href=\"%s.pdf\"> PDF </a>\n",sumN);
      
    
}// end of loop over eta bins
  
  if(wfd!=stdout)fclose(wfd);
  fclose(gfd);
}

//=================================
plotOne(TH1F *ha, TH1F *hd) {
  assert(ha);
  assert(hd);

  float xMax=220;

  hd->SetAxisRange(4,80);
  float sum=hd->Integral();
  float hdY=hd->GetMaximum();
  printf("%s sum=%f\n",hd->GetName(),sum);
  if(sum>210) 
    hd->Rebin(2);
  else 
    hd->Rebin(4);

  //exceptions:
  if(strstr(hd->GetName(),"d12PB05")) hd->Rebin(2);
  if(strstr(hd->GetName(),"d12RC05")) hd->Rebin(2);

  ha->SetAxisRange(5,50);
  int kb=ha->GetMaximumBin();
  float yMax=ha->GetBinContent(kb);
  ha->SetAxisRange(-10,xMax); 
  hd->SetAxisRange(-10,xMax);
  
  c->Divide(1,2);

  c->cd(1);
  ha->Draw();
  //gPad->SetGrid();
  ha->Fit("gaus","R","",-2,3);
  
 
  ha->SetMaximum(yMax*1.5);
 

  c->cd(2);
  hd->SetMaximum(2.5*hdY);
  fitGausPlusLand(hd);
  // gPad->SetGrid();

}


//---------------------------------------
float fitGausPlusLand(TH1F *h) {

  float xMax=160.;

  gF=new TF1("gF","gaus",-2.,2.);
  gF->SetLineWidth(2);
  gF->SetLineColor(kBlue);
  gF->SetLineStyle(2);

  lF=new TF1("lF","landau",3,xMax);
  lF->SetLineWidth(2);
  lF->SetLineStyle(2);
  lF->SetLineColor(kRed);

  glF=new TF1("glF","gaus(0)+landau(3)",-2,xMax);
  glF->SetLineWidth(1);
  glF->SetLineColor(kGreen);
  glF->SetParNames("ampl-G","mean-G","sig-G","const-L","MPV-L","sig-L");


  h->Fit("gF","R");
  // return 1;
  h->Fit("lF","R0Q+");
  
  
  // copy starting point
  gF->GetParameters(par+0);
  lF->GetParameters(par+3);//return 1;
  glF->SetParameters(par);
  //freez position & width of pedestal residuum
  glF->FixParameter(1,par[1]);
  glF->FixParameter(2,par[2]);


  h->Fit("glF","R");

  // retrieve components
  glF->GetParameters(par);
  epar= glF->GetParErrors();

  gF->SetParameters(par+0);
  lF->SetParameters(par+3);

  gF->SetRange(-5,5);
  gF->Draw("same");

  lF->SetRange(0,xMax);
  lF->Draw("same");

  float yMax=lF->GetMaximum();
  printf("max=%f\n",yMax);
  return yMax;
}

//=================================
TString QaOne(TH1F *ha, TH1F *hd,char cT ) {
  assert(ha);
  assert(hd);

  //............. pedestal
  ha->SetAxisRange(-10,10);
  int kb=ha->GetMaximumBin();
  float xb=ha->GetBinCenter(kb);
  //  printf("ped xAdc=%f\n",xb);
  if(fabs(xb)>1.) return "ped";
  if(ha->GetEntries() <=0) return "empty";
  // change limit to 0.9999 for R analysis - sww
  if(ha->Integral()/ha->GetEntries()>0.9999) return "mask";


  //.............Landau 
  //printf("xxx=%f\n",epar[4]/par[4]);

  switch (cT) {
  case 'R': // post shower
    if(par[4]<6. || par[4]>30.)  return "valL";
    break;
  default:
    if(par[4]<10. || par[4]>75.)  return "valL";
  }

  if(fabs(epar[4])<0.35)  return "erL"; // single bin pathology
  if(epar[4]/par[4]>0.5)  return "sigL";
  
  hd->SetAxisRange(5,50);
  float sum=hd->Integral();
  if(sum<20)  return "lSum";
  //  if(sum>500)  return "hSum";
  return "";
}



//========================
void openAll(char cT) {
  int i;
  char txt[200];
  for(i=0;i<12;i++) {
    //  sprintf(txt,"/star/data05/scratch/balewski/2005-eemcCal/day171-hist/iter4-outA/sum-sect%d.hist.root",i+1);
    // sprintf(txt,"./iter12-mc/sum-sect%d.hist.root",i+1);
    // sprintf(txt,"/star/data05/scratch/balewski/2005-eemcCal/mc-hist/iter8-mc/sum-sect%d.hist.root",i+1);
    if(i<9) sprintf(txt,"iter4-pp/sect0%d/sum-sect%d.hist.root",i+1,i+1);
    if(i>8) sprintf(txt,"iter4-pp/sect%d/sum-sect%d.hist.root",i+1,i+1);
    fdA[i]=new TFile(txt);
    assert(fdA[i]->IsOpen());
  }
  sprintf(txt,"/star/u/wissink/cal2006/iter4-pp/gains%c-allSect.dat",cT); 
  gfd=fopen(txt,"w");
  assert(gfd);
  fprintf(gfd,"# final %c-gains from MIP using UxV\n",cT);
  fprintf(gfd,"# format: gain (ch/GeV), errGain, LandauFit: MPV(ADC) sigMPV\n");

  
  sprintf(txt,"/star/u/wissink/cal2006/iter4-pp/gains%c.html",cT);
  
  wfd=fopen(txt,"w");
  assert(wfd);
  
  fprintf(wfd,"MIP -- > <a href=\"gains%c-allSect.dat\"> final %c-gains</a> table \n",cT,cT);
  fprintf(wfd,"<table border=1>\n");
  fprintf(wfd,"<tr> <th> etaBin <th> problems <th> # of good<th> # of errors <th> spectra <br> (60 tiles)<th> MPV range <br>(ADC) <th> summary <br> plot\n");
  
  
}

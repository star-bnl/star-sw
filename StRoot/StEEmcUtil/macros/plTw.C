plTw( int pid=14, char *name="deT+S") {   //type of particles, histo name

  const int ng=4;
  TGraphErrors *hg[ng];
  int nH=InitGraph(hg,ng);
  printf("%d graphs initialized\n",nH);
  int i;
  for(i=0;i<ng;i++) printf("i=%d, add=%0x\n",i,hg[i]);
  
  gStyle->SetOptStat(111111);
  gStyle->SetOptFit(111111);
  
  //pt bins
  float pt[]={0.5,1,2,3,4,5,10,20,30,40,50}; int npt=11;
  //float pt[]={0.5,1,10,50}; int npt=4;
  
  //eta bins 
  float eta[]={1.2,1.75};
  int neta=2;

  int i=0,k=0;
  
  for(i=0;i<npt;i++) 
        for(k=0;k<neta;k++)
      {
	float Etot=pt[i]*cosh(eta[k]); // approximate, mass ignored
	char fname[1000];
	sprintf(fname,"/star/data22/MC/balewski/eemc/sim2002/f/mc_pid%d_pt%.1f_eta%4.2f",pid,pt[i],eta[k]);
	//  printf( "file%s Etot=%f  eta=%f\n",fname,Etot,eta[k]);// continue;
	// E=pT/sin(theta)
	doJob(fname,hg,Etot,k,pid,name);
	//	if(i>1) break;
	//	return;
      }
  
  //hg[0]->Print();
  // hg[2]->Print();
  //    return;
  char tit[100];
  sprintf(tit,"%s-pid%d",name,pid);
  
  int kkk=0;
  if (pid<4) kkk=1;
  TCanvas *can = new TCanvas(tit,tit,600,400+400*kkk);
  if(kkk)  { can->Divide(1,2); can->cd(1);}
   hgDrawE(hg,pid,name);
   
  if(kkk)  { can->cd(2);
  hgDrawSE(hg+2,pid,name);
  }
  can->Print();
  
}

//--------------------------------------------
//--------------------------------------------
//--------------------------------------------

void doJob(char *fname0,TGraphErrors **hg, float Etot, int ieta,int pid,char *hname) {
  printf("job--> %s Etot=%f  ieta=%d hist=%s\n",fname0,Etot,ieta,hname);
  assert(ieta>=0 && ieta<=1);

  TString fname=fname0;
  TFile *dir=new TFile(fname+".hist.root");
  // dir->ls();
  
  if(!dir->IsOpen()){ printf("Open failed, take next\n"); return;}
  
  TH1F* h1=0;
  double x,y,ey;
  int n=-1;
   
  h1=(TH1F*) dir->Get(hname);  
  assert(h1);
  
  TF1 *ff=h1->GetFunction("gaus");  assert(ff);

  // total energy  ........................
  x=Etot;
  
  if(pid<4) { // use relative energy for gam & ele
    y=ff->GetParameter(1)/Etot;
    ey=ff->GetParError(1)/Etot;
  }
  
  if(pid>4) { // use relative MEAN & RMS for others
    y=h1->GetMean();
    ey=h1->GetRMS();
  }
  
  
  TGraphErrors *gr=hg[0+ieta];
  n=gr->GetN();
  gr->SetPoint(n,x,y);
  gr->SetPointError(n,0.,ey);
  
    if(pid>4) return;

  // energy resolution ........................
  x=1/sqrt(Etot);
  y=ff->GetParameter(2)/Etot;
  ey=ff->GetParError(2)/Etot;
  printf("Etot=%f x=%f y=%f ey=%g\n",Etot,x,y);

  TGraphErrors *gr=hg[2+ieta];
  n=gr->GetN();
  gr->SetPoint(n,x,y);
  gr->SetPointError(n,0.,ey);
  
}


//------------------------------------------------
//------------------------------------------------
//------------------------------------------------

int InitGraph( TGraphErrors **hg,int ng){
  char *name[]={"reco E or E/E, eta=1.20"
		,"reco E or E/E, eta=1.75"
		,"reco sigE , eta=1.20"
		,"reco sigE , eta=1.75"
  }
  
  int j;
  
  {int j; for(j=0;j<ng;j++) hg[j]=0;  }

  int kk=0;
  for(j=0;j<ng;j++) {
    int i=j; // tmp
    TGraphErrors *gr=new TGraphErrors();
    hg[kk]=gr;
    gr->SetMarkerStyle(28);
    gr->SetMarkerColor(kGreen);
    gr->SetName(name[i]);
    int icol=kBlue, isym=28;
    if(j%2==0 )isym=25;
    if(j%2==1 )isym=24;

    if(j%2==0) icol=kRed;
    if(j%2==1) icol=kGreen;
 
    gr->SetMarkerStyle(isym);
    gr->SetMarkerColor(icol);
    gr->SetLineColor(icol);
    kk++;
  }
  printf("Initialized %d Graphs\n",kk);
  return kk;
}


//------------------------------------------------
//------------------------------------------------
//------------------------------------------------


hgDrawE(TGraphErrors **hg, int pid, char *name) {   //type of particles, histo name
  char *pname[]={"gamma","xx","electron","xx","xx","muon","xx","xx","pion-","xx","xx","xx","xx","proton"};
  
  TGraphErrors *gr=0;
  gr=hg[0];
  gr->Draw("AP");
  TString titHead;

  if(pid>4 && strstr(name,"deT+S")){
    titHead="mean #pm RMS E_{TOWER+SMD} (GeV)";
    gr->SetMinimum( -0.2);
 }
  if(pid>4 && strstr(name,"deTw")){
    titHead="mean #pm RMS E_{TOWER only} (GeV)";
    gr->SetMinimum( -0.2);
  }

  if(pid<4 && strstr(name,"deT+S")) {
    gr->SetMaximum( 0.054);  gr->SetMinimum( 0.048);
    titHead="Gauss E_{REC}/E_{INC}  #pm #sigma/E_{INC}  for  TOWER+SMD ";
  }
  if(pid<4 && strstr(name,"deTw")) {
    gr->SetMaximum( 0.042);  gr->SetMinimum( 0.037);
    titHead="Gauss E_{REC}/E_{INC}  #pm #sigma/E_{INC}  for  TOWER only ";
  }

  gPad->SetLogx();
  gr->GetXaxis()->SetLimits(0.7,170.);
  gr->GetXaxis()->SetTitle("Incident Energy (GeV)");
  
  char tit[1000];
  sprintf(tit," %s;    PID=%d\n",titHead.Data(),pid);
  gr->SetTitle(tit);
  hg[1]->Draw("P");

  lg=new TLegend(0.3,0.65,0.55,0.8);
  TString head="Incident    ";
  head+=pname[pid-1];
  lg->SetHeader(head);
  lg->AddEntry(hg[0],"  #eta=1.20","LP");
  lg->AddEntry(hg[1],"  #eta=1.75","LP");
  lg->Draw();


  
}




hgDrawSE(TGraphErrors **hg, int pid, char *name) {   //type of particles
  char *pname[]={"gamma","xx","electron","xx","xx","muon","xx","xx","pion-","xx","xx","xx","xx","proton"};
  
  TGraphErrors *gr=0;
  gr=hg[0];
  gr->Draw("AP");
  gr->Fit("pol1");

  TString titHead;
  if(strstr(name,"deT+S")) titHead="  #sigma/ E_{INC} for TOWER+SMD";
  if(strstr(name,"deTw")) titHead="  #sigma/ E_{INC} for TOWER only";

  gr->SetMinimum( 0.0);

  if(pid>4){
    gr->SetMaximum( 0.004); 
  }

  if(pid<4){
    gr->SetMaximum( 0.01); 
  }

  gr->GetXaxis()->SetTitle("1 /  #sqrt{E_{INC}/GeV}");
  
  char tit[1000];
  sprintf(tit,"Gauss %s;    PID=%d\n",titHead.Data(),pid);
  gr->SetTitle(tit);


  hg[1]->Draw("P");

  lg=new TLegend(0.6,0.15,0.85,0.3);
  TString head="Incident    ";
  head+=pname[pid-1];
  lg->SetHeader(head);
  lg->AddEntry(hg[0],"  #eta=1.20 +FIT","LP");
  lg->AddEntry(hg[1],"  #eta=1.75","LP");
  lg->Draw();


  
}




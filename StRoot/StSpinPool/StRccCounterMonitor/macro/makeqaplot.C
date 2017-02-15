TCanvas *c1;
static TFile* file;
static int runnum, yearday, png, pdf;

static const int NCRATE=14;
static const char* CRATE[NCRATE]={"L1","BC1","MXQ","MIX","BCW","BCE","FEQ",
				  "BBC","BBQ","FMS","QT1","QT2","QT3","QT4"};
static const int color[NCRATE]=  {kGreen+2,kBlue,kRed,kBlue,kBlue,kBlue,kOrange-3,
                                  kBlue,kRed,kBlue,kMagenta,kMagenta,kMagenta,kMagenta};

float mMean[NCRATE];
float mRms[NCRATE];
float mMax[NCRATE];
static float MAX=65536*3.0;

void makeqaplot(int run=0, float max=MAX, int plt=0, float min=0){
  if(max==0) max=MAX;
  runnum=run;
  yearday=run/1000;

  c1 = new TCanvas("c1","RCC Counter",50,0,1200,900);
  gStyle->SetPalette(1);
  gStyle->SetStatW(0.4);
  gStyle->SetStatH(0.3);
  gStyle->SetTitleH(0.2);

  char fname[100];
  if(run==0) {sprintf(fname,"rccqa.root");}
  else {sprintf(fname,"%d/rccqa.%d.root",yearday,run);}

  cout << "Opening "<<fname<<endl;
  file=new TFile(fname,"old");

  if(plt==0 || plt==1){
    gStyle->SetOptStat(111110);
    gStyle->SetOptTitle(1);
    c1->Clear();  
    c1->Divide(2,7);     
    TH1F *h;
    for(int i=0; i<NCRATE; i++){
      c1->cd(i+1)->SetLogy(); 
      h=(TH1F*)file->Get(CRATE[i]); 
      mMean[i]=h->GetMean();
      mRms[i]=h->GetRMS();
      mMax[i]=h->GetXaxis()->GetBinCenter(h->FindLastBinAbove(0.0,1));
      if(mMax[i]>=65536) {h->SetFillColor(kRed);}
      else               {h->SetFillColor(kBlue);}
      h->SetLabelSize(0.1);
      if(max<MAX) {
	h->GetXaxis()->SetRangeUser(0, max);
      }
      h->Draw();
      cout << Form("%3s Mean=%9.1f RMS=%9.1f MaxDiff=%9.1f",CRATE[i],mMean[i],mRms[i],mMax[i])<<endl;
    }
    
    if(yearday==0){
      sprintf(fname,"%d.diff.png",runnum);    
    }else{
      if(max==MAX){
	sprintf(fname,"%d/%d.diff.png",yearday,runnum);    
      }else{
	sprintf(fname,"%d/%d.diff.zoom.png",yearday,runnum);    
      }
    }
    c1->SaveAs(fname);
    
    if(max==MAX){
      if(run==0) {sprintf(fname,"rcc.txt");}
      else {sprintf(fname,"%d/rcc.%d.txt",yearday,run);}
      printf("Writing %s\n",fname);
      FILE* txfile = fopen(fname,"w");
      if(!txfile){
	printf("Could not open file\n");
      }else{
	for(int i=0; i<NCRATE; i++){
	  fprintf(txfile,"%12.1f %12.1f %12.1f\n",
		  mMean[i],mRms[i],mMax[i]);
	}
	fclose(txfile);
      }    
    }
  }
  
  if(plt==0 || plt==2){
    c1->Clear();
    gStyle->SetTitleH(0.05);    
    gStyle->SetOptStat(0);    
    TTree* t=(TTree*)file->Get("RCC");
    unsigned int EVT;
    unsigned int TCU;
    unsigned int DIFF[NCRATE];
    t->SetBranchAddress("evt",&EVT);
    t->SetBranchAddress("tcu",&TCU);
    t->SetBranchAddress("diff",&DIFF);
    int n = t->GetEntries();
    unsigned long long one=1;
    unsigned long long tmin=one<<32;
    unsigned long long tmax=0;
    unsigned long long dmin=one<<32;
    unsigned long long dmax=0;

    TGraph* g[NCRATE];
    for(int i=0; i<NCRATE; i++){
      g[i] = new TGraph(1);
      g[i]->SetLineColor(color[i]);
    }
    printf("Found %d entries in Tree\n",n);
    for(int i=0; i<n; i++){
      t->GetEvent(i);
      //printf("%d %u %u %u\n",i,EVT,TCU,DIFF[2],DIFF[9]);
      if(tmin>TCU) tmin=TCU;
      if(tmax<TCU) tmax=TCU;
      for(int j=0; j<NCRATE; j++){
	long long diff=DIFF[j];
	int diff2;
	if(diff>2147483647) diff=diff-4294967296;
	diff2=diff;
	if(j==6) printf("Crate=%2d TCU=%10d tmin=%10d DIFF=%10u diff=%10d diff2=%10d %f\n",
			j,TCU,tmin,DIFF[j],diff,diff2,(double)diff2);

	g[j]->SetPoint(i,double(TCU),double(diff2));
	if(dmin>diff) dmin=diff;
	if(dmax<diff) dmax=diff;
      }
    }
    //    tmax=111000000;
    printf("Tmin=%f Tmax=%f Dmin=%f Dmax=%f\n",tmin,tmax,dmin,max);
    TH2F* frame=new TH2F("diff","RCC-TCU [xing]",1,
			 float(tmin),float(tmax),1,min,max);
    frame->Draw();
    for(int i=0; i<NCRATE; i++){
      g[i]->Draw("L");
    }    
    TLine *l = new TLine(tmin, 65536.0 , tmax, 65536.0); 
    l->Draw();
    TText *tt;
    tt=new TText(0.7,0.03,"TCU counter [xing]");  tt->SetNDC(); tt->SetTextSize(0.03); tt->Draw();
    tt=new TText(0.7,0.38,"Memory Overwrite above");  tt->SetNDC(); tt->SetTextSize(0.02); tt->Draw();
    float x=0.15, y=0.87, dy=0.025;
    for(int i=0; i<NCRATE; i++){
      tt=new TText(x,y,CRATE[i]); tt->SetNDC(); tt->SetTextSize(0.025); tt->SetTextColor(color[i]); tt->Draw();
      y-=dy;
    }
    if(yearday==0){
      sprintf(fname,"%d.tdiff.png",runnum);
    }else{
      if(max==MAX){
        sprintf(fname,"%d/%d.tdiff.png",yearday,runnum);
      }else{
        sprintf(fname,"%d/%d.tdiff.%d.png",yearday,runnum,max);
      }
    }
    c1->SaveAs(fname);
  }  

  if(plt==0 || plt==3){
    c1->Clear();
    gStyle->SetTitleH(0.05);    
    gStyle->SetOptStat(0);    
    TTree* t=(TTree*)file->Get("RCC");
    t->Draw("evt:tcu");
    if(yearday==0){
      sprintf(fname,"%d.rate.png",runnum);
    }else{
      sprintf(fname,"%d/%d.rate.png",yearday,runnum);
    }
    c1->SaveAs(fname);
  }
}

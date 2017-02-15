static int startday=082;
static int endday=365;

static const int NRUN=10000;
static int nrun=0;
static int ngoodrun=0;

static const int NCRATE=14;
static const char* CRATE[NCRATE]={"L1","BC1","MXQ","MIX","BCW","BCE","FEQ",
				  "BBC","BBQ","FMS","QT1","QT2","QT3","QT4"};
static const int color[NCRATE]=  {kGreen,kBlue,kRed,kBlue,kBlue,kBlue,kRed,
				  kBlue,kRed,kBlue,kRed,kRed,kRed,kRed};

static const int NVAL=3;
static int run[NRUN];
static Float_t frun[NRUN];
static Long64_t idx[NRUN];
static double val[NCRATE][NVAL][NRUN];  //2nd index 0=mean,1=rms,2=max
static TGraph* g[NCRATE][NVAL];         //last index 0=mip,1=mean,2=tac,3=tacdiff

//#include <stdio.h>
//#include <dirent.h>
//#include <algorithm>

void read(int irun, int runnum){
  char file[100];
  int yearday=runnum/1000;
  sprintf(file,"www/%d/rcc.%d.txt",yearday,runnum);
  FILE* f=fopen(file,"r");
  if(!f) { printf("Failed to open %s\n",file); return; }
  //printf("Reading %s\n",file);
  run[irun]=runnum;
  int nzero=0;
  for(int i=0; i<NCRATE; i++){
    float rmean,rrms,rmax;
    fscanf(f,"%f %f %f",&rmean,&rrms,&rmax);
    val[i][0][irun]=rmean;  
    val[i][1][irun]=rrms;  
    val[i][2][irun]=rmax;  
    g[i][0]->SetPoint(ngoodrun,double(irun),val[i][0][irun]);
    g[i][1]->SetPoint(ngoodrun,double(irun),val[i][1][irun]);
    g[i][2]->SetPoint(ngoodrun,double(irun),val[i][2][irun]);
  }
  ngoodrun++;
  fclose(f);
}

void readall(int day){
  int i=0;
  char tmp[100];
  float r;
  int year=day/1000;
  int yearday=day%1000;
  //printf("%d %d %d\n",day,year,yearday);
  if(yearday!=0) {startday=yearday; endday=yearday;}
  for(int d=startday; d<=endday; d++){
    int yday=year*1000+d;
    char dirname[100]; sprintf(dirname,"www/%5d/",yday);
    TSystemDirectory dir(dirname,dirname);
    TList *files = dir.GetListOfFiles();
    if(files){
      TSystemFile *file;
      TString fname;
      TIter next(files);
      while ((file=(TSystemFile*)next())) {
	fname = file->GetName();
	if (!file->IsDirectory() && fname.BeginsWith("rcc") && fname.EndsWith(".txt")) {
	  sscanf(fname.Data(),"%4s%8f%4s",tmp,&r,tmp);
	  frun[i]=(Long64_t)r;
          i++;                 
	  //printf("%s %8.0f\n",fname.Data(),r);
	}
      }
    }    
  }
  nrun=i;
  TMath::Sort((long)i,frun,idx,0);
  for(int j=0; j<nrun; j++){ 
    read(j,(int)frun[idx[j]]);
  }
}

void plot(int val=1, float xmin=0.0, float xmax=0.0, float ymin=0.0, float ymax=500.0){
  if(xmax==0) xmax=nrun;
  TH2F *frame = new TH2F("frame","",1,xmin,xmax,1,ymin,ymax); frame->Draw(); 
  for(int i=0; i<NCRATE; i++){
    g[i][val]->Draw("L");
  }  
}

void history(int day=15170, int zoom=0,
	     float xmin=0.0,  float xmax=0.0, 
	     float ymin=0.0,  float ymax=65536.0, 
	     float ymin2=0.0, float ymax2=65536.0/3.0,
	     float ymin3=0.0, float ymax3=65536.0*3.0
	     ){
  if(zoom>0) {
    ymax=4000;
    ymax2=600;
    ymax3=20000;
  }
  for(int i=0; i<NCRATE; i++){
    for(int v=0; v<NVAL; v++){
      g[i][v]=new TGraph(1);
      g[i][v]->SetMarkerStyle(20);
      g[i][v]->SetMarkerSize(1); 
      g[i][v]->SetLineColor(color[i]);
    }
  }
  readall(day);
  gStyle->SetOptStat(0);
  TText *t;
  c1 = new TCanvas("c1","History",50,0,700,720);  
  c1->Divide(1,3);
  c1->cd(1); plot(0,xmin,xmax,ymin,ymax);  
  t=new TText(0.15,0.92,"Mean(RCC-TCU)[xing]"); t->SetNDC(); t->SetTextSize(0.1); t->Draw();
  t=new TText(0.75,0.8,"DSM Crates");  t->SetNDC(); t->SetTextSize(0.07); t->SetTextColor(kBlue); t->Draw();
  t=new TText(0.75,0.7,"QT Crates");   t->SetNDC(); t->SetTextSize(0.07); t->SetTextColor(kRed); t->Draw();
  c1->cd(2); plot(1,xmin,xmax,ymin2,ymax2);
  t=new TText(0.15,0.92,"RMS(RCC-TCU)[xing]"); t->SetNDC(); t->SetTextSize(0.1); t->Draw();
  c1->cd(3); plot(2,xmin,xmax,ymin3,ymax3);
  t=new TText(0.15,0.92,"MAX(RCC-TCU)[xing]"); t->SetNDC(); t->SetTextSize(0.1); t->Draw();
  t=new TText(0.77,0.01,"RunIdx");  t->SetNDC(); t->SetTextSize(0.1); t->Draw();
  TLine *l = new TLine(0.0, 65536.0,nrun, 65536.0); l->Draw();
  if(zoom==0){
    t=new TText(0.15,0.45,"Memory Overwrite above");  t->SetNDC(); t->SetTextSize(0.05); t->Draw();
  }
  c1->Update();
  char file[100]; 
  if(day%1000!=0) { 
    if(zoom>0) {sprintf(file,"%d/historyz.%5d.png",day,day);}
    else       {sprintf(file,"%d/history.%5d.png",day,day);}
  }else{ 
    if(zoom>0) {sprintf(file,"historyz.png",day);}
    else       {sprintf(file,"history.png",day);}
  }
  c1->SaveAs(file);
  printf("Create %s\n",file);
}


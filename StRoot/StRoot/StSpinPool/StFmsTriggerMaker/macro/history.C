static int startday=050;
static int endday=365;

static const int NRUN=10000;
static int nrun=0;
static int ngoodrun=0;

static const int NVAL=19;
static int run[NRUN];
static int flag[NRUN];
static Float_t frun[NRUN];
static Long64_t idx[NRUN];
static double val[NVAL][NRUN];
static TGraph* g[NVAL];       

void read(int irun, int runnum, int log){
  char file[100];
  int yearday=runnum/1000;
  sprintf(file,"www/%d/%d.Mismatch.txt",yearday,runnum);
  FILE* f=fopen(file,"r");
  if(!f) { printf("Failed to open %s\n",file); return; }
  //printf("Reading %s\n",file);
  run[irun]=runnum;
  flag[irun]=1;
  int nzero=0;
  char line[200];
  for(int i=0; i<NVAL+1; i++){ 
    float v;
    fscanf(f,"%f",&v); 
    if(i<18)  val[i][irun]=v;
    if(i==19) val[i-1][irun]=v;
  }
  printf("irun=%d run=%8d : ",irun,runnum);
  for(int i=0; i<NVAL; i++) {printf("%6.3f ",val[i][irun]);}
  printf("\n");
  fclose(f);
  if(val[NVAL-1][irun]>=0.0 && val[NVAL-1][irun]<=100.0){
    for(int i=0; i<NVAL; i++){    
      if(log==1){
	float v=-3;    
	if(val[i][irun]>=0.001) v=log10(val[i][irun]);
	g[i]->SetPoint(ngoodrun,double(irun),v);
      }else{
	g[i]->SetPoint(ngoodrun,double(irun),val[i][irun]);      
      }    
    }
    ngoodrun++;
  }
}

void readall(int day, int log){
  int i=0;
  char tmp[100];
  float r;
  int year=day/1000;
  int yearday=day%1000;
  printf("Yearday=%d Year=%d Day=%d\n",day,year,yearday);
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
	if (!file->IsDirectory() && fname.EndsWith(".Mismatch.txt")) {
	  sscanf(fname.Data(),"%8f",&r);
	  frun[i]=(Long64_t)r;
          i++;                 
	  printf("%s %8.0f\n",fname.Data(),r);
	}
      }
    }    
  }
  nrun=i;
  TMath::Sort((long)i,frun,idx,0);
  for(int j=0; j<nrun; j++){ 
    read(j,(int)frun[idx[j]], log);
  }
}

void plot(float xmin=-1, float xmax=0, float ymin=-3.1, float ymax=2){
  if(xmax==0) xmax=nrun;
  TH2F *frame = new TH2F("frame","",1,xmin,xmax,1,ymin,ymax); 
  frame->Draw(); 
  for(int v=0; v<NVAL; v++){
    g[v]->Draw("L");
  }  
  TText* t0=new TText(0.05 ,0.95,"BitMismatch%"); t0->SetNDC(); t0->SetTextSize(0.05); t0->SetTextColor(kBlack);  t0->Draw();
  TText* t1=new TText(0.15 ,0.85,"QT12->Layer0"); t1->SetNDC(); t1->SetTextSize(0.05); t1->SetTextColor(kRed);    t1->Draw();
  TText* t2=new TText(0.15 ,0.80,"QT34->Layer0"); t2->SetNDC(); t2->SetTextSize(0.05); t2->SetTextColor(kMagenta);t2->Draw();
  TText* t3=new TText(0.15 ,0.75,"Layer0->1");    t3->SetNDC(); t3->SetTextSize(0.05); t3->SetTextColor(kBlue);   t3->Draw();
  TText* t4=new TText(0.15 ,0.70,"Layer1->2");    t4->SetNDC(); t4->SetTextSize(0.05); t4->SetTextColor(kOrange); t4->Draw();
  TText* t5=new TText(0.15 ,0.65,"Layer2->TCU");  t5->SetNDC(); t5->SetTextSize(0.05); t5->SetTextColor(kGreen); t5->Draw();
  TText* t6=new TText(0.15 ,0.60,"Average");      t6->SetNDC(); t6->SetTextSize(0.05); t6->SetTextColor(kBlack);  t6->Draw();
}

void history(int day=16054, int log=0){ 
  int color[NVAL]={kRed,kRed,kMagenta,kMagenta,
		   kRed,kRed,kRed,kRed,
		   kMagenta,kMagenta,kMagenta,kMagenta,
		   kBlue,kBlue,kBlue,kBlue,
		   kOrange, kGreen, kBlack};
  for(int v=0; v<NVAL; v++){
    g[v]=new TGraph(1);
    g[v]->SetMarkerStyle(20);
    g[v]->SetMarkerSize(1); 
    g[v]->SetLineColor(color[v]);
    g[v]->SetLineWidth(2);
  }
  readall(day,log);
  gStyle->SetOptStat(0);
  c1 = new TCanvas("c1","History",50,0,700,720);  
  if(log==0) {
    c1->Divide(1,2);
    c1->cd(1); plot(-1,0,-5.0,60.0);
    c1->cd(2); plot(-1,0,-0.5,4.0);
  }else {
    plot();
  }
  c1->Update();
  char file[100]; 
  if(day%1000!=0) { sprintf(file,"%d/history.%5d.png",day,day); }
  else            { sprintf(file,"history.png",day);}
  c1->SaveAs(file);
  printf("Create %s\n",file);
}


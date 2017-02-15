static const int NCRATE=14;
static const char* CRATE[NCRATE]={"L1","BC1","MXQ","MIX","BCW","BCE","FEQ",
				  "BBC","BBQ","FMS","QT1","QT2","QT3","QT4"};
static const int color[NCRATE]=  {kGreen,kBlue,kBlue,kBlue,kBlue,kBlue,kRed,
				  kBlue,kRed,kBlue,kOrange,kRed,kGreen,kYellow};

static const int NEVT=10000;
static int  tcu[NEVT];
static int  diff[NCRATE][NEVT];
static TGraph* g[NCRATE];    

void read(int runnum){
  char file[100];
  //int yearday=runnum/1000;
  //sprintf(file,"www/%d/rcc.%d.txt",yearday,runnum);
  sprintf(file,"%d.txt",runnum);
  FILE* f=fopen(file,"r");
  if(!f) { printf("Failed to open %s\n",file); return; }
  printf("Reading %s\n",file);
  char tmp[200];
  //fscanf(f,"%s\n",tmp);
  //printf("%s",tmp);
  for(int i=0; i<NEVT; i++){
    //for(int i=0; i<10; i++){
    float rmean,rrms,rmax;
    fscanf(f,"%6s %d %d %d %d %d %d\n",
	   tmp, &tcu[i],
	   &diff[2][i],&diff[10][i],&diff[11][i],&diff[12][i],&diff[13][i]);
    printf("%6s %d %d %d %d %d %d\n",
	   tmp, tcu[i],
	   diff[2][i],diff[10][i],diff[11][i],diff[12][i],diff[13][i]);
    for(int j=0; j<NCRATE; j++){
      g[j]->SetPoint(i,double(tcu[i]),double(diff[j][i]));
    }
  }
  fclose(f);
}

void plot(double xmin=0.0, double xmax=0.0, double ymin=0.0, double ymax=500.0){
  TH2F *frame = new TH2F("frame","",1,xmin,xmax,1,ymin,ymax); frame->Draw(); 
  for(int i=0; i<NCRATE; i++){
    g[i]->Draw("L");
  }  
}

void time(int run=17084043,
	     double xmin=71225190.0,  double xmax=118061728.0, 
	     double ymin=0.0,  double ymax=65536.0*2.5){
  for(int i=0; i<NCRATE; i++){
    g[i]=new TGraph(1);
    g[i]->SetMarkerStyle(20);
    g[i]->SetMarkerSize(1); 
    g[i]->SetLineColor(color[i]);
  }
  gStyle->SetOptStat(0);
  read(run);

  c1 = new TCanvas("c1",Form("%d",run),50,0,700,720);
  plot(xmin,xmax,ymin,ymax);

  TLine *l = new TLine(xmin, 65536.0 , xmax, 65536.0); l->Draw();
  TText *t;
  t=new TText(0.7,0.03,"TCU counter[xing]");  t->SetNDC(); t->SetTextSize(0.03); t->Draw();
  t=new TText(0.05,0.95,"RCC-TCU[xing]");  t->SetNDC(); t->SetTextSize(0.03); t->Draw();
  t=new TText(0.5,0.4,"Memory Overwrite above");  t->SetNDC(); t->SetTextSize(0.03); t->Draw();
  t=new TText(0.2,0.8,"MXQ");  t->SetNDC(); t->SetTextSize(0.03); t->SetTextColor(color[2]); t->Draw();
  t=new TText(0.2,0.75,"QT1");  t->SetNDC(); t->SetTextSize(0.03); t->SetTextColor(color[10]); t->Draw();
  t=new TText(0.2,0.70,"QT2");  t->SetNDC(); t->SetTextSize(0.03); t->SetTextColor(color[11]); t->Draw();
  t=new TText(0.2,0.65,"QT3");  t->SetNDC(); t->SetTextSize(0.03); t->SetTextColor(color[12]); t->Draw();
  t=new TText(0.2,0.60,"QT4");  t->SetNDC(); t->SetTextSize(0.03); t->SetTextColor(color[13]); t->Draw();

  /*

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
  */
  
  char file[100]; 
  sprintf(file,"rcc_time_%d.png",run);
  c1->SaveAs(file);
  printf("Create %s\n",file);
}


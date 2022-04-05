static const int MAXP=100;
static const int MAXRUN=500;
static const int NQ=4;
static const int NL=3;
static const int NS=21;
static const int NC=2;
static const int NID=NQ*NL*NS*NC;

static int   N[MAXRUN][NID];
static int   T[MAXRUN][NID][MAXP];
static float V[MAXRUN][NID][MAXP];
static float C[MAXRUN][NID][MAXP];
static float A[MAXRUN][NID];
static TString FNAME[MAXRUN];
static TString DATE[MAXRUN];
static int NRUN=0;
static int DEBUG=0;

static int COL[NC][NL]={{kRed, kOrange, kMagenta},{kBlue, kCyan, kBlack}};

static int NBAD=0;

int getID(int quad, int layer, int slat, int ch=0){
  return (quad-1)*NL*NS*NC + (layer-1)*NS*NC + (slat-1)*NC + ch;
}

int readdir(int forceread=0, const char* dir="data/"){
  TSystemDirectory Dir(dir,dir);
  TList *files = Dir.GetListOfFiles();
  if (files) {
    TSystemFile *file;
    TString fname;
    TIter next(files);
    double time[MAXRUN];
    int n=0, lastnrun=0;
    while ((file=(TSystemFile*)next())) {
      fname = file->GetName();
      if (!file->IsDirectory() && fname.BeginsWith("fps_ivscan_")) {
	TString f(fname);
	f.ReplaceAll("fps_ivscan_","");
	f.ReplaceAll("_"," ");
	int y,mo,d,h,m;
	sscanf(f.Data(),"%d %d %d %d %d",&y,&mo,&d,&h,&m);
	//printf("%s year=%d mo=%d day=%d\n",fname.Data(),y,mo,d);
	if(y==2014 && mo<12) continue;
	if(y==2014 && mo==12 && d<17) continue;
        time[n]=(y-2015)*372+mo*31+d+h/24.0+m/24.0/60.0;
	FNAME[n]=fname;
	n++;
	if(n==MAXRUN) {printf("Reached MAXRUN\n"); break;}
      }
    }
    FILE* ff=fopen("ivnrun.txt","r");    
    fscanf(ff,"%d",&lastnrun);
    fclose(ff);
    printf("%d files found, and ivnrun.txt says %d runs found when checked last\n",n,lastnrun);
    if(n<=lastnrun && forceread<0) {
      printf("No new runs found. Skipping!\n");
      return -1;
    }
    ff=fopen("ivnrun.txt","w");
    fprintf(ff,"%d\n",n);
    fclose(ff);
    Int_t index[MAXRUN]; memset(index,0,sizeof(index));
    TMath::Sort(n,time,index,false);    
    for(int i=0; i<n; i++){
      fname=FNAME[index[i]];
      fname.Prepend(dir);
      DATE[i]=FNAME[index[i]];
      DATE[i].ReplaceAll("fps_ivscan_","");
      //printf("%d %d %s\n",i,index[i],fname.Data());
      read(fname.Data());
    }
  }
  return lastnrun;
}

void read(const char* file){
  int r=NRUN;  
  FILE* f=fopen(file,"r");
  if(!f) { printf("Failed to open %s\n",file); return; }
  printf("%6d Reading %s\n",r+1,file);
  char line[1000];
  while( fgets(line,1000,f) != NULL){
    TString tline(line);
    tline.ReplaceAll("67.000,67.000,0.000,","");
    tline.ReplaceAll(","," ");
    tline.ReplaceAll(".000","");
    if(line[0]=='\n') continue;
    int t,q,l,s;
    float a,temp,v0,v1,i0,i1;
    sscanf(tline.Data(),"%d %d %d %d %f %f %f %f %f %f",
	   &t,&q,&l,&s,&a,&v0,&v1,&temp,&i0,&i1);
    int id = getID(q,l,s);
    int n=N[r][id];
    if(v0>71.9) break;
    if(n>=MAXP) {
      //if(n==MAXP) printf("Too many data points run=%d Q%1dL%1dS%02d\n",r,q,l,s); 
      //N[r][id]++;
      break;
    }
    //if(r==9 && q==3 && l==1 && s==1){
    //  printf("%10d %1d %1d %2d %8.3f %8.3f %8.3f %8.3f Run=%4d n=%4d\n",t,q,l,s,v0,v1,i0,i1,r,n);
    // }
    T[r][id][n]=t;
    V[r][id][n]=v0;
    C[r][id][n]=i0;
    N[r][id]++;
    T[r][id+1][n]=t;
    V[r][id+1][n]=v1;
    C[r][id+1][n]=i1;
    N[r][id+1]++;
  }
  fclose(f);
  if(DEBUG){
    for(int q=1; q<=NQ; q++){
      for(int l=1; l<=NL; l++){
	printf("Q%1d L%1d ",q,l);
	for(int s=1; s<=NS; s++){
	  printf(" %4d ",N[r][getID(q,l,s)]);
	}
	printf("\n");
      }
    }
  }
  NRUN++;
}

void writetxt(int run){
  char file[100] = "txt/iv.txt";
  FILE* f=fopen(file,"w");
  if(!f) { printf("Failed to open %s\n",file); return; }
  printf("Writing %s\n",file);
  for(int q=1; q<=NQ; q++){
    for(int l=1; l<=NL; l++){
      for(int s=1; s<=NS; s++){
	for(int c=0; c<NC; c++){
	  for(int r=0; r<NRUN; r++){
	    if(run>0 && r!=run) continue;
	    int id=getID(q,l,s,c);	 
	    if(A[r][id]==0.0) continue;
	    fprintf(f,"%1d %1d %2d %1d %4d %f\n",q,l,s,c,r,A[r][id]);
	  }
	}
      }
    }
  }
  fclose(f);
}

void plot(int q, int l, int s, int ch, int r, int k, int plt){
  float low=65.0, high=72.0;
  int id=getID(q,l,s,ch);
  TGraph* g = new TGraph(1);      
  TGraph* gg = new TGraph(1);      
  int j=0, jj=0;
  for(int i=0; i<N[r][id]; i++) { 
    float v=V[r][id][i];
    float c=C[r][id][i];
    g->SetPoint(j,double(v),double(log10(c)));
    //printf("Q%1dL%1dS%02d Run=%4d %4d v=%8.3f c=%8.3f\n",q,l,s,r+1,j,v,c);
    j++;
    if(c<0.1 && v>low) low=v;
    if(c>40.0 && v<high) high=v;
  }
  float low2=low+(high-low)*0.1;
  float high2=low+(high-low)*0.8;
  for(int i=0; i<N[r][id]; i++) {
    float v=V[r][id][i];
    float c=C[r][id][i];
    if(low2<v && v<high2){
      gg->SetPoint(jj,double(v),double(log10(c)));
      jj++;
    }	       
  }
  //printf("low=%f high=%f range=%f %f\n",low,high,low2,high2);
  if(j>0){
    int style = 20 + k%8;
    int color = 1 + k%8; if(color>=5) {color++;}
    if(plt==1) color=COL[ch][l-1];
    g->SetMarkerStyle(style);  gg->SetMarkerStyle(style); 
    g->SetMarkerSize(0.5);     gg->SetMarkerSize(0.5);    
    g->SetMarkerColor(color);  gg->SetMarkerColor(color); 
    g->SetLineColor(color);    gg->SetLineColor(color);   
    g->Draw("PL");             gg->Draw("PL");	     
  }
  int bad=0;
  double pzero;
  if(j>20 && jj<=2) bad=1;
  if(jj>10){	
    //printf("low=%f high=%f range=%f %f\n",low,high,low2,high2);
    TF1 *f= new TF1("f","-1 + (x-[0])*[1] + (x-[0])*(x-[0])*[2]");
    double p0[3]={66.0,66.0,67.5};
    f->SetLineColor(color);
    f->SetParameters(0,p0[l]);
    f->SetParameters(1,0.2);
    f->SetParameters(2,0.1);
    gg->Fit(f,"WQ");
    //gg->Fit(f,"W");
    pzero=f->GetParameter(0);
    if(l!=3) {A[r][id]=f->GetX(-0.5,65.5,68.5);}
    if(l==3) {A[r][id]=f->GetX(-0.5,68.0,70.0);}
    //printf("Q%1dL%1dS%02dC%1d %8.3f %8.3f\n",q,l,s,ch,pzero,A[r][id]);
    if( (pzero<50.0 || pzero>80.0) || 
	(l!=3 && (A[r][id]<66.0 || A[r][id]>68.0) ) || 
	(l==3 && (A[r][id]<68.0 || A[r][id]>70.0) ) ) bad=1;
  }
  //if(bad==1) {
  if(bad==999) {
    printf("BAD : Q%1dL%1dS%02dC%1d %s(Run=%4d) p0=%8.3f A=%8.3f\n",q,l,s,ch,DATE[r].Data(),r+1,pzero,A[r][id]);
    TText *t1=new TText(0.15,0.88-NBAD*0.02,Form("Q%1dL%1dS%02dC%1d %s(%d)",q,l,s,ch,DATE[r].Data(),r+1));
    t1->SetNDC(); t1->SetTextSize(0.018); t1->SetTextColor(color); t1->Draw();
    NBAD++;
  }
}

void history(int q, int l, int s, int ch, int k){
  int id=getID(q,l,s,ch);
  TGraph* g = new TGraph(1);      
  int j=0, jj=0;
  for(int r=0; r<NRUN; r++){
    int i=10;
    if(l<3) i=14;
    float c=C[r][id][i];
    if(c>0.001){
      g->SetPoint(j,double(r+1),double(log10(c)));
      j++;
    }
  }
  if(j>0){
    int style = 20 + k%8;
    int color = 1 + k%8; if(color>=5) {color++;}
    g->SetMarkerStyle(style);  
    g->SetMarkerSize(0.5);     
    g->SetMarkerColor(color);  
    g->SetLineColor(color);    
    g->Draw("PL");             
  }
}

void iv(int plt=-3, int quad=0, int layer=0, int slat=0, int run=-1, int log=1, float max=100){
   memset(N,0,sizeof(N));
   memset(V,0,sizeof(V));
   memset(C,0,sizeof(C));
   memset(T,0,sizeof(T));
   memset(A,0,sizeof(A));

   int lastnrun=readdir(plt);
   if(lastnrun==-1) return;

   gStyle->SetOptStat(0);
   gStyle->SetOptTitle(1);
   gStyle->SetOptFit(0);
   TCanvas *c1,*c2;
   c1 = new TCanvas("iv","iv",100,20,800,900);
   c2 = new TCanvas("IV hist","iv hist",910,20,800,900);
   
   char tit[100]="";
   TVirtualPad *pad=0;
   if(plt==0){
     pad = c1->cd(0);    
     pad->Draw();
     if(run==0){ sprintf(tit,"Q%1d L%1d S%2d ALL RUNS",quad,layer,slat); }
     else{
       int r=run-1;
       if(run==-1) r=NRUN-1;
       sprintf(tit,"Q%1d L%1d S%2d %s",quad,layer,slat,DATE[r].Data());
     }
   }
   TH2F *frame =  new TH2F(tit,tit,1,64.5,72,1,-2.0,2.0);
   TH2F *frame2 =  new TH2F(tit,tit,1,0.0,float(NRUN+1),1,-2.0,2.0);

   if(plt==0) frame->Draw();
   int k=0, nbad=0;   
   
   if(plt==-1 || plt==-3) {
     char filename[100];
     for(int r=0; r<NRUN; r++){
       c1->Clear();
       sprintf(filename,"www/iv/iv.%s.png",DATE[r].Data());
       frame->Draw();
       for(int q=1; q<=NQ; q++){
	 for(int l=1; l<=NL; l++){
	   for(int s=1; s<=NS; s++){
	     for(int ch=0; ch<2; ch++){
	       plot(q,l,s,ch,r,k,plt);
	       k++;
	     }//ch
	   }//s
	 }//l
       }//q
       c1->Update();
       c1->SaveAs(filename);       
       NBAD=0;
     }//r
   }
   if(plt==-2 || plt==-3) {
     char filename[100];
     k=0;
     c2->Clear();
     frame2->Draw();
     sprintf(filename,"www/iv/iv.history.png");
     for(int q=1; q<=NQ; q++){
       for(int l=1; l<=NL; l++){
	 for(int s=1; s<=NS; s++){
	   for(int ch=0; ch<2; ch++){
	     history(q,l,s,ch,k);
	     k++;
	   }//ch                                                                                                                                                                                 
	 }//s                                                                                                                                                                                    
       }//l                                                                                                                                                                                      
     }//q                        
     c2->Update();
     c2->SaveAs(filename);
   }else{
     for(int q=1; q<=NQ; q++){
       if(quad>0 && quad!=q) continue;
       if(plt==1){
	 c1->Clear();
	 c1->Divide(3,7);
       }
       for(int l=1; l<=NL; l++){
	 if(plt==0 && layer>0 && layer!=12 && layer!=l) continue;
	 if(layer==12 && layer==3) continue;
	 for(int s=1; s<=NS; s++){
	   if(q==2 && s>19) continue;
	   if(q==4 && s>19) continue;
	   if(plt==0 && slat>0 && slat!=s) continue;	 
	   if(plt==1){
	     pad = c1->cd(s);
	     if(l==1){
	       float mergin=0.005;
	       pad->SetRightMargin(mergin); pad->SetLeftMargin(mergin);
	       pad->SetTopMargin(mergin);   pad->SetBottomMargin(mergin);
	       pad->Draw();
	       frame->Draw();	   
	       TText *t1=new TText(0.75,0.05,Form("Q%1dS%02d",q,s)); t1->SetNDC(); t1->SetTextSize(0.12); t1->Draw();
	       if(s==1) {
		 TText *t2=new TText(0.05,0.85,"L1C0"); t2->SetNDC(); t2->SetTextSize(0.15); t2->SetTextColor(COL[0][0]); t2->Draw();
		 TText *t3=new TText(0.20,0.85,"L1C1"); t3->SetNDC(); t3->SetTextSize(0.15); t3->SetTextColor(COL[1][0]); t3->Draw();
		 TText *t4=new TText(0.05,0.70,"L2C0"); t4->SetNDC(); t4->SetTextSize(0.15); t4->SetTextColor(COL[0][1]); t4->Draw();
		 TText *t5=new TText(0.20,0.70,"L2C1"); t5->SetNDC(); t5->SetTextSize(0.15); t5->SetTextColor(COL[1][1]); t5->Draw();
		 TText *t6=new TText(0.05,0.55,"L3C0"); t6->SetNDC(); t6->SetTextSize(0.15); t6->SetTextColor(COL[0][2]); t6->Draw();
		 TText *t7=new TText(0.20,0.55,"L3C1"); t7->SetNDC(); t7->SetTextSize(0.15); t7->SetTextColor(COL[1][2]); t7->Draw();
	       }
	     }
	   }	   
	   for(int ch=0; ch<2; ch++){
	     for(int r=0; r<NRUN; r++){      
	       if(run>0 && run!=r+1) continue;	   	   
	       if(run==-1 && r!=NRUN-1) continue;	   	   
	       plot(q,l,s,ch,r,k,plt);
	       k++;
	     }//r
	   } //ch
	 } //s
       } //l
       if(plt==1){
	 c1->Update();
	 char filename[100];
	 sprintf(filename,"plot/iv.Q%1d.Run%d.d.png",q,run);
	 c1->SaveAs(filename);
       }
     } //q
   }
   
   writetxt(0);
   
   if(plt==0) {
     c1->Update();
     char filename[100];
     if(quad==0 && layer==0 && slat==0 && run==0){
       sprintf(filename,"plot/iv.png"); 
     }else{
       if(run==0) { 
	 sprintf(filename,"plot/iv-Q%1dL%1dS%1d.png",quad,layer,slat); 
       }else{
	 int r=run-1;
	 if(run==-1) r=NRUN-1;
	 sprintf(filename,"plot/iv-Q%1dL%1dS%1d.%s.png",quad,layer,slat,DATE[r].Data()); }
     }
     c1->SaveAs(filename);
     //system("/bin/cp iv.png www/");
   }
}

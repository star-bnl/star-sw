int use_background=1;
int x_end=4;

#define runB

#ifdef runA 
#define jan 1
// for run 10097097
float x[]={.00, .10, .20, .35, .50, .75, 1.0,
	   .00,-.10,-.20,-.35,-.50,-.75,-1.0,.00,
	   0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0};//milimeters
float y[]={0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,
	   .00, .10, .20, .35, .50, .75, 1.0,
	   .00,-.10,-.20,-.35,-.50,-.75,-1.0,.00};//milimeters
int timediff[]={00,35,35,37,36,37,37,41,35,35,37,35,37,37,42,
		31,35,36,36,35,37,38,41,35,35,35,36,37,37,42};//s
float Nblue=11349.49/107;//10^9 Ions
float Nyellow=10274.33/107;//10^9 Ions
int Nbunches=98;//96;
int max_time=1220;
float kbb=1036758;//from vernier/wcm using xml right before run.
//
#endif

#ifdef runB
#define jan 2
//for run 10103044
float x[]={.00, .15, .30, .45, .60, .75, .90,
	   .00,-.15,-.30,-.45,-.60,-.75,-.90,.00,
	   0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0};//milimeters
float y[]={0,0,0,0,0,0,0,
	   0,0,0,0,0,0,0,0,
	   .00, .15, .30, .45, .60, .75, .90,
	   .00,-.15,-.30,-.45,-.60,-.75,-.90,.00};//milimeters
int timediff[]={00,36,37,36,36,36,36,41,36,36,36,36,36,36,41,
		31,36,35,36,36,36,36,41,37,36,36,36,36,36,41};//s
float Nblue=10505.05/106;//10^9 Ions
float Nyellow=10039.98/106;//10^9 Ions
int Nbunches=96;
int max_time=1400;
float kbb=938419;//from vernier/wcm using xml right before run.
//
#endif // end of setup switch between runs

int integration_width=30;
float *x_longname ,*y_longname ;
bool extra_use;

float frequency=9.383/120.0; //MHz*buckets / buckets

Double_t gaus_stepping(Double_t *v, Double_t *par)
{
  Double_t skfNN=par[0]/1e7*frequency*kbb;
  //sigma*10^24 / s
  //since we subsume the 10^24, sigma*10^24/mm^2==unitless
  //hence, sigma is in units of 10^-24mm^2=10^-26cm^2=.01 barns
  //if we input sigma in nanobarns (my preference for the fit), 
  //then we must divide by 10^7 to get the number of centibarns:
  //printf("skfNN=%f\n",skfNN);
  Double_t sx2=par[1];//sigx1*sigx1+sigx2*sigx2; //milimeters^2
  Double_t sy2=par[2];//sigy1*sigy1+sigy2*sigy2; //milimeters^2
  Double_t background=par[3];


  Int_t step=v[0];
  if (step>29) 
    {
      printf("Requested step out of bounds, for drawing.  This shouldn't be possible.\n");
      return -999;
    }
  Double_t dx=x [step];
  Double_t dy=y [step];

  if (dx*dx>1.1 || dy*dy>1.1)
    {
    printf("v[0]=%f,step=%d ==> dx=%f, dy=%f\n",v[0],step,dx,dy);
    printf("Array out of bounds.  Checking arrays:\n");
    for(int i=0;i<30;i++)
      printf("step=%d x=%1.2f y=%1.2f\n",i,x [i],y [i]);
    assert(1==2);
    }
  
  if (dx*dx>(x[x_end]*x[x_end]*.95) || dy*dy>(x[x_end]*x[x_end]*.95))
    return use_background*background*integration_width; 
  
  if ((sx2*sy2)<0.00000001) 
    {
      printf("gaus_Step with: 0=%f,1=%f,2=%f\n",skfNN,sx2,sy2);
      return 999;
    }

  //comment me back in!
 Double_t sL= skfNN/(2*TMath::Pi()*sqrt(sx2*sy2));
 //Double_t sL= 18.84+9.44;
  Double_t val=sL*exp(-dx*dx/(2*sx2))*exp(-dy*dy/(2*sy2));

  /*we now have the expected /rate/, but since we integrated over a certain number of seconds, we 
 need to multiple to get the expected counts.  N=rate*time 
  */
  //printf("gaus_Step: 0=%f,1=%f,2=%f,3=%f,4=%f ===> %f\n",skfNN,sigx1,sigx2,sigy1,sigy2, val*20.0);
  //don't forget to add in the background rate!
  return (val+background)*integration_width;
}


//==============================
void janNicePlot(TH1F * hin){
  printf("AAA\n");
  char txt[1000]; 

  int runID=999999;
  float yMax=999;
  if(jan==1) {
    runID=10097097; 
    yMax=1290;
  }
  if(jan==2) {
    runID=10103044; 
    yMax=850;
  }

  sprintf(txt,"BHT3 yield for  R%d; vernier scan i-th step",runID);
  hin->SetTitle(txt);
  hin->SetMaximum(yMax);
  hin->SetMarkerStyle(8);
  hin->SetMarkerColor(kBlue);

  sprintf(txt,"vernierR%dbht3_1gaus",runID);
  c2=new TCanvas(txt,txt,450,600);
  c2->cd();

   TPad *cU,*cD;   splitPadY(0.4,&cU,&cD);
   cU->cd();
   hin->SetStats(0);//temp
   hin->Draw("e");
   gStyle->SetOptStat(0);
   gStyle->SetOptFit(11111);

   TF1 *ff=hin->GetFunction("steppy"); assert(ff);
   float nb=hin->GetNbinsX();

   TH1F *hdif=(TH1F*) hin->Clone();
   hdif->Reset();
   sprintf(txt,"yield residua;  step index; data-fit");
   hdif->SetTitle(txt);

   float par0=ff->GetParameter(0);
   float erPar0=ff->GetParError(0);
   float relEr=erPar0/par0;
   printf("par0=%f +/- %f , relEr=%f\n",par0,erPar0,relEr);

   for(int k=1;k<=nb;k++) {
     float x=hin->GetBinCenter(k);
     float y=hin->GetBinContent(k);
     float ey=hin->GetBinError(k);
     float fy=ff->Eval(x);
     float eFit=relEr*fy;
     float totEr=sqrt(ey*ey+eFit*eFit);
     // printf("k=%d %f %f %f %f\n",k,x,y,ey,fy);
     hdif->SetBinContent(k,y-fy);
     hdif->SetBinError(k,totEr);
   }
   cD->cd();
   hdif->Draw("e");
   hdif->SetMaximum(yMax/10.);
   hdif->SetMinimum(-yMax/10.);
   ln=new TLine(0,0,30,0); ln->SetLineStyle(2);ln->Draw();ln->SetLineColor(kBlack);

}

//------------------------
void splitPadY(float y, TPad **cU, TPad **cD) {
  (*cU) = new TPad("padD", "apdD",0,y+0.005,1.0,1.);
  (*cU)->Draw();    
  (*cD) = new TPad("padU", "apdU",0.0,0.,1.,y);
  (*cD)->Draw();

  /* use case:
    TPad *cU,*cD;   splitPadY(0.4,&cU,&cD);
    cU->cd(); h->Draw() 
  */
}


//==============================
void makeVernierScanFitFromHist(char * infile="temp.hist.root")
{
  if(jan==1)  infile="10097097.bht3.hist.root";
  if(jan==2)  infile="10103044.bht3.hist.root";

  TFile *f=new TFile(infile);
  TH1F* times=(TH1F*)(f->Get("time"));

  float *xpos=x;
  float *ypos=y;
  int counts[30];
  int timestamp[30];

  timestamp[0]=timediff[0];
  for (int i=1;i<30;i++)
    timestamp[i]=timediff[i]+timestamp[i-1];
  //according to Angelika Drees, these timestamps represent the time that the file was saved, and hence is ~1 second /after/ the end of data-taking for that step.  At timestamp[0], the beam should be moving from x=0 to x=0.10.


  TH1F *stamps=new TH1F("timestamps","timestamps",max_time,0,max_time);


  //timestamps of the big jumps:  6,13,21,28

  //find the offfset between the time stamps and the data by maximizing the differential across each jump:

  int best_value=0;
  int best_offset=-1;

  int temp_value=0;
  TH1F *hOffset=new TH1F("offset","summed differential across big steps as a function of proposed offset;offset;dHz/dt",600,0,600);
  for (int i=4;i+timestamp[28]<max_time;i++)
    {
      temp_value=0;
      temp_value-=times->GetBinContent(i+timestamp[6]-2);
      temp_value-=times->GetBinContent(i+timestamp[6]-1);
      temp_value+=times->GetBinContent(i+timestamp[6]+1);
      temp_value+=times->GetBinContent(i+timestamp[6]+2);

      temp_value-=times->GetBinContent(i+timestamp[13]-2);
      temp_value-=times->GetBinContent(i+timestamp[13]-1);
      temp_value+=times->GetBinContent(i+timestamp[13]+1);
      temp_value+=times->GetBinContent(i+timestamp[13]+2);

      temp_value-=times->GetBinContent(i+timestamp[21]-2);
      temp_value-=times->GetBinContent(i+timestamp[21]-1);
      temp_value+=times->GetBinContent(i+timestamp[21]+1);
      temp_value+=times->GetBinContent(i+timestamp[21]+2);

      temp_value-=times->GetBinContent(i+timestamp[28]-2);
      temp_value-=times->GetBinContent(i+timestamp[28]-1);
      temp_value+=times->GetBinContent(i+timestamp[28]+1);
      temp_value+=times->GetBinContent(i+timestamp[28]+2);
      hOffset->Fill(i,temp_value);
      if (temp_value>best_value)
	{
	  best_value=temp_value;
	  best_offset=i;
	}
    }
  hOffset->Draw();
  printf("Best offset is %d\n",best_offset);


  //Have:  Correct offset
  //define the ranges we intend to integrate over for each step:
  int end_offset=5;//give us a buffer of five seconds between the end of our range and the timestamp that apparently occurs ~1 second after the step ended.
  int start_offset=25;//give us a substantial buffer from the end of the previous to make sure we're well clear of the beam motion.

  //change that to be a generalized width (defined above):
   end_offset=16-integration_width/2;
   start_offset=end_offset+integration_width;

  for (int i=0;i<30;i++)
    {
      counts[i]=0;
      //      printf("summing bin %d to %d for new bin %d\n",
      //	     best_offset+timestamp[i]-start_offset,
      //	     best_offset+timestamp[i]-end_offset,
      //	     i);
      for (int j=best_offset+timestamp[i]-start_offset;
	   j<(best_offset+timestamp[i]-end_offset);j++)
	  counts[i]+=times->GetBinContent(j);
    }

  TH1F *idealized=new TH1F("idealized","counts vs step;step;counts",30,0,30);
  //gStyle->SetOptStat(1);
  //gStyle->SetOptFit(11101);
  //printf("Step\tXPos\tYPos\tCounts\n");
  for (int i=0;i<30;i++)
    //printf("%d\t%f\t%f\t%d\n",i,x[i],y[i],counts[i]-1.7*integration_width);
    //return;
    idealized->Fill(i,counts[i]);
  idealized->GetYaxis()->SetRangeUser(0,1600);
  //idealized->Draw("e");

  //N=L*sigma
  //N=kfNN*1/(2pi sqrt(sigx1^2+sigx2^2)sqrt(sigx1^2+sigy2^2)) * exp(-d^2/2(sigx1^2+sigx2^2))
  //NN=11349.49*10274.33*10^18

  //define errors:
  float err_counts[30];
  for (int i=0;i<30;i++)
    err_counts[i]=sqrt(counts[i]);

  float err_pos[30];
  for (int i=0;i<30;i++)
    err_pos[i]=0.01;

  x_longname=xpos;
  y_longname=ypos;

  /*testing the function:
  Double_t a[1];
  Double_t b[]={1.0,1.0,1.0,1.1,1.1};
  for (int i=0;i<30;i++)
    {
      printf("step: i=%d ==>I=%f\n  ",i,i*1.0);
      a[0]=i*1.0;
      gaus_stepping(a,b);
    }
  return;
  */

  TF1 *func=new TF1("steppy",gaus_stepping,0.1,29.9,4);
  func->SetParameters(2,0.25,0.25,.05);
  //old: func->SetParNames("skfNN","sigx_blue","sigx_yellow","sigy_blue","sigy_yellow");
  func->SetParNames("sigma (nb)","#sigma_{X}^{2} (mm^{2})","#sigma_{Y}^{2^} (mm^{2})","background (Hz)");
  //func->SetParLimits(0,1000*1000,5000*1000);
  func->SetParLimits(1,0.01,10);
  func->SetParLimits(2,0.01,10);
  //func->SetParLimits(3,0,0.1);


  idealized->Fit("steppy");
  //  func->Draw();
  //func->SetParameter(0,342);
  //func->SetParameter(1,0.0180);
  //func->SetParameter(2,0.0125);
  //func->SetParameter(3,1.72);

  idealized->Draw("e");
  //func->Draw("same");

  janNicePlot(idealized);

  return;

  Double_t a[1];
  Double_t b[]={4.997,.1733,-0.068,.1323,.1323};
  TF1 *copyfunc=new TF1("copy",gaus_stepping,0.1,29.9,4);
  copyfunc->SetParameters(b);
  copyfunc->Draw("SAME");

  for (int w=0;w<30;w+=1)
    {
      a[0]=w;
      printf("bin=%f counts=%f, func=%f\n",w,gaus_stepping(a,b),func->Eval(w*1.0));
    } 

  return;
}

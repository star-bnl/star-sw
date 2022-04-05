//#ifndef HISTUTIL_H
//#define HISTUTIL_H

#include <cstring>

// 3d to 2d
TH2D* HistSlice(TH3* h3,const char* basename, const char* basetitle,
	       const char* slicename, Axis_t min, Axis_t max,
	       const char* slicetype,
	       Option_t* opt="");


// 3d to 2d but keep varying width histos
TH2D* HistSlice(TH3* h3,const char* basename, const char* basetitle,
	       const char* slicename, const char* slicetype);

// 2d to 1d
TH1D* HistSlice(TH2* h2,const char* basename, const char* basetitle,
	       const char* slicename,Axis_t min, Axis_t max,
	       const char* projaxis,
	       Option_t* opt="");

// 3d to 1d
// if project to x then slicename1="y",slicename2="z"
// if project to y then slicename2="z",slicename2="x"
// if project to z then slicename1="x",slicename2="y"

TH1D* HistSlice(TH3* h3,const char* basename, const char* basetitle,
	       const char* slicename1,Axis_t min1, Axis_t max1,
	       const char* slicename2,Axis_t min2, Axis_t max2,
	       const char* projecttype,
	       Option_t* opt="");

TProfile* Profile(TH3* h3,const char* basename, const char* basetitle,
		  const char* slicename, Axis_t min, Axis_t max,
		  const char* slicetype,
		  const char* projecttype,
		  Option_t* opt="");

TProfile* Profile(TH2* h2,const char* basename,const char* basetitle,
		  const char* slicename, Axis_t min, Axis_t max,
		  const char* profaxis,
		  Option_t* opt="");

TH1D* Rms(TH3* h3,const char* basename, const char* basetitle,
		  const char* slicename, Axis_t min, Axis_t max,
		  const char* slicetype,
		  const char* projecttype);
TH1D* Rms(TH2* h2,const char* basename,
	      const char* projecttype);
// rebin an axis of a 3D histogram (with varying bins);

void Rebin(TH3* h3,int axisType,int nBin,Axis_t* ary);

Int_t FindMinBin(const TAxis* axis,Stat_t val);
Int_t FindMaxBin(const TAxis* axis,Stat_t val);
void  SetRange(const TAxis* axis,Stat_t min,Stat_t max);
Stat_t FindMax(TH1* ha,TH1* hb);
Stat_t FindMin(TH1* ha,TH1* hb);
Stat_t FindMax(TH1* ha,TH1* hb,TH1* hc);
void Divide(TCanvas* c1,int nx,int ny,const char* title=0,const char* file=0);
void Print(TCanvas* c1, const char* outDir, TString basename);

void SetMinMax(TH1* h);
void SetMinMax(TH1* h,float min, float max);
void GetMeanRms(TH1* ha,float& cmean,float& crms);
void GetMeanRmsYield(TH1* ha,float& cmean,float& crms);
void PrintMeanRms(TH1* ha,float x, float y,float size=0.04);
void PrintMeanRmsYield(TH1* ha,float x, float y,float size=0.04);
void ReplaceTitle(TH1* h,char* current, char* replace);
void DrawLine(TH1* h,float y=0);

void showHistogramValues(TH1D* h1, char* more=0);
void showTGraphValues(TGraphAsymmErrors* graph,char* more=0);
void dump(TH1* h1);
void dump(TGraph* g);
void dump(TGraphAsymmErrors* g);
void scale2D(TH2*);
//-----------------------------------------------

void DrawLine(TH1* h,float y)
{
  TLine* line=new TLine; TAxis* axis=h->GetXaxis();
  line->DrawLine(axis->GetBinLowEdge(axis->GetFirst()),y,
		 axis->GetBinUpEdge(axis->GetLast()),y);

}

void ReplaceTitle(TH1* h,char* current, char* replace)
{
  TString s=h->GetTitle();
  s.ReplaceAll(current,replace);
  h->SetTitle(s.Data());
}

void SetMinMax(TH1* h,float min, float max)
{
  h->SetMinimum(min);
  h->SetMaximum(max);
}

void SetMinMax(TH1* h)
{
  h->SetMinimum(h->GetMinimum()*0.8);
  h->SetMaximum(h->GetMaximum()*1.2);
}

void GetMeanRms(TH1* ha,float& mean,float& rms)
{
  TAxis* axis=ha->GetXaxis();
  mean=0;rms=0;
  int n=0;

  for(int i=axis->GetFirst(); i<=axis->GetLast(); i++){
    if(ha->GetBinContent(i)>0){
      float val = (ha->GetBinCenter(i)*ha->GetBinContent(i));
      mean += val;
      rms += val*val;

    }
  }
  n=ha->Integral(axis->GetFirst(),axis->GetLast());
  //n=ha->Integral(1,axis->GetNbins());
  if(n>0){
    mean /= n; rms /= n; rms=sqrt(rms - mean*mean);
  }
}

void GetMeanRmsYield(TH1* ha,float& mean,float& rms)
{
  TAxis* axis=ha->GetXaxis();
  mean=0;rms=0;
  int n=0;

  //  cout << ha->GetName() << endl;

  for(int i=axis->GetFirst(); i<=axis->GetLast(); i++){
    if(ha->GetBinContent(i)>0){
      float val = ha->GetBinContent(i);
      mean += val;
      rms += val*val;
      //  cout << "bin=" << i << ",val=" << val << endl;
      
    }
  }
  n=axis->GetLast()-axis->GetFirst()+1;
  //  cout << "n=" << n << endl;
  //n=ha->Integral(1,axis->GetNbins());
  if(n>0){
    mean /= n; rms /= n; rms=sqrt(rms - mean*mean);
  }
  //  cout << mean << ", " << rms << endl;
}

void PrintMeanRms(TH1* ha,float x,float y,float size)
{
  TText* text=new TText; float mean(0),rms(0);
  text->SetTextSize(size);
  char buf[100];
  mean = ha->GetMean(); rms = ha->GetRMS();
  //  if(compute)GetMeanRms(ha,mean,rms); 
  sprintf(buf,"mean: %.3f",mean);
  text->DrawTextNDC(x,y,buf);
  sprintf(buf,"rms: %.3f",rms);
  text->DrawTextNDC(x,y-0.05,buf);
}

void PrintMeanRmsYield(TH1* ha,float x,float y,float size)
{
  TText* text=new TText; float mean(0),rms(0);
  text->SetTextSize(size);
  char buf[100];
  //mean = ha->GetMean(); rms = ha->GetRMS();
  GetMeanRmsYield(ha,mean,rms); 
  sprintf(buf,"mean: %.3f",mean);
  text->DrawTextNDC(x,y,buf);
  sprintf(buf,"rms: %.3f",rms);
  text->DrawTextNDC(x,y-0.05,buf);
}

// divide a canvas and print a title

void Divide(TCanvas *c1,int nx,int ny,const char* title,const char* file)
{
  c1->Clear();  
  TString cTitle = (title) ? title : c1->GetTitle();
  if(title) c1->SetTitle(title);
  
  TPaveLabel *mP = new TPaveLabel(.02,.95,.5,.98,title);
  mP->SetFillColor(0);
  mP->SetBorderSize(0);
  mP->Draw();
  
  if(file){
    TText tFile;
    tFile.SetTextSize(0.02);
    tFile.DrawTextNDC(0.1,0.0,file);
  }

  char buf[10];
  
  Double_t ymax = .94;
  Double_t dy = ymax/ny;
  Double_t dx = 1./nx;
  Double_t ymargin = 0.02;
  double   xmargin = 0.02;
  int n=0;
  for (Int_t iy=0;iy<ny;iy++) {
    double yhigh = ymax - iy*dy - ymargin;
    double ylow = yhigh - dy + 3*ymargin;
    if (ylow < 0) ylow = 0;
    if (ylow > yhigh) continue;
    for (Int_t ix=0;ix<nx;ix++) {
      n++;
      double xlow = ix*dx + xmargin;
      double xhigh = xlow +dx -2*xmargin;
      if (xlow > xhigh) continue;
      
      TPad* p = new TPad(buf,buf,xlow,ylow,xhigh,yhigh);
      p->SetNumber(n);
      p->Draw();
    }
  }
  
}
	        
// given a maximum value, find it's bin
Int_t 
FindMinBin(const TAxis* axis,Stat_t val)
{
  Int_t bin = axis->FindBin(val);
  if(bin<0) return 1;
  return (axis->GetBinCenter(bin)<val) ? ++bin : bin;
  
}
 
// given a minimum value, find it's bin
Int_t FindMaxBin(const TAxis* axis,Stat_t val)
{
  Int_t bin = axis->FindBin(val);
  if(bin>axis->GetNbins()) return axis->GetNbins();
  return (axis->GetBinCenter(bin)>val) ? --bin : bin;
}

void SetRange(const TAxis* axis, Stat_t min, Stat_t max)
{
  axis->SetRange(FindMinBin(axis,min),FindMaxBin(axis,max));
}
Stat_t
FindMax(TH1* ha, TH1* hb)
{
  Stat_t maxa = ha->GetMaximum();
  Stat_t maxb = hb->GetMaximum();
  return (maxa > maxb) ? maxa : maxb;

}

Stat_t
FindMin(TH1* ha, TH1* hb)
{
  Stat_t mina = ha->GetMinimum();
  Stat_t minb = hb->GetMinimum();
  return (mina > minb) ? mina : minb;

}

Stat_t 
FindMax(TH1* ha,TH1* hb,TH1* hc)
{
  Stat_t max1 = FindMax(ha,hb);
  Stat_t max2 = FindMax(hb,hc);
  return (max1>max2) ? max1 : max2;
}


// 3d to 2d

TH2D* HistSlice(TH3* h3,
	       const char* basename,const char* basetitle,
	       const char* slicename, Axis_t min, Axis_t max,
	       const char* slicetype,
	       Option_t* opt)
{
  char name[200], title[200],option[20],buf[100];
  Int_t minBin, maxBin;
  Axis_t mmin,mmax;
  TH2D* h2;

  if(!h3){
    cout << "Null pointer for h3 : " << basename << endl;
    return 0;
  }

  // set the range
  // x slice

  TAxis* axis = 0; TAxis* yAxis=0,*xAxis=0;
  if(strcmp(slicetype,"yz")==0 || strcmp(slicetype,"zy")==0 ){
    axis = h3->GetXaxis();
    if(strcmp(slicetype,"yz")==0){ yAxis=h3->GetYaxis(); xAxis=h3->GetZaxis();}
    else { yAxis=h3->GetZaxis(); xAxis=h3->GetYaxis(); }
  }
  // y slice
  else if (strcmp(slicetype,"xz")==0 || strcmp(slicetype,"zx")==0){
    axis = h3->GetYaxis();
    if(strcmp(slicetype,"xz")==0){ yAxis=h3->GetXaxis(); xAxis=h3->GetZaxis();}
    else { yAxis=h3->GetZaxis(); xAxis=h3->GetXaxis(); }
  }
  // z slice
  else if (strcmp(slicetype,"xy")==0 || strcmp(slicetype,"yx")==0){
    axis = h3->GetZaxis();
    if(strcmp(slicetype,"xy")==0){ yAxis=h3->GetXaxis(); xAxis=h3->GetYaxis();}
    else { yAxis=h3->GetYaxis(); xAxis=h3->GetXaxis(); }
  }
  else {
    cerr << "Wrong value for slice axis for " << basename << endl;
    return 0;
  }    
  
  if(min>=max){
    minBin = 1; maxBin = axis->GetNbins();
  }
  else{
    minBin = FindMinBin(axis,min);
    maxBin = FindMaxBin(axis,max);
  }
  sprintf(option,"%s%s",slicetype,opt);

  if(!slicename){
    strcpy(buf,axis->GetTitle()); slicename=buf;
  }


  axis->SetRange(minBin,maxBin);

  h2 = (TH2D*) h3->Project3D(option);
  h2->SetYTitle(yAxis->GetTitle());
  h2->SetXTitle(xAxis->GetTitle());


  mmin = axis->GetBinLowEdge(minBin);
  mmax = axis->GetBinUpEdge(maxBin);
  
  // set the name and title
  sprintf(name,"%s(%.2f<%s<%.2f)",basename,mmin,slicename,mmax);
  sprintf(title,"%s (%.2f<%s<%.2f)",basetitle,mmin,slicename,mmax);
  
  h2->SetName(name); h2->SetTitle(title);

  axis->SetRange(0,9999999);

  return h2;
}

// 3d to 2d var bins

TH2D* HistSlice(TH3* h3,const char* basename, const char* basetitle,
	       const char* slicename, const char* slicetype)
{
  TString opt = slicetype;

  int projX=-1,projY=-1,projZ=-1;

  if(opt.Contains("xy")){ projX=1; projY=0; projZ=2; }
  else if(opt.Contains("yx")){ projX=0; projY=1; projZ=2;}
  else if(opt.Contains("zy")){ projX=1; projY=2; projZ=0; }
  else if(opt.Contains("yz")){ projX=2; projY=1; projZ=0; }
  else if(opt.Contains("xz")){ projX=2; projY=0; projZ=1; }
  else if(opt.Contains("zx")){ projX=0; projY=2; projZ=1; }

  TAxis* xAxis=0, *yAxis=0, *zAxis=0;
  switch(projX){
  case 0: xAxis = h3->GetXaxis(); break;
  case 1: xAxis = h3->GetYaxis(); break;
  case 2: xAxis = h3->GetZaxis(); break;
  }
  switch(projY){
  case 0: yAxis = h3->GetXaxis(); break;
  case 1: yAxis = h3->GetYaxis(); break;
  case 2: yAxis = h3->GetZaxis(); break;
  }
  switch(projZ){
  case 0: zAxis = h3->GetXaxis(); break;
  case 1: zAxis = h3->GetYaxis(); break;
  case 2: zAxis = h3->GetZaxis(); break;
  }
  
  // create the 2d histogram
  int ixmin = xAxis->GetFirst();
  int ixmax = xAxis->GetLast();
  int iymin = yAxis->GetFirst();
  int iymax = yAxis->GetLast();
  int izmin = zAxis->GetFirst();
  int izmax = zAxis->GetLast();
  int nx = ixmax-ixmin+1;
  int ny = iymax-iymin+1;
  int nz = izmax-izmin+1;

  char* name = new char[strlen(basename)+200];
  char* title = new char[strlen(basename)+200];
  
  sprintf(name,"%sSbin%dto%d",basename,izmin,izmax);
  sprintf(title,"%s(%.2f<%s<%.2f)",basetitle,
	  zAxis->GetBinLowEdge(izmin),slicename,zAxis->GetBinUpEdge(izmax));

  /*
  cout << "xmin=" << xAxis->GetBinLowEdge(ixmin) 
       << ",xmax=" << xAxis->GetBinUpEdge(ixmax)
       << endl
       << "ymin=" << yAxis->GetBinLowEdge(iymin)
       << ",ymax=" << yAxis->GetBinUpEdge(iymax)
       << endl
       << "zmin=" << zAxis->GetBinLowEdge(izmin)
       << ",zmax=" << zAxis->GetBinUpEdge(izmax)
       << endl;
  */

  double* xAry=0; xAry=xAxis->GetXbins()->GetArray();
  double* yAry=0; yAry=yAxis->GetXbins()->GetArray();

  // normal binning
  TH2D* h2=0;
  if(!xAry){
    h2=(TH2D*) h3->Project3D(slicetype);
    h2->SetName(name); h2->SetTitle(title);
    return h2;
  }

  // varying binning
  h2=new TH2D(name,title,
	      nx,&xAry[ixmin-1],ny,&yAry[iymin-1]);

  // fill the new histogram

  int iBinXreal=0, iBinYreal=0, iBinZreal=0;
  for(int ix=ixmin; ix<=ixmax; ix++){
    switch(projX){
    case 0: iBinXreal = ix; break;
    case 1: iBinYreal = ix; break;
    case 2: iBinZreal = ix; break;
    }
    for(int iy=iymin; iy<=iymax; iy++){
      switch(projY){
      case 0: iBinXreal = iy; break;
      case 1: iBinYreal = iy; break;
      case 2: iBinZreal = iy; break;
      }
      double content=0;
      for(int iz=izmin; iz<=izmax; iz++){
	switch(projZ){
	case 0: iBinXreal = iz; break;
	case 1: iBinYreal = iz; break;
	case 2: iBinZreal = iz; break;
	}
	int bin = h3->GetBin(iBinXreal,iBinYreal,iBinZreal);
	h2->Fill(xAxis->GetBinCenter(ix),
		 yAxis->GetBinCenter(iy),h3->GetBinContent(bin));
      }
    }
  }

  return h2;
}


// 2d to 1d

TH1D* HistSlice(TH2* h2,const char* basename, const char* basetitle,
	       const char* slicename,Axis_t min, Axis_t max,
	       const char* projaxis,
	       Option_t* opt)
{
  char name[200], title[200];
  Axis_t mmin(0),mmax(0);
  Int_t minBin(0), maxBin(0);
  TH1D* h1;
  TAxis* axis = 0;
  char buf[100];

  if(strcmp(projaxis,"x")==0){
    axis = h2->GetYaxis();
  }
  else if(strcmp(projaxis,"y")==0){
    axis = h2->GetXaxis();
  }
  else{
    cout << "The argument must be either 'x' or 'y' " << endl;
    return 0;
  }
  
  if(min>=max){
    minBin = 1; maxBin = axis->GetNbins();
 
  }
  else{
    minBin = FindMinBin(axis,min);
    maxBin = FindMaxBin(axis,max);
  }

  if(strcmp(projaxis,"x")==0){
    h1= h2->ProjectionX("dummy",minBin,maxBin,opt);
    if(!slicename){
      strcpy(buf,axis->GetTitle()); slicename=buf;
    }    
    h1->SetXTitle(h2->GetXaxis()->GetTitle());
  }
  else if(strcmp(projaxis,"y")==0){
    h1= h2->ProjectionY("dummy",minBin,maxBin,opt);
    if(!slicename){
      strcpy(buf,axis->GetTitle()); slicename=buf;
    }    
    h1->SetXTitle(h2->GetYaxis()->GetTitle());
  }

  mmin = axis->GetBinLowEdge(minBin);
  mmax = axis->GetBinUpEdge(maxBin);
  

  //
  // change the name and title
  //
  sprintf(name,"%s(%.2f<%s<%.2f)",basename,mmin,slicename,mmax);
  sprintf(title,"%s (%.2f<%s<%.2f)",basetitle,mmin,slicename,mmax);
  h1->SetTitle(title);
  h1->SetName(name);
  
  return h1;
}

TH1D* HistSlice(TH3* h3,const char* basename, const char* basetitle,
	       const char* slicename1,Axis_t min1, Axis_t max1,
	       const char* slicename2,Axis_t min2, Axis_t max2,
	       const char* projecttype,
	       Option_t* opt)
{
  char slicetype[10];
  char buf1[100],buf2[100];
  TAxis* axis1, *axis2;
  if(strcmp(projecttype,"x")==0){ // slice out y
    strcpy(slicetype,"zx");
    axis1=h3->GetYaxis(); axis2=h3->GetZaxis();
  }
  else if(strcmp(projecttype,"y")==0){ // slice out z
    strcpy(slicetype,"xy");
    axis1=h3->GetZaxis(); axis2=h3->GetXaxis();
  }
  else if(strcmp(projecttype,"z")==0){ // slice out x
    strcpy(slicetype,"yz");
    axis1=h3->GetXaxis(); axis2=h3->GetYaxis();
  }
  else{
    cout << "error: wrong project type " << basename << endl;
    return;
  }
  if(!slicename1){
    strcpy(buf1,axis1->GetTitle()); slicename1=buf1;
  }
  if(!slicename2){
    strcpy(buf2,axis2->GetTitle()); slicename2=buf2;
  }

  TH2D* h2 =
    (TH2D*) HistSlice(h3,basename,basetitle,
		      slicename1,min1,max1,
		      slicetype,opt);

  return HistSlice(h2,h2->GetName(),h2->GetTitle(),
		   slicename2,min2,max2,
		   "x",opt);
		      
}

TProfile* Profile(TH3* h3,const char* basename, const char* basetitle,
		  const char* slicename, Axis_t min, Axis_t max,
		  const char* slicetype,
		  const char* projecttype,
		  Option_t* opt)
{
  // first project to 2d
  TH2D* h2 = HistSlice(h3,basename,basetitle,slicename,min,max,
		      slicetype);
  char name[200];
  TProfile* p;
  // profile
  if(strstr(projecttype,"x")){
    sprintf(name,"%s_profx",h2->GetName());
    p=h2->ProfileX(name,0,9999999,opt);
  }
  else{
    sprintf(name,"%s_profy",h2->GetName());
    p=h2->ProfileY(name,0,9999999,opt);
  }
  p->SetTitle(h2->GetTitle());
  return p;
}

TProfile* Profile(TH2* h2,const char* basename,const char* basetitle,
		  const char* slicename, Axis_t min, Axis_t max,
		  const char* profaxis,
		  Option_t* opt)
{
  TProfile* p;
  char name[200], title[200];
  Axis_t mmin(0),mmax(0);
  Int_t minBin(0), maxBin(0);
  TAxis* axis = 0;
  char buf[100];

  if(strcmp(profaxis,"x")==0){
    axis = h2->GetYaxis();
  }
  else if(strcmp(profaxis,"y")==0){
    axis = h2->GetXaxis();
  }
  else{
    cout << "The argument must be either 'x' or 'y' " << endl;
    return 0;
  }
  
  if(min>=max){
    minBin = 1; maxBin = axis->GetNbins();
 
  }
  else{
    minBin = FindMinBin(axis,min);
    maxBin = FindMaxBin(axis,max);
  }

  if(strcmp(profaxis,"x")==0){
    p= h2->ProfileX("dummy",minBin,maxBin,opt);
    if(!slicename){
      strcpy(buf,axis->GetTitle()); slicename=buf;
    }    
    p->SetXTitle(h2->GetXaxis()->GetTitle());
  }
  else if(strcmp(profaxis,"y")==0){
    p= h2->ProfileY("dummy",minBin,maxBin,opt);
    if(!slicename){
      strcpy(buf,axis->GetTitle()); slicename=buf;
    }    
    p->SetXTitle(h2->GetYaxis()->GetTitle());
  }

  mmin = axis->GetBinLowEdge(minBin);
  mmax = axis->GetBinUpEdge(maxBin);
  

  //
  // change the name and title
  //
  sprintf(name,"%s(%.2f<%s<%.2f)",basename,mmin,slicename,mmax);
  sprintf(title,"%s (%.2f<%s<%.2f)",basetitle,mmin,slicename,mmax);
  p->SetTitle(title);
  p->SetName(name);
  
  return p;

}

TProfile* Rms(TH2* h2,const char* basename,
	      const char* projecttype)
{
  char name[100], xtitle[100];
  TProfile* p;
  if(strstr(projecttype,"x")){
    p=h2->ProfileX("dummy",1,h2->GetNbinsY(),"s");
    strcpy(xtitle,h2->GetXaxis()->GetTitle());
  }
  else if(strstr(projecttype,"y")){
    p=h2->ProfileY("dummy",1,h2->GetNbinsX(),"s");
    strcpy(xtitle,h2->GetYaxis()->GetTitle());
  }
  else{
    cout << "wrong type " << projecttype << endl; exit(1);
  }
  sprintf(name,"%s%s_%s",h2->GetName(),basename,projecttype);
  p->SetName(name);
  sprintf(name,"%s_%s_rms",h2->GetName(),basename,projecttype);
  Int_t nBin= p->GetNbinsX();
  TH1* h1 = new TH1D(name,name,nBin,
		     p->GetXaxis()->GetBinLowEdge(1),
		     p->GetXaxis()->GetBinUpEdge(nBin));
  // now loop over the tprofile and fill the histogram
  for(int i=1;i<=nBin;i++){
    h1->SetBinContent(i,p->GetBinError(i));
    // set the error as 1% of the content
    h1->SetBinError(i,p->GetBinContent(i)*.001);
  }
  h1->SetXTitle(xtitle);
  return h1;
}

TH1D* Rms(TH3* h3,const char* basename, const char* basetitle,
	      const char* slicename, Axis_t min, Axis_t max,
	      const char* slicetype,
	      const char* projecttype)
{
  char name[200]; sprintf(name,"%s_prms",basename);
  TProfile* p = Profile(h3,name,basetitle,slicename,
			min,max,slicetype,projecttype,"s");
  
  // make a 1 d histogram
  
  sprintf(name,"%s_h1",p->GetName());
  Int_t nBin= p->GetNbinsX();
  TH1D* h1 = new TH1D(name,p->GetTitle(),nBin,
		     p->GetXaxis()->GetBinLowEdge(1),
		     p->GetXaxis()->GetBinUpEdge(nBin));

  // now loop over the tprofile and fill the histogram
  for(int i=1;i<=nBin;i++){
    h1->SetBinContent(i,p->GetBinError(i));
    // set the error as 1% of the content
    h1->SetBinError(i,p->GetBinContent(i)*.001);
  }
  h1->SetXTitle(p->GetXaxis()->GetTitle());
  return h1;

}

void
Printps(TCanvas* c1, const char* outDir, TString basename)
{

  if(!basename.EndsWith(".ps")) basename += ".ps";
  TString s = outDir; s += "/"; s += basename;
  c1->Print(s.Data());

}

void
Printgif(TCanvas* c1, const char* outDir, TString basename)
{

  if(!basename.EndsWith(".gif")) basename += ".gif";
  TString s = outDir; s += "/"; s += basename;
  c1->Print(s.Data());

}

// errors are not reset...
void
Rebin(TH3* h3,int axisType,int nBin, Axis_t* ary)
{
  if(!h3) { cout << "null pointer?" << endl; return; }


  // clone the histogram
  TH3* hClone = (TH3*)h3->Clone();
  TString name = h3->GetName(); name += "Clone";

  hClone->SetName(name.Data());

  TAxis *rebinAxis=0, *aAxis=0, *bAxis=0;
  TAxis *xAxis=hClone->GetXaxis(), 
    *yAxis=hClone->GetYaxis(), *zAxis=hClone->GetZaxis();
  TAxis *newAxis,*newAaxis,*newBaxis;

  switch(axisType){
  case 0: 
    rebinAxis=xAxis; aAxis=yAxis; bAxis=zAxis; 
    newAxis=h3->GetXaxis(); newAaxis=h3->GetYaxis(); newBaxis=h3->GetZaxis();
    break;
  case 1: 
    rebinAxis=yAxis; aAxis=xAxis; bAxis=zAxis; 
    newAxis=h3->GetYaxis(); newAaxis=h3->GetXaxis(); newBaxis=h3->GetZaxis();
    break;
  case 2: 
    rebinAxis=zAxis; aAxis=xAxis; bAxis=yAxis; 
    newAxis=h3->GetZaxis(); newAxis=h3->GetXaxis(); newBaxis=h3->GetYaxis();
    break;
  default: cout << "Wrong axis type: " << axisType << endl; exit(-1);
  }

  
  // set new axis of the original histogram
  cout << "old axis " << rebinAxis->GetNbins() << endl;
  newAxis->Set(nBin,ary);
  cout << "new axis " << newAxis->GetNbins() << endl;

  int aNbin=aAxis->GetNbins(), bNbin=bAxis->GetNbins();
  double* aAry = new double[aAxis->GetNbins()+1];
  double* bAry = new double[bAxis->GetNbins()+1];

  for(int i=0; i<aNbin; i++){
    int bin=i+1;
    aAry[i]=aAxis->GetBinLowEdge(bin);
  }
  aAry[aNbin] = aAxis->GetBinUpEdge(aNbin);
  newAaxis->Set(aNbin,aAry);

  for(int i=0; i<bNbin; i++){
    int bin=i+1;
    bAry[i]=bAxis->GetBinLowEdge(bin);
  }
  bAry[bNbin] = bAxis->GetBinUpEdge(bNbin);
  newBaxis->Set(bNbin,bAry);

  delete aAry; delete bAry;

  // now fill it

  double newval=0;
  int iBinXreal=0, iBinYreal=0, iBinZreal=0, bin=0;
  
  for(int aBin=1; aBin<=aAxis->GetNbins(); aBin++){
    for(int bBin=1; bBin<=bAxis->GetNbins(); bBin++){
      for(int newBin=1; newBin<=nBin; newBin++){
	newval=0;
	
	for(int rebinBin=1; rebinBin<=rebinAxis->GetNbins(); rebinBin++){
	  if(newAxis->FindBin(rebinAxis->GetBinCenter(rebinBin))==newBin){
	    switch(axisType){
	    case 0: iBinXreal=rebinBin; iBinYreal=aBin; iBinZreal=bBin; break;
	    case 1: iBinXreal=aBin; iBinYreal=rebinBin; iBinZreal=bBin; break;
	    case 2: iBinXreal=aBin; iBinYreal=bBin; iBinZreal=rebinBin; break;
	    }
	    bin = hClone->GetBin(iBinXreal, iBinYreal, iBinZreal);
	    newval += hClone->GetBinContent(bin);
	  }
	}
	// set new bin content
	switch(axisType){
	case 0: iBinXreal=newBin; iBinYreal=aBin; iBinZreal=bBin; break;
	case 1: iBinXreal=aBin; iBinYreal=newBin; iBinZreal=bBin; break;
	case 2: iBinXreal=aBin; iBinYreal=bBin; iBinZreal=newBin; break;
	}
	bin = h3->GetBin(iBinXreal,iBinYreal,iBinZreal);
	h3->SetBinContent(bin,newval);
	
      } // newBin
    }
  }
  // reality check
  /*
  TH1* h1Clone=hClone->Project3D("x");
  TH1* h1New  =h3->Project3D("x");

  for(int i=1; i<=h1New->GetNbinsX(); i++){
    cout << "new bin " << i 
	 << ",low = "<< h1New->GetXaxis()->GetBinLowEdge(i)
	 << ",high = " << h1New->GetXaxis()->GetBinUpEdge(i)
	 << ",val = " << h1New->GetBinContent(i) << endl;
    double sum=0;
    for(int j=1; j<=h1Clone->GetNbinsX(); j++){
      if(h1New->GetXaxis()->FindBin(h1Clone->GetXaxis()->GetBinCenter(j))==i){
	cout << "\told bin " << j
	     << ",low = " << h1Clone->GetXaxis()->GetBinLowEdge(j)
	     << ",high = " << h1Clone->GetXaxis()->GetBinUpEdge(j)
	     << ",val = " << h1Clone->GetBinContent(j) << endl;
	sum+= h1Clone->GetBinContent(j);
      }
    }
    cout << "\tsum = " << sum << endl;
  }
  */
  
  // get rid of the clone
  delete hClone; delete aAry; delete bAry; 
  
}
//#endif

//____________________

void
dump(TH1* h1)
{
  cout << h1->GetName() << endl;
  Int_t nBin = h1->GetNbinsX();

  for(Int_t i=1; i<=nBin; i++){
    float errorFrac = (h1->GetBinContent(i)>0)?
      h1->GetBinError(i)/h1->GetBinContent(i) : 0;
    
    printf("\t%d : %.4f -- %.4f \t%.4f \t%.4f \t%.4f\n",
	   i,
	   h1->GetXaxis()->GetBinLowEdge(i),
	   h1->GetXaxis()->GetBinUpEdge(i),
	   h1->GetBinContent(i),
	   h1->GetBinError(i),
	   errorFrac);
  }
}

void 
dump(TGraph* g)
{
  cout << g->GetName() << endl;
  double *x = g->GetX();
  double *y = g->GetY();
  
  for(int i=0; i<g->GetN(); i++){
    printf("\t%d : %.4f \t %.4f\n",i,x[i],y[i]);
  }
}

void 
dump(TGraphAsymmErrors* graph)
{
  double* xValues = graph->GetX();
  double* yValues = graph->GetY();
  double* errXLow = graph->GetEXlow();
  double* errXHigh = graph->GetEXhigh();
  
  double* errYLow  = graph->GetEYlow();
  double* errYHigh = graph->GetEYhigh();

  for(int i=0; i<graph->GetN(); i++){
    printf("\t%d : %.4f < %.4f <%.4f \t %.3e %.3e\n",
	   i,xValues[i]-errXLow[i],xValues[i],xValues[i]+errXHigh[i],
	   yValues[i],errYLow[i]);
  }


}


void
showHistogramValues(TH1D* h1, char* more)
{
  *ofVal << "#################################################" << endl;
  *ofVal << h1->GetName();
  if(more) *ofVal << ":::::"<<more << endl;
  else *ofVal << endl;

  Int_t nBin = h1->GetNbinsX();

  Double_t errorFrac;

  // no under and over flows

  for(Int_t i=1; i<=nBin; i++){
    errorFrac = (h1->GetBinContent(i)>0)?
      h1->GetBinError(i)/h1->GetBinContent(i) : 0;

    *ofVal << "\t" << h1->GetXaxis()->GetBinLowEdge(i) << " < pt < "
	   << h1->GetXaxis()->GetBinUpEdge(i) << "    "
	   << "\t" << h1->GetBinContent(i) << "  \t" << h1->GetBinError(i)
	   << "\t" << errorFrac << endl;
  }

}

//____________________

void 
showTGraphValues(TGraphAsymmErrors* graph,char* more)
{
  *ofVal << "#################################################" << endl;
  *ofVal << graph->GetName() << ":::::" << more << endl;
  
  const Int_t nBin = graph->GetN();
  Double_t* xValues = graph->GetX();
  Double_t* yValues = graph->GetY();
  Double_t* errYHigh = graph->GetEYhigh();
  Double_t* errXLow = graph->GetEXlow();
  Double_t* errXHigh = graph->GetEXhigh();

  Double_t lowEdge, upEdge, errorFrac;

  for(Int_t i=0; i<nBin; i++){
    lowEdge = xValues[i] - errXLow[i];
    upEdge  = xValues[i] + errXHigh[i];
    
    errorFrac = errYHigh[i]/yValues[i];

    *ofVal << "\t" << lowEdge << "<" << xValues[i] << "<" << upEdge
	   << "\t" << yValues[i] << "\t" << errYHigh[i] << "\t" << errorFrac 
	   << endl;
  }
}
// for each x slice, divide the y bins by the integral of the x slice
void scale2D(TH2* h2)
{
  TAxis *xAxis=h2->GetXaxis();
  TAxis *yAxis=h2->GetYaxis();

  for(int ix=1; ix<=xAxis->GetNbins(); ix++){
    // first get the sum of this slice
    float sum=0;
    for(int iy=1; iy<=yAxis->GetNbins(); iy++){
      int bin=h2->GetBin(ix,iy);
      sum += h2->GetBinContent(bin);
    }
    // now scale
    for(int iy=1; iy<=yAxis->GetNbins(); iy++){
      int bin=h2->GetBin(ix,iy);
      if(sum)
	h2->SetBinContent(bin,h2->GetBinContent(bin)/sum);
    }
  }
}

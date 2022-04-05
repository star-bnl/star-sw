
int DEBUG=0;

TGraphErrors* ua1Data200();

TF1* ua1Fit130(scale=1);

void
scale(TGraphAsymmErrors* graph,double scale);

void
scale(TGraphErrors* graph,double scale);

// divide by a float (e.g. TAA)
void
divide(TGraphASymmErrors* graph, double div, double divErr=0);

// sets default
// type==1 : lower limit of ua1
TGraphAsymmErrors*
makeUA1(const TGraphAsymmErrors* graph, int type=0);

TGraphAsymmErrors*
divide(TGraphAsymmErrors* graphA,TGraphAsymmErrors* graphB);

// scale is the overall scale of the ratio
// type is which ua1 spectrum to create
TGraphAsymmErrors*
makeUA1Ratio(TGraphAsymmErrors* gSTAR,
	     int type, double scale);

// divide ua1 ratio by TAA
TGraphAsymmErrors*
makeUA1RatioDiv(TGraphAsymmErrors* gSTAR,
		double TAA, double TAAErr=0);

// error on TAA
TH1D*
makeUA1TAAHistError(TGraphAsymmErrors* gSTAR,
		    double TAA, double TAAError);

// scaleLow is the lower limit of the scale
TGraphAsymmErrors*
makeUA1ScaleError(TGraphAsymmErrors* gSTAR,
		  float scale, float scaleLow,float scaleHigh);

// manuel's h-
TGraphAsymmErrors* 
makeHMinus();

// get rid of x errors
TGraphAsymmErrors*
removeXErrors(TGraphAsymmErrors* g);

// kludge. divide by background
void
kludgeBackground(TGraphAsymmErrors* graph,float val)
{
  double* xValues = graph->GetX();
  double* yValues = graph->GetY();
  double* errXLow = graph->GetEXlow();
  double* errXHigh = graph->GetEXhigh();
  
  double* errYLow  = graph->GetEYlow();
  double* errYHigh = graph->GetEYhigh();

  for(int i=0; i<graph->GetN(); i++){
    double y=yValues[i]*(1-val);
    double yErr=errYLow[i]*(1-val);

    graph->SetPoint(i,xValues[i],y);
    graph->SetPointError(i,errXLow[i],errXHigh[i],
			 yErr,yErr);

  }
}

// kludge systematic errors
void
kludgeSystematics(TGraphAsymmErrors* graph)
{
  // add 8% up to 4 GeV. 10% for the second to last bin
  // 115% for the last bin
  
  double* xValues = graph->GetX();
  double* yValues = graph->GetY();
  double* errXLow = graph->GetEXlow();
  double* errXHigh = graph->GetEXhigh();
  
  double* errYLow  = graph->GetEYlow();
  double* errYHigh = graph->GetEYhigh();

  int n=graph->GetN();
  for(int i=0; i<n; i++){
    double yErr;
 
    if(i==(n-2)) yErr=yValues[i]*.10;
    else if(i==(n-1)) yErr=yValues[i]*.15;
    else yErr=yValues[i]*.08;

    yErr += errYHigh[i];

    graph->SetPointError(i,errXLow[i],errXHigh[i],
			 yErr,yErr);

  }


}

//--------------------------------------
/*
  just scales the TGraph by some value.
  would be nice if root had this as a method fot TGraph like TH1.
 */
void
scale(TGraphAsymmErrors* graph,double scale)
{
  if(DEBUG)printf("SCALE=%.2e\n (1/SCALE=%.2e)\n",scale,1./scale);
  double* xValues = graph->GetX();
  double* yValues = graph->GetY();
  double* errXLow = graph->GetEXlow();
  double* errXHigh = graph->GetEXhigh();
  
  double* errYLow  = graph->GetEYlow();
  double* errYHigh = graph->GetEYhigh();

  for(int i=0; i<graph->GetN(); i++){
    double y=yValues[i], eYLow=errYLow[i], eYHigh=errYHigh[i];
    double yScaled=yValues[i]*scale;
    double eYLowScaled = errYLow[i]*scale;
    double eYHighScaled =errYHigh[i]*scale;

    graph->SetPoint(i,xValues[i],yScaled);
    graph->SetPointError(i,errXLow[i],errXHigh[i],
			 eYLowScaled,eYHighScaled);
    if(DEBUG)
      printf("\tbin=%d : %.2f<%.2f<%.2f: y=%.2e, yScaled=%.2e\n\t\t yErrLow=%.2e, yErrLowScaled=%.2e\n",
	     i,xValues[i]-errXLow[i],xValues[i],xValues[i]+errXHigh[i],
	     y,yScaled,eYLow,eYLowScaled);
	     
  }
}

/*
  divide TGraphAsymmErrors by a float (and hopefully propagate errors).
  only symmetric y errors.
*/
void
divide(TGraphAsymmErrors* graph, double div, double divErr)
{
  if(DEBUG)printf("DIVIDE=%.2e, err=%.2e\n",div,divErr);
  double* xValues = graph->GetX();
  double* yValues = graph->GetY();
  double* errXLow = graph->GetEXlow();
  double* errXHigh = graph->GetEXhigh();
  
  double* errYLow  = graph->GetEYlow();
  double* errYHigh = graph->GetEYhigh();

  for(int i=0; i<graph->GetN(); i++){
    double y=yValues[i], eYLow=errYLow[i], eYHigh=errYHigh[i];
    double yDiv=yValues[i]/div;
    double eY = errYLow[i];
    double eYDiv = errYLow[i]/div;

    if(divErr>0){
      eYDiv=yDiv*sqrt((eY/y)*(eY/y) + (divErr/div)*(divErr/div));
    }
    

    graph->SetPoint(i,xValues[i],yDiv);
    graph->SetPointError(i,errXLow[i],errXHigh[i],
			 eYDiv,eYDiv);
    if(DEBUG)
      printf("\tbin=%d : %.2f<%.2f<%.2f: y=%.2e, yScaled=%.2e\n\t\t yErrLow=%.2e, yErrLowScaled=%.2e\n",
	     i,xValues[i]-errXLow[i],xValues[i],xValues[i]+errXHigh[i],
	     y,yDiv,eY,eYDiv);
	     
  }

}


/*
  scale for TGraphErrors
*/
void
scale(TGraphErrors* graph,double scale)
{
  if(DEBUG)printf("SCALE=%.2e\n (1/SCALE=%.2e)\n",scale,1./scale);
  double* xValues = graph->GetX();
  double* yValues = graph->GetY();
  double* errX = graph->GetEX();
  double* errY = graph->GetEY();
  
  for(int i=0; i<graph->GetN(); i++){
    double y=yValues[i],eY=errY[i];
    double yScaled=yValues[i]*scale;

    double eYScaled = errY[i]*scale;

    graph->SetPoint(i,xValues[i],yScaled);
    graph->SetPointError(i,errX[i],eYScaled);
  
    /*
    if(DEBUG)
      printf("\tbin=%d : %.2f<%.2f<%.2f: y=%.2e, yScaled=%.2e\n\t\t yErrLow=%.2e, yErrLowScaled=%.2e\n",
	     i,xValues[i]-errXLow[i],xValues[i],xValues[i]+errXHigh[i],
	     y,yScaled,eYLow,eYLowScaled);
    */
  }
}
// ua1 data 

TGraphErrors* ua1Data200()
{
  TGraphErrors* g=new TGraphErrors;
  g->SetName("gUA1Data200GeV");

  const int n=40;
  double ex[n]={0};
  double x[n],y[n],ey[n];

  
  x[0] = 0.25; y[0] = 62.0;  ey[0] = 3.1;
  x[1] = 0.35; y[1] = 36.2;  ey[1] = 1.8;
  x[2] = 0.45; y[2] = 20.8;  ey[2] = 1.0;
  x[3] = 0.55; y[3] = 12.32;  ey[3] = 0.62;
  x[4] = 0.65; y[4] = 7.33;  ey[4] = 0.37;
  x[5] = 0.75; y[5] = 4.54;  ey[5] = 0.23;
  x[6] = 0.85; y[6] = 2.77;  ey[6] = 0.14;
  x[7] = 0.95; y[7] = 1.724;  ey[7] = 0.088;
  x[8] = 1.05; y[8] = 1.162;  ey[8] = 0.060;
  x[9] = 1.15; y[9] = 0.769;  ey[9] = 0.040;
  x[10] = 1.25; y[10] = 0.521;  ey[10] = 0.028;
  x[11] = 1.35; y[11] = 0.344;  ey[11] = 0.019;
  x[12] = 1.45; y[12] = 0.240;  ey[12] = 0.013;
  x[13] = 1.55; y[13] = 0.1680;  ey[13] = 0.0095;
  x[14] = 1.65; y[14] = 0.1180;  ey[14] = 0.0069;
  x[15] = 1.75; y[15] = 0.0861;  ey[15] = 0.0053;
  x[16] = 1.85; y[16] = 0.0579;  ey[16] = 0.0038;
  x[17] = 1.95; y[17] = 0.0440;  ey[17] = 0.0030;    
  x[18] = 2.05; y[18] = 0.0308;  ey[18] = 0.0023;
  x[19] = 2.15; y[19] = 0.0223;  ey[19] = 0.0018;
  x[20] = 2.25; y[20] = 0.0167;  ey[20] = 0.0014;
  x[21] = 2.35; y[21] = 0.0143;  ey[21] = 0.0013;
  x[22] = 2.45; y[22] = 0.0092;  ey[22] = 0.0010;
  x[23] = 2.55; y[23] = 0.00575;  ey[23] = 0.00071;
  x[24] = 2.65; y[24] = 0.00558;  ey[24] = 0.00069;
  x[25] = 2.75; y[25] = 0.00376;  ey[25] = 0.00054;
  x[26] = 2.85; y[26] = 0.00362;  ey[26] = 0.00052;
  x[27] = 2.95; y[27] = 0.00322;  ey[27] = 0.00048;
  x[28] = 3.05; y[28] = 0.00191;  ey[28] = 0.00035;
  x[29] = 3.15; y[29] = 0.00179;  ey[29] = 0.00034;
  x[30] = 3.25; y[30] = 0.00126;  ey[30] = 0.00028;
  x[31] = 3.35; y[31] = 0.00155;  ey[31] = 0.00030;
  x[32] = 3.45; y[32] = 0.00072;  ey[32] = 0.00020;
  x[33] = 3.55; y[33] = 0.00060;  ey[33] = 0.00018;
  x[34] = 3.65; y[34] = 0.000286;  ey[34] = 0.000086;
  x[35] = 3.85; y[35] = 0.000389;  ey[35] = 0.000098;
  x[36] = 4.10; y[36] = 0.000172;  ey[36] = 0.000051;
  x[37] = 4.50; y[37] = 0.000114;  ey[37] = 0.000035;
  x[38] = 5.70; y[38] = 0.0000135;  ey[38] = 0.0000067;
  x[39] = 6.70; y[39] = 0.0000066;  ey[39] = 0.0000043;


  for(int i=0; i<n; i++){
    g->SetPoint(i,x[i],y[i]);
    g->SetPointError(i,ex[i],ey[i]);
  }    
  return g;
}

// UA1(130)

TF1* ua1Fit130(double scale)
{
  double A = 266.6; double p0 = 1.895; double n = 12.98;
  const char* fcn = "[3]*[0]*(1+x/[1])^-[2]";
  
  fUA1 = new TF1("fUA1)130",fcn,lowPt,highPt);
  fUA1->SetParameter(0,A);
  fUA1->SetParameter(1,p0);
  fUA1->SetParameter(2,n);
  fUA1->SetParameter(3,scale);

  return fUA1;
}

/*
  Note: this is the 130 GeV version from thomas.
  couple functions to make ua1 spectra (fitted).
  'graph' is your spectrum.(needed for binning)
  no UA1 errors set.
  2pi*A(1+pt/p0)^n .

  type 0: best guess
       1: lower limit (of ua1)
       2: upper limit

*/


TGraphAsymmErrors*
makeUA1(const TGraphAsymmErrors* graph, int type)
{
  if(DEBUG)printf("MAKEUA1 type=%d (2piA(1=pt/p0)^-n)\n",type);
  //
  // get the values from graph
  //
  const int nBin = graph->GetN();
  double* xValues = graph->GetX();
  double* yValues = graph->GetY();
  double* errXLow = graph->GetEXlow();
  double* errXHigh = graph->GetEXhigh();

  TGraphAsymmErrors* gUA1=new TGraphAsymmErrors;
  gUA1->SetName("ua1");

  // ual parameters from thomas's fit
  // no errors set
  double A,p0,n;

  switch(type){
  case 0: // best guess 
    A = 266.6; p0 = 1.895; n = 12.98; break;
  case 1: // lower limit
    A = 260.6; p0 = 2.0701; n = 13.9; break;
  case 2: // upper
    A = 270.5; p0 = 1.8053; n = 12.514; break;
  default:
    cout << "wrong type=" << type << endl; exit(1);
  }
  
  double lowPt=0, highPt=6;

  //                       A       p0     n
  const char* fcn = "2*pi*[0]*(1+x/[1])^-[2]";
  TF1* fUA1;
  fUA1 = new TF1("fUA1",fcn,lowPt,highPt);
  fUA1->SetParameter(0,A);
  fUA1->SetParameter(1,p0);
  fUA1->SetParameter(2,n);
  
  
  for(int i=0;i<nBin; i++){
    //
    // find x value
    //
    double lowEdge  = xValues[i]-errXLow[i];
    double highEdge = xValues[i]+errXHigh[i];
    double binWidth = highEdge-lowEdge;

    double value   = fUA1->Integral(lowEdge,highEdge)/binWidth;
    
    if(DEBUG)
      printf("\tbin=%d : %.2f<%.2f<%.2f : val=%.4e\n",
	     i,lowEdge,xValues[i],highEdge,value);

    gUA1->SetPoint(i,xValues[i],value);
    gUA1->SetPointError(i,errXLow[i],errXHigh[i],0,0);

  }
  return gUA1;
}

/*
  graphA/graphB.
  only sets errors based on the high errors.
*/

TGraphAsymmErrors*
divide(TGraphAsymmErrors* graphA,TGraphAsymmErrors* graphB)
{
  if(DEBUG)printf("DIVIDE\n");

  const int nBin = graphA->GetN();
  if(nBin != graphB->GetN()){
    cout << "divide(..) :different bins sizes" << endl; exit(1);
  }  
  // A
  double* xValuesA = graphA->GetX();
  double* yValuesA = graphA->GetY();
  double* errXLowA = graphA->GetEXlow();
  double* errXHighA = graphA->GetEXhigh();
  double* errYA  = graphA->GetEYhigh();
  
  // B
  double* xValuesB = graphB->GetX();
  double* yValuesB = graphB->GetY();
  double* errYB  = graphB->GetEYhigh();

  TGraphAsymmErrors* gRatio = new TGraphAsymmErrors;
  gRatio->SetName("gRatio"); gRatio->SetTitle("gRatio");

  for(int i=0; i<nBin; i++){
    if(yValuesB[i]==static_cast<double>(0)) {
      cout << "divide: cant divide by 0" << endl;
    }
    double yRatio = yValuesA[i]/yValuesB[i];

    double errY = 
      sqrt(yRatio*yRatio*(
			 errYA[i]*errYA[i]/(yValuesA[i]*yValuesA[i])+
			 errYB[i]*errYB[i]/(yValuesB[i]*yValuesB[i])
			 )
	   );
    gRatio->SetPoint(i,xValuesA[i],yRatio);
    gRatio->SetPointError(i,errXLowA[i],errXHighA[i],
			  errY,errY);
    double low=xValuesA[i]-errXLowA[i];
    double high=xValuesA[i]+errXHighA[i];
    if(DEBUG)
      printf("\tbin=%d : %.2f<%.2f<%.2f : \n\t\tvalA=%.2e, valB=%.2e, A/B=%.2e\n\t\t errA=%.2e, errB=%.2e, errA/B=%.2e\n",
	     i,low,xValuesA[i],high,yValuesA[i],yValuesB[i],
	     yRatio,errYA[i],errYB[i],errY);

  }
  return gRatio;
}


/*
  STAR/UA1
  'type' - see makeUA1 (0 is best guess, 1 is lower limit, 2 is upper limit)
  'scale' is the overall scale of the ratio.
  e.g. for central, we want scale = 1./TAA.
 */

TGraphAsymmErrors*
makeUA1Ratio(TGraphAsymmErrors* gSTAR,
	     int type, double scale)
{
  TGraphAsymmErrors* gUA1=makeUA1(gSTAR,type);
  TGraphAsymmErrors* gRatio=divide(gSTAR,gUA1);
  double *y=gRatio->GetY();
  scale(gRatio,scale);
  return gRatio;

}
/*
  STAR/UA1 - but divide by TAA. TAA errors optional
*/
TGraphAsymmErrors*
makeUA1RatioDiv(TGraphAsymmErrors* gSTAR,
		double TAA, double TAAErr)
{
  TGraphAsymmErrors* gUA1=makeUA1(gSTAR,0);
  TGraphAsymmErrors* gRatio=divide(gSTAR,gUA1);
  divide(gRatio,TAA,TAAErr);
  return gRatio;
}


TH1D* 
makeUA1TAAHistError(TGraphAsymmErrors* gSTAR,
		    double TAA, double TAAErr)
{
  if(DEBUG)printf("makeUA1TAAHistError\n");


  // make a ratio with the TAA errors included
  TGraphAsymmErrors* gRatioErr=makeUA1RatioDiv(gSTAR,TAA,TAAErr);
  
  double* xValues = gRatioErr->GetX();
  double* yValues = gRatioErr->GetY();
  double* errXLow = gRatioErr->GetEXlow();
  double* errXHigh = gRatioErr->GetEXhigh();
  
  double* errYLow  = gRatioErr->GetEYlow();
  double* errYHigh = gRatioErr->GetEYhigh();

  // find the bins of the hist
  const int nAry=gRatioErr->GetN()+1;
  const int nBin=nAry-1;
  double* xAry=new double[nAry];
  double* yAry=new double[nBin];// by bin (0 is the first bin)
  double* yErr=new double[nBin];// by bin ...

  for(int i=0;i<nBin;i++){
    float lowEdge=xValues[i]-errXLow[i];
    xAry[i]=lowEdge;
    yAry[i]=yValues[i];
    yErr[i]=errYLow[i]; // assume symmetric    
  }
  // the upper edge
  xAry[nAry-1]=xValues[gRatioErr->GetN()-1]+errXHigh[gRatioErr->GetN()-1];

  // now make the 1d hist
  TH1D* h=new TH1D("h","h",nBin,xAry);
  for(int i=0;i<nBin;i++){
    //    cout << h->GetXaxis()->GetBinLowEdge(i+1) 
    //	 << "--" << h->GetXaxis()->GetBinUpEdge(i+1)<<endl;
    h->SetBinContent(i+1,yAry[i]);
    h->SetBinError(i+1,yErr[i]);
  }
  if(DEBUG) dump(h);
  h->SetFillColor(17);


  return h;

}

/*
  makes graph with total errors.
  scaleLow,scaleHigh is the error on the scale in absolute terms.
  i.e. TAA=26+/-2. scale=1./26., scaleLow=1./28 (will give a lower
  value to the ratio)
 */

TGraphAsymmErrors*
makeUA1ScaleError(TGraphAsymmErrors* gSTAR,
		  float scale, float scaleLow,float scaleHigh)
{
  if(DEBUG)printf("ERROR\n");
  if(DEBUG)printf("BEST\n");
  TGraphAsymmErrors* gBestRatio=makeUA1Ratio(gSTAR,0,scale);

  // dont get confused. gLowRatio is the smaller ratio
  // or larger denominator.(upper limit of UA1, lower scale)
  //
  if(DEBUG)printf("LOW RATIO\n");
  TGraphAsymmErrors* gLowRatio=makeUA1Ratio(gSTAR,2,scaleLow); //upper ua1
  if(DEBUG)printf("HIGH RATIO\n");
  TGraphAsymmErrors* gHighRatio=makeUA1Ratio(gSTAR,1,scaleHigh);

  // clone the best ratio
  // all the points are the same except for the errors.
  // the high errors will be:
  // gHighRatio-gBestRatio + gBestRatio error.
  // similar for low errors.
  
  TGraphAsymmErrors* gError=
    (TGraphAsymmErrors*)gBestRatio->Clone();
  gError->SetName("gError");gError->SetTitle("gError");
  
  // ey best (should just be star's errors)
  double* eYBest=gBestRatio->GetEYlow();
  
  // yvalues
  double* yValuesBest=gBestRatio->GetY();
  double* yValuesLow=gLowRatio->GetY();
  double* yValuesHigh=gHighRatio->GetY();    

  // x values
  double* xValues=gBestRatio->GetX();
  double* eXHigh=gBestRatio->GetEXhigh();
  double* eXLow=gBestRatio->GetEXlow();
  
  if(DEBUG)printf("SET ERRORS\n");

  for(int i=0; i<gBestRatio->GetN(); i++){

    double eYHigh=yValuesHigh[i]-yValuesBest[i]; // should be positive
    double eYLow=yValuesBest[i]-yValuesLow[i];
    gError->SetPointError(i,0,0,eYLow+eYBest[i],eYHigh+eYBest[i]);

    if(DEBUG)
      printf("\tbin=%d : %.2f<%.2f<%.2f : yBest=%.2e\n\t\teYhigh=%.2e, eYlow=%.2e, eYbest=%.2e\n",
	     i,xValues[i]-eXLow[i],xValues[i],xValues[i]+eXHigh[i],
	     yValuesBest[i],eYHigh,eYLow,eYBest[i]);
    
  }
  return gError;

}


// manuel's h-
// frank's h-+h+/2
// 0 is minbias
// 1 is 60-80   (add 70-80 and 60-70 / 2)
// 2 is central
 
TGraphAsymmErrors* 
makeHMinus(int cent) 
{
  cout << "###makeHMinus : " << cent << endl;

  char* fname="~/afs/real/PJacobs.root";

  TFile* frank = new TFile(fname);
  if(!frank ||!frank->IsOpen()) exit(1);

  TGraphAsymmErrors* gHMinus = new TGraphAsymmErrors;
  gHMinus->SetName("hMinus");

  char buf[100];  char* basename="hPlusMinus";
  TH1* hMinus, *hMinusb;

  // change cases later.
  switch(cent){
  case 0: // minbias
    sprintf(buf,"%s_00",basename); 
    hMinus=(TH1*)frank->Get(buf);
    break;
  case 1: // 60-80
    sprintf(buf,"%s_02",basename);
    hMinus=(TH1*)frank->Get(buf);
    sprintf(buf,"%s_03",basename);
    hMinusb=(TH1*)frank->Get(buf);
    hMinus->Add(hMinusb); hMinus->Scale(0.5);
    break;
  case 2: // central
    sprintf(buf,"%s_10",basename);
    hMinus=(TH1*)frank->Get(buf);
    break;
  case 3: // 10-20
    sprintf(buf,"%s_08",basename);
    hMinus=(TH1*)frank->Get(buf);
    break;
  case 4: // 20-30
    sprintf(buf,"%s_07",basename);
    hMinus=(TH1*)frank->Get(buf);
    break;
  case 5: // 30-40
    sprintf(buf,"%s_06",basename);
    hMinus=(TH1*)frank->Get(buf);
    break;
  case 6: // 40-60
    sprintf(buf,"%s_05",basename);
    hMinus=(TH1*)frank->Get(buf);
    sprintf(buf,"%s_04",basename);
    hMinusb=(TH1*)frank->Get(buf);
    hMinus->Add(hMinusb); hMinus->Scale(0.5);
    break;
  default:
    cout << "Unknown cent = " << cent << endl; exit(1);
  }
  float lastpt=2.0;
  int firstbin=2;

  //  dump(hMinus);

  for(int i=firstbin; i<=hMinus->GetNbinsX(); i++){
    double y=hMinus->GetBinContent(i);
    double yerr=hMinus->GetBinError(i);
    double x  =hMinus->GetBinCenter(i);
    double xerr=hMinus->GetBinWidth(i)/2.;

    if(x>lastpt) break;
    gHMinus->SetPoint(i-firstbin,x,y);
    gHMinus->SetPointError(i-firstbin,xerr,xerr,yerr,yerr);

  }
  /*
  gHMinus->SetPoint(0,.15,2226);   gHMinus->SetPointError(0,.05,.05,94.84,94.84);
  gHMinus->SetPoint(1,.25,1729.07);gHMinus->SetPointError(1,.05,.05,36.92,36.92);
  gHMinus->SetPoint(2,.35,1117.08);gHMinus->SetPointError(2,.05,.05,26.09,26.09);
  gHMinus->SetPoint(3,.45,732.16); gHMinus->SetPointError(3,.05,.05,13.7,13.7);
  gHMinus->SetPoint(4,.55,478.81); gHMinus->SetPointError(4,.05,.05,7.30,7.30);
  gHMinus->SetPoint(5,.65,304.32); gHMinus->SetPointError(5,.05,.05,5.99,5.99);
  gHMinus->SetPoint(6,.75,204.69); gHMinus->SetPointError(6,.05,.05,3.97,3.97);
  gHMinus->SetPoint(7,.85,139.48); gHMinus->SetPointError(7,.05,.05,4.62,4.62);
  gHMinus->SetPoint(8,.95,97.62);  gHMinus->SetPointError(8,.05,.05,2.04,2.04);
  gHMinus->SetPoint(9,1.05,68.36); gHMinus->SetPointError(9,.05,.05,1.86,1.86);
  gHMinus->SetPoint(10,1.15,46.96);gHMinus->SetPointError(10,.05,.05,1.48,1.48);
  gHMinus->SetPoint(11,1.25,34.15);gHMinus->SetPointError(11,.05,.05,1.15,1.15);
  gHMinus->SetPoint(12,1.35,23.82);gHMinus->SetPointError(12,.05,.05,0.98,0.98);
  gHMinus->SetPoint(13,1.45,16.61);gHMinus->SetPointError(13,.05,.05,0.93,0.93);
  gHMinus->SetPoint(14,1.55,11.72);gHMinus->SetPointError(14,.05,.05,0.74,0.74);
  gHMinus->SetPoint(15,1.65,8.69); gHMinus->SetPointError(15,.05,.05,0.46,0.46);
  gHMinus->SetPoint(16,1.75,6.20); gHMinus->SetPointError(16,.05,.05,0.43,0.43);
  gHMinus->SetPoint(17,1.85,4.44); gHMinus->SetPointError(17,.05,.05,0.39,0.39);
  gHMinus->SetPoint(18,1.95,3.38); gHMinus->SetPointError(18,.05,.05,0.32,0.32);
  */
  return gHMinus;
}

TGraphAsymmErrors*
removeXErrors(TGraphAsymmErrors* g)
{
  TGraphAsymmErrors* gClone=(TGraphAsymmErrors*)g->Clone();

  double* eYHigh=gClone->GetEYhigh();
  double* eYLow=gClone->GetEYlow();

  for(int i=0;i<g->GetN(); i++){
    gClone->SetPointError(i,0,0,eYLow[i],eYHigh[i]);
  }
  return gClone;
}

void drawAxisBins(TGraphAsymmErrors* g,float size,float yMax)
{
  
  TLine* li=new TLine; li->SetLineWidth(1);

  double* x=g->GetX();
  double* eXLow=g->GetEXlow();
  double* eXHigh=g->GetEXhigh();

  // get the bin edges from the tgraph
  for(int i=0; i<g->GetN(); i++){
    double low = x[i]-eXLow[i];
    double high= x[i]+eXHigh[i];

    //    cout << low << "<" << x[i] << "<" << high << endl;
    //   cout << yMax << endl;
    li->DrawLine(low,yMax-size,low,yMax);
    li->DrawLine(high,yMax-size,high,yMax);
  }

}

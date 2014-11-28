#include "commonmacro/common.h"
#include "common/Name.cc"
#include "commonmacro/histutil.h"

void makeMinbiasUA1Ratio(
			 const char* inName=
			 "links/P01hi.minbias.2000.hist/hianalysis_1000.hist.root",
			 const char* psDir="ps",
			 int cut = 1,
			 const char* outDir="./",
			 const char* more = "west",
			 float extraValue = 10)
{
  cout << "--------------------------" << endl;
  cout << "in name=" << inName << endl
       << "ps dir=" << psDir << endl
       << "cut=" << cut << endl;
  cout << "--------------------------" << endl;
 
  inRoot = new TFile(inName);
  
  if(!inRoot){
    cout << "cannot find the infile" << endl;
    return;
  }
  gStyle->SetOptStat(0);gStyle->SetPadTickX(1);gStyle->SetPadTickY(1);
  TCanvas c1("c1","c1",500,400);
  //-------------------------------------------------------
  const int nbin=2;
  TGraphAsymmErrors* gSpec[2]; //plus+minus
  TGraphAsymmErrors* gSpecPM[2][2]; // plus,minus
  TGraphAsymmErrors* gUA1, *gRatio;

  float min=0.0,max=1.2;

  float geom=7200;
  // get stuff
  
  for(int ibin=0;ibin<nbin;ibin++){
    setName(name,"gSpecCorrected",ibin);
    gSpec[ibin]=(TGraphAsymmErrors*)inRoot.Get(name); 
    cout << gSpec[ibin]->GetName() << endl;
    if(!gSpec[ibin]) return;
    for(int ipm=0;ipm<2;ipm++){
      setName(name,"gSpecCorrected",ibin,sPM[ipm].Data());
      cout << name << endl;
      gSpecPM[ibin][ipm]=(TGraphAsymmErrors*)inRoot.Get(name); 
      if(!gSpecPM[ibin][ipm]) return;
    }
  }
  // ratios
  for(int ibin=0;ibin<2; ibin++){
    cout << "setting title"<< endl;
    sprintf(title,"%s / UA1 (bin %d)(cut %d)",
	    gSpec[ibin]->GetName(),ibin,cut);
    Divide(&c1,1,1,title,inName);
    cout << "making" << endl;
    gUA1=makeUA1(gSpec[ibin]);
    cout << "done" << endl;
    cout << "making ratio" << endl;
    gRatio=makeRatio(gSpec[ibin],gUA1);
    cout << "done " << endl;

    double* x=gRatio->GetX(); double* y=gRatio->GetY();
    double* exHigh=gRatio->GetEXhigh();
    double* exLow =gRatio->GetEXlow();
    double* ey=gRatio->GetEYhigh();
    for(int i=0; i<gRatio->GetN(); i++){
      gRatio->SetPoint(i,x[i],y[i]*geom);
      gRatio->SetPointError(i,exLow[i],exHigh[i],ey[i]*7200,ey[i]*7200);
    }



    gRatio->Draw("ap");
    sprintf(title,"%sOverUA1",gSpec[ibin]->GetName());
    gRatio->SetMaximum(max); gRatio->SetMinimum(min);
    Print(&c1,psDir,title);

    /*
    for(int ipm=0;ipm<2;ipm++){
      //     sName=gSpec[ibin][ipm]->GetName(); // charge sign
      // sName.Replace(0,sName.First("."),""); 
      //sName.Replace(sName.First("."),sName.Length(),"");
      sprintf(title,"%s / UA1 (bin %d)(cut %d)",
	      sName.Data(),gSpecPM[ibin][ipm]->GetName(),ibin,cut);
      Divide(&c1,1,1,title,inName);
      gRatio=makeRatio(gSpec,gUA1);
      gRatio->Scale(geom);
    
      gRatio->Draw("ap");
      gRatio->SetMaximum(max); gRatio->SetMinimum(min);
 
      sprintf(title,"%sOverUA1",gSpecPM[ibin][ipm]->GetName());
      Print(&c1,psDir,title);
      
    }
    */
  }
  



}


TGraphAsymmErrors*
makeUA1(const TGraphAsymmErrors* graph)
{
  
  //
  // find the bins
  //
  const Int_t nBin = graph->GetN();
  Double_t* xValues = graph->GetX();
  Double_t* yValues = graph->GetY();
  Double_t* errXLow = graph->GetEXlow();
  Double_t* errXHigh = graph->GetEXhigh();

  // 
  // ratios
  //
  /*
   1.500000      0.6146703      6.9558643E-02  0.1131642    
   2.000000      0.5883511      8.5044047E-03  1.4454641E-02
   2.500000      0.5589531      1.7606174E-03  3.1498480E-03
   3.000000      0.5283710      4.8784356E-04  9.2329737E-04
   3.500000      0.5029035      1.6314255E-04  3.2440131E-04
   4.000000      0.4790372      6.2226289E-05  1.2989865E-04
   4.500000      0.4574749      2.6327352E-05  5.7549289E-05
   5.000000      0.4378719      1.2056932E-05  2.7535298E-05
   5.500000      0.4186996      5.9010908E-06  1.4093855E-05
   6.000000      0.4019553      3.0456495E-06  7.5770845E-06
  */

  sprintf(name,"h1tmp%s",graph->GetName());
  TH1D* h1 = new TH1D(name,name,9,1.5,6.0);

  h1->SetBinContent(1,.61467);
  h1->SetBinContent(2,.58835);
  h1->SetBinContent(3,.55895);
  h1->SetBinContent(4,.52837);
  h1->SetBinContent(5,.50290);
  h1->SetBinContent(6,.47903);
  h1->SetBinContent(7,.45747);
  h1->SetBinContent(8,.43787);
  h1->SetBinContent(9,.40195);

  
  // fixed
  Double_t A  = 266.6;
  Double_t p0 = 1.895;
  Double_t n  = 12.98;
  

  // old
  /*
  double A=286;
  double p0=1.8;
  double n=12.14;
  */
  Double_t TAA = 26;
  double A_ion=197;

  char func[200], mean[200];
  
  sprintf(func,"2*pi*%.1f*%.0f*(1+x/%.2f)^(-%.2f)",A,A_ion*A_ion,p0,n);

  sprintf(mean,"2*pi*x*%.1f*%.0f*(1+x/%.2f)^(-%.2f)",A,TAA,p0,n);


  TF1* fUA1 = new TF1("fUA1",func,lowPt,highPt);
  TF1* fMean = new TF1("fUA1Mean",mean,lowPt,highPt);

  gUA1 = new TGraphAsymmErrors;

  for(Int_t i=0; i<nBin; i++){
    //
    // find the x value
    //
    Double_t lowEdge = xValues[i]-errXLow[i];
    Double_t highEdge = xValues[i]+errXHigh[i];


    Double_t num = fMean->Integral(lowEdge,highEdge);
    Double_t denom = fUA1->Integral(lowEdge,highEdge);

    //
    // just use star's x
    //

    Double_t UA1ErrLow = fabs(lowEdge-xValues[i]);
    Double_t UA1ErrHigh= fabs(highEdge-xValues[i]);

    //Double_t y = fUA1->Eval(xValues[i]);
    double y = denom/(highEdge-lowEdge);

    // ratio

    //Int_t iBin = h1->GetXaxis()->FindBin(xValues[i]);
    //Double_t fraction = h1->GetBinContent(iBin);
    
    //    cout << "ratio : " << h1->GetXaxis()->GetBinLowEdge(iBin) << " " 
    // << h1->GetXaxis()->GetBinUpEdge(iBin) << " = " << fraction << endl;

    
    //Double_t yCorrected = fraction*y;
    double yCorrected = y;

    cout << "UA1 uncorrected " << y << ", corrected y " << yCorrected  
	 << " graph y " << yValues[i] << endl;

    gUA1->SetPoint(i,xValues[i],yCorrected);
    gUA1->SetPointError(i,UA1ErrLow,UA1ErrHigh,0,0);
  }    
  
  gUA1->SetName("gUA1Spectrum");
  gUA1->SetTitle("gUA1Spectrum");

  return gUA1;

}

TGraphAsymmErrors*
makeRatio(TGraphAsymmErrors* gStar, TGraphAsymmErrors* gUA1)
{
  cout << "********************************" << endl;
  cout << gStar->GetName() << endl;
  const Int_t nBin = gStar->GetN();

  Double_t* starY = gStar->GetY();
  Double_t* starX = gStar->GetX();
  Double_t* starXErrLow = gStar->GetEXlow();
  Double_t* starXErrHigh = gStar->GetEXhigh();
  Double_t* starYErrHigh = gStar->GetEYhigh();

  Double_t* ua1Y  = gUA1->GetY();
  Double_t* ua1YErrHigh = gUA1->GetEYhigh();

  TGraphAsymmErrors* gRatio = new TGraphAsymmErrors;

  for(Int_t i=0; i<nBin; i++){
    
    Double_t ratio = starY[i]/ua1Y[i];
    
    Double_t yError = 
      TMath::Sqrt(ratio*ratio*(starYErrHigh[i]*starYErrHigh[i]/(starY[i]*starY[i])));
    //			       ua1YErrHigh[i]*ua1YErrHigh[i]/(ua1Y[i]*ua1Y[i])));
    
    cout << "star x : " << starX[i] << endl;
    cout << "star y : " << starY[i] << " ua1 Y : " << ua1Y[i]
	 << " ratio  : " << ratio << endl
	 << "error : " << yError << endl;
    
    gRatio->SetPoint(i,starX[i],ratio);
    gRatio->SetPointError(i,starXErrLow[i],starXErrHigh[i],yError,yError);
  }

  TString sName = gStar->GetName();
  sName.Append("UA1Ratio");
  
  gRatio->SetName(sName.Data());
  gRatio->SetTitle(sName.Data());

  return gRatio;

}







#include "TMinuit.h"
#include "../../StEeFit/DeDxTree.h"
#include "binsiz.cxx"

//Carry the data information globally (cludge)
Float_t x[45],y[45],errory[45];
Int_t nflag0points = 45;
//Used For Binning Histogram
Double_t minVal;
Double_t maxVal;
//Used for seed of fit
Double_t arithmeticMean;
//Force the minuit guesses to be within this range
Double_t minMu=1.e-8;
Double_t maxMu=1.e-4;
Double_t minSigma=1.e-8;
Double_t maxSigma=1.e-4;

void Ifit()
{
    void fillArrays(const DeDxTree* myDeDx);
    void printArrays();
    Double_t doFit();
    void plotResults(TCanvas*, Double_t mean);
    Double_t callFcn();
   
    f1 = new TFile("/star/rcf/pwg/spectra/mmiller/rootfiles/DeDxTree_5000evt.root");
    c1 = new TCanvas("c1","Fit Results",200,20,600,600);
    
    DeDxTree myDeDx;
    for (int i=0; i<1 && i<DeDxTree->GetEntries(); i++) {
	cout <<"Processing Entry:\t"<<i<<endl;
	myDeDx.GetEntry(i);
	fillArrays(&myDeDx);
	Double_t mean = doFit();
	plotResults(c1, mean);
    }
    return;
}

Double_t callFcn(Double_t mean, Double_t sigma)
{
    Int_t npar=1;
    Double_t* gin=0;
    Double_t f;
    Double_t* par = new Double_t[2];
    par[0] = mean;
    par[1] = sigma;
    Int_t iflag=0;
    fcn(npar, gin, f, par, iflag);
    delete par;
    if (f>1.e9) {f=1.e9;} //Watch bounds 
    return f;
}

//Define the function to be fit (Guassian, Landau, etc)
Double_t func(float x, Double_t *par)
{
    TF1* mylandau = new TF1("mylandau","landau",0.,1.);
    mylandau->SetParameter(0,1.);
    mylandau->SetParameter(1,par[0]);
    mylandau->SetParameter(2,par[1]);
    //mylandau->SetParameter(2,1.e-6);
    Double_t value = mylandau->Eval(x);
    
    return value;
}

//Define the function to be minimized (Chi-Squared, -sum(log), etc)
void fcn(Int_t &npar, Double_t *gin, Double_t &f, Double_t *par, Int_t iflag)
{
    //Check that parameters are in bounds    
    //if ( (par[0]<minMu) || (par[0]>maxMu) || (par[1]<minSigma) || (par[1]>maxSigma) ) {
    //f=1.e9;
    //return;
    //}

    //If parameters are ok, continue
    Double_t sumLog = 0.;
    for (int i=0;i<nflag0points; i++) {
	sumLog = sumLog + -1.*log( func(y[i],par) );
    }
    f = sumLog;
    if (f>1.e9) {f=1.e9;} //Watch Return value, can return "inf"
    return;
}

Double_t doFit()
{
    int getFitStatus( char* Migrad);
    TMinuit *gMinuit = new TMinuit(2);  //initialize TMinuit with a maximum of 5 params
    gMinuit->SetFCN(fcn);

    Double_t arglist[10];
    Int_t ierflg = 0;

    arglist[0] = 1;
    gMinuit->mnexcm("SET PRI",arglist,2,ierflg);
    //gMinuit->mnexcm("SET ERR", arglist ,1,ierflg);

    gMinuit->mnparm(0, "mean", arithmeticMean, arithmeticMean/100., minMu,maxMu,ierflg); //First Guess
    gMinuit->mnparm(1, "sigma", arithmeticMean/10., arithmeticMean/1000., minSigma,maxSigma,ierflg); //First Guess
    
    arglist[0] = 500; //Probably the number of iterations allowed
    arglist[1] = 1.;  //????
    gMinuit->mnexcm("MIGRAD", arglist ,2, ierflg);     // Now ready for minimization step

    //Print Results to screen
    Double_t m, dm, s, ds;
    Int_t gotMean = gMinuit->GetParameter(0, m, dm);
    if (gotMean) {cout <<"mean:\t\t"<<m<<"\t +- "<<dm<<endl;}
    Int_t gotSigma = gMinuit->GetParameter(1, s, ds);
    if (gotSigma) {cout <<"sigma:\t\t"<<s<<"\t +- "<<ds<<endl;}
    cout <<"\narithmeticMean:\t"<<arithmeticMean<<endl;

    delete gMinuit;
    
    return m;
}

void plotResults(TCanvas* c1, Double_t mean)
{
    TH1F* makeHistogram();
    void fillHistogram(TH1F* h1);

    TH1F* h1 = makeHistogram();
    fillHistogram(h1);
    
    c1->cd();
    h1->Draw();
    TLine* myline = new TLine(mean, 0., mean, nflag0points/2.);
    myline->SetLineColor(2);
    myline->SetLineStyle(2);
    myline->Draw("same");
    c1->Update();
    
    cout <<"Enter any integer to continue"<<endl;
    int temp;
    cin>>temp;
    
    delete h1;
    delete myline;
    return;
}

TH1F* makeHistogram()
{
    int binsiz_(double *a1, double *a2, int *naa, double *bl, double *bh, int *nb, double *bwid);
    int naa = 5;
    int nBin = 0;
    double minBin, maxBin, binWidth;
    minBin = maxBin = binWidth = 0.;
    
    int returnVal = binsiz_(&minVal, &maxVal, &naa, &minBin, &maxBin, &nBin, &binWidth);
    TH1F* h1 = new TH1F("h1","de/dx of hit dist", nBin, minBin, maxBin);
    return h1;
}

void fillHistogram(TH1F* h1)
{
    for (int i=0; i<nflag0points; i++) {h1->Fill(y[i]);}
    return;
}

void fillArrays(const DeDxTree* myDeDx)
{
    nflag0points = myDeDx->m_flag0points;
    minVal = 1.e9;
    maxVal =-1.;
    arithmeticMean=0.;
    for (int ihit=0; ihit<nflag0points; ihit++) {
	x[ihit] = ihit;
	y[ihit] = myDeDx->m_hitArray_m_de[ihit] / myDeDx->m_hitArray_m_dx[ihit];
	errory[ihit] = 1.e-9;
	if (y[ihit]<minVal) {minVal = y[ihit];} //Remember the min and max
	if (y[ihit]>maxVal) {maxVal = y[ihit];}
	arithmeticMean = arithmeticMean + y[ihit];
    }
    arithmeticMean = arithmeticMean/nflag0points;
    return;
}

void printArrays()
{
    for (int i=0; i<nflag0points; i++) {cout <<x[i]<<"\t"<<y[i]<<"\t"<<errory[i]<<endl;}
    cout <<"\t MinVal:\t"<<minVal<<"\t MaxVal:\t"<<maxVal<<endl;
    return;
}

int getFitStatus( char* Migrad)
{
    int i;
    char Status[][12] = {"FAILED","PROBLEMS","CONVERGED","SUCCESSFUL"};
    for(int i=0; i<4; i++) {
	if (strstr(Migrad,Status[i]) != NULL) return i;
    }
    return -1;
}


//Extras------------------------
//cout <<"Entering Loop"<<endl;
//for (Double_t mean=1.e-7; mean<1.e-5; mean=mean+1.e-7) {
//Double_t mean = 1.e-6;
//   Double_t sigma = 1.e-6;
//  Double_t value = callFcn(mean, sigma);
//  cout<<mean<<"\t"<<value<<endl;
//  callntuple->Fill(mean, value);
//}


//Check Fit Status
//Double_t fcnmin, errdef, edmval;
//Int_t nvpar, nparx;
//int res = getFitStatus((char*)gMinuit->fCstatu.Data());
//if (res==0 || res==1 || res==-1) {cout <<"FAILED/PROBLEMS"<<endl;}
//if (res==2 || res==3) {cou t<<"CONVERGED/SUCCESSFUl"<<endl;}
    

#include "StEStructFitter.h"
#include "TMath.h"
#include "TFile.h"
#include "Stiostream.h"


ClassImp(StEStructFitter)

StEStructFitter* StEStructFitter::mInstance=0;

StEStructFitter* StEStructFitter::Instance(){

  if(!mInstance)mInstance=new StEStructFitter();
  return mInstance;

}


StEStructFitter::StEStructFitter(): mhists(NULL) {  
  mpi=3.141593; m2pi=2.0*mpi; 
  mmean=0.;//3.3935;
  msigma=0.;//1.38;//0.9745;

};

StEStructFitter::~StEStructFitter(){};

//
//-----------------------------------------------------------
double StEStructFitter::detadphiFit(double* x, double* par){

  double arg1=0;
  double arg2=0;
  double arg3=0;
  double arg4=0;
  double yval=x[1];
  double xval=x[0];


  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);

  if(par[3]!=0)arg3=(yval-mpi)/par[3];
  fitval+=par[4]*TMath::Exp(-0.5*arg3*arg3);
  fitval+=par[5];
  if(par[6]!=0)arg4=(xval)/par[6];
  fitval+=par[7]*TMath::Exp(-0.5*arg4*arg4);


  // added a small 2d-gaussian about phi_delta=pi,eta_delta=0
  // mainly for US low pt
  arg1=0;
  arg2=0;
  if(par[8]!=0)arg1=(xval)/par[8];
  if(par[9]!=0)arg2=(yval-mpi)/par[9];
  double fitval2=par[10]*TMath::Exp(-0.5*arg1*arg1);
  fitval2*=TMath::Exp(-0.5*arg2*arg2);
  fitval+=fitval2;

  if(yval<0. || yval>mpi){
    double xnew[2];
    xnew[0]=xval;
    if(yval<0){
      yval+=m2pi;
    } else {
     yval-=m2pi;
    }
    xnew[1]=yval;
    fitval+=seconddetadphiFit(xnew,par);
  }
  return fitval;
}


double StEStructFitter::seconddetadphiFit(double* x, double* par){

  double arg1=0;
  double arg2=0;
  double arg3=0;
  //  double arg4=0;
  double yval=x[1];
  double xval=x[0];


  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);

  if(par[3]!=0)arg3=(yval-mpi)/par[3];
  fitval+=par[4]*TMath::Exp(-0.5*arg3*arg3);

  // added a small 2d-gaussian about phi_delta=pi,eta_delta=0
  // mainly for US low pt
  arg1=0;
  arg2=0;
  if(par[8]!=0)arg1=(xval)/par[8];
  if(par[9]!=0)arg2=(yval-mpi)/par[9];
  double fitval2=par[10]*TMath::Exp(-0.5*arg1*arg1);
  fitval2*=TMath::Exp(-0.5*arg2*arg2);
  fitval+=fitval2;


  //  fitval+=par[5];
  //  if(par[6]!=0)arg4=(xval)/par[6];
  //  fitval+=par[7]*TMath::Exp(-0.5*arg4*arg4);


  return fitval;
}

double StEStructFitter::softLS(double* x,double* par){

  double arg1=0;
  double arg2=0;
  double yval=x[1];
  double xval=x[0];

  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);
  fitval+=par[3];
  if(yval<0. || yval>mpi){
    double xnew[2];
    xnew[0]=xval;
    if(yval<0){
      yval+=m2pi;
    } else {
     yval-=m2pi;
    }
    xnew[1]=yval;
    fitval+=secondSoftLS(xnew,par);
  }

  return fitval;
}

double StEStructFitter::secondSoftLS(double* x,double* par){

  double arg1=0;
  double arg2=0;
  double yval=x[1];
  double xval=x[0];

  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);

  return fitval;
}

double StEStructFitter::softUS(double* x,double* par){

  double arg1=0;
  double xval=x[0];

  if(par[0]!=0)arg1=(xval)/par[0];
  double fitval=par[1]*TMath::Exp(-0.5*arg1*arg1);
  fitval+=par[2];

  return fitval;
}

double StEStructFitter::syt(double* x,double* par){

  double arg1=0;
  double arg2=0;
  double xval=x[0];

  if(par[2]!=0)arg1=(xval-par[1])/par[2];
  double fitval=par[3]*TMath::Exp(-0.5*arg1*arg1);
  arg2=(xval-mmean)/msigma;
  fitval+=(par[0]*TMath::Exp(-0.5*arg2*arg2));

  return fitval;
}


//------------------------------------------------------------
double StEStructFitter::softCD(double* x,double* par){

  double arg1=0;
  double arg2=0;
  double arg3=0;
  double yval=x[1];
  double xval=x[0];

  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);
  if(par[3]!=0)arg3=(xval)/par[3];
  fitval+=par[4]*TMath::Exp(-0.5*arg3*arg3);
  fitval+=par[5];

  if(yval<0. || yval>mpi){
    double xnew[2];
    xnew[0]=xval;
    if(yval<0){
      yval+=m2pi;
    } else {
     yval-=m2pi;
    }
    xnew[1]=yval;
    fitval+=secondSoftCD(xnew,par);
  }

  return fitval;
}


double StEStructFitter::secondSoftCD(double* x,double* par){

  double arg1=0;
  double arg2=0;
  double yval=x[1];
  double xval=x[0];

  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);

  return fitval;
}

double StEStructFitter::hardCI(double* x,double* par){

  double arg1=0;
  double arg2=0;
  double arg3=0;
  double yval=x[1];
  double xval=x[0];

  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);
  if(par[3]!=0)arg3=(yval-mpi)/par[3];
  fitval+=par[4]*TMath::Exp(-0.5*arg3*arg3);
  fitval+=(par[5]-mmean*xval*xval);

  if(yval<0. || yval>mpi){
    double xnew[2];
    xnew[0]=xval;
    if(yval<0){
      yval+=m2pi;
    } else {
     yval-=m2pi;
    }
    xnew[1]=yval;
    fitval+=secondHardCI(xnew,par);
  }

  return fitval;
}


double StEStructFitter::secondHardCI(double* x,double* par){

  double arg1=0;
  double arg2=0;
  double arg3=0;
  double yval=x[1];
  double xval=x[0];

  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);
  if(par[3]!=0)arg3=(yval-mpi)/par[3];
  fitval+=par[4]*TMath::Exp(-0.5*arg3*arg3);

  return fitval;
}

double StEStructFitter::hardCICosine(double* x,double* par){

  double arg1=0;
  double arg2=0;
  double arg3=0;
  double yval=x[1];
  double xval=x[0];

  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);
  if(par[3]!=0)arg3=(yval-mpi)/par[3];
  fitval+=par[4]*TMath::Exp(-0.5*arg3*arg3);
  fitval+=(par[5]-mmean*xval*xval);

  if(yval<0. || yval>mpi){
    double xnew[2];
    xnew[0]=xval;
    if(yval<0){
      yval+=m2pi;
    } else {
     yval-=m2pi;
    }
    xnew[1]=yval;
    fitval+=secondHardCI(xnew,par);
  }

  return fitval;
}


double StEStructFitter::secondHardCICosine(double* x,double* par){

  double arg1=0;
  double arg2=0;
  double arg3=0;
  double yval=x[1];
  double xval=x[0];

  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);
  if(par[3]!=0)arg3=(yval-mpi)/par[3];
  fitval+=par[4]*TMath::Exp(-0.5*arg3*arg3);

  return fitval;
}



double StEStructFitter::detadphiSS(double* x, double* par){

  double arg1=0;
  double arg2=0;
  double yval=x[1];
  double xval=x[0];


  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);

  fitval+=par[3];

  /*
  if(yval<0. || yval>mpi){
    double xnew[2];
    xnew[0]=xval;
    if(yval<0){
      yval+=m2pi;
    } else {
     yval-=m2pi;
    }
    xnew[1]=yval;
    fitval+=seconddetadphiSS(xnew,par);
  }
  */

  return fitval;
}


double StEStructFitter::seconddetadphiSS(double* x, double* par){

  double arg1=0;
  double arg2=0;
  double yval=x[1];
  double xval=x[0];

  if(par[0]!=0)arg1=(xval)/par[0];
  if(par[1]!=0)arg2=(yval)/par[1];
  double fitval=par[2]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);

  return fitval;
}


//------------------------------------------------------

double StEStructFitter::mcComponents(double* x, double* par){

  if(!mhists){
    cout<<"opening file rmxTest2.root"<<endl;
    TFile* tf=new TFile("rmxTest2.root");
    mhists=new TH2F*[6];

    const char* snames[]={"S001","S010","S100","S011","S101","S110"};

    double xsum=0;
    for(int i=0;i<6;i++){
      mhists[i]=(TH2F*)(tf->Get(snames[i]))->Clone();
      float xval=mhists[i]->Integral();
      cout<<"scale for i="<<i<<" is "<<xval<<endl;
      xsum+=mhists[i]->Integral();
    }
    for(int i=0;i<6;i++)mhists[i]->Scale(1.0/xsum);
  }
   
  int ibin=mhists[0]->FindBin(x[0],x[1]);

  double retVal=par[0]*mhists[0]->GetBinContent(ibin);
  retVal+=par[1]*mhists[1]->GetBinContent(ibin);
  retVal+=par[2]*mhists[2]->GetBinContent(ibin);
  retVal+=par[3]*mhists[3]->GetBinContent(ibin);
  retVal+=par[4]*mhists[4]->GetBinContent(ibin);
  retVal+=par[5]*mhists[5]->GetBinContent(ibin);
  retVal*=par[6];

  return retVal;

}

//-------------------------------------------------------
double StEStructFitter::dytGsytG(double* x, double* par){


  double arg1=0;
  double arg2=0;

  double yval=x[1];
  double xval=x[0];

  if(par[0]!=0)arg1=(xval-par[1])/par[0];
  if(par[1]!=0)arg2=(yval)/par[2];

  double fitval=par[3]*TMath::Exp(-0.5*arg1*arg1);
  fitval*=TMath::Exp(-0.5*arg2*arg2);
  fitval+=par[4];

  return fitval;
}


//---------------------------------------------------------
double StEStructFitter::DoubleE(double* x, double* par){

  double arg1=0;
  double arg2=0;
  double yval=x[1];
  double xval=x[0]; // assume x independent....

  if(par[0]!=0)arg1=(yval)/par[0];
  if(par[2]!=0)arg2=(yval-mpi)/par[2];
  double fitval=par[1]*TMath::Exp(-0.5*arg1*arg1);
  fitval+=par[3]*TMath::Exp(-0.5*arg2*arg2);
  fitval+=par[4];

  if(yval<0. || yval>mpi){
    double xnew[2];
    xnew[0]=xval;
    if(yval<0){
      yval+=m2pi;
    } else {
     yval-=m2pi;
    }
    xnew[1]=yval;
    fitval+=secondDoubleE(xnew,par);
  }

  return fitval;
}


double StEStructFitter::secondDoubleE(double* x,double* par){

  double arg1=0;
  double arg2=0;
  double yval=x[1];
  //double xval=x[0];

  if(par[0]!=0)arg1=(yval)/par[0];
  if(par[2]!=0)arg2=(yval-mpi)/par[2];
  double fitval=par[1]*TMath::Exp(-0.5*arg1*arg1);
  fitval+=par[3]*TMath::Exp(-0.5*arg2*arg2);

  return fitval;
}

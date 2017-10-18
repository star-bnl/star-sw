#include "eeTowerFunction.h"
#include "TSpline.h"
#include "TMath.h" 
Double_t eeTowerFunction( Double_t *x, Double_t *p ) {

  /// note-- need to make cyclical in phi

  Double_t p0[]={ 0.235772, 0.239713, 0.239243, 0.238843, 0.24141,  0.241641, 0.2433,   0.243124, 0.242957, 0.243987, 0.244477, 0.245425, 0.245425};
  Double_t p2[]={ 0.266629, 0.283157, 0.270271, 0.257208, 0.246998, 0.236122, 0.226747, 0.218976, 0.21176,  0.205661, 0.198209, 0.197987, 0.197987};
  Double_t p3[]={ 0.497183, 0.490622, 0.491357, 0.491976, 0.492601, 0.493278, 0.493775, 0.494394, 0.495469, 0.494036, 0.493174, 0.504341, 0.504341};

  Double_t xx[]={ 0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.};

  /*
  TSpline3 *mEtaPar0=new TSpline3("EtaPar0",xx,p0,13);
  TSpline5 *mEtaPar2=new TSpline5("EtaPar2",xx,p2,13);
  TSpline5 *mEtaPar3=new TSpline5("EtaPar3",xx,p3,13);
  */
  TSpline3 mEtaPar0("EtaPar0",xx,p0,13);
  TSpline5 mEtaPar2("EtaPar2",xx,p2,13);
  TSpline5 mEtaPar3("EtaPar3",xx,p3,13);

  Double_t pp0[]={ 0.23439,  0.242071, 0.243334, 0.243623, 0.245357, 0.245606, 0.245668, 0.247,    0.247328, 0.247872, 0.248534, 0.247648, 0.247648 };
  Double_t pp2[]={ 0.296042, 0.273496, 0.248509, 0.227491, 0.208265, 0.195849, 0.189432, 0.176265, 0.166762, 0.151045, 0.14633,  0.136524, 0.136524 };
  Double_t pp3[]={ 0.490751, 0.490351, 0.492535, 0.495746, 0.496635, 0.498088, 0.497785, 0.498563, 0.499,    0.499639, 0.499539, 0.499887, 0.499887 };
  TSpline3 mPhiPar0("PhiPar0",xx,pp0,13);
  TSpline3 mPhiPar2("PhiPar2",xx,pp2,13);
  TSpline3 mPhiPar3("PhiPar3",xx,pp3,13);

  Double_t xphi=x[0];
  Double_t xeta=x[1];

  Double_t xphi0 = p[0];
  Double_t xeta0 = p[1];
  Double_t energy = p[2];


  Double_t norm = energy * ( mPhiPar0 .Eval(xeta) * mEtaPar0 .Eval(xeta) );

  Double_t phi_par2 = mPhiPar2 .Eval( xeta );
  Double_t eta_par2 = mEtaPar2 .Eval( xeta );

  Double_t phi_par3 = mPhiPar3 .Eval( xeta );
  Double_t eta_par3 = 0.50; // Fixed, since it varies so little.

  //  Double_t norm = energy * 0.25;
  //  Double_t phi_par2=0.27;
  //uble_t eta_par2=0.30;
  //  Double_t phi_par3=0.50;
  //  Double_t eta_par3=0.49;

  /// Product of two comp error functions vs phi
  Double_t phi1 = TMath::Erfc( -(xphi-xphi0+phi_par3) / phi_par2 );
  Double_t phi2 = TMath::Erfc( +(xphi-xphi0-phi_par3) / phi_par2 );

  /// Product of two comp error functions vs eta
  Double_t eta1 = TMath::Erfc( -(xeta-xeta0+eta_par3) / eta_par2 );
  Double_t eta2 = TMath::Erfc( +(xeta-xeta0-eta_par3) / eta_par2 );

  /// Full product for the 2D function
  Double_t result = norm * phi1*phi2 * eta1*eta2;

  return result;

};




Double_t eeTower2Function( Double_t *x, Double_t *p ) {


  Double_t phibin=p[0]; // Fractional phi of higher E gamma
  Double_t etabin=p[1]; // Fractional eta of higher E gamma
  Double_t energy=p[2]; // Energy of higher E gamma

  Double_t dgg=p[3];    // gamma-gamma separation in eta-phi space
  Double_t theta=p[4];  // rotation angle in eta-phi space
  Double_t zgg=p[5];    // energy sharing between gammas

  Double_t phibin2=phibin+TMath::Cos(theta) * dgg;
  Double_t etabin2=etabin+TMath::Sin(theta) * dgg;
  Double_t energy2=energy * (1.0-zgg)/(1.0+zgg);

  Double_t p1[]={phibin,etabin,energy};
  Double_t p2[]={phibin2,etabin2,energy2};

  return eeTowerFunction(x,p1) + eeTowerFunction(x,p2);

}

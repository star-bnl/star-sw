///////////////////////////////////////////////////////////////////////////
//
//    Copyright 2010
//
//    This file is part of starlight.
//
//    starlight is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    starlight is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with starlight. If not, see <http://www.gnu.org/licenses/>.
//
///////////////////////////////////////////////////////////////////////////
//
// File and Version Information:
// $Rev::                             $: revision of last commit
// $Author: jwebb $: author of last commit
// $Date: 2016/05/06 14:03:43 $: date of last commit
//
// Description:
//    Added incoherent factor to luminosity table output--Joey
//
//
///////////////////////////////////////////////////////////////////////////


#include <iostream>
#include <fstream>
#include <cmath>
#include <assert.h>

#include "inputParameters.h"
#include "beambeamsystem.h"
#include "beam.h"
#include "starlightconstants.h"
#include "nucleus.h"
#include "bessel.h"
#include "twophotonluminosity.h"


using namespace std;


//______________________________________________________________________________
twoPhotonLuminosity::twoPhotonLuminosity(beam beam_1,beam beam_2,int,double luminosity,inputParameters& input)
 :beamBeamSystem(beam_1,beam_2,luminosity,input),_input2photon(input)
{ }


//______________________________________________________________________________
twoPhotonLuminosity::twoPhotonLuminosity(beam beam_1,beam beam_2,int,inputParameters& input):beamBeamSystem(beam_1,beam_2,input),_input2photon(input)
{
  cout <<"Inside 2photonlumin, beam_1 woodsaxon: "<<beam_1.woodSaxonRadius()<<endl;
  cout <<"Inside 2photonlumin, beam_2 woodsaxon: "<<beam_2.woodSaxonRadius()<<endl;
  //Lets check to see if we need to recalculate the luminosity tables
  twoPhotonDifferentialLuminosity();
}


//______________________________________________________________________________
twoPhotonLuminosity::~twoPhotonLuminosity()
{ }


//______________________________________________________________________________
void twoPhotonLuminosity::twoPhotonDifferentialLuminosity()
{
  ofstream wylumfile;
  wylumfile.precision(15);
  wylumfile.open("slight.txt");
  double w[starlightLimits::MAXWBINS];
  double y[starlightLimits::MAXYBINS];
  double xlum = 0., wmev=0,Normalize = 0.,OldNorm;
 
  Normalize = 1./sqrt(1*(double)_input2photon.nmbWBins()*_input2photon.nmbRapidityBins()); //if your grid is very fine, you'll want high accuracy-->small Normalize
  OldNorm   = Normalize;
  
  //Writing out our input parameters+(w,y)grid+diff._lum.
  wylumfile << beam1().Z() <<endl;
  wylumfile << beam1().A() <<endl;
  wylumfile << beam2().Z() <<endl;
  wylumfile << beam2().A() <<endl;
  wylumfile << _input2photon.beamLorentzGamma() <<endl;
  wylumfile << _input2photon.maxW() <<endl;
  wylumfile << _input2photon.minW() <<endl;
  wylumfile << _input2photon.nmbWBins() <<endl;
  wylumfile << _input2photon.maxRapidity() <<endl;
  wylumfile << _input2photon.nmbRapidityBins() <<endl;
  wylumfile << _input2photon.productionMode() <<endl;
  wylumfile << _input2photon.beamBreakupMode() <<endl;
  wylumfile << _input2photon.interferenceEnabled() <<endl;
  wylumfile << _input2photon.interferenceStrength() <<endl;
  wylumfile << _input2photon.coherentProduction() <<endl;
  wylumfile << _input2photon.incoherentFactor() <<endl;
  wylumfile << _input2photon.deuteronSlopePar() <<endl;
  wylumfile << _input2photon.maxPtInterference() <<endl;
  wylumfile << _input2photon.nmbPtBinsInterference() <<endl;
  for (unsigned int i = 1; i <= _input2photon.nmbWBins(); ++i) {
    w[i] = _input2photon.minW() + (_input2photon.maxW()-_input2photon.minW())/_input2photon.nmbWBins()*i;
    //Old code had it write to a table for looking up...
    wylumfile << w[i] <<endl;
  }
  for (unsigned int i = 1; i <= _input2photon.nmbRapidityBins(); ++i) {
    y[i] = _input2photon.maxRapidity()*(i-1.)/_input2photon.nmbRapidityBins();
    //Old code had it write to a table for looking up...
    wylumfile << y[i] <<endl;
  }
  for (unsigned int i = 1; i <= _input2photon.nmbWBins(); ++i) {   //For each (w,y) pair, calculate the diff. _lum
      //double SUM = 0.;not used
      for (unsigned int j = 1; j <= _input2photon.nmbRapidityBins(); ++j) {
	wmev = w[i]*1000.;
	xlum = wmev * D2LDMDY(wmev,y[j],Normalize);   //Convert photon flux dN/dW to Lorentz invariant photon number WdN/dW
	if (j==1) OldNorm = Normalize;       //Save value of integral for each new W(i) and Y(i)
	wylumfile << xlum <<endl;
      }
      Normalize = OldNorm;
  }
  wylumfile.close();
  return;
}


//______________________________________________________________________________
double twoPhotonLuminosity::D2LDMDY(double M,double Y,double &Normalize)
{
  // double differential luminosity 

  double D2LDMDYx = 0.;
  
  _W1    =  M/2.0*exp(Y);
  _W2    =  M/2.0*exp(-Y);
  _gamma = _input2photon.beamLorentzGamma();
  int Zin=beam1().Z();
  D2LDMDYx = 2.0/M*Zin*Zin*Zin*Zin*(starlightConstants::alpha*starlightConstants::alpha)*integral(Normalize);  //treats it as a symmetric collision
  Normalize = D2LDMDYx*M/(2.0*beam1().Z()*beam1().Z()*
			  beam1().Z()*beam1().Z()*
			  starlightConstants::alpha*starlightConstants::alpha); 
  //Normalization also treats it as symmetric
  return D2LDMDYx;
}


//______________________________________________________________________________ 
double twoPhotonLuminosity::integral(double Normalize)
{
  int NIter = 0;
  int NIterMin = 0;
  double EPS = 0.;
  double RM = 0.;
  double u1 = 0.;
  double u2 = 0.;
  double B1 = 0.;
  double B2 = 0.;
  double Integrala = 0.;
  double totsummary = 0.;
  double NEval      = 0.;
  double Lower[3];
  double Upper[3];
  double WK[500000]; //used to be [1000000]
  double Result, Summary, ResErr, NFNEVL;

  EPS = .01*Normalize;   //This is EPS for integration, 1% of previous integral value.
  // Change this to the Woods-Saxon radius to be consistent with the older calculations (JN 230710) 
  //  RM  = beam1().nuclearRadius()/starlightConstants::hbarcmev;  //Assumes symmetry?
  RM  = beam1().woodSaxonRadius()/starlightConstants::hbarcmev;  

  NIter = 10000 + (int)1000000*(int)Normalize; //if integral value is very small, we don't do too many intertions to get precision down to 1%
  NIterMin = 600;
  u1 = 9.*_gamma/_W1; //upper boundary in B1
  u2 = 9.*_gamma/_W2; //upper boundary in B2
  B1 = .4*_gamma/_W1; //intermediate boundary in B1
  B2 = .4*_gamma/_W2; //intermediate boundary in B2
  //The trick is that u1,2 and b1,2 could be less than RM-the lower integration boundary, thus integration area splits into 4,2 or 1 pieces
  
  if (u1 < RM){
    Integrala = 0;
    totsummary = 0;
    NEval      = 0;
  }
  else if (B1 > RM){
    if (u2 < RM){
      Integrala = 0;
      totsummary = 0;
      NEval      = 0;
    }
    else if (B2 > RM){            //integral has 4 parts
      Integrala = 0;
      totsummary = 40000;
      NEval      = 0;
      Lower[0]   = RM;       //1
      Lower[1]   = RM;       //2
      Lower[2]   = 0.;       //3
      Upper[2]   = 2.*starlightConstants::pi;    //3
      Upper[0]   = B1;       //1
      Upper[1]   = B2;       //2
      radmul(3,Lower,Upper,NIterMin,NIter,EPS,WK,NIter,Result,ResErr,NFNEVL,Summary);
      Integrala   = Integrala + Result;
      totsummary = totsummary + 1000*Summary;
      NEval      = NEval + NFNEVL;
      Upper[0]   = u1;       //1
      Upper[1]   = B2;       //2
      Lower[0]   = B1;       //1
      Lower[1]   = RM;       //2
      radmul(3,Lower,Upper,NIterMin,NIter,EPS,WK,NIter,Result,ResErr,NFNEVL,Summary);
      Integrala   = Integrala + Result;
      totsummary = totsummary + 100*Summary;
      NEval      = NEval + NFNEVL;
      Upper[0]   = B1;       //1
      Upper[1]   = u2;       //2
      Lower[0]   = RM;       //1
      Lower[1]   = B2;       //2
      radmul(3,Lower,Upper,NIterMin,NIter,EPS,WK,NIter,Result,ResErr,NFNEVL,Summary);
      Integrala   = Integrala + Result;
      totsummary = totsummary + 100*Summary;
      NEval      = NEval + NFNEVL;
      Upper[0]   = u1;       //1
      Upper[1]   = u2;       //2
      Lower[0]   = B1;       //1
      Lower[1]   = B2;       //2
      radmul(3,Lower,Upper,NIterMin,NIter,EPS,WK,NIter,Result,ResErr,NFNEVL,Summary);
      Integrala   = Integrala + Result;
      totsummary = totsummary + Summary;
      NEval      = NEval + NFNEVL;
    }
    else {
      //integral has 2 parts, b2 integral has only 1 component
      Integrala   = 0;
      totsummary = 20000;
      NEval      = 0;
      Lower[0]   = RM;       //1
      Lower[1]   = RM;       //2
      Lower[2]   = 0.;       //3
      Upper[2]   = 2.*starlightConstants::pi;    //3
      Upper[0]   = B1;       //1
      Upper[1]   = u2;       //2
      radmul(3,Lower,Upper,NIterMin,NIter,EPS,WK,NIter,Result,ResErr,NFNEVL,Summary);
      Integrala   = Integrala + Result;
      totsummary = totsummary + 100*Summary;
      NEval      = NEval + NFNEVL;
      Upper[0]   = u1;       //1
      Lower[0]   = B1;       //1
      radmul(3,Lower,Upper,NIterMin,NIter,EPS,WK,NIter,Result,ResErr,NFNEVL,Summary);
      Integrala   = Integrala + Result;
      totsummary = totsummary + Summary;
      NEval      = NEval + NFNEVL;
    }
  }
  else{
    if (u2 < RM ){
      Integrala   = 0;
      totsummary = 0;
      NEval      = 0;
    }
    else if (B2 > RM){
      //integral has 2 parts, b1 integral has only 1 component
      Integrala   = 0;
      totsummary = 20000;
      NEval      = 0;
      Lower[0]   = RM;       //1
      Lower[1]   = RM;       //2
      Lower[2]   = 0.;       //2
      Upper[2]   = 2.*starlightConstants::pi;    //3
      Upper[0]   = u1;       //1
      Upper[1]   = B2;       //2
      radmul(3,Lower,Upper,NIterMin,NIter,EPS,WK,NIter,Result,ResErr,NFNEVL,Summary);
      Integrala   = Integrala + Result;
      totsummary = totsummary + 100*Summary;
      NEval      = NEval + NFNEVL;
      Upper[1]   = u2;       //2
      Lower[1]   = B2;       //2
      radmul(3,Lower,Upper,NIterMin,NIter,EPS,WK,NIter,Result,ResErr,NFNEVL,Summary);
      Integrala   = Integrala + Result;
      totsummary = totsummary + Summary;
      NEval      = NEval + NFNEVL;
    }
    else{                 //integral has 1 part
      Integrala   = 0;
      totsummary = 10000;
      NEval      = 0;
      Lower[0]   = RM;       //1
      Lower[1]   = RM;       //2
      Lower[2]   = 0.;       //3
      Upper[2]   = 2.*starlightConstants::pi;    //3
      Upper[0]   = u1;       //1
      Upper[1]   = u2;       //2
      radmul(3,Lower,Upper,NIterMin,NIter,EPS,WK,NIter,Result,ResErr,NFNEVL,Summary);
      Integrala   = Integrala + Result;
      totsummary = totsummary + Summary;
      NEval      = NEval + NFNEVL;
    }
  }
  Integrala = 2*starlightConstants::pi*Integrala;
  return Integrala;
}


//______________________________________________________________________________ 
double twoPhotonLuminosity::radmul(int N,double *A,double *B,int MINPTS,int MAXPTS,double EPS,double *WK,int IWK,double &RESULT,double &RELERR,double &NFNEVL,double &IFAIL)
{
  double wn1[14] = {       -0.193872885230909911, -0.555606360818980835,
			   -0.876695625666819078, -1.15714067977442459,  -1.39694152314179743,
			   -1.59609815576893754,  -1.75461057765584494,  -1.87247878880251983,
			   -1.94970278920896201,  -1.98628257887517146,  -1.98221815780114818,
			   -1.93750952598689219,  -1.85215668343240347,  -1.72615963013768225};
  
  double wn3[14] = {        0.0518213686937966768,    0.0314992633236803330,
			    0.0111771579535639891, -0.00914494741655235473,  -0.0294670527866686986,
			    -0.0497891581567850424, -0.0701112635269013768,   -0.0904333688970177241,
			    -0.110755474267134071,  -0.131077579637250419,    -0.151399685007366752,
			    -0.171721790377483099,  -0.192043895747599447,    -0.212366001117715794};
  
    double wn5[14] = {        0.871183254585174982e-01,  0.435591627292587508e-01,
			      0.217795813646293754e-01, 0.108897906823146873e-01,  0.544489534115734364e-02,
			      0.272244767057867193e-02, 0.136122383528933596e-02,  0.680611917644667955e-03,
			      0.340305958822333977e-03, 0.170152979411166995e-03,  0.850764897055834977e-04,
			      0.425382448527917472e-04, 0.212691224263958736e-04,  0.106345612131979372e-04};

    double wpn1[14] = {       -1.33196159122085045, -2.29218106995884763,
			      -3.11522633744855959, -3.80109739368998611, -4.34979423868312742,
			      -4.76131687242798352, -5.03566529492455417, -5.17283950617283939,
			      -5.17283950617283939, -5.03566529492455417, -4.76131687242798352,
			      -4.34979423868312742, -3.80109739368998611, -3.11522633744855959};
    
    double wpn3[14] = {        0.0445816186556927292, -0.0240054869684499309,
			       -0.0925925925925925875, -0.161179698216735251,  -0.229766803840877915,
			       -0.298353909465020564,  -0.366941015089163228,  -0.435528120713305891,
			       -0.504115226337448555,  -0.572702331961591218,  -0.641289437585733882,
			       -0.709876543209876532,  -0.778463648834019195,  -0.847050754458161859};

    RESULT = 0;

    double ABSERR = 0.;
    double ctr[15], wth[15], wthl[15], z[15];
    double R1  = 1.;
    double HF  = R1/2.;
    double xl2 = 0.358568582800318073;
    double xl4 = 0.948683298050513796;
    double xl5 = 0.688247201611685289;
    double w2  = 980./6561.;
    double w4  = 200./19683.;
    double wp2 = 245./486.;
    double wp4 = 25./729.;
    int j1 =0;
    double SUM1, SUM2, SUM3, SUM4, SUM5, RGNCMP=0., RGNVAL, RGNERR, F2, F3, DIF, DIFMAX, IDVAXN =0.;
    IFAIL  = 3;
    if (N < 2 || N > 15) return 0;
    if (MINPTS > MAXPTS) return 0;
    int IFNCLS = 0;
    int IDVAX0 = 0;
    bool LDV = false;
    double TWONDM = pow(2.,(double)(N));
    int IRGNST = 2*N+3;
    int IRLCLS = (int)pow(2.,(N))+2*N*(N+1)+1;
    int ISBRGN = IRGNST;
    int ISBTMP, ISBTPP;
    int ISBRGS = IRGNST;

    if ( MAXPTS < IRLCLS ) return 0;
    for ( int j = 0; j < N; j++ ){  //10
      ctr[j]=(B[j] + A[j])*HF;
      wth[j]=(B[j] - A[j])*HF;      
    }
 L20:
    double RGNVOL = TWONDM; //20
    for ( int j = 0; j < N; j++ ){            //30
      RGNVOL = RGNVOL*wth[j];
      z[j] = ctr[j];  
    }
    
    SUM1 = integrand(N,z); 
    
    DIFMAX = 0;
    SUM2   = 0;
    SUM3   = 0;
    for ( int j = 0; j < N; j++ ) {   //40
      z[j]=ctr[j]-xl2*wth[j];
      F2=integrand(N,z);
      
      z[j]=ctr[j]+xl2*wth[j];
      F2=F2+integrand(N,z);
      wthl[j]=xl4*wth[j];
      
      z[j]=ctr[j]-wthl[j];
      F3=integrand(N,z);
      
      z[j]=ctr[j]+wthl[j];
      F3=F3+integrand(N,z);
      SUM2=SUM2+F2;
      SUM3=SUM3+F3;
      DIF=fabs(7.*F2-F3-12.*SUM1);
      DIFMAX=max(DIF,DIFMAX);
      
      if ( DIFMAX == DIF) IDVAXN = j+1;
      z[j]=ctr[j];
      
    }
    
    SUM4   = 0;
    for ( int j = 1; j < N; j++){ //70
      
	j1=j-1;
        for ( int k = j; k < N; k++){  //60
	  for ( int l = 0; l < 2; l++){      //50
	    wthl[j1]=-wthl[j1];
	    z[j1]=ctr[j1]+wthl[j1];
	    for ( int m = 0; m < 2; m++){   //50
	      wthl[k]=-wthl[k];
	      z[k]=ctr[k]+wthl[k];
	      SUM4=SUM4+integrand(N,z);
	    }
	  }
	  z[k]=ctr[k];
	}
        z[j1]=ctr[j1];
    }
    
    SUM5  = 0;
    
    for ( int j = 0; j < N; j++){             //80
      wthl[j]=-xl5*wth[j];
      z[j]=ctr[j]+wthl[j];
    }
 L90:
    SUM5=SUM5+integrand(N,z);   //line 90
    
    for (int j = 0; j < N; j++){ //100
      wthl[j]=-wthl[j];
      z[j]=ctr[j]+wthl[j];  
      if ( wthl[j] > 0. ) goto L90;
    }

    RGNCMP = RGNVOL*(wpn1[N-2]*SUM1+wp2*SUM2+wpn3[N-2]*SUM3+wp4*SUM4);
    RGNVAL = wn1[N-2]*SUM1+w2*SUM2+wn3[N-2]*SUM3+w4*SUM4+wn5[N-2]*SUM5;
   
    RGNVAL = RGNVOL*RGNVAL;
    RGNERR = fabs(RGNVAL-RGNCMP);
    RESULT = RESULT+RGNVAL;
    ABSERR = ABSERR+RGNERR;
    IFNCLS = IFNCLS+IRLCLS;
    
    
    if (LDV){
    L110:

      ISBTMP = 2*ISBRGN;

      if ( ISBTMP > ISBRGS ) goto L160;
      if ( ISBTMP < ISBRGS ){
	ISBTPP = ISBTMP + IRGNST;
	
	if ( WK[ISBTMP-1] < WK[ISBTPP-1] ) ISBTMP = ISBTPP;
      }
      
      if ( RGNERR >= WK[ISBTMP-1] ) goto L160;
      for ( int k = 0; k < IRGNST; k++){
	WK[ISBRGN-k-1] = WK[ISBTMP-k-1];
                }
      ISBRGN = ISBTMP;
      goto L110;
    }
 L140:
    
    ISBTMP = (ISBRGN/(2*IRGNST))*IRGNST;
    
    if ( ISBTMP >= IRGNST && RGNERR > WK[ISBTMP-1] ){
      for ( int k = 0; k < IRGNST; k++){
	WK[ISBRGN-k-1]=WK[ISBTMP-k-1];
      }
      ISBRGN = ISBTMP;
      goto L140;
    }
 L160:
    
    WK[ISBRGN-1] = RGNERR;
    WK[ISBRGN-2] = RGNVAL;
    WK[ISBRGN-3] = IDVAXN;
    
    for ( int j = 0; j < N; j++) {
      
      ISBTMP = ISBRGN-2*j-4;
      WK[ISBTMP]=ctr[j];
      WK[ISBTMP-1]=wth[j];
    }
    if (LDV) {
      LDV = false;
      assert(IDVAX0>0);      ctr[IDVAX0-1]=ctr[IDVAX0-1]+2*wth[IDVAX0-1];
      ISBRGS = ISBRGS + IRGNST;
      ISBRGN = ISBRGS;
      goto L20;
    }
    
    RELERR=ABSERR/fabs(RESULT);
    
    
    if ( ISBRGS + IRGNST > IWK ) IFAIL = 2;
    if ( IFNCLS + 2*IRLCLS > MAXPTS ) IFAIL = 1;
    if ( RELERR < EPS && IFNCLS >= MINPTS ) IFAIL = 0;
    
    if ( IFAIL == 3 ) {
      LDV = true;
      ISBRGN = IRGNST;
      ABSERR = ABSERR-WK[ISBRGN-1];
      RESULT = RESULT-WK[ISBRGN-2];
      IDVAX0 = (int)WK[ISBRGN-3];
      
      for ( int j = 0; j < N; j++) {
	ISBTMP = ISBRGN-2*j-4;
	ctr[j] = WK[ISBTMP];
	wth[j] = WK[ISBTMP-1];
      }
      
      wth[IDVAX0-1] = HF*wth[IDVAX0-1];
      ctr[IDVAX0-1] = ctr[IDVAX0-1]-wth[IDVAX0-1];
      goto L20;
    }
    NFNEVL=IFNCLS;
    return 1;
}


//______________________________________________________________________________
double twoPhotonLuminosity::integrand(double ,  // N (unused)
                                      double X[])
{
  double  b1 = X[0];      //1
  double  b2 = X[1];      //2
  double  theta = X[2];   //3
  //breakup effects distances in fermis, so convert to fermis(factor of hbarcmev)
  double  D = sqrt(b1*b1+b2*b2-2*b1*b2*cos(theta))*starlightConstants::hbarcmev;
  double  integrandx = Nphoton(_W1,_gamma,b1)*Nphoton(_W2,_gamma,b2)*b1*b2*probabilityOfBreakup(D); 
  //why not just use gamma?
  //switching _input2photon.beamLorentzGamma()to gamma
  return integrandx;
}


//______________________________________________________________________________
double twoPhotonLuminosity::Nphoton(double W,double gamma,double Rho)
{
 double Nphoton1 =0.;
 double WGamma = W/gamma;
 double WGR    = 1.0*WGamma*Rho;
 //factor of Z^2*alpha is omitted
 double Wphib = WGamma*bessel::dbesk1(WGR);

 Nphoton1 = 1.0/(starlightConstants::pi*starlightConstants::pi)*(Wphib*Wphib);
 return Nphoton1;
}

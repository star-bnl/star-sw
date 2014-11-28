/***************************************************************************
 *
 * $Id: StSvtElectronCloud.cc,v 1.16 2010/04/14 18:21:52 baumgart Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *`
 * Description: Calculatos cloud expansion in principal axis frame
 *
 ***************************************************************************
 *
 * $Log: StSvtElectronCloud.cc,v $
 * Revision 1.16  2010/04/14 18:21:52  baumgart
 * Fixed a problem for SVT hits near eta=0
 *
 * Revision 1.15  2009/08/10 05:21:32  baumgart
 * Fix Minor Axis of Initial Electron Cloud Size
 *
 * Revision 1.14  2009/07/29 18:23:44  baumgart
 * Modified several formulae to better account for non-infinitesimal initial hit sizes
 *
 * Revision 1.13  2009/06/28 04:01:46  baumgart
 * Correction of SDD thickness
 *
 * Revision 1.12  2009/06/28 03:56:07  baumgart
 * Fix of angular dependencies
 *
 * Revision 1.11  2009/06/11 23:17:21  baumgart
 * Increase initial hit sizes and add projection to tSigMaj in function setInitWidths
 *
 * Revision 1.10  2008/11/07 15:11:31  caines
 * Initialize variables (that REALLY do not need to be) to see if this makes valgrind happy
 *
 * Revision 1.9  2008/10/22 17:33:34  fine
 * Initialize all data-members of the class StSvtElectronCloud
 *
 * Revision 1.8  2007/03/21 17:25:51  fisyak
 * Ivan Kotov's drift velocities, TGeoHMatrix
 *
 * Revision 1.7  2005/07/23 03:37:34  perev
 * IdTruth + Cleanup
 *
 * Revision 1.6  2005/02/09 14:33:35  caines
 * New electron expansion routine
 *
 * Revision 1.5  2003/11/15 20:24:29  caines
 * fixes to remove warnings at compilation
 *
 * Revision 1.4  2003/11/13 16:24:59  caines
 * Further improvements to get simulator looking like reality
 *
 * Revision 1.1  2000/11/30 20:47:48  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/
#include <string.h>
#include "StSvtElectronCloud.hh"
#include "TMath.h"
ClassImp(StSvtElectronCloud)

//#define EL_CL_DEBUG 


StSvtElectronCloud::StSvtElectronCloud()
  : mSigX(-1956),      mSigY(-1956),      mSigXY(-1956)
  , mChargeNow(-1956), mTotCharge(-1956), mEnergy(-1956)
  , mTheta(-1956)    , mPhi(-1956),       mInitPhi(-1956)
  , mDriftVel(-1956) , mTimBinSize(-1956),mSDD_thickness(-1956)                      //  [mm]
  , mTrapConst(-1956), mDiffusionConst(-1956)                           //  [mm**2/micro seconds] X and Y are calculated from this
  , mDiffConstX(-1956)                          //  [mm**2/micro seconds] in drift direction
  , mDiffConstY(-1956)                          //  [mm**2/micro seconds] in anode direction
  , mSi_DielConst(-1956), mSi_EnergyGap(-1956)
  , mPermitivity(-1956)                         // [e/(mm-V)]
  , mSi_Mobility(-1956)                         // [mm**2/(V-micro seconds)]
  , mLifeTime(-1956)                            // [micro seconds]
  , mTrackId(-1956)
{
  setSiliconProp();               //important - sets SVT silicon properties
  mTrackId = 0;
  mTotCharge = 0;
  mChargeNow = 0;

  for(int i = 0; i < 4; i++)
    {
      m_dSigX[i] = 0;
      m_dSigY[i] = 0;
      m_dSigXY[i] = 0;
    }
}

StSvtElectronCloud::~StSvtElectronCloud()
{

}


void StSvtElectronCloud::setSiliconProp()
{
 mSDD_thickness = 0.28;                                 //  [mm] 
 mInitHitSize = 0.12;                                  //  [mm] 
 mLifeTime = 1000000.0;                                // [micro seconds]
 mTrapConst = 0.0;                                     //  [micro seconds]
 mDiffusionConst=0.0035;                               //  [mm**2/micro seconds]
 CalculateDiffXY();

 //!!this is not realy used - the multfactor is hardwired for 
 //these values in order to speed up the calculation!!!
 mSi_DielConst = 12.0;               
 mSi_EnergyGap = 3.6;                                  // [eV]
 // mPermitivity = (8.854187817/1.60217733)*10000;       // [e/(mm-V)]
 mPermitivity = 55263.46959983512;
 mSi_Mobility = 0.135;                                 // [mm**2/(V-micro seconds)]
 
}


 
void StSvtElectronCloud::setElectronLifeTime(double tLife)
{
  mLifeTime = tLife;
}





void StSvtElectronCloud::CalculateDiffXY()   
{// recalculate "diffusion" in X and Y direction after the parameters has changed
  mDiffConstX = mDiffusionConst;
  mDiffConstY = mDiffusionConst;
  if(mTrapConst)
    mDiffConstX +=mTrapConst*pow(mDriftVel,2);     //  [mm**2/micro seconds]
  //cout<<"StSvtElectronCloud::CalculateDiffXY():   diffusion in anode direction = "<< mDiffConstY <<" in time direction = "<<mDiffConstX<<endl;
}

void StSvtElectronCloud::setDiffusionConst(double diffConst)
{
  mDiffusionConst=diffConst;
  CalculateDiffXY();
}

void StSvtElectronCloud::setDriftVelocity(double driftVel)
{
  mDriftVel = driftVel; //  [mm/micro seconds]
  CalculateDiffXY();
}

void StSvtElectronCloud::setTrappingConst(double trapConst)
{

  mTrapConst = trapConst;   //  [micro seconds]
  CalculateDiffXY();
 
}

void StSvtElectronCloud::setPar(double energy,double theta, double phi, double timBinSize,int trackId)
{
  mTrackId = trackId;  
  mTimBinSize = timBinSize;
  mEnergy = energy;
  mTheta = theta;
  mInitPhi = phi;
  //mTheta = 2*acos(-1.0)/360;
  
  //mTotCharge = mEnergy/mSi_EnergyGap;
  mTotCharge = mEnergy*0.27777777777777;    // in number of electrons, ~25000 electrons for MIPs
   mTotCharge = mTotCharge*(1- 2*mInitHitSize*fabs(sin(mTheta))/(3*mSDD_thickness)); // Fix eta-dependence
  //cout<<"mTotCharge = "<<mTotCharge<<endl;
  }

void StSvtElectronCloud::setInitWidths(double w1, double w2)
{
  // cout<<"======= NEW HIT========="<<endl;
  double tSigMaj,tSigMin;
  mSigX=0; mSigY=0; mSigXY=0;
  
  //tSigMaj = 0.288675134*fabs(mSDD_thickness*tan(mTheta));  //  [mm]
   tSigMaj = 0.288675134*(fabs(mSDD_thickness*tan(mTheta))+2*w1*cos(mTheta));  //  [mm]

  if (tSigMaj<w1) {        //almost perpendicular
    tSigMaj = w1;          //initial size cannot be smaller than minimal
    mTheta=0.;
    mPhi = 0.;
   }
  tSigMin= w1;          

  if ((1. - tSigMin/tSigMaj) < 0.001 || (fabs(mPhi)< 0.001)) mPhi=0.;

  //calculate initial size in XY
  if (mPhi==0.){
    mSigX = tSigMaj;  
    mSigY = tSigMin;
    mSigXY = 0.;
  }
  else{
    double C2=cos(mPhi);
    double S2=sin(mPhi);
    mSigX = sqrt(fabs(tSigMaj*tSigMaj*C2*C2 + tSigMin*tSigMin*S2*S2)); //fabs --numerical safety
    mSigY = sqrt(fabs(tSigMaj*tSigMaj*S2*S2 + tSigMin*tSigMin*C2*C2)); 
    mSigXY =sqrt(fabs(C2*S2*(tSigMaj*tSigMaj-tSigMin*tSigMin)));
    if(((1. - tSigMin/tSigMaj) < 0.001) || (fabs(mPhi)< 0.001)){
      mPhi=0.;
      mSigXY = 0.;
    } 
  }
  /*
    cout<<"major="<<tSigMaj<<endl;
    cout<<"initial mPhi = "<<mPhi<<endl;
    cout<<"initial width along drift= "<<sqrt(sigmaXSq)<<endl;
    cout<<"initial width along anode = "<<sqrt(sigmaYSq)<<endl;
    cout<<"initial XY = "<<sqrt(sigmaXYSq)<<endl;
  */
  
  for(int i = 0; i < 4; i++)
    {    
      m_dSigX[i] = 0;
      m_dSigY[i] = 0;
      m_dSigXY[i] = 0; 
    }

  GetDerivatives( m_dSigX[0],m_dSigY[0], m_dSigXY[0],mSigX,mSigY,mSigXY,0.);
 }

void StSvtElectronCloud::CalcExpansion(double mTc)
{//mTc is in timebins
  //cout<<"#### NEW HIT####"<<endl;
  //cout <<"mDiffConstX="<<mDiffConstX <<",  mDiffConstY="<<mDiffConstY <<endl;
  const int AdBashDiv=30; //number od steps ber timebin for adams-bashfort
  const int RungeDiv =200; //number of steps inside one AdBashStep 

  //the phi has to be from -2pi to 2pi
  //now remember in which quandrant was phi
  double sign = (mInitPhi<0.)?-1.:1.;
  mPhi=fabs(mInitPhi);
  int quad=(int)floor(mPhi/TMath::PiOver2());
  //cout<<"quandrant="<<quad<<endl;
  switch (quad){
  case 0:break;
  case 1:
    mPhi=TMath::Pi()-mPhi;
    break;
  case 2:
    mPhi=mPhi-TMath::Pi();
    break;
  case 3:
    mPhi=TMath::TwoPi()-mPhi;
    break;
  default: //just in case
    mPhi=asin(sin(mPhi));
    break;
  }
  

  //set initial sizes
  mPhi = mInitPhi;
 
  //setInitWidths(0.002, 0.002);
  setInitWidths(mInitHitSize,mInitHitSize); // Stephen tune to Run 7 Au+Au
  
  double steps=mTc*AdBashDiv*RungeDiv;
  int isteps=(int)floor(steps + 0.5);
  /*
  cout<<"   time="<<mTc<<" isteps="<<isteps<<endl; 
  cout<<"   ..initial size:  mSigX="<<mSigX<<" mSigY="<<mSigY<<" mSigXY="<<mSigXY<<endl;
  cout<<"   ..comparing  :    SigT="<<getSigmaDrift()<<" Anode="<<getSigmaAnode()<<endl;
  cout<<"   ..comparing  :   Major="<<getSigmaMajor()<<" Minor="<<getSigmaMinor()<<" Phi="<<getPhi()<<endl;    
  */
  if (isteps<=3*RungeDiv){//very short drift time -handle all by runge-kutta
    //cout<<"    going for short runge-kutta: steplen="<<(mTimBinSize/((double)AdBashDiv*(double)RungeDiv))<<endl;
    runge_kutta4(isteps,0., mTimBinSize/((double)AdBashDiv*(double)RungeDiv),0); 
    return;
  }

  //now four runge-kutta runs to get input for adams-bashfort
  //first runs to whole number of adams-basfort steps
  int tAdBashSteps=isteps/RungeDiv;
  int firstNumStep=isteps-tAdBashSteps*RungeDiv;
  tAdBashSteps-=3;
  double steplen=mTimBinSize/((double)AdBashDiv*(double)RungeDiv);

#ifdef EL_CL_DEBUG 
  cout<<"   runge-kutta: firstNumSteps="<<firstNumStep<<" steplen="<<steplen<<endl;
#endif
  runge_kutta4(firstNumStep, 0.,steplen,0); 
  runge_kutta4(RungeDiv,steplen*(double)firstNumStep ,steplen,1); 
  runge_kutta4(RungeDiv, steplen*((double)firstNumStep+(double)RungeDiv), steplen,2); 
  runge_kutta4(RungeDiv, steplen*((double)firstNumStep+2.*(double)RungeDiv), steplen,3); 
  
  //Now run Adams-Bashfort for the rest
#ifdef EL_CL_DEBUG 
  cout<<"   going for adamsBushFort: steps:="<<tAdBashSteps<<endl;
#endif
  double Adsteplen=mTimBinSize/(double)AdBashDiv;
#ifdef EL_CL_DEBUG 
  cout<<"T0="<<steplen*((double)firstNumStep+3.*(double)RungeDiv)<<endl;
#endif
  adamsBushFort(tAdBashSteps,steplen*((double)firstNumStep+3.*(double)RungeDiv),Adsteplen);

  //return to the propper quadrant
  switch (quad){
  case 0:break;
  case 1:
    mPhi=TMath::Pi()-mPhi;
    break;
  case 2:
    mPhi=mPhi+TMath::Pi();
    break;
  case 3:
    mPhi=TMath::TwoPi()-mPhi;
    break;
  default:
    break;
  }
  
  mPhi=sign*mPhi;
}

void StSvtElectronCloud::GetDerivatives(double &dSx,double &dSy,double &dSxy,double SX,double SY,double SXY,double time)
{ //these are derivatives for sigmas^3 - it's needed for numerical stability

  // double sr, sr2, sr3,sr4, sd, sd2, cons,cons3; 
  //double fun1 ,multFactor, denominator,numerator;
#ifdef EL_CL_DEBUG
  cout<<"SX, SY, SXY : "<<SX<<", "<<SY<<", "<<SXY<<endl; 
#endif

  //----first get major, minor and angle
  double R=SX*SX + SY*SY;
  double S=sqrt(pow((SX*SX - SY*SY),2) + 4.*pow(SXY*SXY,2)); 
  double sigMaj = sqrt(0.5*fabs(R + S)); 
  double sigMin = sqrt(0.5*fabs(R - S)); //safety fabs
  double phi;
  if ((1. - sigMin/sigMaj) < 0.001)phi=0;
  else 
    {
      double ar=(SX*SX-SY*SY)/(sigMaj*sigMaj-sigMin*sigMin);
 #ifdef EL_CL_DEBUG
     cout<<"  ar="<<(double)ar<<endl;
#endif
      if (ar>.999999) ar=(double)1e0;
      if (ar<-.999999) ar=(double)-1e0;
      phi=0.5*acos(ar); 
    }
#ifdef EL_CL_DEBUG
  cout<<"  R S sigMaj sigMin phi: "<<S<<", "<< sigMaj<<", "<< sigMin<<", "<< phi<<endl;
#endif
  //----calc derivatives
  const double K1 = 0.43;
  const double K2 = 0.25;
  const double K3 = 0.58;

  mChargeNow = mTotCharge*exp(-time/mLifeTime);
  //multFactor = (1.0/(4*pi))*(mSi_Mobility/(mSi_DielConst*mPermitivity));
  double multFactor = 0.00000001619961;


  double vd=sigMin/mSDD_thickness;
  double r = sigMin/sigMaj;
  double r2=r*r;
  
  double tmp=K1*vd/K2;
  tmp=1. + pow(tmp,1.5);
  double denominator=pow(tmp,2./3.);

  tmp=(1.+1./(vd*vd));
  double numerator = K1*pow(r,2./3.) - (K3/4.)*log(r2*r2 + 1./(tmp*tmp));
#ifdef EL_CL_DEBUG 
  cout<<"  vd="<<vd<<" r2="<<r2<<" den="<<denominator<<" num1="<<numerator;
#endif
  double f1 = multFactor*mChargeNow*(numerator/denominator);

  //now func2
  numerator = K3 - (K3-K1)*sqrt(r);
  double f2 = multFactor*mChargeNow*(numerator/denominator);
#ifdef EL_CL_DEBUG 
  cout<<" num2="<<numerator<<endl;
#endif
  double sumFunc=f1+f2;
  double diffFunc=f1-f2;
  double C=cos(2.*phi);
  
  dSx = 1.5*SX*(2*mDiffConstX + 0.5*(sumFunc + diffFunc*C));
  dSy = 1.5*SY*(2*mDiffConstY + 0.5*(sumFunc - diffFunc*C));
  dSxy = 1.5*SXY*(2.*(sin(2*phi)/C)*(mDiffConstX*pow(sin(phi),2) - mDiffConstY*pow(cos(phi),2)) 
		  + 0.5*diffFunc*sin(2.*phi));
#ifdef EL_CL_DEBUG
      cout<<"  dSx,dSy,dSxy= "<<dSx<<","<<dSy<<","<<dSxy<<","<<endl;
#endif

  /*-exponential test 
  dSx =3*SX*SX*SX;
  dSy = 3*SY*SY*SY;
  dSxy=3*SXY*SXY*SXY;
  */
}

void StSvtElectronCloud::runge_kutta4(int steps, double t0, double steplen,int save)
{
  //runge kutta 4th order
  double  m1 = 0.0, m2 = 0.0, m3 = 0.0, m4 = 0.0;
  double  n1 = 0.0, n2 = 0.0, n3 = 0.0, n4 = 0.0; 
  double  o1 = 0.0, o2 = 0.0, o3 = 0.0, o4 = 0.0; 
 
  double tim = t0;

  

  /*
  cout<<"####Runge-kutta -entry: tim = "<<tim<<" #############"<<endl;
  cout<<"step length = "<<steplen<<endl;
  cout<<"theta = "<<mTheta<<"   phi = "<<phi<<endl;
  cout<<"sigma1Sq = "<<sigma1Sq<<"  sigma2Sq = "<<sigma2Sq<<endl;
  cout<<" width along drift= "<<sqrt(mSigX)<<"  width along anode = "<<sqrt(mSigY)<<endl;
  cout<<"initial XY = "<<sqrt(mSigXY)<<endl;
  */

  //phi for first loop
  const double third=1./3.; 
  if (steps>0)
    for(int i = 1; i <= steps; i++)
      {
	/*
	cout<<"   +++++ Runge Kutta -step"<<i ;
	cout<<"             SigT="<<getSigmaDrift()<<" Anode="<<getSigmaAnode()<<endl;
	cout<<"             Major="<<getSigmaMajor()<<" Minor="<<getSigmaMinor()<<" Phi="<<getPhi()<<endl;    
	*/
	double xx=mSigX*mSigX*mSigX;
	double yy=mSigY*mSigY*mSigY;
	double xy=mSigXY*mSigXY*mSigXY; 
	
	GetDerivatives(m1,n1,o1,mSigX,mSigY,mSigXY,tim);
	//cout<<"             dXX="<< m1<<" dYY="<<n1 <<" dXY="<< o1<<endl;

	double tmpX=pow(fabs(xx+ 0.5*steplen*m1),third);
	double tmpY=pow(fabs(yy+ 0.5*steplen*n1),third);
	double tmpXY=pow(fabs(xy+ 0.5*steplen*o1),third);
	GetDerivatives(m2,n2,o2,tmpX,tmpY,tmpXY,tim+0.5*steplen);
	
	tmpX=pow(fabs(xx+0.5*steplen*m2),third);
	tmpY=pow(fabs(yy+ 0.5*steplen*n2),third);
	tmpXY=pow(fabs(xy+ 0.5*steplen*o2),third);
	GetDerivatives(m3,n3,o3,tmpX,tmpY,tmpXY,tim+0.5*steplen);
	
	tmpX=pow(fabs(xx+steplen*m3),third);
	tmpY=pow(fabs(yy+ steplen*n3),third);
	tmpXY=pow(fabs(xy+ steplen*o3),third);
	GetDerivatives(m4,n4,o4,tmpX,tmpY,tmpXY,tim+steplen);
	
	
	mSigX = pow(fabs(xx + ((steplen/6.)*(m1 + 2.*(m2 + m3) + m4))),third);
	mSigY = pow(fabs(yy + ((steplen/6.)*(n1 + 2.*(n2 + n3) + n4))),third);
	mSigXY = pow(fabs(xy + ((steplen/6.)*(o1 + 2.*(o2 + o3) + o4))),third);
	
	


	tim = tim  + steplen;
      }//for loop
  
  if ((mPhi==0.)||(mPhi==TMath::PiOver2()))mSigXY = 0.;
  else
    {
      double R=mSigX*mSigX + mSigY*mSigY;
      double S=sqrt(pow((mSigX*mSigX - mSigY*mSigY),2) + 4.*pow(mSigXY*mSigXY,2)); 
      double sigMaj2 = 0.5*fabs(R + S); 
      double sigMin2 = 0.5*fabs(R - S); //safety fabs
      if (((1. - sigMin2/sigMaj2) < 0.00001)|| (fabs(mPhi)< 0.001))mPhi=0;
      else{
	  double ar=(mSigX*mSigX-mSigY*mSigY)/(sigMaj2-sigMin2);
	  if (ar>.999999) ar=(double)1e0; //due to numerical problems
	  if (ar<-.999999) ar=(double)-1e0;
     	  mPhi=0.5*acos(ar); 
      }
    }
  
  /*
    cout<<"---leaving Runge-kutta----"<<endl;
    cout<<"sigma drift = "<<sqrt(mSigX)<<endl;
    cout<<"sigma anode= "<<sqrt(mSigY)<<endl;
    cout<<"Sigma XY = "<<sqrt(mSigXY)<<endl;
    cout<<"phi ="<<mPhi<<endl;
    cout<<"###########################"<<endl;
  */
  
  GetDerivatives(m1,n1,o1,mSigX,mSigY,mSigXY,tim);
  m_dSigX[save] = m1;
  m_dSigY[save] = n1;
  m_dSigXY[save] = o1;
  
  mChargeNow = mTotCharge*exp(-tim/mLifeTime);
}

void StSvtElectronCloud::adamsBushFort(int steps, double t0, double steplen)
{
  double tim = t0;
  const double third=1./3.;
  
  while (steps>0)
    {
#ifdef EL_CL_DEBUG
      cout<<"time ="<<tim<<endl; 
#endif
      double xx=mSigX*mSigX*mSigX;
      double yy=mSigY*mSigY*mSigY;
      double xy=mSigXY*mSigXY*mSigXY;
#ifdef EL_CL_DEBUG
     cout<<"sigX="<<mSigX<<" sigY="<<mSigY<<" sigXY="<<mSigXY<<endl;
#endif

      double preX = pow(fabs(xx + (steplen/24.)*(-9.0*m_dSigX[0] + 37.0*m_dSigX[1] - 59.0*m_dSigX[2] + 55.0*m_dSigX[3])),third);
      double preY = pow(fabs(yy + (steplen/24.)*(-9.0*m_dSigY[0] + 37.0*m_dSigY[1] - 59.0*m_dSigY[2] + 55.0*m_dSigY[3])),third);
      double preXY = pow(fabs(xy + (steplen/24.)*(-9.0*m_dSigXY[0] + 37.0*m_dSigXY[1] - 59.0*m_dSigXY[2] + 55.0*m_dSigXY[3])),third);
#ifdef EL_CL_DEBUG
      cout<<"preX="<<preX<<" preY="<<preY<<" preXY="<<preXY<<endl;
#endif
      
      tim = tim  + steplen;
      double dX=0;
      double dY=0;
      double dXY=0;
      GetDerivatives(dX,dY,dXY,preX,preY,preXY,tim);

      //get the corrector...also final value
      mSigX=pow(fabs( xx + (steplen/24.)*(m_dSigX[1]-5.0*m_dSigX[2] + 19.0*m_dSigX[3] + 9.0*dX)),third);
      mSigY=pow(fabs( yy + (steplen/24.)*(m_dSigY[1]-5.0*m_dSigY[2] + 19.0*m_dSigY[3] + 9.0*dY)),third);
      mSigXY=pow(fabs( xy + (steplen/24.)*(m_dSigXY[1]-5.0*m_dSigXY[2] + 19.0*m_dSigXY[3] + 9.0*dXY)),third);
      
      
       
      GetDerivatives(dX,dY,dXY,mSigX,mSigY,mSigXY,tim);
      //next CE
     
      mSigX=pow(fabs( xx + (steplen/24.)*(m_dSigX[1]-5.0*m_dSigX[2] + 19.0*m_dSigX[3] + 9.0*dX)),third);
      mSigY=pow(fabs( yy + (steplen/24.)*(m_dSigY[1]-5.0*m_dSigY[2] + 19.0*m_dSigY[3] + 9.0*dY)),third);
      mSigXY=pow(fabs( xy + (steplen/24.)*(m_dSigXY[1]-5.0*m_dSigXY[2] + 19.0*m_dSigXY[3] + 9.0*dXY)),third);
      
      
       
      GetDerivatives(dX,dY,dXY,mSigX,mSigY,mSigXY,tim);
   
      //shift the values
      m_dSigX[3] = dX;
      m_dSigY[3] = dY;
      m_dSigXY[3] = dXY;
      for(int i = 0; i < 3; i++){
	m_dSigX[i] = m_dSigX[i+1];
	m_dSigY[i] = m_dSigY[i+1];
	m_dSigXY[i] = m_dSigXY[i+1];
      } 
      
      mChargeNow = mTotCharge*exp(-tim/mLifeTime);
      steps--;
    }

  //get phi
  if (mPhi==0.||(mPhi==TMath::PiOver2())) mSigXY = 0.;
  else
    {
      double R=mSigX*mSigX + mSigY*mSigY;
      double S=sqrt(pow((mSigX*mSigX - mSigY*mSigY),2) + 4.*pow(mSigXY*mSigXY,2)); 
      double sigMaj2 = 0.5*fabs(R + S); 
      double sigMin2 = 0.5*fabs(R - S); //safety fabs
      if (((1. - sigMin2/sigMaj2) < 0.00001)|| (fabs(mPhi)< 0.001))mPhi=0;
      else{
	  double ar=(mSigX*mSigX-mSigY*mSigY)/(sigMaj2-sigMin2);
	  if (ar>.999999) ar=(double)1e0;
     	  if (ar<-.999999) ar=(double)-1e0;
     	  mPhi=0.5*acos(ar); 
      }
    }
}



double StSvtElectronCloud::getPhi()
{
  return mPhi;
}

double StSvtElectronCloud::getChargeAtAnode()
{
  //cout<<" mChargeNow = "<<mChargeNow<<endl;
return mChargeNow;
 
}


double StSvtElectronCloud::getSigmaDrift()
{
   return mSigX;
}

double StSvtElectronCloud::getSigmaAnode()
{
  return mSigY;
}

double StSvtElectronCloud::getSigmaCorr()
{
  return mSigXY;
}

double StSvtElectronCloud::getSigmaMajor()
{
  double c2f=mSigX*mSigX + mSigY*mSigY;
  double s2f=sqrt(pow((mSigX*mSigX - mSigY*mSigY),2) + 4.*pow(mSigXY*mSigXY,2));
  return sqrt(0.5*fabs(c2f+s2f));
}

double StSvtElectronCloud::getSigmaMinor()
{
  //return sqrt(mSigY);
  return  sqrt(0.5*fabs(((mSigX*mSigX + mSigY*mSigY) - sqrt(pow((mSigX*mSigX - mSigY*mSigY),2) + 4.*pow(mSigXY*mSigXY,2)))));
}


#ifdef EL_CL_DEBUG 
#undef EL_CL_DEBUG
#endif

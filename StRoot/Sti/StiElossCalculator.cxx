#include <stdio.h>
#include <stdlib.h>
#include <cmath>
#include <assert.h>
#include "Sti/StiElossCalculator.h"
#include "StiUtilities/StiDebug.h"
#include "Stiostream.h"
static double gsigma2(double ZoverA,double DENS,double CHARGE2
                     ,double AMASS ,double BET2,double STEP  );
static double gdrelx (double A     ,double Z   ,double DENS ,double T,double HMASS);

//______________________________________________________________________________
StiElossCalculator::StiElossCalculator(double zOverA, double ionization, double A, double Z, double Dens) 
    : _zOverA(zOverA), _ionization2(ionization*ionization), _A(A), _Z(Z), _Dens(Dens) 
{
static int nCall=0; nCall++;
  assert(_A<=0 || _Z >0);
  mId=nCall;
}
//______________________________________________________________________________
StiElossCalculator::StiElossCalculator() 
    : _zOverA(0), _ionization2(0), _A(0), _Z(0), _Dens(0) 
{
static int nCall=0; nCall++;
  mId=-nCall;
}

//______________________________________________________________________________
void StiElossCalculator::set(double zOverA, double ionization, double A, double Z, double Dens) 
{
  _zOverA = zOverA; _ionization2 = ionization*ionization; _A = A; _Z = Z; _Dens = Dens; 
}
//______________________________________________________________________________

/// Energy Loss Calculator Constructor
///\param zOverA Ratio of Z to A of the scattering material,
///\param  ionization2 square of the ionization potential.
//______________________________________________________________________________
StiElossCalculator::~StiElossCalculator(){}

///Calculate and return the average energy loss for an incoming 
///particle of atomic number "z", mass "m", and velocity "beta"
///according to the Bethe-Bloch specific energy loss equation.
/// This function uses the material parameters stored within this object.
///<p>
///The calculation of the maximum kinetic energy done here is exact.
///The density effects are neglected. 
///\param z2 square of atomic number "z" of incoming particle,
///\param _zOverA Ratio of Z to A of the scattering material,
///\param m  mass (in GeV/c2) of the incoming particle,
///\param beta2 square of the relative velocity (beta=v/c) of the incoming particle.
///\param  _ionization2 square of the ionization potential.
///\throw runtime_error whenever beta2==1
///\return energy loss in GeV*cm^2/g.
//______________________________________________________________________________
double StiElossCalculator::calculate(double z2, double m, double beta2) const
{
static int nCall=0; nCall++;
static int noEloss = StiDebug::iFlag("NOELOSS");
if (noEloss) return 0;

  if (_A <=0.) return 0.;
  double beta21 = 1 - beta2; if (beta21 < 1.e-10) beta21 = 1.e-10; 
  double T = m*(1./::sqrt(beta21) - 1);
  double dedx = gdrelx(_A,_Z,_Dens,T,m)*z2*_Dens;
assert(!std::isnan(dedx));
assert(dedx>=0 && dedx<1e3);
  return dedx;
}
//______________________________________________________________________________
double StiElossCalculator::calcError(double z2, double m, double beta2) const
{
static int noEloss = StiDebug::iFlag("NOELOSS");
if (noEloss) return 0;

  if (_A<=0.) return 0.;
  double err2=gsigma2(_Z/_A,1.,z2,m ,beta2,1.);
assert(!std::isnan(err2));
assert(err2>=0 && err2<1e3);
  return err2;
}

//______________________________________________________________________________
ostream& operator<<(ostream& os, const StiElossCalculator& m) {
  os << "zOverA "<< m.getzOverA()
     << "\tionization2 "<< m.getionization2()
     << "\tA "<< m.getA()
     << "\tZ "<< m.getZ()
     << "\tDens"<< m.getDens() << endl;
    return os;
}
//______________________________________________________________________________
//* Revision 1.1.1.1  1995/10/24 10:21:24  cernlib
//* Geant
//*
//*
//#include "geant321/pilot.h"
//*CMZ :  3.21/03 06/10/94  18.22.43  by  S.Giani
//*-- Author :
//______________________________________________________________________________
//      SUBROUTINE GDRELX(A,Z,DENS,T,HMASS,dedx)
double gdrelx(double A,double Z,double DENS,double T,double HMASS)
{
//
//    ******************************************************************
//    *                                                                *
//    *  Calculates the mean 1/DENS*dE/dx of a particle with kinetic   *
//    *  energy T in an element of atomic number Z, atomic weight A    *
//    *  and density DENS ( the density is just used for the           *
//    *  calculation of the density effect in the case of high T).     *
//    *  The routine reproduces the experimental and/or tabulated      *
//    *  energy losses rather well down to T -> 0.                     *
//    *  Simple parametrization is used for  T .le. T2L=2 MeV (see     *
//    *  H.H.Andersen,J.F.Ziegler:Hydrogen stopping powers and         *
//    *  ranges in all element,Pergamon Press,1977.).                  *
//    *  For T .gt. T2L=2 MeV the corrected Bethe-Bloch stopping       *
//    *  power / restricted energy loss formula is used.               *
//    *                                                                *
//    *                                                                *
//    *    ==>Called by : GDRELA                                       *
//    *       Author    L.Urban    *********                           *
//    *                                                                *
//    ******************************************************************
//
//#include "geant321/gconsp.inc"
//#include "geant321/gccuts.inc"
//#include "geant321/gcunit.inc"
static const double  AMUKEV=931494.32,AMUKEV50=pow(AMUKEV,0.50),AMUKEV45=pow(AMUKEV,0.45);
static const double  D=0.000153537,T1L=0.00001,T2L=0.002;
static const double  AVO=0.60221367,EMPROT=0.9382723,EMASS=0.0005109990615;
static const double  DCUTM=9999.;
//DIMENSION B(6,92),C(6,92),CECOF[6]
//*
static const double B[93][6]={ 
  {0.},
  {1.262	,1.44	,242.6	,12000.	,0.1159		,18.8},
  {1.229	,1.397	,484.5	,5873.	,0.05225	,41.7},
  {1.411	,1.6	,725.6	,3013.	,0.04578	,47.6},
  {2.248	,2.59	,966.	,153.8	,0.03475	,62.7},
  {2.474	,2.815	,1206.	,1060.	,0.02855	,76.0},
  {2.631	,2.989	,1445.	,957.2	,0.02819	,77.3},
  {2.954	,3.35	,1683.	,1900.	,0.02513	,86.7},
  {2.652	,3.	,1920.	,2000.	,0.0223		,97.7},
  {2.085	,2.352	,2157.	,2634.	,0.01816	,120.},
  {1.951	,2.199	,2393.	,2699.	,0.01568	,139.},
  {2.542	,2.869	,2628.	,1854.	,0.01472	,148.},
  {3.792	,4.293	,2862.	,1009.	,0.01397	,156.},
  {4.154	,4.739	,2766.	,164.5	,0.02023	,162.},
  {4.15		,4.7	,3329.	,550.	,0.01321	,165.},
  {3.232	,3.647	,3561.	,1560.	,0.01267	,172.},
  {3.447	,3.891	,3792.	,1219.	,0.01211	,180.},
  {5.047	,5.714	,4023.	,878.6	,0.01178	,185.},
  {5.731	,6.5	,4253.	,530.	,0.01123	,194.},
  {5.151	,5.833	,4482.	,545.7	,0.01129	,193.},
  {5.521	,6.252	,4710.	,553.3	,0.01112	,196.},
  {5.201	,5.884	,4938.	,560.9	,0.009995	,218.},
  {4.862	,5.496	,5165.	,568.5	,0.009474	,230.},
  {4.48		,5.055	,5391.	,952.3	,0.009117	,239.},
  {3.983	,4.489	,5616.	,1336.	,0.008413	,259.},
  {3.469	,3.907	,5725.	,1461.	,0.008829	,270.},
  {3.519	,3.963	,6065.	,1243.	,0.007782	,280.},
  {3.14		,3.535	,6288.	,1372.	,0.007361	,296.},
  {3.553	,4.004	,6205.	,555.1	,0.008763	,310.},
  {3.696	,4.175	,4673.	,387.8	,0.02188	,322.},
  {4.21		,4.75	,6953.	,295.2	,0.006809	,320.},
  {5.041	,5.697	,7173.	,202.6	,0.006725	,324.},
  {5.554	,6.3	,6496.	,110.	,0.009689	,330.},
  {5.323	,6.012	,7611.	,292.5	,0.006447	,338.},
  {5.874	,6.656	,7395.	,117.5	,0.007684	,340.},
  {5.611	,6.335	,8046.	,365.2	,0.006244	,349.},
  {6.411	,7.25	,8262.	,220.	,0.006087	,358.},
  {5.694	,6.429	,8478.	,292.9	,0.006087	,358.},
  {6.339	,7.159	,8693.	,330.3	,0.006003	,363.},
  {6.407	,7.234	,8907.	,367.8	,0.005889	,370.},
  {6.734	,7.603	,9120.	,405.2	,0.005765	,378.},
  {6.902	,7.791	,9333.	,442.7	,0.005587	,390.},
  {6.425	,7.248	,9545.	,480.2	,0.005367	,406.},
  {6.799	,7.671	,9756.	,517.6	,0.005315	,410.},
  {6.108	,6.887	,9966.	,555.1	,0.005151	,423.},
  {5.924	,6.677	,10180.	,592.5	,0.004919	,443.},
  {5.238	,5.9	,10380.	,630.	,0.004758	,458.},
  {5.623	,6.354	,7160.	,337.6	,0.01394	,466.},
  {5.814	,6.554	,10800.	,355.5	,0.004626	,471.},
  {6.23		,7.024	,11010.	,370.9	,0.00454	,480.},
  {6.41		,7.227	,11210.	,386.4	,0.004474	,487.},
  {7.5		,8.48	,8608.	,348.	,0.009074	,494.},
  {6.979	,7.871	,11620.	,392.4	,0.004402	,495.},
  {7.725	,8.716	,11830.	,394.8	,0.004376	,498.},
  {8.231	,9.289	,12030.	,397.3	,0.004384	,497.},
  {7.287	,8.218	,12230.	,399.7	,0.004447	,490.},
  {7.899	,8.911	,12430.	,402.1	,0.004511	,483.},
  {8.041	,9.071	,12630.	,404.5	,0.00454	,480.},
  {7.489	,8.444	,12830.	,406.9	,0.00442	,493.},
  {7.291	,8.219	,13030.	,409.3	,0.004298	,507.},
  {7.098	,8.	,13230.	,411.8	,0.004182	,521.},
  {6.91		,7.786	,13430.	,414.2	,0.004058	,537.},
  {6.728	,7.58	,13620.	,416.6	,0.003976	,548.},
  {6.551	,7.38	,13820.	,419.	,0.003877	,562.},
  {6.739	,7.592	,14020.	,421.4	,0.003863	,564.},
  {6.212	,6.996	,14210.	,423.9	,0.003725	,585.},
  {5.517	,6.21	,14400.	,426.3	,0.003632	,600.},
  {5.219	,5.874	,14600.	,428.7	,0.003498	,623.},
  {5.071	,5.706	,14790.	,433.	,0.003405	,640.},
  {4.926	,5.542	,14980.	,433.5	,0.003342	,652.},
  {4.787	,5.386	,15170.	,435.9	,0.003292	,662.},
  {4.893	,5.505	,15360.	,438.4	,0.003243	,672.},
  {5.028	,5.657	,15550.	,440.8	,0.003195	,682.},
  {4.738	,5.329	,15740.	,443.2	,0.003186	,684.},
  {4.574	,5.144	,15930.	,442.4	,0.003144	,693.},
  {5.2		,5.851	,16120.	,441.6	,0.003122	,698.},
  {5.07		,5.704	,16300.	,440.9	,0.003082	,707.},
  {4.945	,5.563	,16490.	,440.1	,0.002965	,735.},
  {4.476	,5.034	,16670.	,439.3	,0.002871	,759.},
  {4.856	,5.46	,18320.	,438.5	,0.002542	,755.},
  {4.308	,4.843	,17040.	,487.8	,0.002882	,756.},
  {4.723	,5.311	,17220.	,537.	,0.002913	,748.},
  {5.319	,5.982	,17400.	,586.3	,0.002871	,759.},
  {5.956	,6.7	,17800.	,677.	,0.00266	,765.},
  {6.158	,6.928	,17770.	,586.3	,0.002812	,775.},
  {6.204	,6.979	,17950.	,586.3	,0.002776	,785.},
  {6.181	,6.954	,18120.	,586.3	,0.002748	,793.},
  {6.949	,7.82	,18300.	,586.3	,0.002737	,796.},
  {7.506	,8.448	,18480.	,586.3	,0.002727	,799.},
  {7.649	,8.609	,18660.	,586.3	,0.002697	,808.},
  {7.71		,8.679	,18830.	,586.3	,0.002641	,825.},
  {7.407	,8.336	,19010.	,586.3	,0.002603	,837.},
  {7.29		,8.204	,19180.	,586.3	,0.002573	,847.}};

static const double CECOF[7]={0.,0.42237,0.0304,-0.00038,3.858,-0.1668,0.00158};
double C[6]={0};

double poti,p,e,beta,bet2,tau,sl,sh,eta,eta2,b2g2,tmax,cc,x0,x1,xa,xm,delta;
double f1,f2,f3,f4,f5,tupp,ce,st,sbb,dedx;
//*     ------------------------------------------------------------------
//*      in the case of non-integer Z the low energy parameters
//*      and the ionization potential are taken at INT(Z) !
//*
  int iz=(int)(Z+1e-8); if (iz == 92) iz = 91;
  double wt1=Z-iz,wt0 = 1-wt1;
  assert((iz>0)&&(iz<92));
//*
//*     Calculate coefficients C(I,J) if it has not been done already
//*
    double fac=AVO/A;
    C[0]=fac*AMUKEV50*(B[iz][0]*wt0+B[iz+1][0]*wt1);
    C[1]=fac*AMUKEV45*(B[iz][1]*wt0+B[iz+1][1]*wt1);
    C[2]=fac*         (B[iz][2]*wt0+B[iz+1][2]*wt1)/AMUKEV;
    C[3]=             (B[iz][3]*wt0+B[iz+1][3]*wt1)/AMUKEV;
    C[4]=AMUKEV*      (B[iz][4]*wt0+B[iz+1][4]*wt1);
//*                     poti=16.E-9*Z**0.9
    C[5]=             (B[iz][5]*wt0+B[iz+1][5]*wt1)*1.E-9;
//*
//*     ----------------------------------------------------------------
  double hmass2 = HMASS*HMASS;
  double T1LIM=HMASS*T1L/EMPROT;
  double T2LIM=HMASS*T2L/EMPROT;
//*
//*     Calculate dE/dx
//*     ---> for T .le. T1LIM (very low energy)
//*
  if(T<=T1LIM) {
     tau=T/HMASS;
     dedx=C[0]*pow(tau,0.5);
  } else {
//*
//*     ---> for T1LIM .lt. T   and  T .le. T2LIM (low energy)
//*
     if(T<=T2LIM) {
       tau=T/HMASS;
       sl=C[1]*pow(tau,0.45);
       sh=C[2]*log(1.+C[3]/tau+C[4]*tau)/tau;
       dedx=sl*sh/(sl+sh);
//*
//*     ---> for T .gt. T2LIM ( "high " energy , Bethe-Bloch formula)
//*
     } else {
       p=sqrt(T*(T+2.*HMASS));
       e=T+HMASS;
       beta=p/e;
       bet2=beta*beta;
       eta=p/HMASS;
       eta2=eta*eta;
//*+++ new line follows.....
       b2g2=eta*eta;
//*+++ end of correction
       tmax=2.*EMASS*T*(T+2.*HMASS);
//*+++  correction of the next line
//*           tmax=tmax/(HMASS**2+EMASS**2+EMASS*(T+HMASS));
       tmax=tmax/(hmass2+EMASS*(EMASS+2.*e));
//*+++ end of correction
//*
//*         density correction
//*
       poti=C[5];
       cc=1.+2.*log(poti/(28.8E-9*sqrt(DENS*Z/A)));
//*         condensed material ? ( dens .gt. 0.05 ? )
       if(DENS>0.05) {
         if(poti < 1.E-7) {
            if(cc < 3.681) {
               x0=0.2;
            } else {
               x0=0.326*cc-1.;
            }
            x1=2.;
         } else {
            if(cc < 5.215) {
               x0=0.2;
            } else {
               x0=0.326*cc-1.5;
            }
            x1=3.;
         }
//*         gas ?   ( dens . le . 0.05 ? )
       } else {
          if(cc<=12.25) {
             int ip=(int)((cc-10.)/0.5)+1;
             if(ip < 0) ip=0;
             if(ip>4) ip=4;
             x0=1.6+0.1*ip;
             x1=4.;
          } else {
             if(cc<=13.804) {
                x0=2.;
                x1=5.;
             } else {
                x0=0.326*cc-2.5;
                x1=5.;
             }
          }
       }
//*
       xa=cc/4.606;
       xm=3.;
       double aa=4.606*(xa-x0)/pow(x1-x0,xm);
//*
       double x=log10(eta);
       delta=0.;
       if(x>x0) {
          delta=4.606*x-cc;
          if(x < x1) delta=delta+aa*pow(x1-x,xm);
       }
//*
//*         calculate shell correction
//*
       double potsq=poti*poti;
       if(eta>0.13) {
          f1=1./eta2;
          f2=f1*f1;
          f3=f1*f2;
          f4=(f1*CECOF[1]+f2*CECOF[2]+f3*CECOF[3])*1.E+12;
          f5=(f1*CECOF[4]+f2*CECOF[5]+f3*CECOF[6])*1.E+18;
          ce=f4*potsq+f5*potsq*poti;
       } else {
          eta2=0.0169;
          f1=1./eta2;
          f2=f1*f1;
          f3=f1*f2;
          f4=(f1*CECOF[1]+f2*CECOF[2]+f3*CECOF[3])*1.E+12;
          f5=(f1*CECOF[4]+f2*CECOF[5]+f3*CECOF[6])*1.E+18;
          ce=f4*potsq+f5*potsq*poti;
          ce=ce*log(T/T2LIM)/log(0.0079/T2LIM);
       }
//*
       f1=D*Z/(A*bet2);
//*
//*         stopping power or restricted dE/dx ?
//*
//*+++  correction of the next few lines
//*           if(DCUTM.GE.tmax) {
//*              f2=2.*(log(tmax/poti)-bet2)
//*           } else {
//*              f2=log(tmax*DCUTM/potsq)-bet2*(1.+DCUTM/tmax)
//*           }
       tupp=DCUTM;
       if(tmax < DCUTM) tupp=tmax;
       f2=log(2.*EMASS*b2g2/poti)+log(tupp/poti)-bet2*(1.+tupp/tmax);
//*+++ end of correction
       dedx=f1*(f2-delta-2.*ce/Z);
//*
//*
       tau=T2LIM/HMASS;
       sl=C[1]*pow(tau,0.45);
       sh=C[2]*log(1.+C[3]/tau+C[4]*tau)/tau;
       st=sl*sh/(sl+sh);
//*
       tmax=2.*EMASS*T2LIM*(T2LIM+2.*HMASS);
//*+++  correction of the next line
//*           tmax=tmax/(HMASS**2+EMASS**2+EMASS*(T2LIM+HMASS));
       tmax=tmax/(HMASS*HMASS+EMASS*EMASS+2.*EMASS*(T2LIM+HMASS));
//*+++  end of correction
       bet2=T2LIM*(T2LIM+2.*HMASS)/pow(T2LIM+HMASS,2);
       sbb=2.*(log(tmax/poti)-bet2);
       sbb=D*Z*sbb/(A*bet2);
       double corbb=(st/sbb-1.)*T2LIM;
//*
       dedx=dedx*(1.+corbb/T);
//*
     }
  }
  return dedx;
}
//*CMZ :  3.21/02 29/03/94  15.41.21  by  S.Giani
//-- Author :
//______________________________________________________________________________
double gsigma2(double ZoverA,double DENS,double CHARGE2
              ,double AMASS ,double BET2,double STEP  )
{
//      SUBROUTINE GFLUCT(DEMEAN,DE)

static const double DGEV=0.153536E-3,EMASS=0.0005109990615;
//
  double gamm2 = 1./(1.-BET2);
  double gamma = sqrt(gamm2);
//
// ***    low energy transfer
  double xi = DGEV*CHARGE2*STEP*DENS*ZoverA/(BET2);
//
//  Energy straggling using Gaussian
//  STEP   =  current step-length (cm)
//  Author      : G.N. Patrick
//
//
//     Maximum energy transfer to atomic electron (GeV).
   double eta2 = BET2*gamm2;
   double ratio = EMASS/AMASS;
   double emax =(2*EMASS*eta2)/(1+2*ratio*gamma+ratio*ratio); 
//
//     +-----------------------------------+
//     I Sample from Gaussian distribution I
//     +-----------------------------------+
   double sigma2 = xi*(1.-0.5*BET2)*emax;
   return sigma2;
}

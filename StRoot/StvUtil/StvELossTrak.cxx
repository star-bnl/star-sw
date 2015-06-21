// $Id: StvELossTrak.cxx,v 1.17 2015/06/18 23:31:26 perev Exp $
//
//
// Class StvELossTrak
// ------------------
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include "TGeoMaterial.h"
#include "StvUtil/StvDebug.h"
#include "StvELossTrak.h"
static int gNoEloss = 0;

//SUBROUTINE G3DRELX(A,Z,DENS,T,HMASS,DEDX)

static double gsigma2(double ZoverA,double DENS,double CHARGE2
                     ,double AMASS ,double BET2,double STEP  );
//static double gdrelx (double A     ,double Z   ,double DENS ,double T,double HMASS);

static const double kPiMass=0.13956995;
static const double kMinP = 0.01,kMinE = sqrt(kMinP*kMinP+kPiMass*kPiMass);
static const double kMaxP = 1000,kMaxE = sqrt(kMaxP*kMaxP+kPiMass*kPiMass);
static const double kMaxFak=1,kMaxTheta2=1,kMaxPartOfE = 0.1;
static const double kMomTol=1e-2;

ClassImp(StvELossTrak)

//_____________________________________________________________________________
StvELossTrak::StvELossTrak()
{
  gNoEloss = StvDebug::iFlag("NOELOSS");
  memset(fBeg,0,fEnd-fBeg+1);
}
//______________________________________________________________________________
void StvELossTrak::reset()
{
  fMats.clear();
  memset(fBeg,0,fEnd-fBeg+1);
}
//______________________________________________________________________________
void StvELossTrak::unset()
{
  fMats.clear();
}
//_____________________________________________________________________________
void StvELossTrak::Reset(int dir,double mass, double charge)
{
  memset(fBeg,0,fEnd-fBeg+1);
  fMats.clear();
  fDir = dir; fM=mass; fCharge=charge;
  
}
//_____________________________________________________________________________
void StvELossTrak::Clear(const char*)
{
  memset(fMed,0,fEnd-fMed+1);
}
//_____________________________________________________________________________
void StvELossTrak::Set(double A, double Z, double dens, double x0, double p,const TGeoMaterial *mate)
{
  fdEdX=0;
  if (p<=0) p = fP[1];
  if (p<kMinP) p=kMinP;
  if (p>kMaxP) p=kMaxP;
  if (A<=0) x0 = 1e+11;
  if (!Same(A,Z,dens,x0,p)) {
  // Normal, non update mode. Save material data
    fMats.resize(fMats.size()+1);
    Aux &M = fMats.back();
    M.fLen=0;
    M.fA=A;
    M.fZ=Z;
    M.fDens=dens;
    M.fX0=x0; 
    M.fP = p;
assert(mate);
    M.fMat = mate;
  }  
  if (fP[0]<=0) fP[0] = p; 
  fP[1] = p;
  fdEdX=0;fdEdXErr2=0;
  double p2 = fP[1]*fP[1],m2 = fM*fM;
  fE = sqrt(p2+m2);
  fFak = (14.1*14.1)*(p2+m2)/(p2*p2*1e6);
  if (fFak>kMaxFak) fFak = kMaxFak;
  double T = fE-fM;
  if (A>0) {
    double charge2 = fCharge*fCharge;
    if (!mate || !mate->IsMixture()) { 	//it is not mixture
      fdEdX = gdrelx(A,Z,dens,T,fM)*dens*charge2;

    } else {				//Mixture case

      fdEdX = 0;
      const TGeoMixture *mix = (const TGeoMixture*)mate;
      int nMix 		= mix->GetNelements();
      const double *wt  = mix->GetWmixt();
      const double *zz  = mix->GetZmixt();
      const double *aa  = mix->GetAmixt();
      for (int iMix=0;iMix<nMix;iMix++) {
        fdEdX+= gdrelx(aa[iMix],zz[iMix],dens*wt[iMix],T,fM)*dens*wt[iMix];
      }
      fdEdX*=charge2;
    }

    fMats.back().fdEdX =fdEdX; 
    double beta2 = p2/(p2+m2);
    fdEdXErr2 = gsigma2(Z/(A+1e-6),dens,charge2,fM ,beta2,1.);
  }
}
//_____________________________________________________________________________
int StvELossTrak::Same(double A, double Z, double dens, double x0, double p) const
{
static const double kAccu = 1e-2;
  if (!fMats.size()) return 0;
  const Aux &M = fMats.back();
  

  if (fabs(M.fA-A)> kAccu*M.fA) 		return 0;
  if (fabs(M.fZ-Z)> kAccu*M.fZ) 		return 0;
  if (fabs(M.fDens-dens)> 1e-5+kAccu*M.fDens) 	return 0;
  if (fabs(M.fP-p)> 1e-3*M.fP) 			return 0;
  						return 1;
}
//_____________________________________________________________________________
void StvELossTrak::Set(const TGeoMaterial *mate,double p)
{
    Set(mate->GetA(),mate->GetZ(),mate->GetDensity(),mate->GetRadLen(),p,mate);
}
//______________________________________________________________________________
//_____________________________________________________________________________
void StvELossTrak::Add(double len)
{
// fMCS[0] = Thet2*L1*( L1*L1/3+L0*(L1+L0)   );
// fMCS[1] = Thet2*L1*( -L1-2*L0);
// fMCS[2] = Thet2*L1;
// return fMCS[0]+L*(fMCS[1]+L*fMCS[2]); 
static int nCall=0; nCall++;
StvDebug::Break(nCall);
  double myLen = len;
  while (1) {
    double dP = fP[1]*kMomTol;
    double dE = dP*fP[1]/fE;
    double dL = dE/(fdEdX+1e-11);
    if (fDir && fP[1]-dP<kMinP) dL=myLen;

    if (dL>myLen) dL=myLen;
    Aux &aux = fMats.back();
    aux.fLen+=dL;
    double QQ = fFak/fMats.back().fX0;
    double theta2  	= QQ*dL;
    if (theta2 >kMaxTheta2) theta2 = kMaxTheta2;
    if (fMCS[2]+theta2>kMaxTheta2) theta2 = kMaxTheta2-fMCS[2];
    fMCS[2] += theta2;
    fMCS[1] += theta2*(-2*fTotLen - dL);
    fMCS[0] += theta2*(dL*dL/3+fTotLen*(fTotLen+dL));
assert(fMCS[2]< kMaxTheta2*1.1);
    double ELoss  = fdEdX *dL;
    if (    ELoss > kMaxPartOfE*fE)     ELoss = kMaxPartOfE*fE;
    fTotELoss += ELoss;
    if (fTotELoss > kMaxPartOfE*fE) fTotELoss = kMaxPartOfE*fE;

    fTotELossErr2 += fdEdXErr2*dL;
    dP = ELoss*fE/fP[1];
    if (fDir) { fP[1]-=dP; if (fP[1]<kMinP) fP[1]=kMinP; fE-=ELoss;} else { fP[1]+=dP; fE+=ELoss;}

//    fP[1] = fP[0]; ///??????????????????????????????????????????????????????????????????

    fTotLen+=dL;  myLen-=dL;
    if (myLen<=1e-5) break;
    const TGeoMaterial *mat = aux.fMat;
assert(mat);
    Set(aux.fA, aux.fZ, aux.fDens, aux.fX0, fP[1],mat);
  }

}
//_____________________________________________________________________________
const StvELossTrak::Aux &StvELossTrak::GetMate(int idx)
{
  assert(idx<(int)fMats.size());
  return fMats[idx];
}

//_____________________________________________________________________________
double StvELossTrak::GetTheta2() const 
{
  if (gNoEloss) return 0;
  return fMCS[2];
}
//_____________________________________________________________________________
double StvELossTrak::GetOrt2() const 
{
  if (gNoEloss) return 0;
  return fMCS[0]+fTotLen*(fMCS[1]+fTotLen*fMCS[2]);
}
//_____________________________________________________________________________
double StvELossTrak::PLoss(double p) const 
{
  if (gNoEloss) return 0;
  double ep = (fM/p)*(fM/p);
  ep = (ep<0.1)? (1+ep*0.5) : sqrt(1+ep);
  return fTotELoss*ep/fTotLen;
}
//_____________________________________________________________________________
double StvELossTrak::dPLossdP0(double p) const 
{
  if (gNoEloss) return 0;
  double ep = (fM/p)*(fM/p);
  ep = (ep<0.1)? (1+ep*0.5) : sqrt(1+ep);
  if (fabs(fdLogEdLogP) <=0) Update();
  return fdLogEdLogP*fTotELoss*ep/(p*fTotLen);

}
//_____________________________________________________________________________
void StvELossTrak::Update(int dir,double pMom)
{
static int nCall = 0; nCall++;
StvDebug::Break(nCall);

  int jBeg=0,jEnd=fMats.size(),jStp=1;
  if (dir!=(int)fDir) {jBeg=jEnd-1;jEnd=-1;jStp=-1;}
//              Save the eloss and p to calculate (dP/P/len)/dP
  double pBef = fP[0];;
  double eBef = fTotELoss/fTotLen;
  
  double p = pMom;
  if (fabs(pBef -pMom)<1e-2*pBef) return;
  Clear(); 
  fDir=dir; fP[0]=0;
  AuxVect mats; mats.swap(fMats);
  for (int jMat=jBeg; jMat!=jEnd; jMat+=jStp) {
    Aux &M = mats[jMat];
    Set(M.fA, M.fZ, M.fDens, M.fX0,p,M.fMat);
    Add(M.fLen);
assert(M.fMat);
    p = 0;
  }
  double pNow =  fP[0]; 
  double eNow = fTotELoss/fTotLen;
  double dP = (pNow-pBef)/pBef;
  if (fabs(dP)<1e-3) return;
  if (fabs(dP)>0.1) dP = log(1.+dP);

  double dE = (eNow-eBef)/eBef;
  if (fabs(dE)>0.1) dE = log(1.+dE);
  fdLogEdLogP = dE/dP;


}
//_____________________________________________________________________________
void StvELossTrak::Update() const
{
   double dP = fP[0]*0.01;
   StvELossTrak elt = *this;
   elt.Update(fDir,fP[0]+dP);
   fdLogEdLogP = elt.fdLogEdLogP;
}
//_____________________________________________________________________________
void StvELossTrak::Print(const char *opt) const
{
  printf("ELossTrak: dir=%d q=%d mass=%g p=%g %g totLen=%g totELoss=%g\n"
        ,int(fDir),int(fCharge),fM,fP[0],fP[1],fTotLen,fTotELoss);
  double totLen = 0,totE=0;
  for (int i=0;i<(int)fMats.size();i++) {
    const Aux &M = fMats[i];
    double dE = M.fLen*M.fdEdX;
    double T = sqrt(M.fP*M.fP+fM*fM)-fM;
    printf("%6.3f totE=%g T=%g dL=%g dens=%g dE=%g dEdX=%g\n"
          ,totLen,totE,T,M.fLen,M.fDens,dE,M.fdEdX);
    totLen+=M.fLen; totE+=dE;
  }
}
//_____________________________________________________________________________
class AofZ_t {
public:
int mZ;
double mA;
char mName[8];
char mTitle[20];
};
#if 1
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
double StvELossTrak::gdrelx(double A1,double Z1,double DENS1,double T1,double HMASS1)
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
#define double float
float A=A1,Z=Z1,DENS=DENS1,T=T1,HMASS=HMASS1;



static const double  AMUKEV=931494.32,AMUKEV50=pow(AMUKEV,0.50),AMUKEV45=pow(AMUKEV,0.45);
static const double  D=0.000153537,T1L=0.00001,T2L=0.002;
static const double  AVO=0.60221367,EMPROT=0.9382723,EMASS=0.0005109990615;
static const double  DCUTM=0.001;
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
static       double C[6]={0};
enum {kNAW=118};
AofZ_t AW[kNAW]={
{1,1.0079,	"H",	"Hydrogen"	},
{2,4.0026,	"He",	"Helium"	},
{3,6.941,	"Li",	"Lithium"	},
{4,9.0122,	"Be",	"Beryllium"	},
{5,10.811,	"B",	"Boron"		},
{6,12.0107,	"C",	"Carbon"	},
{7,14.0067,	"N",	"Nitrogen"	},
{8,15.9994,	"O",	"Oxygen"	},
{9,18.9984,	"F",	"Fluorine"	},
{10,20.1797,	"Ne",	"Neon"		},
{11,22.9897,	"Na",	"Sodium"	},
{12,24.305,	"Mg",	"Magnesium"	},
{13,26.9815,	"Al",	"Aluminum"	},
{14,28.0855,	"Si",	"Silicon"	},
{15,30.9738,	"P",	"Phosphorus"	},
{16,32.065,	"S",	"Sulfur"	},
{17,35.453,	"Cl",	"Chlorine"	},
{18,39.948,	"Ar",	"Argon"		},
{19,39.0983,	"K",	"Potassium"	},
{20,40.078,	"Ca",	"Calcium"	},
{21,44.9559,	"Sc",	"Scandium"	},
{22,47.867,	"Ti",	"Titanium"	},
{23,50.9415,	"V",	"Vanadium"	},
{24,51.9961,	"Cr",	"Chromium"	},
{25,54.938,	"Mn",	"Manganese"	},
{26,55.845,	"Fe",	"Iron"		},
{27,58.9332,	"Co",	"Cobalt"	},
{28,58.6934,	"Ni",	"Nickel"	},
{29,63.546,	"Cu",	"Copper"	},
{30,65.39,	"Zn",	"Zinc"		},
{31,69.723,	"Ga",	"Gallium"	},
{32,72.64,	"Ge",	"Germanium"	},
{33,74.9216,	"As",	"Arsenic"	},
{34,78.96,	"Se",	"Selenium"	},
{35,79.904,	"Br",	"Bromine"	},
{36,83.8,	"Kr",	"Krypton"	},
{37,85.4678,	"Rb",	"Rubidium"	},
{38,87.62,	"Sr",	"Strontium"	},
{39,88.9059,	"Y",	"Yttrium"	},
{40,91.224,	"Zr",	"Zirconium"	},
{41,92.9064,	"Nb",	"Niobium"	},
{42,95.94,	"Mo",	"Molybdenum"	},
{43,98,		"Tc",	"Technetium"	},
{44,101.07,	"Ru",	"Ruthenium"	},
{45,102.906,	"Rh",	"Rhodium"	},
{46,106.42,	"Pd",	"Palladium"	},
{47,107.868,	"Ag",	"Silver"	},
{48,112.411,	"Cd",	"Cadmium"	},
{49,114.818,	"In",	"Indium"	},
{50,118.71,	"Sn",	"Tin"		},
{51,121.76,	"Sb",	"Antimony"	},
{52,127.6,	"Te",	"Tellurium"	},
{53,126.904,	"I",	"Iodine"	},
{54,131.293,	"Xe",	"Xenon"		},
{55,132.905,	"Cs",	"Cesium"	},
{56,137.327,	"Ba",	"Barium"	},
{57,138.905,	"La",	"Lanthanum"	},
{58,140.116,	"Ce",	"Cerium"	},
{59,140.908,	"Pr",	"Praseodymium"	},
{60,144.24,	"Nd",	"Neodymium"	},
{61,145,	"Pm",	"Promethium"	},
{62,150.36,	"Sm",	"Samarium"	},
{63,151.964,	"Eu",	"Europium"	},
{64,157.25,	"Gd",	"Gadolinium"	},
{65,158.925,	"Tb",	"Terbium"	},
{66,162.5,	"Dy",	"Dysprosium"	},
{67,164.93,	"Ho",	"Holmium"	},
{68,167.259,	"Er",	"Erbium"	},
{69,168.934,	"Tm",	"Thulium"	},
{70,173.04,	"Yb",	"Ytterbium"	},
{71,174.967,	"Lu",	"Lutetium"	},
{72,178.49,	"Hf",	"Hafnium"	},
{73,180.948,	"Ta",	"Tantalum"	},
{74,183.84,	"W",	"Tungsten"	},
{75,186.207,	"Re",	"Rhenium"	},
{76,190.23,	"Os",	"Osmium"	},
{77,192.217,	"Ir",	"Iridium"	},
{78,195.078,	"Pt",	"Platinum"	},
{79,196.966,	"Au",	"Gold"		},
{80,200.59,	"Hg",	"Mercury"	},
{81,204.383,	"Tl",	"Thallium"	},
{82,207.2,	"Pb",	"Lead"		},
{83,208.98,	"Bi",	"Bismuth"	},
{84,209,	"Po",	"Polonium"	},
{85,210,	"At",	"Astatine"	},
{86,222,	"Rn",	"Radon"		},
{87,223,	"Fr",	"Francium"	},
{88,226,	"Ra",	"Radium"	},
{89,227,	"Ac",	"Actinium"	},
{90,232.038,	"Th",	"Thorium"	},
{91,231.036,	"Pa",	"Protactinium"	},
{92,238.029,	"U",	"Uranium"	},
{93,237,	"Np",	"Neptunium"	},
{94,244,	"Pu",	"Plutonium"	},
{95,243,	"Am",	"Americium"	},
{96,247,	"Cm",	"Curium"	},
{97,247,	"Bk",	"Berkelium"	},
{98,251,	"Cf",	"Californium"	},
{99,252,	"Es",	"Einsteinium"	},
{100,257,	"Fm",	"Fermium"	},
{101,258,	"Md",	"Mendelevium"	},
{102,259,	"No",	"Nobelium"	},
{103,262,	"Lr",	"Lawrencium"	},
{104,261,	"Rf",	"Rutherfordium"	},
{105,262,	"Db",	"Dubnium"	},
{106,266,	"Sg",	"Seaborgium"	},
{107,264,	"Bh",	"Bohrium"	},
{108,277,	"Hs",	"Hassium"	},
{109,268,	"Mt",	"Meitnerium"	},
{110,0,		"Ds",	"Darmstadtium"	},
{111,272,	"Rg",	"Roentgenium"	},
{112,0,		"Uub",	"Ununbium"	},
{113,0,		"Uut",	"Ununtrium"	},
{114,0,		"Uuq",	"Ununquadium"	},
{115,0,		"Uup",	"Ununpentium"	},
{116,0,		"Uuh",	"Ununhexium"	},
{117,0,		"Uus",	"Ununseptium"	},
{118,0,		"Uuo",	"Ununoctium"	}};

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
    double fac0=AVO/AW[iz  ].mA;
    double fac1=AVO/AW[iz+1].mA;
    C[0]=AMUKEV50*(B[iz][0]*wt0*fac0+B[iz+1][0]*wt1*fac1);
    C[1]=AMUKEV45*(B[iz][1]*wt0*fac0+B[iz+1][1]*wt1*fac1);
    C[2]=         (B[iz][2]*wt0*fac0+B[iz+1][2]*wt1*fac1)/AMUKEV;
    C[3]=         (B[iz][3]*wt0     +B[iz+1][3]*wt1     )/AMUKEV;
    C[4]=AMUKEV*  (B[iz][4]*wt0     +B[iz+1][4]*wt1     );
//*                     poti=16.E-9*Z**0.9
    C[5]=         (B[iz][5]*wt0+B[iz+1][5]*wt1)*1.E-9;
//*
//*     ----------------------------------------------------------------
  double hmass2 = HMASS*HMASS;
  double T1LIM=HMASS*T1L/EMPROT;
  double T2LIM=HMASS*T2L/EMPROT;
//*
//*     Calculate dE/dx
//*     ---> for T .le. T1LIM (very low energy)
//*
  if (gNoEloss) return 0;
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
#endif //0
//
#undef double

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

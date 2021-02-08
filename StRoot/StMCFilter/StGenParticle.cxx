// @(#)root/eg:$Id: StGenParticle.cxx,v 1.10 2015/12/03 23:19:22 jwebb Exp $


//______________________________________________________________________________
#include "stdlib.h"
#include "math.h"
#include <map>
#include "StGenParticle.h"
#include <iostream>
using namespace std;
//#include "StMessMgr.h"
//______________________________________________________________________________
void StGenParticle::Print(const char *opt) const
{
  static int nCall=0; nCall++;
  cout << GetIdx() << " -"  << endl;
  cout << " Ist=" << GetStatusCode() << endl;
  cout << " Pdg=" << GetPdgCode() << endl;
  cout << " Gea=" << GetGeaCode() << endl;

  double V[3];  Vertex(V); cout << " Z=" << V[2] << endl;

  if (GetNDaughters()) cout << "\tKids=" << GetNDaughters() << endl;

  int moth1 = -1,moth2=-1;
  const StGenParticle *m = GetMother(0);
  if (m) moth1 = m->GetIdx();
  m = GetMother(1);
  if (m) moth2 = m->GetIdx();

  if (moth1>=0 || moth2>=0) {
    cout << "\tMoth=(" << endl;
    if (moth1>=0) {cout << moth1 << endl;} else {cout << "_" <<endl;}
    cout << " " << endl;
    if (moth2>=0) {cout << moth2 << endl;} else {cout << "_" << endl;}
    cout << ")" <<endl;
  }

  //  cout << endl;

}
//______________________________________________________________________________
double  StGenParticle::GetCalcMass()  const
{
  double p[4];
  Momentum(p);
  double m =p[3]*p[3]-(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
  return (m>0) ? sqrt(m) : -sqrt(-m);
}
//______________________________________________________________________________
double StGenParticle::R()  const 
{ 
  double x[3]; Vertex(x); return sqrt(x[0]*x[0]+x[1]*x[1]);

}
//______________________________________________________________________________
double StGenParticle::Rho()  const 
{ 
  double x[3]; Vertex(x); return sqrt(x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);                   

}
//______________________________________________________________________________
double StGenParticle::P()  const 
{ 
  double p[4]; Momentum(p); return sqrt(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);

}
//______________________________________________________________________________
double StGenParticle::Pt()  const 
{ 
  double p[4]; Momentum(p); return sqrt(p[0]*p[0]+p[1]*p[1]);      

}
//______________________________________________________________________________
double StGenParticle::Energy()  const 
{ 
  double p[4]; Momentum(p); return p[3];                  

}
//______________________________________________________________________________
double StGenParticle::Eta()  const
{
  double p[4]; Momentum(p);                 
  double pmom = P();
  if (pmom > fabs(p[2])) return 0.5*log((pmom+p[2])/(pmom-p[2]));
  else                   return 1.e30;
}
//______________________________________________________________________________
double StGenParticle::Phi()  const
{
  double p[4]; Momentum(p);                 
  return atan2(p[1],p[0]);
}  
//______________________________________________________________________________
double StGenParticle::Theta()  const
{
  double p[4]; Momentum(p);                 
  return acos(p[2]/P());
}  
int  StGenParticle::GetPdgCode() const { return StGenParticleMaster::Gea2Pdg(GetGeaCode());}
int  StGenParticle::GetGeaCode() const { return StGenParticleMaster::Pdg2Gea(GetPdgCode());}
//______________________________________________________________________________
//______________________________________________________________________________
//______________________________________________________________________________
void StGenParticleMaster::Print(const char *tit) const
{
   if (!tit) tit = "";
   cout << "StGenParticleMaster::Print(" << tit << ")" << endl;
   const StGenParticle *p=0;

   // Loop over all particles stored by the particle master list and print them
   int i = 0;
   p = (*this)(i);
   while (p) {
     p->Print();
     p = (*this)(++i);
   };
   cout << endl;

}

// *
// * In rough order of multiplicity: gamma,pi+,pi-,pi0,etc
// *                -   gamma  pi+   pi-   pi0   K0L   K+   K-    K0S   Eta
// *               prot  neut  ~p    ~n    e+    e-   nu_e ~nu_e  mu+   mu-
// *              lamda sigm+ sigm0 sigm-  xi0   xi- omeg-
// *           ~: lamda sigm- sigm0 sigm+  xi0   xi+ omeg+
// *              tau+  tau-    D+    D-    D0  ~D0   DS+   DS-  LamC+
// *                W+    W-    Z0   nu_m ~nu_m nu_t ~nu_t  geant geant geant
// *              Deutron Triton Alpha HE3
enum {kNGEA = 50};
static const int IDGEA[] = {
                0,    1,    8,    9,    7,   10,   11,  12,   16,   17,
                14,   13,   15,   25,    2,    3,   4,    4,    5,    6,
                18,   19,   20,   21,   22,   23,  24,
                26,   27,   28,   29,   30,   31,  32,
                33,   34,   35,   36,   37,   38,  39,   40,   41,
                42,   43,   44,    4,    4,    4,   4,   48,   48,   48,
                45,    46,    47,    49};

static const int IDPDG[] = {
                 0,   22,  211, -211,  111,  130,  321, -321, 310,  221,
              2212, 2112,-2212,-2112,  -11,   11,  -12,  12,  -13,   13,
              3122, 3222, 3212, 3112, 3322, 3312, 3334,
             -3122,-3222,-3212,-3112,-3322,-3312,-3334,
               -15,   15,  411, -411,  421, -421,  431, -431, 4122,
                24,  -24,   23,  -14,   14,  -16,   16,   71,   72,  75,
            700201,700301,700202,700302};
//______________________________________________________________________________
int StGenParticleMaster::Gea2Pdg(int igea)  
{

static int PdgOfGea[kNGEA]={0};
  
  if (! PdgOfGea[1]) {
    for (int i = 0;i<kNGEA; i++) {  PdgOfGea[IDGEA[i]] = IDPDG[i]; }
  }
  if (igea >=kNGEA)	return 0;
  return PdgOfGea[igea];
}
//______________________________________________________________________________
double StGenParticleMaster::Gea2Mas(int igea)  
{
static double mass[kNGEA]={
	 0		,0		,0.00051	,0.00051	,0
	,0.10566	,0.10566	,0.13498	,0.13957	,0.13957
	,0.49767	,0.4936		,0.4936		,0.93957	,0.93827
	,0.93827	,0.49767	,0.54745	,1.11568	,1.18937
	,1.19255	,1.19744	,1.3149		,1.3213		,1.67245
	,0.93957	,1.11568	,1.18937	,1.19255	,1.19744
	,1.3149		,1.3213		,1.67245	,1.777		,1.777
	,1.8693		,1.8693		,1.8645		,1.8645		,1.9685
	,1.9685		,2.2849		,80.33		,80.33		,91.187
	,0		,0		,0		,0		,0};
	
  if (igea>=kNGEA) return 0.;
  return mass[igea];
}

//______________________________________________________________________________
int StGenParticleMaster::Pdg2Gea(int ipdg)  
{
typedef std::map<int, int > pdgMap_t;
static  pdgMap_t pdgMap;
static  int once = 2009;

  if (once) { once=0; for (int i=0;i<kNGEA;i++) {pdgMap[IDPDG[i]] = IDGEA[i];}}  
  
  pdgMap_t::iterator it = pdgMap.find(ipdg);
  if (it == pdgMap.end()) return 0;
  return (*it).second;
}  

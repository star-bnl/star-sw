/* 
   root.exe 'kfpAnalysis.C(1)' 
   gSystem->Load("StKFPX")
   StKFParticleXPropagator::Test()
*/

#include "StKFParticleXPropagator.h"
#include "TGeoManager.h"
#include "TMath.h"
#include "TVector3.h"
#include "KFPTrack.h"
#include "StDcaGeometry.h"
#include "TVector3.h"
#include "TVector2.h"
#include "TComplex.h"
ClassImp(StKFParticleXPropagator);
vector<MyMat_t> StKFParticleXPropagator::fgMaterials;
//________________________________________________________________________________
StKFParticleXPropagator::StKFParticleXPropagator() {//: KFParticleXPropagator() {
  fgMaterials.clear();
  MyMat_t myMat[] = { 
    //         Media name, type, No,   R_min,   R_max,       A,       Z,   density.       X_0,   Z_min,   Z_max, *Media
    //{"Vacuum"            ,"gas",  0,   3.900,   4.000,   1.000,   0.000, 0.000e+00, 1.000e+11, -76.200,  76.200, 0},//PIPE_1/PIPC_1
    //{"Vacuum"            ,"gas",  2,   3.875,   4.000,   1.000,   0.000, 0.000e+00, 1.000e+11,  76.200, 383.000, 0},//PIPE_1/PIPO_1/PVAO_1
    //{"Vacuum"            ,"gas",  4,   3.875,   4.000,   1.000,   0.000, 0.000e+00, 1.000e+11,-383.000, -76.200, 0},//PIPE_2/PIPO_1/PVAO_1
    {"Vacuum"            ,"mat",  2,   0.000,   3.875,   1.000,   0.000, 0.000e+00, 1.000e+11,  76.200, 383.000, 0},
    {"Vacuum"            ,"mat",  4,   0.000,   3.875,   1.000,   0.000, 0.000e+00, 1.000e+11,-383.000, -76.200, 0},
    {"Vacuum"            ,"mat",  0,   0.000,   3.900,   1.000,   0.000, 0.000e+00, 1.000e+11, -76.200,  76.200, 0},//PIPE_1/PIPC_1
    {"BERILLIUM"         ,"mat",  1,   3.900,   4.000,   9.010,   4.000,     1.848, 3.446e+01, -76.200,  76.200, 0},//PIPE_1/PIPC_1
    {"PIPE"              ,"mat",  3,   3.875,   4.000,  26.980,  13.000,     2.700, 8.875e+00,  76.200, 383.000, 0},//PIPE_1/PIPO_1/PVAO_1
    {"PIPE"              ,"mat",  5,   3.875,   4.000,  26.980,  13.000,     2.700, 8.875e+00,-383.000, -76.200, 0},//PIPE_2/PIPO_1/PVAO_1
    {"AIR"               ,"mat", -1,   4.000,   46.60,   14.61,     7.3,0.1205E-02,   30412.6,-383.000, 383.000, 0},
    {"ALUMINIUM_TIFC"    ,"mat",  7,  46.600,  51.700,  21.181,  10.311,     0.089, 3.220e+02,-230.876, 230.484, 0},///TIFC_1
    {"TPCE_SENSITIVE_GAS","gas",  8,  51.700, 200.000,  38.691,  17.436, 1.541e-03, 1.302e+04,-224.186, 223.794, 0},///TOFC_1
    {"ALUMINIUM_TOFC"    ,"mat",  9, 200.000, 207.731,  26.827,  12.886,     0.324, 7.480e+01,-224.186, 223.794, 0} ///TOFC_1
  };
  if (! gGeoManager) new TGeoManager("StKFParticleXPropagator","StKFParticleXPropagator");
  Int_t N = sizeof(myMat)/sizeof(MyMat_t);
  for (Int_t i = 0; i < N; i++) {
    TGeoMaterial *mat = gGeoManager->GetMaterial(myMat[i].Material);
    if (! mat) mat =  new TGeoMaterial ( myMat[i].Material
					 ,myMat[i].A
					 ,myMat[i].Z
					 ,myMat[i].Density
					 ,myMat[i].RadLen);
    myMat[i].mat = mat;
    fgMaterials.push_back(myMat[i]);
  }
}
//________________________________________________________________________________

int StKFParticleXPropagator::SqEqu(double *cba, double *sol)
{
//	
//	made from fortran routine GVPSQR (Geant320)
/*
************************************************************************
*                                                                      *
*     SUBROUTINE GVPSQR (CBA,SOL,NSOL)             870924  VP          *
*                                                                      *
*       SOLVE  QUADRATIC  EQUATION                                     *
*                                                                      *
*   ARGUMENTS:                                                         *
*       CBA     Array of coeff's A0 + A1*x + A2*x**2                   *
*       SOL     Solutions                                              *
*       NSOL    Number of solutions :                                  *
*               if zero - SOL[0]= extremum                             *
*               if -ve  - No solution at all                           *
*                                                                      *
************************************************************************
*/
  const double zero2=1.e-12;
  double swap,a,b,c,amx,dis,bdis;
  int nsol;
/*--------------------------------------------------------------------*/

  a = cba[2]; b = cba[1]*0.5; c = cba[0];
  if (b < 0.) {a = -a; b = -b; c = -c;}
  amx = fabs(a); if (amx<b) amx = b; if (amx<fabs(c)) amx = fabs(c);
  if (amx <= 0.) return -1;
  a = a/amx; b = b/amx; c = c/amx;

  dis = b*b - a*c;
  nsol = 1;
  if (fabs(dis) <= zero2)  dis = 0;
  if (dis < 0.) { nsol = 0; dis  = 0.;}

  dis = ::sqrt(dis); bdis = b + dis;
  if (fabs(c) > 1.e+10*bdis)	return -1;
  sol[0] = 0.;
  if (fabs(bdis) <= 0.)      	return nsol;
  sol[0] = (-c/bdis);		
  if (dis <= 0.)            	return nsol;
  if (bdis >= 1.e+10*fabs(a))   return nsol;    
  nsol   = 2; sol[1] = (-bdis/a);
  if (sol[0] > sol[1]) { swap = sol[0]; sol[0] = sol[1]; sol[1] = swap;}
  return nsol;
}
//_____________________________________________________________________________
// from THelixTrack::Step(double stmin,double stmax, const double *s, int nsurf, double *xyz, double *dir, int nearest) const
double StKFParticleXPropagator::GetDStoSurfaceBz(Float_t B,double stmin,double stmax, const double *s, int nsurf, Int_t nearest) //, Float_t dsdr[6], KFParticle *particle) 
{
  static Float_t kClight = 1e-12*TMath::C(); // EC=2.99792458E-4 = 0.000299792458f; [CM/S] [kG] [GeV/c]
  int ix,jx,nx,ip,jp;
  double poly[4][3],tri[3],sol[2],cos1t,f1,f2,step,ss;
  const double *sp[4][4] = {{s+0,s+1,s+2,s+3}, {s+1,s+4,s+7,s+9}, 
                            {s+2,s+7,s+5,s+8}, {s+3,s+9,s+8,s+6}}; 
  //  if (particle) fgmParticle = *particle;
  //  else          
  fgmParticle = fgParticle;
  KFParticle tempParticle = fgmParticle;
  tempParticle.Print();
  Double_t pMom = tempParticle.GetP();
  Double_t fRho = - tempParticle.GetQ()*B*kClight/pMom;
  TVector3 fP(tempParticle.GetPx(), tempParticle.GetPy(), tempParticle.GetPz());
  TVector3 fDir = fP.Unit();
  Double_t fCosL = fDir.Perp();
  double myMax = 1./(fabs(fRho*fCosL)+1.e-10);
  //  THelixTrack th(fX,fP,fRho);
  cos1t = 0.5*fRho*fCosL;
  double totStep=0;
  while (2005) {
    //    double hXp[3]={-th.fP[1],th.fP[0],0};
    double hXp[3]={-fDir.Py(),fDir.Px(),0};
    poly[0][0]=1.;poly[0][1]=0.;poly[0][2]=0.;
    tri[0]=tri[1]=tri[2]=0;
    for(ix=1;ix<4;ix++) {
//       poly[ix][0] =th.fX  [ix-1]; 
//       poly[ix][1] =th.fP  [ix-1]; 
      poly[ix][0] =tempParticle.GetParameter(ix-1);
      poly[ix][1] =fDir  [ix-1]; 
      poly[ix][2] =hXp[ix-1]*cos1t;
    }

    nx = (nsurf<=4) ? 1:4;
    for(ix=0;ix<nx;ix++) {
      for(jx=ix;jx<4;jx++) {  
	ss = *sp[ix][jx]; if(!ss) 	continue;
	for (ip=0;ip<3;ip++) {
          f1 = poly[ix][ip]; if(!f1) 	continue;
          f1 *=ss;
          for (jp=0;jp+ip<3;jp++) {
            f2 = poly[jx][jp]; if(!f2) 	continue;
            tri[ip+jp] += f1*f2;
    } } } }

    int nsol = SqEqu(tri,sol);
    step = 1.e+12;
    if (nsol<0) 	return step;
    if (nsol == 2) {
      if (sol[1] < stmin || sol[1] > stmax) nsol = 1;
    }
    if (nsol == 2) {
      if (sol[0] < stmin || sol[0] > stmax) {nsol = 1; sol[0] = sol[1];}
    }
    if (nsol == 1) {
      if (sol[0] < stmin || sol[0] > stmax) nsol = 0; 
    }
    if (nearest && nsol>1) {
      if(fabs(sol[0])>fabs(sol[1])) sol[0]=sol[1];
      nsol = 1;
    }
    if (nsol <= 0) return step;
    if (nsol) step = sol[0];
    if (step < stmin && nsol > 1) step = sol[1];
    if (step < stmin || step > stmax) 	{
      nsol = 0; 
      if (step>0) {step = stmax; stmin+=myMax/2;}
      else        {step = stmin; stmax-=myMax/2;}}

    if (!nsol && fabs(step) < 0.1*myMax) return 1.e+12;
    if (fabs(step)>myMax) {step = (step<0)? -myMax:myMax; nsol=0;}

    //    th.Step(step,x,d);
    //    Float_t  P[8], C[36], dsdr1[6], F[36], F1[36];
    //    tempParticle.TransportBz(B,step/pMom, dsdr, P, C); //, dsdr1, F, F1, kFALSE);
    Float_t dsdr[6] = {0};
    //    cout << "before\t"; tempParticle.Print();
    tempParticle.TransportToDS(step/pMom, dsdr);
    //    cout << "after \t"; tempParticle.Print();
    const Float_t *x = &tempParticle.X();
    if (nsol) {//test it
      ss = s[0]+s[1]*x[0]+s[2]*x[1]+s[3]*x[2];
      if (nsurf > 4) ss += s[4]*x[0]*x[0]+s[5]*x[1]*x[1]+s[6]*x[2]*x[2];
      if (nsurf > 7) ss += s[7]*x[0]*x[1]+s[8]*x[1]*x[2]+s[9]*x[2]*x[0];
      if (fabs(ss)<1.e-3 || TMath::Abs(step) < 1e-3) {
// 	if (xyz) memcpy(xyz,x,sizeof(*xyz)*3);
// 	if (dir) memcpy(dir,d,sizeof(*dir)*3);
	fgmParticle = tempParticle;
	return (totStep+step)/pMom;
    } }

    stmax -=step; stmin -=step;
    if (stmin>=stmax) return 1.e+12;
    totStep+=step;
    //    th.Move(step);
    //    fgmParticle.TransportBz(B,step/pMom, dsdr, P, C, dsdr1, F, F1, kFALSE);
//     cout << "before\t"; fgmParticle.Print();
//     fgmParticle.TransportToDS(step/pMom, dsdr);
//     cout << "after \t"; fgmParticle.Print();
  }
}
//________________________________________________________________________________
Double_t StKFParticleXPropagator::GetDStoR(Float_t BZ, Double_t R, Double_t stmin, Double_t stmax) {
  Double_t cyl[6] = { -R*R , 0, 0, 0, 1, 1};
  //  Float_t dsdr[6] = {0};
  return GetDStoSurfaceBz(BZ, stmin, stmax, cyl, 6, 1); //, dsdr);
}
//________________________________________________________________________________
Bool_t StKFParticleXPropagator::Propagete2Radius(const KFParticle &p, Float_t Radius) {
  fgParticle = p;
  Float_t CurrentR = TMath::Sqrt(fgParticle.GetX()*fgParticle.GetX() + fgParticle.GetY()*fgParticle.GetY());
  Float_t CurrentZ = fgParticle.GetZ();
  Float_t B[3];
  Double_t R;
  fgParticle.GetFieldValue(&fgParticle.X(), B);
  //  float P[8], C[36];
  if (CurrentR < Radius) {
    // go up
    for (auto it = fgMaterials.begin(); it != fgMaterials.end(); it++) {
      if (it->Rmax < CurrentR) continue;
      if (it->Rmin > Radius)   break;
      if (! (it->Zmin < CurrentZ && CurrentZ < it->Zmax)) continue;
      R = TMath::Min(it->Rmax, Radius);
      //      cout << "Target R = " << R << "\t"; fgParticle.Print();
      Double_t dS = GetDStoR(B[2], R, 0., 222.);
      if (TMath::Abs(dS) > 2e4) return kFALSE;
      //      fgParticle.TransportBz(B[2], dS, dsdr, P, C);
      fgParticle = fgmParticle;
      //      fgParticle.TransportToDS(dS, dsdr);
      cout << "dS = " << dS << endl;
      //      cout << "M\t"; fgmParticle.Print();
      //      cout << "P\t"; fgParticle.Print();
      CurrentR = TMath::Sqrt(fgParticle.GetX()*fgParticle.GetX() + fgParticle.GetY()*fgParticle.GetY());
      CurrentZ = fgParticle.GetZ();
      //      cout << "Target R " << R << "\tCurrent R = " << CurrentR << "\tCurrent Z = " << CurrentZ << endl;
    }
  } else {
    // go down
    for (auto it = fgMaterials.rbegin(); it != fgMaterials.rend(); it++) {
      if (it->Rmin > CurrentR) continue;
      if (it->Rmax < Radius)  break;
      if (! (it->Zmin < CurrentZ && CurrentZ < it->Zmax)) continue;
      R = TMath::Max(it->Rmin, Radius);
      //      cout << "Target R = " << R << "\t"; fgParticle.Print();
      //      float dsdr[6];
      if (R > 1e-3) {
	//	Float_t dS = GetDStoSurfaceBz(B[2], -222., 222., cyl, 6, 1);// , dsdr);
	Double_t dS = GetDStoR(B[2], R, -222, 0);
	if (TMath::Abs(dS) > 2e4) return kFALSE;
	fgParticle = fgmParticle;
	//	fgParticle.TransportToDS(dS, dsdr);
	cout << "dS = " << dS << endl;
	//      cout << "M\t"; fgmParticle.Print();
	//      cout << "P\t"; fgParticle.Print();
	//      fgParticle.TransportBz(B[2], dS, dsdr, P, C);
	CurrentR = TMath::Sqrt(fgParticle.GetX()*fgParticle.GetX() + fgParticle.GetY()*fgParticle.GetY());
	CurrentZ = fgParticle.GetZ();
	cout << "Target R " << R << "\tCurrent R = " << CurrentR << "\tCurrent Z = " << CurrentZ << endl;
      } else {
	Float_t xy[2] = {0,0};
	TVector2 X(fgParticle.X(),fgParticle.Y());
	TVector3 P(&fgParticle.Px());
	TVector2 P2(fgParticle.Px(), fgParticle.Py());
	TVector2 D = P2.Unit();
	static Float_t kClight = 1e-12*TMath::C(); // EC=2.99792458E-4 = 0.000299792458f; [CM/S] [kG] [GeV/c]
	Double_t mHz = kClight*B[2];
	Double_t fRho = -mHz * fgParticle.GetQ()/P2.Mod();
	// double TCircle::Path(const double pnt[2]) const
	TComplex CX1(xy[0]-X.X(),xy[1]-X.Y());
	TComplex CP(D.X(),D.Y());
	TComplex CXP = TComplex(0,1)*CX1/CP;
	TComplex CXPRho = CXP*fRho;
	Double_t s;
	if (TComplex::Abs(CXPRho)>0.001) {
	  s = TComplex::Log(1.+CXPRho).Im()/fRho;
	} else {
	  s = (CXP*(1.-CXPRho*(0.5-CXPRho*(1/3.-CXPRho*0.25)))).Im();
	}
	Float_t dS = s/P.Perp();
	Float_t dsdr[6] = {0};
	KFParticle tempParticle = fgParticle;
	tempParticle.TransportToDS(dS, dsdr);
	cout << "dS = " << dS << "\t"; tempParticle.Print();
	fgParticle = tempParticle;
	//	cout << "M\t"; fgmParticle.Print();
	//	cout << "P\t"; fgParticle.Print();
      }
    }
  }
  //  cout << "P2R::Final\t"; fgParticle.Print();
  return kTRUE;
}
//________________________________________________________________________________
void StKFParticleXPropagator::Test(const Option_t *opt) {
  // MC
  Float_t K_S01xyz[3] = {-17.110554, -4.202028, 71.238022};
  Double_t pKS0[3]    = { -2.426288, -0.594788,  3.127483};
  Double_t piMC[2][3] = {
    {-1.100831,-0.071224, 1.264852}, //pi+
    {-1.325457,-0.523564, 1.862632}  //pi-
  };
  Float_t kfPAt1stp[2][6] = {
    { -56.142410 ,  -7.787385 , 116.047699 , -1.038002 , -0.121174, 1.197453},
    { -54.396317 , -18.034021 , 123.139816 , -1.307011 , -0.454879, 1.806683}
  };
  Float_t kfCAt1ste[2][21] = {
    {  1.22e-10, -2.03e-07,  0.000338,  1.54e-08, -2.57e-05,   0.00421,  1.75e-07, -0.000292,  4.32e-05,  0.000699,  3.24e-08,  -5.4e-05,  7.52e-06,   0.00011,   1.8e-05, -2.03e-07,  0.000338, -0.000153,  -0.00081, -0.000128,  0.000943},
    {   0.00014, -0.000244,  0.000426, -3.97e-05,  6.92e-05,   0.00595, -0.000679,   0.00118,  0.000367,   0.00678, -0.000208,  0.000362,  0.000115,   0.00214,  0.000677,  0.000925,  -0.00161, -0.000721,  -0.00928,  -0.00293,    0.0127}
  };
  Double_t pars[2][6] =      {
    //       mImp,    mZ,    mPsi,      mPti,    nTan,   mCurv
    { 3.514305,51.372265,-3.106114,-0.953679,1.145828, 0.001426},
    {-2.618352,48.497208,-2.744909, 0.720908,1.305488,-0.001078}
  };
  Double_t errs[2][15] = {
    {0.026465,-0.023209,0.063966,-0.000639, 0.000559,0.000030,-0.003432, 0.003109,0.000085,0.000670, 0.000007,-0.000707,-0.000000,-0.000020,0.000046},
    {0.064900, 0.027517,0.069824,-0.001595,-0.000675,0.000047,-0.011415,-0.004870,0.000284,0.002246,-0.000005,-0.000857, 0.000000, 0.000009,0.000034}
  };
  new StKFParticleXPropagator();
  StKFParticleXPropagator *instance = (StKFParticleXPropagator*) KFParticleXPropagator::instance();
  KFParticle particleF;
  Double_t R = 0;
  for (Int_t t = 0; t < 2; t++) {
    cout << "t = " << t << "--------------------------------------------------------------------------------" << endl;
    Int_t pdg = 211*(1 - 2*t);
    StDcaGeometry dca;
    dca.set(pars[t], errs[t]); cout << pdg << "\t"; dca.Print();
    KFParticle particle = dca.Particle(t+1); particle.Print();
    KFPTrack track; track.SetParameters(kfPAt1stp[t]); track.SetCovarianceMatrix(kfCAt1ste[t]); 
    track.SetCharge(1 - 2*t);
    track.SetID(t+1);
    KFParticle particle1(track, pdg); cout << "particle1" << endl; particle1.Print();
    KFParticle particle2 = particle1;
    cout << "t = " << t << "--------------------------------------------------------------------------------" << endl;
    cout << "Transport particle1 to 0" << endl;
    R = 0;
    instance->Propagete2Radius(particle1, 0); // pars[t][0]);
                                            cout << "Start at \t"; particle1.Print();
					    cout << "Target   \t"; particle.Print();
    particleF = instance->GetFParticle(); cout << "Final at R = " << R << "\t"; particleF.Print();
                  
    R = TMath::Sqrt(K_S01xyz[0]*K_S01xyz[0]+K_S01xyz[1]*K_S01xyz[1]);
    cout << "Transport particle to K_S01xyz R = " << R << endl;
    instance->Propagete2Radius(particle, R);
                                            cout << "Start at \t"; particle.Print();
					    cout << "Target   \txyz = "; 
					    for (Int_t i  = 0; i < 3; i++) cout << "\t" <<   K_S01xyz[i]; cout << "\tpxyz = "; 
					    for (Int_t i = 0; i < 3; i++) cout << "\t" << piMC[t][i]; cout << endl;
    particleF = instance->GetFParticle(); cout << "Final at R = " << R << "\t"; particleF.Print();
    cout << "t = " << t << "--------------------------------------------------------------------------------" << endl;
    R = TMath::Sqrt(kfPAt1stp[t][0]*kfPAt1stp[t][0] + kfPAt1stp[t][1]*kfPAt1stp[t][1]);
    cout << "Transport particle1 to R = " << R << endl;
    instance->Propagete2Radius(particle, R); 
                                            cout << "Start at \t"; particle.Print();
					    cout << "Target   \t"; particle1.Print();
    particleF = instance->GetFParticle(); cout << "Final at R = " << R << "\t"; particleF.Print();
  }
}

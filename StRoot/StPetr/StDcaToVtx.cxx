#include <assert.h>
#include "StDcaToVtx.h"
#include "THelixTrack.h"
#include "TGeoMaterial.h"
#include "StvELossTrak.h"
#include "TRSymMatrix.h"
#include "KFParticle/KFPTrack.h"
#define PrPP(A,B) {cout << "StDcaToVtx::" << (#A) << "\t" << (#B) << " = \t" << (B) << endl;}
//_____________________________________________________________________________
StDcaToVtx::StDcaToVtx() : mM(0), mN(0)
{
  mHlx = new THelixTrack();
  mELoss = new StvELossTrak();
}
//_____________________________________________________________________________
StDcaToVtx::~StDcaToVtx()
{
  delete mHlx;
  delete mELoss;
}
//_____________________________________________________________________________
void StDcaToVtx::Set(double M
		     ,const TGeoMaterial *matA,double RxyA
		     ,const TGeoMaterial *matB,double RxyB
		     ,const TGeoMaterial *matC,double RxyC)
{
  mM = M;
  mRxy[0] = RxyA;
  mMat[0] = matA;
  mRxy[1] = RxyB;
  mMat[1] = matB;
  mRxy[2] = RxyC;
  mMat[2] = matC;
}
//________________________________________________________________________________
void StDcaToVtx::Set(Float_t Vtx[3],StDcaGeometry *dcaG) {
  mDca = *dcaG;
  mDca.Print();
  for (int i=0;i<3;i++) {mVtx[i]=Vtx[i];};
  mP = mDca.momentum().mag();
  mHz = mDca.hz();
  
  mHlxPar.mPos[0] = -mDca.impact()*sin(mDca.psi());
  mHlxPar.mPos[1] =  mDca.impact()*cos(mDca.psi());
  mHlxPar.mPos[2] =  mDca.z();
  double cosL = cos(atan(mDca.tanDip()));
  double sinL = sin(atan(mDca.tanDip()));
  mHlxPar.mDir[0] =  cosL*cos(mDca.psi());
  mHlxPar.mDir[1] =  cosL*sin(mDca.psi());
  mHlxPar.mDir[2] =  sinL;
  mHlxPar.mCur    =  mDca.curvature();
  static const int idx[] = {
    0,
    6, 9,
    1, 7,  2,
    3, 8,  4, 5,
    10,13,11,12,14
  };
#if 0
  TRSymMatrix DcaErr(5, mDca.errMatrix()); PrPP(Set,DcaErr);
#endif
  for (int i=0;i<15;i++) {
    mHlxEmx[idx[i]] = mDca.errMatrix()[i];
    //    cout << "mHlxEmx[" << idx[i] << "]  = " << mHlxEmx[idx[i]] << " ==> mDca.errMatrix()[" << i << "] = " <<  mDca.errMatrix()[i] << endl;
  }
  double dif[5]={1,1,mHz,1,1+mDca.tanDip()*mDca.tanDip()};;
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++) {
      //      cout << "mHlxEmx[" << li+j << "] = " << mHlxEmx[li+j];
      mHlxEmx[li+j]*= dif[i]*dif[j];
      //      cout << " => " << mHlxEmx[li+j] << endl;
    } }
  mHlx->Set(mHlxPar.mPos, mHlxPar.mDir,mHlxPar.mCur);
  mHlx->SetEmx(mHlxEmx.Arr());
#if 1
  cout << "Set:\t";
  mHlx->Print();
  mHlx->Emx()->Print();
#endif
  mParticle = mDca.Particle(0,mPdg);
  mParticle.Print();
}
//_____________________________________________________________________________
void  StDcaToVtx::UpdateDca() {
  cout << "Old\t"; mDca.Print();
  cout << "Old\t"; mParticle.Print();
#if 0
  TRSymMatrix dcaE(5,mDca.errMatrix());
  PrPP(Print,dcaE);
#endif
#if 1
  cout << "Update\t";
  mHlx->Print();
  mHlx->Emx()->Print();
#endif
  Double_t xyz[3], dir[3], rho;
  mHlx->Get(xyz, dir, rho);
  Double_t psi    =   TMath::ATan2(dir[1],dir[0]);
  Double_t impact = - TMath::Sin(psi)*xyz[0] + TMath::Cos(psi)*xyz[1];
  Double_t z      =   xyz[2];
  Double_t tanDip =   dir[2]/TMath::Sqrt(dir[0]*dir[0] + dir[1]*dir[1]);
  Double_t Pti    =   rho/mDca.hz();
  Double_t params[6] = {impact, z, psi, Pti, tanDip, rho};
  // back to dca
  static const int idx[] = {
    0,
    6, 9,
    1, 7,  2,
    3, 8,  4, 5,
    10,13,11,12,14
  };
  
  Double_t HlxEmx[15];
  const Double_t *hlxEmx = mHlx->Emx()->Arr();
  double dif[5]={1,1,mHz,1,1+mDca.tanDip()*mDca.tanDip()};
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++) {
      //      cout << "hlxEmx[" << li+j << "] = " << hlxEmx[li+j];
      HlxEmx[li+j] = hlxEmx[li+j]/(dif[i]*dif[j]);
      //      cout << " => " << HlxEmx[li+j] << endl;
    } }
#if 0
  TRSymMatrix HElx(5,HlxEmx); PrPP(Print,HElx);
#endif
  Double_t sete[15];
  for (int i=0;i<15;i++) {
    sete[i] = HlxEmx[idx[i]];
    //    cout << "sete[" << i << "]  = " << sete[i] << " ==> HlxEmx[" << idx[i] << "] = " << HlxEmx[idx[i]] << endl;
  }
#if 0
  TRSymMatrix DE(5,sete);
  PrPP(Print,DE);
#endif
  mDca.set(params,sete);
  cout << "New\t"; mDca.Print();
  mP = mDca.momentum().mag();
  static KFPTrack track;
  Double_t xyzp[6], CovXyzp[21];
  mDca.GetXYZ(xyzp,CovXyzp);
  TCL::ucopy(xyz,xyzp,3);
  TCL::vscale(dir,mP,&xyzp[3],3);
  Float_t xyzF[6], CovXyzF[21];
  TCL::ucopy(xyzp,xyzF,6);
  TCL::ucopy(CovXyzp,CovXyzF,21);
  track.SetParameters(xyzF);
  track.SetCovarianceMatrix(CovXyzF);
  track.SetNDF(1);
  //    track.SetChi2(GlobalTracks_mChiSqXY[k]);
  //  track.SetId(kg);
  Int_t q   = 1;
  if (! mPdg) mPdg = 211;
  if (mDca.charge() < 0) {
    q = -1;
    mPdg = -mPdg;
  } 
  track.SetCharge(q);
  mParticle = KFParticle(track, mPdg);
  mParticle.SetPDG(mPdg);
  //  mParticle.SetId(kg);
  //  mParticle.AddDaughterId(kg);
  //  mParticle = mDca.Particle(0,mPdg);
  cout << "New\t";   mParticle.Print();
}
//_____________________________________________________________________________
void  StDcaToVtx::Print(Option_t *option) const {
  mDca.Print();
  mParticle.Print();
#if 0
  TRSymMatrix dcaE(5,mDca.errMatrix());
  PrPP(Print,dcaE);
  mHlx->Print();
  mHlx->Emx()->Print();
  // back to dca
  static const int idx[] = {
    0,
    6, 9,
    1, 7,  2,
    3, 8,  4, 5,
    10,13,11,12,14
  };
  Double_t HlxEmx[15];
  const Double_t *hlxEmx = mHlx->Emx()->Arr();
  double dif[5]={1,1,mHz,1,1+mDca.tanDip()*mDca.tanDip()};
  for (int i=0,li=0;i< 5;li+=++i) {
    for (int j=0;j<=i;j++) {
      //      cout << "hlxEmx[" << li+j << "] = " << hlxEmx[li+j];
      HlxEmx[li+j] = hlxEmx[li+j]/(dif[i]*dif[j]);
      //      cout << " => " << HlxEmx[li+j] << endl;
    } }
  TRSymMatrix HElx(5,HlxEmx); PrPP(Print,HElx);
  Double_t sete[15];
  for (int i=0;i<15;i++) {
    sete[i] = HlxEmx[idx[i]];
    //    cout << "sete[" << i << "]  = " << sete[i] << " ==> HlxEmx[" << idx[i] << "] = " << HlxEmx[idx[i]] << endl;
  }
  TRSymMatrix DE(5,sete);
  PrPP(Print,DE);
#endif
}
//_____________________________________________________________________________
void StDcaToVtx::Shooter()
{
  
  for (int idx=0; idx<3;idx++) {
    double len = 0;
    if (idx<2) { //Step to cylinder
      double cyl[] = { -mRxy[idx]*mRxy[idx],0,0,0,1,1};
      len = mHlx->Step(222.,cyl, 6, 0, 0, 1);
    } else {
      len = mHlx->Path(mVtx);
    }
    cout << "Shooter::Before\n"; mHlx->Print("C");
    mHlx->Backward();
    mHlx->Move(len);
    cout << "Shooter::After by " << len << "\t"; mHlx->Print("C");
    mHlx->Backward(); len = -len;
    UpdateELoss(len,mMat[idx]);
  }
  // Convert to DCA
  UpdateDca();
}
//_____________________________________________________________________________
void StDcaToVtx::UpdateELoss(double len, const TGeoMaterial *mat)
{
  // C == Curvature
  // k == dC/dl
  // l == track length 
  // 
  // dX = cos(C*l + k*l*l/2)*dl
  // dY = sin(C*l + k*l*l/2)*dl
  // 
  // k is small (E loss is small)
  // 
  // dX = (cos(C*l) - sin(C*l)*k*l*l/2)*dl
  // dY = (sin(C*l) + cos(C*l)*k*l*l/2)*dl
  // 
  // Normal vector to track in xy plane(-sin(C*l),cos(C*l))
  // 
  // dPerp = ( -cos(C*l)*sin(C*l) + cos(C*l)*sin(C*l) + sin(C*l)**2*k*l*l/2+cos(C*l)**2) *k*l*l/2
  // dPerp = k*l*l/2
  //  Perp = k*L**3/6
  //  
  //  dP/P = -dC/C
  //  dC = -dP*C/P
  //  dC/dl  = -dP*C/P/l
  //  dP == -Ploss
  //  
  //  dC/dl  = (Ploss/P)*C/L
  //  
  //  Perp = (Ploss/P)*C*L**2/6
  //  Dang = (Ploss/P)*C*L/2
  // ==================================================
  // 
  // T = ( cosL*cosP,cosL*sinP ,sinL)
  // U = (-     cosP,     sinP ,   0)
  // V = (-sinL*cosP,-sinL*cosP,cosL)
  // 
  // U*du + V*dv = (cosL*U)*dPhi  + V*dL
  // 
  // Hence:
  // dPhi = du/cosL
  // dLam = dv
  // 
  int sgn = (len<0)? -1:1;
  double *pos = mHlx->Pos();       
  double *dir = mHlx->Dir(); 
  mCurv = mHlx->GetRho();
  THEmx_t *emx = mHlx->Emx();
  mELoss->Reset(1);
  mELoss->Set(mat,mP,mCurv);
  mELoss->Add(len);
  //		Change curvature
  double dCur = (mELoss->PLoss()/mP)*mCurv;
  assert(sgn*mELoss->PLoss()>0);
  
  mP -=mELoss->PLoss();
  mCurv+=dCur;
  //		Chane direction
  dir[0] += -dir[1]*mELoss->Dang();
  dir[1] +=  dir[0]*mELoss->Dang();
  //
  double cosL = mHlx->GetCos();
  double cosP = dir[0]/cosL;
  double sinP = dir[1]/cosL;
  pos[0] += -sinP*mELoss->Perp();
  pos[1] +=  cosP*mELoss->Perp();
  
  // 		Update helix pars
  mHlx->Set(pos,dir,mCurv);
  
  //		Update 
  double theta2 = mELoss->GetTheta2();
  emx->mAA -= sgn*theta2/(cosL*cosL);
  emx->mLL -= sgn*theta2;
  
  printf("%s:\t Len = %g Ploss = %g dCur = %g Perp = %g\n"
	 ,mat->GetName(),len, mELoss->PLoss(),dCur,mELoss->Perp());
  printf("%s:\t P=%g Curv=%g Pos= %g %g %g\n\n"
	 ,mat->GetName(),mP,mCurv,pos[0],pos[1],pos[2]);
#if 1
  emx->Print();
#endif
}
//_____________________________________________________________________________
THelixTrack gTestHlxV;


//_____________________________________________________________________________
#if 1
//_____________________________________________________________________________
void StDcaToVtx::SetTest()
{
static const double PiMASS=0.13956995;
class MyMat_t {public: const char* Material; int No; double A,Z,Density,RadLen;}; 
static MyMat_t myMat[] = {
{"Hydrogen",	1,	1.010,		 1.000,	0.071,		865.000},
{"Beryllium",	5,	9.010,		 4.000,	1.848,		35.300 },
{"Vacuum",	16,	1e-16,		 1e-16,	1e-16,		1e16   }};

 TGeoMaterial *matHyd = new TGeoMaterial (myMat[0].Material
                                         ,myMat[0].A
		  		         ,myMat[0].Z
				         ,myMat[0].Density
				         ,myMat[0].RadLen);

 TGeoMaterial *matBer = new TGeoMaterial (myMat[1].Material
                                         ,myMat[1].A
		  		         ,myMat[1].Z
				         ,myMat[1].Density
				         ,myMat[1].RadLen);
 TGeoMaterial *matVac = new TGeoMaterial (myMat[2].Material
                                         ,myMat[2].A
		  		         ,myMat[2].Z
				         ,myMat[2].Density
				         ,myMat[2].RadLen);

  Set(PiMASS,matVac,4.,matBer,4.2,matHyd,10.);
  double pos[3]={1.,0.,1},dir[3]={0.,100.,20};
  mP = 0.5;
  mCurv = 1./669./mP;

  mHlx->Set(pos,dir,mCurv); cout << "Set:\t"; mHlx->Print("C");
  mHlx->Move(100.);         cout << "Move\t";mHlx->Print("C");
  memcpy(pos,mHlx->Pos(),sizeof(pos));
  memcpy(dir,mHlx->Dir(),sizeof(dir));
  mMat[3] = mMat[2];
  mRxy[3] = 100;

  mHlx->SetEmx(0);
  THEmx_t *emx = mHlx->Emx();
  emx->mHH = pow(0.1     ,2);
  emx->mAA = pow(3.14/180,2);
  emx->mCC = pow(mCurv/10,2);
  emx->mZZ = pow(0.1     ,2);
  emx->mLL = pow(3.14/180,2);
  mHlx->SetEmx(*emx);  cout << "SetEmx\t";mHlx->Print("C");
}
#else
//_____________________________________________________________________________
void StDcaToVtx::SetTest()
{
  static const double PiMASS=0.13956995;
  class MyMat_t {
  public: 
    Double_t R; 
    const Char_t* Material; 
    Int_t No; 
    double A,Z,Density,RadLen, Zmin, Zmax; 
    TGeoMaterial *mat;
  }; 
  static MyMat_t myMat[] = {
    {  0.000000,"Vacuum",              0,     1.000000,       0.000000,       0.000000,        1e11,     -383.00000,      383.000000, 0},//PIPE_1/PIPO_1/PVAO_1	
    {  3.875000,"PIPE",      	       1,    26.980000,      13.000000,       2.700000,    8.875105,      76.200012,      383.000000, 0},//PIPE_1/PIPO_1/PVAO_1	
    {  3.875000,"PIPE",      	       2,    26.980000,      13.000000,       2.700000,    8.875105,    -383.000000,      -76.200012, 0},//PIPE_2/PIPO_1/PVAO_1	
    {  3.900000,"BERILLIUM", 	       3,     9.010000,       4.000000,       1.848000,   34.459958,     -76.199997,       76.199997, 0},//PIPE_1/PIPC_1		
    {  4.000000,"Air",       	       4,    14.610000,       7.300000,       0.001205,      30420.,           -383,            -383, 0},//SBSP_2                 
    { 46.600002,"ALUMINIUM_TIFC",      5,    21.181105,      10.310893,       0.088779,  321.992267,     -230.679993,     230.679993, 0},///TIFC_1
    { 51.700001,"TPCE_SENSITIVE_GAS",  6,    38.691490,      17.436170,       0.001541,13017.799363,     -230.679993,     230.679993, 0},///TIFC_1
    {200.000000,"ALUMINIUM_TOFC",      7,    26.827182,      12.886338,       0.324186,   74.802245,     -223.990005,     223.990005, 0},///TOFC_1
    {207.731003,"TPCE_SENSITIVE_GAS",  8,    38.691490,      17.436170,       0.001541,13017.799363,     -223.990005,     223.990005, 0} ///TOFC_1
  };
  Int_t N = sizeof(myMat)/sizeof(MyMat_t);
  Set(PiMASS);
  for (Int_t i = 0; i < N; i++) {
    myMat[i].mat =  new TGeoMaterial ( myMat[i].Material
				      ,myMat[i].A
				      ,myMat[i].Z
				      ,myMat[i].Density
				      ,myMat[i].RadLen);
    Add(myMat[i].R,myMat[i].mat);
  }
  //  Set(PiMASS,matVac,4.,matBer,4.2,matHyd,10.);
  //  Set(PiMASS,myMat[0],myMat[3].R,myMat[3].mat,myMat[4].R, 
  double pos[3]={1.,0.,1},dir[3]={0.,100.,20};
  mP = 0.5;
  mCurv = 1./669./mP;
  
  mHlx->Set(pos,dir,mCurv);
  mHlx->Move(100.);
  memcpy(pos,mHlx->Pos(),sizeof(pos));
  memcpy(dir,mHlx->Dir(),sizeof(dir));
  mMat[3] = mMat[2];
  mRxy[3] = 100;
  
  mHlx->SetEmx(0);
  THEmx_t *emx = mHlx->Emx();
  emx->mHH = pow(0.1     ,2);
  emx->mAA = pow(3.14/180,2);
  emx->mCC = pow(mCurv/10,2);
  emx->mZZ = pow(0.1     ,2);
  emx->mLL = pow(3.14/180,2);
  mHlx->SetEmx(emx->Arr());
}
#endif
//_____________________________________________________________________________
void StDcaToVtx::InitTest()
{
  SetTest();
  
  double len = 0;
  for (int idx=3; idx>=0;idx--) {
    if (idx == 0) {
      len = mHlx->Path(0.,0.);
    } else {
      //  mHlx->Backward();
      double cyl[] = { -mRxy[idx-1]*mRxy[idx-1],0,0,0,1,1};
      len = mHlx->Step(222.,cyl, 6, 0, 0, 1);
      //      mHlx->Backward(); len = -len;
    }
    cout << "Before\n"; mHlx->Print("C");
    mHlx->Move(len);
    UpdateELoss(len,mMat[idx]);
    cout << "After\t"; mHlx->Print("C");
    if (idx!=3) continue;
    gTestHlxV = *mHlx;
    memcpy(mVtx,mHlx->Pos(),sizeof(mVtx));
  }
}
//_____________________________________________________________________________
void StDcaToVtx::Test()
{
  StDcaToVtx dv;
  //  dv.SetTest();   
  dv.InitTest();
  dv.GetHlx()->Print("StartHlx");
  dv.Shooter();
  //  gTestHlxV.Print("InitHlx");
  dv.GetHlx()->Print("EndHlx");
  
}
//________________________________________________________________________________
void StDcaToVtx::TestDCA()
{
#if 0
  // MC
  Float_t K_S01xyz[3] = { -17.110554, -4.202028, 71.238022};
  Double_t K_S0[3] = {-2.426288,-0.594788, 3.127483};
  Double_t piP[3]  = {-1.100831,-0.071224, 1.264852};
  Double_t piM[3]  = {-1.325457,-0.523564, 1.862632};
#else
  // MC
  Float_t K_S01xyz[3] = {-17.110554,  -4.202028, 71.238022};
  Double_t pKS0[3]  = { -2.426288, -0.594788,  3.127483};
  Double_t piM[3]  = {-1.325457,-0.523564, 1.862632};
  Double_t piP[3]  = {-1.100831,-0.071224, 1.264852};
  Float_t kfPAt1stp[2][6] = {
    { -56.142410 ,  -7.787385 , 116.047699 , -1.038002 , -0.121174, 1.197453},
    { -54.396317 , -18.034021 , 123.139816 , -1.307011 , -0.454879, 1.806683}
  };
  Float_t kfCAt1stp[2][21] = {
    {  1.22e-10, -2.03e-07,  0.000338,  1.54e-08, -2.57e-05,   0.00421,  1.75e-07, -0.000292,  4.32e-05,  0.000699,  3.24e-08,  -5.4e-05,  7.52e-06,   0.00011,   1.8e-05, -2.03e-07,  0.000338, -0.000153,  -0.00081, -0.000128,  0.000943},
    {   0.00014, -0.000244,  0.000426, -3.97e-05,  6.92e-05,   0.00595, -0.000679,   0.00118,  0.000367,   0.00678, -0.000208,  0.000362,  0.000115,   0.00214,  0.000677,  0.000925,  -0.00161, -0.000721,  -0.00928,  -0.00293,    0.0127}
  };
#endif
  Double_t pars[2][6] =      {
    //       mImp,    mZ,    mPsi,      mPti,    nTan,   mCurv
    { 3.514305,51.372265,-3.106114,-0.953679,1.145828, 0.001426},
    {-2.618352,48.497208,-2.744909, 0.720908,1.305488,-0.001078}};
  Double_t errs[2][15] = {
    {0.026465,-0.023209,0.063966,-0.000639,0.000559,0.000030,-0.003432,0.003109,0.000085,0.000670,0.000007,-0.000707,-0.000000,-0.000020,0.000046},
    {0.064900,0.027517,0.069824,-0.001595,-0.000675,0.000047,-0.011415,-0.004870,0.000284,0.002246,-0.000005,-0.000857,0.000000,0.000009,0.000034}};
  
  
  Float_t vtx[3] = {0};
  static const double PiMASS=0.13956995;
  class MyMat_t {public: const char* Material; int No; double A,Z,Density,RadLen;}; 
  static MyMat_t myMat[] = {
    {"Hydrogen",	1,	1.010,		 1.000,	0.071,		865.000},
    {"Beryllium",	5,	9.010,		 4.000,	1.848,		35.300 },
    {"Vacuum",	       16,	1e-16,		 1e-16,	1e-16,		1e16   },
    {"Air",       	4,    14.610000,        7.3000, 0.001205,        30420.}};
  
  TGeoMaterial *matHyd = new TGeoMaterial (myMat[0].Material
					   ,myMat[0].A
					   ,myMat[0].Z
					   ,myMat[0].Density
					   ,myMat[0].RadLen);
  
  TGeoMaterial *matBer = new TGeoMaterial (myMat[1].Material
					   ,myMat[1].A
					   ,myMat[1].Z
					   ,myMat[1].Density
					   ,myMat[1].RadLen);
  TGeoMaterial *matVac = new TGeoMaterial (myMat[2].Material
					   ,myMat[2].A
					   ,myMat[2].Z
					   ,myMat[2].Density
					   ,myMat[2].RadLen);
  TGeoMaterial *matAir = new TGeoMaterial (myMat[3].Material
					   ,myMat[3].A
					   ,myMat[3].Z
					   ,myMat[3].Density
					   ,myMat[3].RadLen);
  
  StDcaToVtx dv[2];
  StDcaGeometry dca[2]; 
  KFParticle particles[2];
  for (Int_t t = 0; t < 2; t++) {
    cout << "Init" << endl;
    dca[t].set(pars[t], errs[t]);// dca[t].Print();
    //    particles[t] = dca[t].Particle(t+1,211*(2*t-1)); particles[t].Print();
    dv[t].Set(PiMASS,matVac,4.,matBer,4.2,matHyd,10.);
    dv[t].Set(K_S01xyz,&dca[t]);
    //    dv[t].Print();
    //    dv[t].mHlx->Print();
    //     dv[t].mP = dca[t].momentum().mag();
    //     dv[t].mHlx = new THelixTrack(dca[t].thelix()); dv[t].mHlx->Print();
    dv[t].Shooter();
    //    cout << "After shooter" << endl;
    //    dv[t].Print();
    //    dv[t].mHlx->Print();
  }
}

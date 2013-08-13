#include "StiDiver.h"
#include "TGeoVolume.h"
#include "TVirtualMCStack.h"
#include "TGeant3TGeo.h"
#include "StarVMC/GeoTestMaker/StVMCApplication.h"
#include "StarVMC/GeoTestMaker/StMCStack.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"
#include "Sti/StiNodePars.h"
#include "StarMagField.h"
#include "StiELossTrak.h"
#include "THelixTrack.h"
static const double gMyPiMass=0.1396;
static const    int gMyPiPdg =9999;
static void mybreak(int key)
{ static int kto=-2010;
  if (kto != key) return;
  printf ("BOT OHO %d\n",key);
}

ClassImp(StiDiver)
//_____________________________________________________________________________
//_____________________________________________________________________________
StiDiver::StiDiver(const char *name):TNamed(name,"")
{
  memset(mBeg,0,mEnd-mBeg+1);
}
//_____________________________________________________________________________
int StiDiver::Init() 
{
  mHelix = new THelixTrack;
  mELoss = new StiELossTrak;
  StVMCApplication *app = (StVMCApplication*)TVirtualMCApplication::Instance();
  assert(app);
  mSteps =  ( StiMCStepping*)app->GetStepping();
  assert(mSteps);
  mSteps->Set(mHelix);
  mSteps->Set(mELoss);
  mGen = (StiMCPrimaryGenerator*)app->GetPrimaryGenerator();
  mFld = (StiMCField*           )app->GetField();
  mGen->Set(mHelix);
  mSteps->Set(mFld);
  
  TVirtualMC::GetMC()->DefineParticle( gMyPiPdg,"MyPi+",kPTHadron,gMyPiMass, 1,1e+10
	                             ,"pType", 0., 0, 1, 0, 1, 1, 1, 0, 1, 1);
  TVirtualMC::GetMC()->DefineParticle(-gMyPiPdg,"MyPi-",kPTHadron,gMyPiMass,-1,1e+10
	                             ,"pType", 0., 0, 1, 0, 1, 1, 1, 0, 1, 1);
  return 0;
}
//_____________________________________________________________________________
void StiDiver::Reset() 
{
 mSteps->Reset();
}
//_____________________________________________________________________________
int  StiDiver::Dive()
{

  mInpPars->fill(mHelix);
  mHelix->SetEmx(0);
  mInpErrs->Get(mHelix->Emx());
  if (!mDir) mHelix->Backward();

  TVirtualMC::GetMC()->ProcessEvent();

  TVector3 pos = mSteps->CurrentPosition().Vect();
  TVector3 mom = mSteps->CurrentMomentum().Vect();
  mOutPars->_x = pos.X();
  mOutPars->_y = pos.Y();
  mOutPars->_z = pos.Z();
  mOutPars->_psi = mom.Phi();
  mOutPars->_ptin = -mSteps->Charge()/mom.Pt();
  mOutPars->_tanl = mom[2]*fabs(mOutPars->_ptin);
  mOutPars->_hz   = mFld->GetHz();
  mOutPars->ready(); 

  mOutErrs->Set(mHelix->Emx(),mOutPars->_hz);
  assert(mOutErrs->mCC>0);
  if (!mDir) {
    mOutPars->reverse();
    mOutErrs->Backward();
  assert(mOutErrs->mCC>0);
  }
  assert (mInpPars->_ptin * mOutPars->_ptin >=0);
  return mSteps->GetExit();
}
//_____________________________________________________________________________
void StiDiver::Set(const StiNodePars *inpar,const StiFitErrs *inerr,int idir)
{
  mInpPars= inpar;
  mInpErrs= inerr;
  assert(mInpErrs->mCC>0);
  mDir    = idir;
  mSteps->SetDir(mDir);
  mFld->SetHz(inpar->_hz);	//Set initial value of mag field
  mGen->Set(inpar,idir);
}
//_____________________________________________________________________________
void StiDiver::Set(StiNodePars *otpar,StiFitErrs *oterr,Mtx55D_t *deriv)
{
  mOutPars = otpar;
  mOutErrs = oterr;
  mOutDeri = deriv;
  mSteps->Set(mOutDeri);
  memset((*mOutDeri)[0],0,5*5*sizeof((*mOutDeri)[0][0]));
  for (int j=0;j<5;j++) {(*mOutDeri)[j][j]=1.;}

}
//_____________________________________________________________________________
double StiDiver::GetLength() const
{
  return TVirtualMC::GetMC()->TrackLength();
}

//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(StiMCInitApp)

//_____________________________________________________________________________
StiMCInitApp::StiMCInitApp()  
{}   
//_____________________________________________________________________________
int  StiMCInitApp::Fun()
{
  StVMCApplication  *app = (StVMCApplication*)TVirtualMCApplication::Instance();
  TVirtualMC *myMC  = new TGeant3TGeo("C++ Interface to Geant3"); 
  Info("Init","TGeant3TGeo has been created.");
  StiMCConstructGeometry *geo = new StiMCConstructGeometry(app->GetName());
  app->SetConstructGeometry(geo);
  app->SetPrimaryGenerator(new StiMCPrimaryGenerator());
  app->SetField(new StiMCField);
  StiMCStepping *steps =   new StiMCStepping("");
  app->SetStepping(steps);

  app->SetDebug(0);
  myMC->SetStack(new StMCStack(1));
  myMC->Init();
  myMC->BuildPhysics(); 
  ((TGeant3*)myMC)->SetDEBU(0,0,0); 
  ((TGeant3*)myMC)->SetMaxNStep(-1000);

  Gcphys_t* gcphys = ((TGeant3*)myMC)->Gcphys(); if (gcphys){}

  Info("Init","switch off physics");
  myMC->SetProcess("DCAY", 0);
  myMC->SetProcess("ANNI", 0);
  myMC->SetProcess("BREM", 0);
  myMC->SetProcess("COMP", 0);
  myMC->SetProcess("HADR", 0);
  myMC->SetProcess("MUNU", 0);
  myMC->SetProcess("PAIR", 0);
  myMC->SetProcess("PFIS", 0);
  myMC->SetProcess("PHOT", 0);
  myMC->SetProcess("RAYL", 0);
  myMC->SetProcess("LOSS", 4); // no fluctuations 
  //  myMC->SetProcess("LOSS 1"); // with delta electron above dcute
  myMC->SetProcess("DRAY", 0);
  myMC->SetProcess("MULS", 0);
  myMC->SetProcess("STRA", 0);
  myMC->SetCut("CUTGAM",	1e-3  );
  myMC->SetCut("CUTELE", 	1e-3  );
  myMC->SetCut("CUTHAD", 	.001  );
  myMC->SetCut("CUTNEU", 	.001  );
  myMC->SetCut("CUTMUO", 	.001  );
  myMC->SetCut("BCUTE", 	.001  );
  myMC->SetCut("BCUTM", 	.001  );
  myMC->SetCut("DCUTE", 	1e-3  );
  myMC->SetCut("DCUTM", 	.001  );
  myMC->SetCut("PPCUTM", 	.001  );
  myMC->SetCut("TOFMAX", 	50.e-6);

  return 0;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(StiMCStepping)

//_____________________________________________________________________________
StiMCStepping::StiMCStepping(const char *name,const char *tit)
  : StMCStepping(name,tit)
{
   memset(fFist,0,fLast-fFist);
}   
//_____________________________________________________________________________
StiMCStepping::~StiMCStepping()
{
}
//_____________________________________________________________________________
void StiMCStepping::Reset() 
{
  memset(fFist,0,fMidl-fFist);
  fELossTrak->Reset();
}		
//_____________________________________________________________________________
void StiMCStepping::Print(const Option_t*) const
{
}		
//_____________________________________________________________________________
int StiMCStepping::Fun()
{
static int nCall = 0;
nCall++;
mybreak(nCall);
  TString ts,modName;
  fKount++;
  Case();

  assert(fCurrentLength< 10000);
  assert(fEnterLength  < 10000);
  
//   StTGeoHelper::Instance()->Print(KazeAsString(fKaze));
//   printf("fEnterLength=%g fCurrentLength=%g Rxy=%g Z=%g\n\n"
//         , fEnterLength, fCurrentLength,fCurrentPosition.Perp(),fCurrentPosition.Z());
SWITCH: int myKaze = fKaze;
if (GetDebug()) {printf("%d - ",nCall); Print();}

  switch (fKaze) {
    case kNEWtrack:;

    case kENTERtrack:;
         if (strcmp(fVolume->GetName(),"HALL")==0) fKaze=kENDEDtrack;
         if (!StTGeoHelper::Inst()->GetHitShape()->Inside(fCurrentPosition.Z(),fCurrentPosition.Perp()))
	    fKaze=kENDEDtrack;
         if (fKaze==kENDEDtrack) break;
         if (BegVolume()) fKaze=kENDEDtrack;
    break;
    
    case kCONTINUEtrack:
    case kIgnore:
    break;
    
    case kOUTtrack:
    case kENDEDtrack:
      fExit = 1;
      EndVolume();
      TVirtualMC::GetMC()->StopTrack();
      break;

    case kEXITtrack:
    {
      fExit = EndVolume();
      if (!fExit) {
        StTGeoHelper *tgh = StTGeoHelper::Instance();
        if (!tgh->IsSensitive(fVolume)) break;
        if (!tgh->IsActive(0)) 		break;
      }
      TVirtualMC::GetMC()->StopTrack();
    }
    break;

    default:
     Error("Case","Unexpected case %x == %s",fCase,fCasName.Data());
     assert(0);
  }
  if (fKaze!=myKaze) goto SWITCH;

  return 0;
}		
//_____________________________________________________________________________
int StiMCStepping::BegVolume()
{
static int nCall=0; nCall++;

  fELossTrak->Reset(); 
  fPrevMat = fMaterial;
  fELossTrak->Set(fMaterial->GetA(),fMaterial->GetZ(),fMaterial->GetDensity(), fX0);
  fELossTrak->Set(fEnterMomentum.Vect().Mag());
  return (IsDca00(0));
}
//_____________________________________________________________________________
int StiMCStepping::EndVolume()
{
static int nCall=0; nCall++;
  double pos[4],mom[4];

  int isDca = (IsDca00(1));
  double dL = fCurrentLength-fEnterLength;
  if (dL<1e-6) return 0;
  fCurrentPosition.GetXYZT(pos);
  fCurrentMomentum.GetXYZT(mom);

  double curva = -fField->GetHz()/fCurrentMomentum.Pt()*fCharge;
  double rho   = fHelix->GetRho();
  assert (curva*rho>=0);
  assert (fabs(curva-rho)<=0.3*(fabs(curva)+fabs(rho))*(dL+1));
    
  fELossTrak->Add(dL);
  fHelix->Set((2*rho+curva)/3);
  double T[5][5],R[5][5];
  fHelix->Move(dL,T);

  assert(fabs(fHelix->Pos()[0]-pos[0])<0.3*(dL+1));
  assert(fabs(fHelix->Pos()[1]-pos[1])<0.3*(dL+1));
  assert(fabs(fHelix->Pos()[2]-pos[2])<0.3*(dL+1));


  THEmx_t *emx = fHelix->Emx();
  assert(emx->mCC>0);
  double theta2 = fELossTrak->GetTheta2();
  emx->mAA+=theta2; emx->mLL+=theta2;
  double ort2 = fELossTrak->GetOrt2();
  assert(ort2>0);
  emx->mHH+=ort2; emx->mZZ+= ort2/pow(fHelix->GetCos(),2);
  double eerr2= fELossTrak->ELossErr2();
  double dC2 =  -mom[3]/(mom[0]*mom[0]+mom[1]*mom[1]+mom[2]*mom[2])*curva;
  dC2 = dC2*eerr2;
  emx->mAA+=dC2;

  Multiply(R,T,*fDeriv);
  fHelix->Set(pos,mom,curva);
  memcpy((*fDeriv)[0],R[0],sizeof(R));
  return isDca;
}
//_____________________________________________________________________________
int StiMCStepping::IsDca00(int begEnd)
{
  fCurrentSign = fCurrentMomentum[0]*fCurrentPosition[0]
               + fCurrentMomentum[1]*fCurrentPosition[1];
  if (begEnd==0) fStartSign = fCurrentSign; 
  if ((fCurrentSign<0)==(fDir==0)) return 0;

  double dL = fCurrentLength-fEnterLength;
  THelixTrack th(*fHelix);
  double dcaL = th.Path(0.,0.);
  double curva = -fField->GetHz()/fCurrentMomentum.Pt()*fCharge;
  if (begEnd) {
    double rho = fHelix->GetRho();
    curva = (rho*(dL-dcaL)+dcaL*curva)/dL;
    th.Set((2*rho+curva)/3);
    dcaL = th.Path(0.,0.);
  }
//		Update end position
  th.Move(dcaL);
  fCurrentLength=fEnterLength+dcaL;
  fCurrentPosition.SetVect(TVector3(th.Pos()));
  double pt = fabs(fField->GetHz()*fCharge/curva);
  double p  = pt/th.GetCos();
  fCurrentMomentum.SetVectM(TVector3(th.Dir())*p,fMass);
  return 1;
}

//_____________________________________________________________________________
//_____________________________________________________________________________
void StiMCStepping::Finish(const char *opt)
{
}
ClassImp(StiMCConstructGeometry)

//_____________________________________________________________________________
StiMCConstructGeometry::StiMCConstructGeometry(const char *gy)
  : GCall(gy,"StiMCConstructGeometry")
{
}   
//_____________________________________________________________________________
int  StiMCConstructGeometry::Fun()
{
  TVirtualMC *myMC = TVirtualMC::GetMC();
  myMC->SetRootGeometry();
  Info("Init","switch off physics");
  myMC->SetProcess("DCAY", 0);
  myMC->SetProcess("ANNI", 0);
  myMC->SetProcess("BREM", 0);
  myMC->SetProcess("COMP", 0);
  myMC->SetProcess("HADR", 0);
  myMC->SetProcess("MUNU", 0);
  myMC->SetProcess("PAIR", 0);
  myMC->SetProcess("PFIS", 0);
  myMC->SetProcess("PHOT", 0);
  myMC->SetProcess("RAYL", 0);
  myMC->SetProcess("LOSS", 4); // no fluctuations 
  //  myMC->SetProcess("LOSS 1"); // with delta electron above dcute
  myMC->SetProcess("DRAY", 0);
  myMC->SetProcess("MULS", 0);
  myMC->SetProcess("STRA", 0);
  myMC->SetCut("CUTGAM",	1e-3  );
  myMC->SetCut("CUTELE", 	1e-3  );
  myMC->SetCut("CUTHAD", 	.001  );
  myMC->SetCut("CUTNEU", 	.001  );
  myMC->SetCut("CUTMUO", 	.001  );
  myMC->SetCut("BCUTE", 	.001  );
  myMC->SetCut("BCUTM", 	.001  );
  myMC->SetCut("DCUTE", 	1e-3  );
  myMC->SetCut("DCUTM", 	.001  );
  myMC->SetCut("PPCUTM", 	.001  );
  myMC->SetCut("TOFMAX", 	50.e-6);
  


  return 0;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(StiMCPrimaryGenerator);
//_____________________________________________________________________________
StiMCPrimaryGenerator::StiMCPrimaryGenerator()
{
 mPars=0;
 mHelix=0;
 mDir=0; // direction of moving 1=along track; 0=opposite
}  

//_____________________________________________________________________________
int StiMCPrimaryGenerator::Fun() 
{     

 // Add one primary particle to the user stack (derived from TVirtualMCStack).
 // Track ID (filled by stack)
 // Option: to be tracked

 int toBeDone = 1; 
 
 // Particle type
 int pdg  = (mPars->getCharge()>0)? gMyPiPdg:-gMyPiPdg;
 // Particle momentum
 double p[3];
 mPars->getMom(p);
 if (!mDir) { pdg = -pdg; p[0]=-p[0]; p[1]=-p[1];p[2]=-p[2];}
 
 // Polarization
static const double polx = 0.,poly = 0.,polz = 0.; 

 // Position
static  const double tof = 0.;
 
 double e  = sqrt(gMyPiMass*gMyPiMass + mPars->getP2());
 // Add particle to stack 
 assert(e>1e-6);
 int ntr=1;
 TVirtualMC::GetMC()->GetStack()->PushTrack(
  	 toBeDone,-1,pdg,p[0],  p[1],  p[2],e 
	,mPars->_x,  mPars->_y, mPars->_z,tof
        ,polx,poly,polz, kPPrimary, ntr, 1.,0);
 return 0;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(StiMCField)
//_____________________________________________________________________________
StiMCField::StiMCField() 
{
  mHz = 3e33;
  mFild =  StarMagField::Instance();
}
//_____________________________________________________________________________
int StiMCField::FunDD(const double *x,double *b) 
{     
  if (!mFild) { mFild =  StarMagField::Instance();assert(mFild);}
static const Double_t EC = 2.99792458e-4;
  mFild->BField(x,b); 
  mHz = b[2]*EC;
  if (fabs(mHz) < 1e-5) mHz=1e-5;
  return 0;
}
  
  

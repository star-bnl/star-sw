#include "StarRoot/TRungeKutta.h"
#include "StvDiver.h"
#include "TSystem.h"
#include "TCernLib.h"
#include "TGeoManager.h"
#include "TGeoVolume.h"
#include "TVirtualMCStack.h"
#include "TGeant3.h"
#include "TGeant3TGeo.h"
#include "StarVMC/GeoTestMaker/StVMCApplication.h"
#include "StarVMC/GeoTestMaker/StMCStack.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
#include "StvUtil/StvDebug.h"
#include "StvUtil/StvNodePars.h"
#include "StarMagField.h"
#include "StvUtil/StvELossTrak.h"
#include "THelixTrack.h"
#include "TRandom.h"
#include "StvToolkit.h"

// Debug only
const StvNodePars *myInpPars = 0;//?????????????????????????????
const StvFitErrs  *myInpErrs = 0;//?????????????????????????????
const StvNodePars *myOutPars = 0;//?????????????????????????????
const StvFitErrs  *myOutErrs = 0;//?????????????????????????????
const StvFitDers  *myOutDeri = 0;//?????????????????????????????
const TRungeKutta *myHelix   = 0;//?????????????????????????????
const StvMCStepping *mySteps = 0;//?????????????????????????????


enum DiverCons { kNearBeam=1 };
int gCharge=0;

int EditTGeo();
//_____________________________________________________________________________
class MyRandom : public TRandom 
{
public:
virtual Double_t Rndm(Int_t i = 0) 				{return 0.5;}
virtual Double_t Gaus(Double_t mean = 0, Double_t sigma = 1)	{return 0.0;}
};
static MyRandom* gMyRandom=0;
static const double gMyPiMass=0.1396;
static const    int gMyPiPdg =9999;

enum {kNMany = 10};
StvDiver* StvDiver::mgInst = 0;

//_____________________________________________________________________________
class StvRKuttaMag: public TRKuttaMag
{
  public:
    StvRKuttaMag (){;}
    virtual ~StvRKuttaMag(){;}    
    // methods
    virtual void operator()(const double X[3], double B[3]) 
    { 
static const Double_t EC          = 2.99792458e-4;
static       StarMagField *myFild =  StarMagField::Instance();
       myFild->BField(X,B); 
       assert(B[2]);//??????????????????????????????????????
       B[0]*= EC; B[1]*= EC;B[2]*=EC;
    };
  protected:
    // data members
};  


ClassImp(StvDiver)


//_____________________________________________________________________________
//_____________________________________________________________________________
StvDiver::StvDiver(const char *name):TNamed(name,"")
{
  assert(!mgInst);
  memset(mBeg,0,mEnd-mBeg+1);
}
//_____________________________________________________________________________
StvDiver* StvDiver::Inst()
{
  if (mgInst) return mgInst;
  mgInst = new StvDiver("Diver");
  mgInst->Init();
  return mgInst;
}
//_____________________________________________________________________________
int StvDiver::Init() 
{
  TRungeKutta::SetMAG(new StvRKuttaMag);
  gMyRandom = new MyRandom;
  mHelix = new TRungeKutta;
  mHelix->SetDerOn();
  mELoss = 0;
  StVMCApplication *app = (StVMCApplication*)TVirtualMCApplication::Instance();
  assert(app);
  mSteps =  ( StvMCStepping*)app->GetStepping();
  assert(mSteps);
  mSteps->Set(mHelix);
  mGen = (StvMCPrimaryGenerator*)app->GetPrimaryGenerator();
  mFld = (StvMCField*           )app->GetField();
  
  TVirtualMC::GetMC()->DefineParticle( gMyPiPdg,"MyPi+",kPTHadron,gMyPiMass, 1,1e+10
	                             ,"pType", 0., 0, 1, 0, 1, 1, 1, 0, 1, 1);
  TVirtualMC::GetMC()->DefineParticle(-gMyPiPdg,"MyPi-",kPTHadron,gMyPiMass,-1,1e+10
	                             ,"pType", 0., 0, 1, 0, 1, 1, 1, 0, 1, 1);
  
  return 0;
}
//_____________________________________________________________________________
StvELossTrak *StvDiver::TakeELoss()      
{ 
  StvELossTrak *el=mELoss; mELoss=0;mSteps->Set(mELoss); return el;
}
//_____________________________________________________________________________
void StvDiver::SetRZmax(double rMax,double zMax) 
{   
  StVMCApplication *app = (StVMCApplication*)TVirtualMCApplication::Instance();
  app->SetRZmax(rMax,zMax);
}

//_____________________________________________________________________________
void StvDiver::Reset() 
{
 mSteps->Reset();
}
//_____________________________________________________________________________
void StvDiver::SetPrev(const char *prev) 
{
 mSteps->SetPrev(prev); 
}
//_____________________________________________________________________________
void StvDiver::SetOpt(int opt)
{
 mSteps->SetOpt(opt);
}
//_____________________________________________________________________________
void StvDiver::SetTarget(const double target[3],int nTarget)
{
 mSteps->SetTarget(target,nTarget);
}
//_____________________________________________________________________________
int  StvDiver::Dive()
{
static StvToolkit* kit = StvToolkit::Inst();
  mGen->Set(mInpPars,mDir);


// ****************** Replacing ROOT random by empty one. Dangerous
// ****************** At the end of method it is returned back
  assert(gRandom != gMyRandom);
  TRandom *myRandom = gRandom;
  gRandom = gMyRandom;

  mELoss = kit->GetELossTrak(); mSteps->Set(mELoss);
  mELoss->Reset();
  int myExit = 0;
//		Convert Stv params into helix
  mHelix->Clear();
  mInpPars->get(mHelix);
myHelix = mHelix;
//		Stv input errors must be +ve
  for (int i=0,li=0;i< 5;li+=++i) {assert((*mInpErrs)[li+i]>0);}
//		Convert Stv errors into helix
  mInpErrs->Get(mHelix);
//		Change helix direction towards the beam
  if (!mDir) mHelix->Backward();

  mOutDeri->Clear();		//Unit matrix for out derivatves
  StvNodePars tmpPars;
  for (int iMany=0; iMany <kNMany;iMany++) {
//		Process one track
    TVirtualMC::GetMC()->ProcessEvent();
    myExit = mSteps->GetExit();
//		Error happened
    if (myExit & StvDiver::kDiveBreak) break;

    double pos[4],p[4];
    mSteps->CurrentPosition().GetXYZT(pos);
    auto &mom = mSteps->CurrentMomentum();
    double pinv = -mSteps->Charge()/mom.P();
    mom.GetXYZT(p);;
//		Convert helix pars&errs back into Stv pars/errs
    mOutPars->set(pos,p,pinv,kit->GetMag(pos));
    mOutErrs->Set(mHelix);
//		Test errors again
    for (int i=0,li=0;i< 5;li+=++i) {assert((*mOutErrs)[li+i]>0);}
//		Change sign back
    if (!mDir) {
      mOutPars->reverse();
      mOutDeri->Backward();
      mOutErrs->Backward();
    }
    assert(mOutErrs->mPP>0);
//  assert(mOutErrs->Sigre()==0);
    assert(!mOutPars->check("Dive:OutPars"));
    if (!(myExit&StvDiver::kDiveMany)) break;
    if ( (myExit&StvDiver::kDiveHits)) break;
    if ( (myExit&StvDiver::kDiveDca )) break;
//	Too many attempts. Move blindly forward and continue.
    double bigStep = mOutPars->getRxy();
    bigStep = 0.1 + bigStep/200;				//Estimate bigstep
    double dcaStep = mHelix->Path(0.,0.);
    if (dcaStep>0 && dcaStep<bigStep) bigStep = 0.9*dcaStep;	//bigStep must be less dca
    mHelix->Move(bigStep);
    tmpPars.set(mHelix);
    if (!mDir) tmpPars.reverse();				//account direction
    mGen->Set(&tmpPars,mDir);
  }
  assert (myExit >1 || mInpPars->_pinv * mOutPars->_pinv >=0);
  gRandom = myRandom;


//   assert(mHelix->Emx()->Len());
//   assert(mHelix->Emx()->Times(0));
//   assert(mHelix->Emx()->Times(1));
// //assert(mHelix->Emx()->Times(0)==mHelix->Emx()->Times(1));
//   StvDebug::Count("EmxLen", mHelix->Emx()->Len());
//   StvDebug::Count("EmxTime",mHelix->Emx()->Times(0));

  return mSteps->GetExit();
}
//_____________________________________________________________________________
void StvDiver::Set(const StvNodePars *inpar,const StvFitErrs *inerr,int idir)
{
  mInpPars= inpar;
  mInpErrs= inerr;
  mDir    = idir;
  mSteps->SetDir(mDir);
  mGen->Set(inpar,idir);

myInpPars = mInpPars;//?????????????????????????????
myInpErrs = mInpErrs;//?????????????????????????????
mySteps   = mSteps;
}
//_____________________________________________________________________________
void StvDiver::Set(StvNodePars *otpar,StvFitErrs *oterr,StvFitDers *deriv)
{
  mOutPars = otpar;
  mOutErrs = oterr;
  mOutDeri = deriv;
  mSteps->Set(mOutDeri);

  myOutPars = mOutPars;//?????????????????????????????
  myOutErrs = mOutErrs;//?????????????????????????????
  myOutDeri = mOutDeri;//?????????????????????????????

}
//_____________________________________________________________________________
double StvDiver::GetLength() const
{
//  return TVirtualMC::GetMC()->TrackLength();
    return mSteps->GetLength();
}
//_____________________________________________________________________________
int *StvDiver::G3Debug(int idebug,int idemin,int idemax,int itest)
{
  auto *tg3 = (TGeant3*)TVirtualMC::GetMC();
  assert(tg3);
  auto *cf = tg3->Gcflag();
  auto *iswit = cf->iswit;
  cf->idebug = idebug;
  cf->idemin = idemin;
  cf->idemax = idemax;
  cf->itest = itest;

  for (int i=0;i<10;i++) {iswit[i]=1;}

//	kinematic debug
  iswit[1 -1] = 1;
  iswit[2 -1] = 2;
  iswit[3 -1] = 1;
  iswit[4 -1] = 1;
  iswit[10-1] = 100;


  return &cf->idebug;
}
//_____________________________________________________________________________
int *StvDiver::G3Debug(int idebug)
{
  int idemin=0,idemax=99,itest=1;
  return StvDiver::G3Debug(idebug,idemin,idemax,itest);
}
//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(StvMCInitApp)

//_____________________________________________________________________________
StvMCInitApp::StvMCInitApp()  
{}   
//_____________________________________________________________________________
int  StvMCInitApp::Fun()
{
  StVMCApplication  *app = (StVMCApplication*)TVirtualMCApplication::Instance();
  TVirtualMC *myMC  = new TGeant3TGeo("C++ Interface to Geant3"); 
  Info("Init","TGeant3TGeo has been created.");
  StvMCConstructGeometry *geo = new StvMCConstructGeometry(app->GetName());
  app->SetConstructGeometry(geo);
  app->SetPrimaryGenerator(new StvMCPrimaryGenerator());
  app->SetField(new StvMCField);
  StvMCStepping *steps =   new StvMCStepping("");
  app->SetStepping(steps);

  app->SetDebug(0);
  myMC->SetStack(new StMCStack(1));
  myMC->Init();
//  myMC->BuildPhysics(); 
  ((TGeant3*)myMC)->SetDEBU(0,0,0); 
  ((TGeant3*)myMC)->SetMaxNStep(-1000);

//Gcphys_t* gcphys = ((TGeant3*)myMC)->Gcphys(); if (gcphys){}
  float &cuthad  = ((TGeant3*)myMC)->Gccuts()->cuthad; 
  Info("Init","switch off physics");
  printf("CUTHAD0 = %g\n",cuthad );

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
  printf("CUTHAD1 = %g\n",cuthad );

  EditTGeo();

  myMC->BuildPhysics(); 
  printf("CUTHAD2 = %g\n",cuthad );

  return 0;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(StvMCStepping)

//_____________________________________________________________________________
StvMCStepping::StvMCStepping(const char *name,const char *tit)
  : StMCStepping(name,tit)
{

  memset(fFist,0,fLast-fFist);
  fNTarget = 2;
  fNearBeam = kNearBeam;
  fHALLVolu = gGeoManager->FindVolumeFast("HALL");
}   
//_____________________________________________________________________________
StvMCStepping::~StvMCStepping()
{
}
//_____________________________________________________________________________
void StvMCStepping::Reset() 
{
  memset(fFist,0,fMidl-fFist);
  fCurrPath.Clear();		
  fPrevPath.Clear();		
  fNode = 0; fPrevNode = 0;
  fNTarget = 2;
  StMCStepping::Reset();
}		
//_____________________________________________________________________________
void StvMCStepping::SetPrev(const char *prev) 
{
  fPrevPath = prev;
}
//_____________________________________________________________________________
void StvMCStepping::SetTarget(const double *target,int nTarget) 
{
  memcpy(fTarget,target,sizeof(double)*nTarget);
  fNTarget = nTarget;
}		
//_____________________________________________________________________________
void StvMCStepping::SetOpt(int opt) 
{
  fOpt = opt;
  fNTarget = (fOpt&StvDiver::kTarg3D)? 3:2;
}		
//_____________________________________________________________________________
void StvMCStepping::Print(const Option_t* opt) const
{
StMCStepping::Print(opt);
}		
//_____________________________________________________________________________
int StvMCStepping::Fun()
{
static       StTGeoProxy    *tgh      	= StTGeoProxy::Instance();
//static const StTGeoHitShape *hitShape 	= tgh->GetHitShape();
static       TVirtualMC     *virtualMC	= TVirtualMC::GetMC();
static const TVirtualMCApplication *virtApp = TVirtualMCApplication::Instance();
static const double Rmax = virtApp->TrackingRmax();
static const double Zmax = virtApp->TrackingZmax();


  TString ts,modName;
  fKount++;

  Case();


  fCurrentSign = 0;
  for (int itg = 0; itg<fNTarget; itg++) {
    fCurrentSign+=(fCurrentPosition[itg]-fTarget[itg])*fCurrentMomentum[itg];}


  int meAgain = (!fPrevNode || fNode==fPrevNode)? (fPrevPath == tgh->GetPath()):0;

  if (fabs(fCurrentPosition.Z()) >Zmax) fKaze = kOUTtrack;
  if (fCurrentPosition.Pt()      >Rmax) fKaze = kOUTtrack;

  assert(fCurrentLength< 10000);
  assert(fEnterLength  < 10000);
  
//   StTGeoProxy::Instance()->Print(KazeAsString(fKaze));
//   printf("fEnterLength=%g fCurrentLength=%g Rxy=%g Z=%g\n\n"
//         , fEnterLength, fCurrentLength,fCurrentPosition.Perp(),fCurrentPosition.Z());

SWITCH: int myKaze = fKaze;
//=========================

if (IsDebug()) {
printf("KASE=%d Pos(%g %g %g) Rxy = %g Phi=%d Lam=%d P=%g In %s\n",fKaze
      ,fCurrentPosition.X(),fCurrentPosition.Y(),fCurrentPosition.Z(),fCurrentPosition.Perp()
      ,int(fCurrentMomentum.Phi()*57),int(90-fCurrentMomentum.Theta()*57),fCurrentMomentum.P()
      ,gGeoManager->GetPath());
};
  double pos[4],mom[4];



  switch (fKaze) {
    case kNEWtrack:;
         fStartSign = fCurrentSign; 
         fPrevSign  = 0;
         fELossTrak->Reset(gMyPiMass,fCharge);

    case kENTERtrack:;{
 // begin volume. Define starting helix
StvDebug::AddGra(fCurrentPosition[0],fCurrentPosition[1],fCurrentPosition[2],0);
         fEnterLength   = fCurrentLength;
         fEnterPosition = fCurrentPosition;
         fEnterMomentum = fCurrentMomentum;
         fTooManyLength = fCurrentLength;
         fELossTrak->Set(fMaterial,fEnterMomentum.Vect().Mag());
         fCurrentPosition.GetXYZT(pos);
         fCurrentMomentum.GetXYZT(mom);
//		Reset helix
         fHelix->Set(fCharge,pos,mom);
         fHitted =  ((!meAgain) 
	         &&  (fOpt & StvDiver::kTargHit)
		 &&  (tgh->IsHitted(pos)));
         if (fVolume==fHALLVolu) fKaze=kENDEDtrack;
    break;}
       
    
    case kOUTtrack:
    case kENDEDtrack:
      fExit=StvDiver::kDiveBreak;
      virtualMC->StopTrack();
      break;

    case kCONTINUEtrack:
    case kIgnore:
    case kEXITtrack:
    {
      fExit = EndVolume();
      fPrevNode = fNode; fPrevPath = tgh->GetPath();
      fCurrentPosition.GetXYZT(pos);
      fCurrentMomentum.GetXYZT(mom);
      fHelix->Set(fCharge,pos,mom);
      
      if (!fExit) break;
if (IsDebug()) {
printf("EXIT=%d Pos(%g %g %g) Rxy = %g Phi=%d Lam=%d P=%g In %s\n",fExit
      ,fCurrentPosition.X(),fCurrentPosition.Y(),fCurrentPosition.Z(),fCurrentPosition.Perp()
      ,int(fCurrentMomentum.Phi()*57),int(90-fCurrentMomentum.Theta()*57),fCurrentMomentum.P()
      ,gGeoManager->GetPath());
}
      virtualMC->StopTrack();
    }
    break;

    default:
     Error("Case","Unexpected case %x == %s",fCase,fCasName.Data());
     assert(0);
  }
  fPrevSign = fCurrentSign;
  if (fKaze!=myKaze) 			goto SWITCH;
  if (fExit) 				return 0;
  if (fKaze!=fLastKaze) {fLastNumb=0; fLastKaze=fKaze; return 0;}
  fLastNumb++; if (fLastNumb<100)	return 0;
  fLastNumb=0;
  fExit  =StvDiver::kDiveMany; 
  fExit |= TooMany();
  virtualMC->StopTrack();
  return 0;
}		
#if 1
//_____________________________________________________________________________
int StvMCStepping::EndVolume()
{
enum { kEndVolu=0,kPerigee=1,kHitted=2,kApogee=4 };

  double deltaL = fCurrentLength-fPrevLength,dL = deltaL, dS=0;
  
  int kase = 0,ans = 0;
  if (fDir==0 && ((fStartSign<0) && (fCurrentSign>0))) 	kase+=kPerigee;
  if (fHitted) 						kase+=kHitted;
//if (fDir==1 && ((fStartSign>0) && (fCurrentSign<0))) 	kApogee=4;
  switch(kase) {

    case kPerigee: 
    case kPerigee|kHitted:
          {
            assert(fPrevSign*fStartSign>0);
            dL = -fPrevSign*dL/(fCurrentSign-fPrevSign);	//First estim
            if (fabs(dL)>kMicron) {
              fHelix->Move(dL);
              (*fDeriv)*=*fHelix->Der();
            }
            dS = 0;
            if (fDir==0) {
              dS = fHelix->Path(fTarget[0],fTarget[1],0);
              dL +=dS;
              if (fabs(dS)>kMicron) {
                fHelix->Move(dS);
                (*fDeriv)*=*fHelix->Der();
              }
            }
            ans |= StvDiver::kDiveDca ; break;
            }

    case kApogee |kHitted:
    case kHitted: dL = (fCurrentLength-fEnterLength)/2 -(fPrevLength-fEnterLength);
            ans |= StvDiver::kDiveHits; 

    case kApogee: 
    case kEndVolu:;
            fHelix->Move(dL);
            (*fDeriv)*=*fHelix->Der();
            break;

    default: assert(0 && kase);
  }          
  fELossTrak->Add(dL);
  fHelix->Emx()->Add(fELossTrak->Theta2(),fELossTrak->Ort2(),fELossTrak->PinvErr2());
  if (kase) {
    double Ppre = fPrevMomentum.P();
    double Pend = fCurrentMomentum.P();
    double dPdL = (Pend-Ppre)/(fCurrentLength-fPrevLength);
    double Pnow = Ppre + dPdL*dL;
    double Pinv = (fHelix->Pinv()>0)? 1./Pnow:-1./Pnow;
    fHelix->SetPinv(Pinv);
    fCurrentLength=fPrevLength+dL;
    fCurrentPosition.SetVect(TVector3(fHelix->Pos()));
    fCurrentMomentum.SetVect(TVector3(fHelix->Mom()));
  }
else {///?????????????????????????????????????????????????????????????
    double dEgea = fCurrentMomentum.E()-fPrevMomentum.E();
    double dElos = fELossTrak->ELoss();
    if (fabs(dEgea)>1e-3) StvDebug::Count("ELos_of_Egea",dElos,dEgea);
}




StvDebug::AddGra(fCurrentPosition[0],fCurrentPosition[1],fCurrentPosition[2],0);
  return ans;
}
#endif
#if 0
//_____________________________________________________________________________
int StvMCStepping::EndVolume()
{
  double deltaL = fCurrentLength-fPrevLength,dL = deltaL, dS=0;
  
  int kase = 0,ans = 0;
  if (fDir==0 && ((fStartSign<0) && (fCurrentSign>0))) 	kase+=1;
  if (fHitted) 						kase+=2;
//if (fDir==1 && ((fStartSign>0) && (fCurrentSign<0))) 	kase+=4;
  switch(kase) {

    case 1: case 3:
    case 4: case 6:{
            assert(fPrevSign*fStartSign>0);
            dL = -fPrevSign*dL/(fCurrentSign-fPrevSign);	//First estim
            fHelix->Move(dL);
            (*fDeriv)*=*fHelix->Der();
            dS = 0;
            if (fDir==0) {
              dS = fHelix->Path(fTarget[0],fTarget[1],0);
              dL +=dS;
#if 0
              if (!(dL>=0 && dL <= deltaL)) {
        	ErrfDiror("EndVolume","%g>0 && %g < %g is WRONG",dL,dL, deltaL);          
        	return StvDiver::kDiveBreak;
	      }
#endif
              fHelix->Move();
              (*fDeriv)*=*fHelix->Der();
            }
            ans |= StvDiver::kDiveDca ; break;
            }

    case 2: dL = (fCurrentLength-fEnterLength)/2 -(fPrevLength-fEnterLength);
            ans |= StvDiver::kDiveHits; 

    case 0:;
            fHelix->Move(dL);
            (*fDeriv)*=*fHelix->Der();
            break;

    default: assert(0);
  }          
  fELossTrak->Add(dL);
  fHelix->Emx()->Add(fELossTrak->Theta2(),fELossTrak->Ort2(),fELossTrak->PinvErr2());
  if (kase) {
    double Ppre = fPrevMomentum.P();
    double Pend = fCurrentMomentum.P();
    double dPdL = (Pend-Ppre)/(fCurrentLength-fPrevLength);
    double Pnow = Ppre + dPdL*dL;
    double Pinv = (fHelix->Pinv()>0)? 1./Pnow:-1./Pnow;
    fHelix->SetPinv(Pinv);
    fCurrentLength=fPrevLength+dL;
    fCurrentPosition.SetVect(TVector3(fHelix->Pos()));
    fCurrentMomentum.SetVect(TVector3(fHelix->Mom()));
  }
else {///?????????????????????????????????????????????????????????????
    double dEgea = fCurrentMomentum.E()-fPrevMomentum.E();
    double dElos = fELossTrak->ELoss();
    if (fabs(dEgea)>1e-3) StvDebug::Count("ELos_of_Egea",dElos,dEgea);
}




StvDebug::AddGra(fCurrentPosition[0],fCurrentPosition[1],fCurrentPosition[2],0);
  return ans;
}
#endif
//_____________________________________________________________________________
int StvMCStepping::TooMany()
{
  double dL = fCurrentLength-fTooManyLength;
  if (dL<1e-6) return 0;
  fEnterLength = fTooManyLength;
  return EndVolume();
}

//_____________________________________________________________________________
//_____________________________________________________________________________
void StvMCStepping::Finish(const char *opt)
{
}
ClassImp(StvMCConstructGeometry)

//_____________________________________________________________________________
StvMCConstructGeometry::StvMCConstructGeometry(const char *gy)
  : GCall(gy,"StvMCConstructGeometry")
{
}   
//_____________________________________________________________________________
int  StvMCConstructGeometry::Fun()
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
  myMC->SetCut("CUTHAD", 	.050  );
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
ClassImp(StvMCPrimaryGenerator);
//_____________________________________________________________________________
StvMCPrimaryGenerator::StvMCPrimaryGenerator()
{
 mPars=0;
 mDir=0; // direction of moving 1=along track; 0=opposite
}  

//_____________________________________________________________________________
int StvMCPrimaryGenerator::Fun() 
{     

 // Add one primary particle to the user stack (derived from TVirtualMCStack).
 // Track ID (filled by stack)
 // Option: to be tracked

 int toBeDone = 1; 
 
 // Particle type
 int pdg  = (mPars->getCharge()>0)? gMyPiPdg:-gMyPiPdg;
 // Particle momentum
 double p[3]={0};
 mPars->getMom(p);

 if (!mDir) { pdg = -pdg; p[0]=-p[0]; p[1]=-p[1];p[2]=-p[2];}
 
 // Polarization
static const double polx = 0.,poly = 0.,polz = 0.; 

 // Position
static  const double tof = 0.;
 
 double e  = sqrt(gMyPiMass*gMyPiMass + pow(mPars->getP(),2));
 // Add particle to stack 
 assert(e>1e-6);
 int ntr=1;
 TVirtualMC::GetMC()->GetStack()->Clear();
 TVirtualMC::GetMC()->GetStack()->PushTrack(
  	 toBeDone,-1,pdg,p[0],  p[1],  p[2],e 
	,mPars->_x[0],  mPars->_x[1], mPars->_x[2],tof
        ,polx,poly,polz, kPPrimary, ntr, 1.,0);
 return 0;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
ClassImp(StvMCField)
//_____________________________________________________________________________
StvMCField::StvMCField() 
{
  mX[2] = 3e33;
  mH[2] = 3e33;
  mFild =  StarMagField::Instance();
}
//_____________________________________________________________________________
int StvMCField::FunDD(const double *x,double *b) 
{     
  do {
    if (fabs(x[2]-mX[2])>0.01) break;
    if (fabs(x[1]-mX[1])>0.01) break;
    if (fabs(x[0]-mX[0])>0.01) break;
    if (b) memcpy(b,mH,sizeof(mH));
    return 0;
  } while(0);
  memcpy(mX,x,sizeof(mX));
  mFild->BField(mX,mH); 
  if (b) memcpy(b,mH,sizeof(mH));
  return 0;
}
//_____________________________________________________________________________
const double *StvMCField::GetMag(const double *x,double *b) 
{     
   FunDD(x,b);
   return mH;   
}
#include "TList.h"
#include "TGeant3.h"
//_____________________________________________________________________________
int EditTGeo()
{
  auto *tMC = (TGeant3*)TVirtualMC::GetMC();

  auto *myList = gGeoManager->GetListOfMedia();
  auto *iter = myList->MakeIterator(); 
  int n = 0;
  while (TGeoMedium *med=(TGeoMedium*)iter->Next()) 
  {
    n++;
//    int imed = tMC->MediumId(med->GetName());
    int itmed = med->GetId();
    printf("@@@@ %d %d - %s\n",n,itmed,med->GetName());
    tMC->Gstpar(itmed,"CUTHAD", 0.01);

  } 


  return 0;
}
//    virtual  void  Gstpar(Int_t itmed, const char *param, Double_t parval);
//   Int_t MediumId(const Text_t *name) const;

#if 0
//_____________________________________________________________________________
double StvMCField::GetHz(const double *x) 
{     
   FunDD(x,0);
   return mH[2];
}   
#endif

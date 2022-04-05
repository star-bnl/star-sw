// @(#)root/base:$Name:  $:$Id: TCFit.cxx,v 1.6 2010/01/28 18:19:05 perev Exp $
// Author: Victor Perev   05/08/03


#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include "TError.h"
#include "TCFit.h"
#include "TNumDeriv.h"
#include "TVector3.h"
#include "TRandom.h"
#include "TLorentzVector.h"
#include "THelixTrack.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
ClassImp(TCFit)
//______________________________________________________________________________  
TCFit::TCFit(const char *name,TCFitData *dat) :TNamed(name,"")
{
   fDebug = 1;
   SetData(dat);
   for (int i=0;i<5;i++) (&fBigM)[i]=new TMatrixD;
   
}
//______________________________________________________________________________  
void TCFit::Reset()
{
   for (int i=0;i<5;i++) {*((&fBigM)[i])=0.;}
   fIter =0;
   fCuts =0;
   fMaxIter =500;
   fMaxCuts =6;
   fUPars = 0;
   fUMeas = 0;
   fUCons = 0;
}
//______________________________________________________________________________  
TCFit::~TCFit() 
{
   for (int i=0;i<4;i++) delete (&fBigM)[i];
}
//______________________________________________________________________________  
int TCFit::SetData(TCFitData *dat)  
{
   fDat = dat;
   fDat->SetFitter(this);
   return 0;	
}
//______________________________________________________________________________  
int TCFit::CheckOut()  
{
   int  nBig = fUPars+fUCons;

   fDat->Evaluate();

   if (!fIter) {
     fBigM->ResizeTo(nBig,nBig);	(*fBigM)*=0.0;
     fBigI->ResizeTo(nBig,nBig);	
     fBigB->ResizeTo(nBig,1);
     fOldP->ResizeTo(nBig,1);
     fAddP->ResizeTo(nBig,1);
   }
   for (int j1=0;j1<fUMeas; j1++) {
     int i1 = fDat->GetId(j1);
     if (!fIter) (*fOldP )[j1][ 0] =  fDat->GetPar(i1);
     (*fBigB )[j1][ 0] = -fDat->DFcn(i1);
     (*fBigM)[j1][j1] =   fDat->DDFcn(i1,i1);
     for (int j2=0;j2<j1; j2++) {
       int i2 = fDat->GetId(j2);
       (*fBigM)[j1][j2] = fDat->DDFcn(i1,i2);
       (*fBigM)[j2][j1] = (*fBigM)[j1][j2];
     } 
   }
   for (int jc=0;jc<fUCons; jc++) {
     int ic = fDat->GetId(jc+fUPars);
     double cc = fDat->GetPar(ic);
     (*fBigB )[jc+fUPars][0] = -cc;
     for (int jx=0;jx<fUPars; jx++) {
       int ix = fDat->GetId(jx);
       double cx = fDat->DCon(ic,ix);
       (*fBigM)[fUPars+jc][jx] = cx;
       (*fBigM)[jx][fUPars+jc] = cx;
   } }
   
   
   for (int jx=0; jx<fUPars;jx++)  {
     for (int jc=0;jc<fUCons;jc++) {
      (*fBigB )[jx][ 0] -= (*fOldP)[fUPars+jc][0]*(*fBigM)[jx][fUPars+jc];
   } }    

   if (fDebug>=3) {
     fBigM->Print("CheckOut.BigM");
     fBigB->Print("CheckOut.BigB");
     fDat->Print("CheckOut.dat");
   }


   return 0;
}
//______________________________________________________________________________  
int TCFit::CheckIn()  
{
   for (int jx=0;jx<fUPars;jx++) {
     int ix = fDat->GetId(jx);
     fDat->GetPar(ix) = (*fOldP)[jx][0] + (*fAddP)[jx][0];
   }
   fDat->Evaluate();
//   fDat->Print("CheckIn");
   return 0;
}
//______________________________________________________________________________  
int TCFit::Fit()  
{
  if (fDat->Ready()) 		return kBadAprx;
  fUPars = fDat->GetUPars();
  fUMeas = fDat->GetUMeas();
  fUCons = fDat->GetUCons();

  fCuts = 0; fFitRes=0;

  fFcnQA[0]=3e33; fFcnQA[1]=3e33;
  fConQA[0]=3e33; fConQA[1]=3e33;
  fAddQA[0]=3e33; fAddQA[1]=3e33;
  



  for (fIter=0; fIter < fMaxIter; fIter++) 
  {
    fAkt = CheckStep();
    switch (fAkt) {
  
    case kNextStep: (*fOldP)+=(*fAddP); FitStep(); break;
    
    case kNextCut:  CutStep(); break;

    case kEndFit:   EndStep(); return fFitRes;

    case kBadFit:   EndStep(); return fFitRes;
    
    default: assert(0);
    }//end case
    if (fDebug>=2) PriStep("OneIter");

 }
   fFitRes=kTooIter;
   EndStep();
   return fFitRes;
}
//______________________________________________________________________________  
int TCFit::FitStep()  
{
  CheckOut();
  fFcnQA[1]=fFcnQA[0];
  fConQA[1]=fConQA[0];
  fAddQA[1]=fAddQA[0];

  (*fBigI) = (*fBigM);
  double det=0;
  (*fBigI).Invert(&det);
  (*fAddP) = (*fBigI)*(*fBigB);
//  fAddP->Print("AddP");
  return 0;
}
//______________________________________________________________________________  
int TCFit::CutStep()  
{
  fCuts++; fIter--;
  (*fAddP)*=0.5; return 0;
}
//______________________________________________________________________________  
int TCFit::EndStep()  
{
  fDat->SetFail(fFitRes);
  if (fDebug>=1) PriStep("EndStep");
  if (fDebug>=2) fDat->Print("EndStep");
  return 0;
}
//______________________________________________________________________________  
int TCFit::CheckStep()  
{
  fFitRes = 0;

  if (fIter==0) return kNextStep;
  CheckIn();
  fFcnQA[0] = fDat->GetFcn();
  fConQA[0] = 0;
  fAddQA[0] = 0;
  for (int jc=0;jc<fUCons; jc++) {
    int ic = fDat->GetId(jc+fUPars);
    fConQA[0]+= fabs(fDat->GetPar(ic)/fDat->GetTiny(ic));
  }
  if (fUCons>1) fConQA[0]/=fUCons;
  for (int jx=0;jx<fUMeas; jx++) {
    int ix = fDat->GetId(jx);
    fAddQA[0]+= fabs((*fAddP)[jx][0]/fDat->GetTiny(ix));
  }
  fAddQA[0]/=fUMeas;

  if (fAddQA[0]<1. && fConQA[0] <1.) 	return kEndFit;

  if (fFcnQA[0] > fDat->GetBigFcn() ) fFitRes |=kBadFcn;
  if (fConQA[0] > 1e10 ) fFitRes |=kBadCon;
  if (fFitRes) 				return kBadFit;

  if (fAddQA[0]<fAddQA[1] 
   || fConQA[0]<fConQA[1] 
   || fFcnQA[0]<fFcnQA[1]
   || fCuts>fMaxCuts) {
    fCuts = 0;
    fAddQA[1]=fAddQA[0];
    fConQA[1]=fConQA[0];
    fFcnQA[1]=fFcnQA[0];
    					return kNextStep;
  }
    					return kNextCut;
}
//______________________________________________________________________________  
double TCFit::ErMx(int jcol,int jrow) const
{
  return (*fBigI)[jcol][jrow];
}
//______________________________________________________________________________  
int TCFit::PriStep(const char *where)  
{
  if (where==0)where=""; 
  printf("PriStep(%s) Iter=%d Cut=%d  Akt=%d Fcn=%g(%g) Con=%g(%g) Add=%g(%g)\n"
        ,where,fIter,fCuts,fAkt
	,fFcnQA[0],fFcnQA[1],fConQA[0],fConQA[1],fAddQA[0],fAddQA[1]);

  return 0;
}





//______________________________________________________________________________  
//______________________________________________________________________________  
class TEST0 : public TCFitData {
public:
  TEST0();

  double   Fcn();  			
  double   Con(int icon);          

  double   MyDFcn(int ix);
  double   MyDDFcn(int ix);
  void     Update() {}
  int      Approx() {return 0;}
public:
  double mX[3],mC;

};
//______________________________________________________________________________  
TEST0::TEST0():TCFitData("TEST0") 
{
 mX[0]=mX[1]=mX[2]=1.1;
 mC=0;
 AddPar(0,0, mX,3,"X",0.0001);
 AddPar(2,3,&mC,1,"C",0.0001);
}
//______________________________________________________________________________  
double TEST0::Fcn() 
{
 return  mX[0]*mX[0]/1. + mX[1]*mX[1]/4 + mX[2]*mX[2]/9;
}
//______________________________________________________________________________  
double TEST0::MyDFcn(int ix) 
{
  switch (ix) {
  case 0: return 2*mX[0];
  case 1: return 2*mX[1]/4;
  case 2: return 2*mX[2]/9;
  }
  assert(0); return 0.;
  
}
//______________________________________________________________________________  
double TEST0::MyDDFcn(int ix) 
{
  switch (ix) {
  case 0: return 2.;
  case 1: return 2./4;
  case 2: return 2./9;
  }
  assert(0); return 0.;
  
}
//______________________________________________________________________________  
double TEST0::Con(int) 
{
 return  mX[2]-1.;
}


//______________________________________________________________________________  
void TCFit::Test0()  
{
   TEST0 t0;
   TCFit f0("Test0",&t0);
   for (int itst=0;itst<3;itst++) {
     double d1 = t0.DFcn(itst); 
     double d2 = t0.MyDFcn(itst); 
     double dd1 = t0.DDFcn(itst,itst); 
     double dd2 = t0.MyDDFcn(itst); 
     printf("%d %g %g   %g %g\n",itst,d1,d2,dd1,dd2);
   }
   f0.Fit();
}



ClassImp(TCFitData)

class Deriv1st : public TNumDeriv {
public:
   Deriv1st(TCFitData* dat);
   Double_t  Fcn(Double_t  arg);  	                
   void    SetKind(int kind) { fKind=kind;}
private:
TCFitData *fFitData;
int fKind;
};   
//______________________________________________________________________________
Deriv1st::Deriv1st(TCFitData* dat):TNumDeriv("Deriv1st")
{
   fFitData=dat;
   fKind = -1;
}
//______________________________________________________________________________
Double_t  Deriv1st::Fcn(Double_t  arg)
{
   double save = fFitData->GetPar(fIArg);
   SetStep(fFitData->GetTiny(fIArg));
   fFitData->GetPar(fIArg)+=arg;
   fFitData->Update(); fFitData->Modify(0);
   double ans = (fKind==(-1)) ? fFitData->Fcn() :fFitData->Con(fKind);
   fFitData->GetPar(fIArg)=save;
   fFitData->Update(); fFitData->Modify(0);
   return ans;
}

//______________________________________________________________________________
//______________________________________________________________________________
class Deriv2nd : public TNumDeriv {
public:
   Deriv2nd(TCFitData* dat);
  ~Deriv2nd();
   Double_t  Fcn(Double_t  arg);  	                
   void    SetKind(int kind) { fD1->SetKind(kind);}
   void    SetIJArg(int i,int j);
private:
TCFitData *fFitData;
Deriv1st *fD1;
};   
//______________________________________________________________________________
Deriv2nd::Deriv2nd(TCFitData* dat):TNumDeriv("Deriv2nd")
{
   fFitData = dat;
   fD1 = new Deriv1st(fFitData);
}
//______________________________________________________________________________
Deriv2nd::~Deriv2nd()
{
   delete fD1;
}
//______________________________________________________________________________
void Deriv2nd::SetIJArg(int i,int j)
{
 this->SetIArg(i);
 fD1 ->SetIArg(j);
}
//______________________________________________________________________________
Double_t  Deriv2nd::Fcn(Double_t  arg)
{
   double save = fFitData->GetPar(fIArg);
   fFitData->GetPar(fIArg)+=arg;
   fFitData->Update(); fFitData->Modify(0);
   double ans = fD1->DFcn(0.);
   fFitData->GetPar(fIArg)=save;
   fFitData->Update(); fFitData->Modify(0);
   return ans;
}
//______________________________________________________________________________
//______________________________________________________________________________
TCFitData::TCFitData(const char *name, const char *title):TNamed(name,title)
{

  Reset();
  fD1st = new Deriv1st(this);
  fD2nd = new Deriv2nd(this);
}
//______________________________________________________________________________
void TCFitData::Reset()
{
  memset(fBeg,0,fEnd-fBeg+1);
  memset(fFixs,1,sizeof(fFixs));
  for (int i=0; i<kMaxId;i++) fNams[i]="";
  fFcn[1]=1e-6; 
  fFcn[2]=1e+6; 
}

//______________________________________________________________________________
TCFitData::~TCFitData()
{
  delete fD1st;
  delete fD2nd;
}
//______________________________________________________________________________
int TCFitData::AddPar(int tyPar, int idPar,double *par,int nPars,const char *name,double tiny)
{
  assert(0<=tyPar && tyPar<=2);
  assert(0<=idPar && idPar+nPars<kMaxId);
  if (fFail  ) return fFail;
  if (!name) name="";
  if (tiny<=0) tiny = 1e-3;

  for (int j=0;j<nPars;j++) {
    TString ts;
    if (name[0]) ts=name; else  ts=idPar;
    if (nPars>1) { ts +="["; ts +=j; ts+="]";} 
    fNams[idPar+j]= ts;
    fTiny[idPar+j]= tiny;
    fPars[idPar+j]= par+j;
    fFixs[idPar+j]= 0;
    fTyps[idPar+j]= tyPar;
  }
  fNPars[tyPar]+=nPars;
  assert(fNPars[tyPar]<=kMaxId);
  return 0;
}
//______________________________________________________________________________  
Int_t TCFitData::GetId(const char *name) const	//get par index by name
{
   int i=0;
   int n = fNPars[0]+fNPars[1]+fNPars[2];
   for (int j=0;i<n;j++) {
     if (!fNams[j].Length()) continue;
     i++;
     if (fNams[j]==name) 	return j;
   } 
   printf("TCFitData::GetId(\"%s\") UNKNOWN name\n",name);	
   return -1;
}	
//______________________________________________________________________________  
const char *TCFitData::GetNam(Int_t idx) const 	//get par name by index	
{
 return fNams[idx].Data();
}  	
//______________________________________________________________________________  
int TCFitData::GetType(int id)    const
{
//get type(Meas,Slac,Constr) by id
  return fTyps[id];
}


//______________________________________________________________________________  
int TCFitData::GetId(int jdx) const 	
{
 return fIndx[jdx];
}  	
//______________________________________________________________________________  
int TCFitData::GetJd(int idx) const 	
{
 return fJndx[idx];
}  	

//______________________________________________________________________________  
void TCFitData::FixPar(int idx,int yes)        //(dis/en)able parameter
{
  yes = (yes!=0);
  assert(fNams[idx].Length());
  if (fFixs[idx]==yes) return;
  fFixs[idx]=yes;
  fNFixs[fTyps[idx]]+= ( (yes)? 1:-1 );

}
//______________________________________________________________________________  
int  TCFitData::IsFixed(int idx) const     		
{ return fFixs[idx];}

//______________________________________________________________________________  
double  TCFitData::GetPar  (int idx) const
{return *fPars[idx];}
//______________________________________________________________________________  
double  &TCFitData::GetPar  (int idx)
{
  fModi=1; return *fPars[idx];
}
//______________________________________________________________________________  
void TCFitData::Evaluate() 
{
  if (Modified()) Update(); Modify(0);
  fFcn[0] = Fcn();
  for (int jc=GetUPars();jc<GetUPars()+GetUCons();jc++) { 
    int ic = GetId(jc);
    GetPar(ic) = Con(ic);
  }
}
//______________________________________________________________________________  
int TCFitData::Ready()
{
   int n = 0;
   for (int ity=0;ity<3;ity++)  {
     for (int i=0;i<kMaxId;i++) {
       if (!fNams[i].Length()) 	continue;
       if ( fFixs[i]) 		continue;
       if ( fTyps[i]!=ity) 	continue;
       fIndx[n]=i;
       fJndx[i]=n++;
   } }
   assert(n==fNPars[0]-fNFixs[0]+fNPars[1]-fNFixs[1]+fNPars[2]-fNFixs[2]);
   return Approx();
}

//______________________________________________________________________________  
Double_t TCFitData::DFcn(int ipar)  		
{
  fD1st->SetKind(-1);
  fD1st->SetIArg(ipar);
  return fD1st->DFcn();
}

//______________________________________________________________________________  
Double_t TCFitData::DDFcn(int ipar,int jpar)
{
  fD2nd->SetKind(-1);
  fD2nd->SetIJArg(ipar,jpar);
  return fD2nd->DFcn();
}

//______________________________________________________________________________  
Double_t TCFitData::Con(int)
{
assert(0);
}

//______________________________________________________________________________  
Double_t TCFitData::DCon(int icon,int ipar)
{
  fD1st->SetKind(icon);
  fD1st->SetIArg(ipar);
  return fD1st->DFcn();
}
//______________________________________________________________________________  
double  TCFitData::ErMx(int icol,int irow) const
{
  int jcol = GetJd(icol);
  int jrow = GetJd(irow);
  return fFitter->ErMx(jcol,jrow);
}
//______________________________________________________________________________  
void TCFitData::Print(const char *name) const
{
static const char *ty[3]={"Meas","Slack","Cnsr"};
static const char *fx[2]={"Used","Fixed"};

  if (!name) name = "";
  printf("TCFitData::Print(%s) nMeas=%d(%d) nSlac=%d(%d) nCons=%d(%d)\n"
         ,name
	 ,GetNMeas(),GetUMeas()
	 ,GetNSlac(),GetUSlac()
	 ,GetNCons(),GetUCons());
  for (int i=0;i<kMaxId;i++) {
    if (!fNams[i].Length()) 	continue;
    printf("%2d - %s\t",i,fNams[i].Data());
    printf(" %s.%s ",ty[fTyps[i]],fx[fFixs[i]]);
    printf(" %g \n",*fPars[i]);
  }

}


//______________________________________________________________________________
void TkPars::Print(const char *tit) const
{
 if (!tit) tit="";
 printf("TkPars::Print(%s) \tDca=%g Z=%g Phi=%g ptin=%g tanl=%g curv=%g"
       ,tit,dca,z,phi,ptin,tanl,curv);

}
//______________________________________________________________________________
void TkPars::Reset()
{
  double tmp = hz;
  memset(this,0,sizeof(TkPars));
  hz = tmp;
}
//______________________________________________________________________________
void TkPars::SetHz(double fact)
{
  hz = (0.000299792458 * 4.98478)*fact;
}
//______________________________________________________________________________
TkPars &TkPars::operator+=(const TkPars &a)
{
  for (int i=0;i<5;i++) Arr()[i]+=a.Arr()[i];
  if (phi<-M_PI) phi+=M_PI*2;
  if (phi> M_PI) phi-=M_PI*2;
  return *this;
}  
//______________________________________________________________________________
TLorentzVector TkPars::P4() const
{
  double pt = fabs(1./ptin);
  double e = sqrt(pt*pt*(1.+tanl*tanl)+mass*mass);
  TLorentzVector v(pt*cos(phi),pt*sin(phi),pt*tanl,e);
  return v;
}
//______________________________________________________________________________
void TkPars::P4D(double D[4][5]) const
{
  enum {Kdca,Kz,Kphi,Kptin,Ktanl};

  memset(D[0],0,4*5*sizeof(D[0][0]));
  double pt = fabs(1./ptin);
  double dpt = -pt/ptin;
  double e = sqrt(pt*pt*(1.+tanl*tanl)+mass*mass);
//  TLorentzVector v(pt*cos(phi),pt*sin(phi),pt*tanl,e);

  D[3][Kptin] = pt*(1.+tanl*tanl)/e*dpt;
  D[3][Ktanl] = pt*pt*tanl/e*dpt;

  D[0][Kptin] = cos(phi)*dpt;
  D[1][Kptin] = sin(phi)*dpt;
  D[2][Kptin] = tanl*dpt;
  D[2][Ktanl] = pt;

}
//______________________________________________________________________________
void TkPars::Set(const TVector3 &v3,const TVector3 &d3,double pti)
{
  dca = v3.Perp();
  phi = d3.Phi();
  z   = v3[2];
  if ((v3.Cross(d3)).Z()<0) dca=-dca;
  ptin = pti;
  tanl = d3.Pz()/d3.Pt();
}
//______________________________________________________________________________
//______________________________________________________________________________
void TkPars::Get(TVector3 *v3,TVector3 *d3,double *pts) const
{
  if (v3 ) { v3->SetXYZ(dca*sin(phi),-dca*cos(phi),z)          ;}
  if (d3 ) { v3->SetXYZ(cos(phi),sin(phi),tanl); v3->SetMag(1.);}
  if (pts) {*pts = 1./ptin                                     ;}
}
//______________________________________________________________________________
TVector3 TkPars::V3() const
{
  TVector3 v3; Get(&v3); return v3;
}
//______________________________________________________________________________
void TkPars::Fill(THelixTrack &hlx)
{
   TLorentzVector p4 = P4();
   TVector3       v3 = V3();
   double h[6];
   v3.GetXYZ(h);
   p4.Vect().GetXYZ(h+3);
   hlx.Set(h,h+3,curv);

}
//______________________________________________________________________________
const char* TkPars::Name(int mem)
{
static const char*  nams[]={       "Dca",  "Z",       "Phi", "Ptin", "TanL"};
  return nams[mem];
}
//______________________________________________________________________________
double TkPars::Tiny(int mem)
{
static const double tiny[]={3.14/180/10., 0.01, 3.14/180/10,   0.01,  0.001};
  return tiny[mem];
}
//______________________________________________________________________________
void  TkPars::Rand(const TkErrs &errs)
{
  for (int iv=0;iv<5;iv++) {
    Arr()[iv]+= gRandom->Gaus(0.,sqrt(errs.Get(iv,iv)));
  }
  Update();
}

//______________________________________________________________________________
//______________________________________________________________________________
void TkErrs::Reset()
{
static const double DX=0.3,DZ=0.3,DPhi=0.03,DPti=1.,DTan=0.05;
  memset(emx,0,sizeof(emx));
  double fak = 0.3*0.3;
  emx[ 0]=DX*DX		*fak;
  emx[ 2]=DZ*DZ		*fak;
  emx[ 5]=DPhi*DPhi	*fak;
  emx[ 9]=DPti*DPti	*fak;
  emx[14]=DTan*DTan	*fak;
}
//______________________________________________________________________________
void TkErrs::Set(int i,int j,double err)
{
  if (i<j) { int ii=i; i=j; j=ii;}
  emx[((i+1)*i)/2+j]=err;
}		
//______________________________________________________________________________
double TkErrs::Get(int i,int j) const
{
  if (i<j) { int ii=i; i=j; j=ii;}
  return emx[((i+1)*i)/2+j];
}		
//______________________________________________________________________________
void TkErrs::Invert()
{
  TCL::trsinv(emx,emx,5);
}
//______________________________________________________________________________
double TkErrs::Xi2(const TkPars &pars) const
{
  double chi2;
  TCL::trasat(pars.Arr(),emx,&chi2,1,5);
  return chi2;
}
//______________________________________________________________________________
void TkErrs::Mpy(const TkPars &pars,double der[5]) const
{
 TCL::trsa(emx, pars.Arr() ,der,5,1);
}
//______________________________________________________________________________
//______________________________________________________________________________
TVector3 VxPars::V3() const
{ return TVector3(x); }

//______________________________________________________________________________
//______________________________________________________________________________
ClassImp(TCFitV0)
//______________________________________________________________________________
TCFitV0::TCFitV0():TCFitData("TCFitV0","Two prong decay fit")
{
  Reset();
}
//______________________________________________________________________________
void TCFitV0::Reset()
{
  for (int i=0;i<2;i++) {
    mTkBas[i].Reset();
    mTEBas[i].Reset();
    mTkFit[i].Reset();
    mTkDif[i].Reset();
  }
  memset(mBeg,0,mEnd-mBeg+1);
  TCFitData::Reset();
}
//______________________________________________________________________________
int TCFitV0::Ready()
{
  if (!mReady) {
    mReady=46;
    TCFitData::Reset();

    for (int itk=0;itk<2;itk++) 	{
      mTkBas[itk].Update();
      mTEBas[itk].Invert();

  //		Measured variables
      for (int mem=0;mem<5;mem++)	{
	TString ts(TkPars::Name(mem)); ts+=itk;
	AddPar(kMEAS,10*itk+mem,mTkDif[itk].Arr()+mem,1,ts,TkPars::Tiny(mem));
    } }

  //		Slack variables
    AddPar(kSLAC,20,mLen,3,"Len"   ,0.001);

  //		Constrains
    AddPar(kCNSR,30, mConr,7,"Same+Nrgy",1e-4);
    Modify();
  }  
  return TCFitData::Ready();
  
}
//______________________________________________________________________________
void TCFitV0::Update()
{
  if (!Modified()) return;
// 	Calc all constrains
  double pos[3],dir[3];
  TLorentzVector P4[3];
  TVector3 Vx[3];
  for (int itk=0;itk<2;itk++) {
// 	Calc all constrains
    THelixTrack th;
    mTkFit[itk] = mTkBas[itk]; 
    mTkFit[itk]+= mTkDif[itk]; 
    mTkFit[itk].Fill(th);
    th.Eval(mLen[itk],pos,dir);
    memcpy(mDConDL[itk],dir,sizeof(dir));
    double P = mTkFit[itk].P();
    double E = mTkFit[itk].E();
    P4[itk].SetXYZT(dir[0]*P,dir[1]*P,dir[2]*P,E);
    Vx[itk] = TVector3(pos);
// 	Calc DFcn
    mTEBas[itk].Mpy(mTkDif[itk],mDFcn[itk]);

  }
  P4[2]= P4[0]+P4[1];
  (P4[2].Vect()*(-1.)).Unit().GetXYZ(mDConDL[2]);
  Vx[2]= TVector3(mVx.x)+ P4[2].Vect().Unit()*mLen[2];
  (Vx[0]-Vx[2]).GetXYZ(mConr+0);  
  (Vx[1]-Vx[2]).GetXYZ(mConr+3);  
  mConr[6] = P4[2].M() - mMas;
  mTkFit[0].P4D(mP4d[0]);
  mTkFit[1].P4D(mP4d[1]);


  Modify(0);
}
//______________________________________________________________________________
int TCFitV0::Approx()
{
static int nCall=0; nCall++;
// 	Calc all constrains
  double s[2],pos[3];
  TVector3 Vx[3];
  THelixTrack th[2];
  for (int itk=0;itk<2;itk++) {mTkBas[itk].Fill(th[itk]);}

  double disMin=3e33;
  for (int sgn=0;sgn<4;sgn++) {
    if (sgn&1) th[0].Backward();
    if (sgn&2) th[1].Backward();
    s[0] = th[0].Path(th[1],&s[1]);
    if (s[0]<100 && s[1]<100) {
      for (int itk=0;itk<2;itk++) {
        th[itk].Eval(s[itk],pos); Vx[itk].SetXYZ(pos[0],pos[1],pos[2]);}
      double dis = (Vx[1]-Vx[0]).Mag();
      if (dis < disMin) {
        disMin=dis;
	mLen[0] = (sgn&1)? -s[0]:s[0];
	mLen[1] = (sgn&2)? -s[1]:s[1];
    } }
    if (sgn&1) th[0].Backward();
    if (sgn&2) th[1].Backward();
  }
  if (disMin>100) return 1;
  Vx[2]=(Vx[0]+Vx[1])*0.5;
  mLen[2] = Vx[2].Mag();
  return 0;
}
//______________________________________________________________________________
double TCFitV0::Fcn()
{
  double chi2 = 0;

  for (int itk=0;itk<2;itk++) {
// 	Calc Fcn
    chi2+=mTEBas[itk].Xi2(mTkDif[itk]);
  }
  return chi2;
}
//______________________________________________________________________________
double TCFitV0::Con(int ic)
{
  return mConr[ic-30];
}

//______________________________________________________________________________
double TCFitV0::DFcn(int ipar)  		
{
   int itk = ipar/10;
   assert( itk>=0 && itk <2);
   int mem = ipar%10;
   assert( mem>=0 && mem <5);
   return mDFcn[itk][mem];
}
//______________________________________________________________________________
double TCFitV0::DDFcn(int ipar,int jpar)    
{
   int itk = ipar/10;
   assert( itk>=0 && itk <2);
   int jtk = jpar/10;
   assert( jtk>=0 && jtk <2);
   if (itk != jtk) return 0.;
   int ivar = ipar%10;
   int jvar = jpar%10;
   assert(ivar<5 && jvar<5);
   return mTEBas[itk].Get(ivar,jvar);
}   
//______________________________________________________________________________
double TCFitV0::DCon(int icon,int ipar)  		
{
   if (ipar <20) return TCFitData::DCon(icon,ipar);
   if (icon==kCNRJ) return 0.;
   int tcon = (icon-kCX_0)/3;
   int jcon = (icon-kCX_0)%3;
   int jpar = ipar-kLEN_0;
   switch(10*tcon+jpar) {
     case  00: ;case 11: ;
     case   2: ;case 12: return  mDConDL[jpar][jcon];
     case   1: ;case 10: return  0.;
     default: assert(0);
   }
   return 0;
}   
//______________________________________________________________________________  
void TCFitV0::Print(const char *name) const
{
static const char *ty[3]={"Meas","Slack","Cnsr"};
static const char *fx[2]={"Used","Fixed"};

  if (!name) name = "";
  printf("TCFitV0::Print(%s) nMeas=%d(%d) nSlac=%d(%d) nCons=%d(%d)\n"
         ,name
	 ,GetNMeas(),GetUMeas()
	 ,GetNSlac(),GetUSlac()
	 ,GetNCons(),GetUCons());
  for (int i=0;i<kMaxId;i++) {
    if (!fNams[i].Length()) 	continue;
    const double *p = 0;
         if (i< 5) { p = mTkFit[0].Arr()+i    ;}
    else if (i<15) { p = mTkFit[1].Arr()+i -10;}
    if (!p) 			continue;
    printf("%2d - %s\t",i,GetNam(i));
    printf(" %s.%s ",ty[GetType(i)],fx[IsFixed(i)]);
    
    printf(" %g \n",*p);
  }
  TCFitData::Print(name);
}
//______________________________________________________________________________
#include "TRandom.h"
#include "TCanvas.h"
#include "TH1F.h"
#include "TSystem.h"
//______________________________________________________________________________
static double Pdk(double ma,double mb,double mc) 
{ 
  double ma2 = ma*ma,ma4=ma2*ma2;
  double mb2 = mb*mb,mb4=mb2*mb2;
  double mc2 = mc*mc,mc4=mc2*mc2;
  double ans= (ma4+mb4+mc4-2*ma2*mb2-2*ma2*mc2-2*mb2*mc2);
  if (ans<0.) ans = 0;
  ans= sqrt(ans)/(2.*ma);
  return ans;
}
//______________________________________________________________________________
void TCFitV0::Test(int mode)
{
static const double D0Mass=1.8646,PiMass=.13956995,KMass=.493677;
static const double D0Tau=0.415e-12,C=299792458e2;
static const double DX=0.003/*,DAng=0.003,DPti=0.1,DTan=0.005*/;
static const double D0P=1
                   ,D0Beta = D0P/sqrt(D0P*D0P+D0Mass*D0Mass)
                   ,D0Gama = 1./sqrt(1.-D0Beta*D0Beta)
                   ,D0Len  = D0Tau*C*D0Gama*D0Beta;
static const double HZ= (0.000299792458 * 4.98478);

static const int NHISTS = 4;
static TCanvas* myCanvas=0;
static TH1F *hh[]={0,0,0,0,0,0};
static const char *hNams[]= {"dL","dL/L", "Xi2","Mass-D0"};
static const double LOW[] = {-D0Len,-3, 0.,-1.5};
static const double UPP[] = {-D0Len, 3,10., 1.5};



  if(!myCanvas)  myCanvas=new TCanvas("TCircleFitter_Test","",600,800);
  myCanvas->Clear();
  myCanvas->Divide(1,NHISTS);
  for (int i=0;i<NHISTS;i++) {
    delete hh[i]; hh[i]= new TH1F(hNams[i],hNams[i],100,LOW[i],UPP[i]);
    myCanvas->cd(i+1); hh[i]->Draw();
  }

TLorentzVector p4[3];
TVector3 dcayPos,d0Mom;
THelixTrack Khlx,Pihlx;
double Q = Pdk(D0Mass,PiMass,KMass);

  TCFitV0 dat;
  TCFit tc("TestD0",&dat); 
  dat.mTkBas[0].SetHz(1.);   
  dat.mTkBas[1].SetHz(1.);   

  for (int iev=0; iev <500; iev++) {
    tc.Reset(); dat.Reset();
    double dist = gRandom->Exp(D0Len);
    double wk[9];
    gRandom->Sphere(wk[0],wk[1],wk[2],dist);
    dcayPos.SetXYZ(wk[0],wk[1],wk[2]);
    d0Mom = dcayPos.Unit()*D0P;
    p4[2].SetVectM( d0Mom,D0Mass);
    gRandom->Sphere(wk[0],wk[1],wk[2],Q);
    p4[0].SetXYZM(-wk[0],-wk[1],-wk[2],KMass );
    p4[1].SetXYZM( wk[0], wk[1], wk[2],PiMass);
    TVector3 boost = p4[2].BoostVector();
    dat.mMas = D0Mass;
    for (int ip=0;ip<2;ip++) {
      p4[ip].Boost(boost);
      p4[ip].GetXYZT(wk+3);
      dcayPos.GetXYZ  (wk+0);
      double ptin = 1./p4[ip].Pt();
      if (ip) ptin=-ptin;

      THelixTrack th(wk+0,wk+3,ptin*HZ);
      th.Backward();
      double s =th.Path(0.,0.);
      th.Move(s);th.Backward();
      th.Eval(0.,wk,wk+3);
      TVector3 pos(wk),dir(wk+3);
      dat.mTkBas[ip].Reset();
      dat.mTkBas[ip].Set(pos,dir,ptin);
      dat.mTkBas[ip].mass= (ip)? PiMass:KMass;
      dat.mTkBas[ip].Update();
      dat.mTEBas[ip].Set(0,0,pow(DX,2));
      dat.mTEBas[ip].Set(1,1,pow(DX,2));
      dat.mTkBas[ip].Rand(dat.mTEBas[ip]);
    }
    dat.Ready();
    if (mode==1) dat.FixPar(kCNRJ);
    if (tc.Fit()) continue;
    double dL = dat.GetPar(kLEN_2)-dist;
    double eL = sqrt(dat.ErMx(kLEN_2,kLEN_2));
    hh[0]->Fill(dL/eL);
    hh[1]->Fill(dL/dist);
    hh[2]->Fill(dat.GetFcn()/dat.GetNDF());
    TLorentzVector D0V = dat.mTkFit[0].P4()+dat.mTkFit[1].P4();
    hh[3]->Fill(D0V.M()-D0Mass);

  }
  myCanvas->Modified();
  myCanvas->Update();
  //  while(!gSystem->ProcessEvents()){}; 
}


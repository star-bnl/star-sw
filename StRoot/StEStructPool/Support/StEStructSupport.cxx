/**********************************************************************
 *
 * $Id: StEStructSupport.cxx,v 1.1 2004/07/01 00:37:17 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description: Simple helper class for calculating
 *              delta-rho, delta-rho/rho, and delta-rho/sqrt(rho)
 *              plus some other goodies
 *
 ***********************************************************************/
#include "StEStructSupport.h"
#include "Stiostream.h"
#include "Stsstream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"


const char* _pair_typename[] = {"Sib","Mix"};
const char* _pair_chargename[]   = {"pp","pm","mm"};
int _pair_typemax=2;
int _pair_chargemax=3;
int _pair_totalmax=_pair_typemax*_pair_chargemax;

ClassImp(StEStructSupport)

//---------------------------------------------------------
bool StEStructSupport::goodName(const char* name){

  TString testName(_pair_typename[0]);
  testName+=_pair_chargename[0];
  testName+=name;
  TObject* obj=mtf->Get(testName.Data());
  if(!obj) return false;

  return true;
}

//---------------------------------------------------------
char* StEStructSupport::getFrontName(int itype){

  /* for itype=0-5, returns Sibpp, Sibpm, Sibmm, Mixpp, Mixpm, Mixmm   */

  if(mtmpString) delete [] mtmpString;
  mtmpString=new char[256];
  ostringstream ts;
  int j,k;
  if(itype<_pair_chargemax){
    j=0; k=itype;
  } else {
    j=1; k=itype-_pair_chargemax;
  }

  ts<<_pair_typename[j]<<_pair_chargename[k];
  strcpy(mtmpString,(ts.str()).c_str());
  return mtmpString;
}

//---------------------------------------------------------
const char* StEStructSupport::getTypeName(int itype){ 
  return _pair_typename[itype]; 
}

//---------------------------------------------------------
const char* StEStructSupport::getChargeSignName(int ics){ 
  return _pair_chargename[ics]; 
}

//---------------------------------------------------------
//
//  Now the real class work 
//
//---------------------------------------------------------

StEStructSupport::StEStructSupport(TFile* tf, int bgmode, float nbar): mtf(tf), mNbar(nbar), mbgMode(bgmode), mtmpString(NULL){}

StEStructSupport::~StEStructSupport(){ if(mtmpString) delete [] mtmpString; };

//---------------------------------------------------------
TH1** StEStructSupport::getHists(const char* name){

  TH1** retVal=NULL;
  if(!goodName(name)) return retVal;
  
  retVal=new TH1*[6];

  for(int i=0;i<_pair_totalmax;i++){
    TString hname(getFrontName(i));
    hname+=name;
    retVal[i] = (TH1*)mtf->Get(hname.Data());
  }

  return retVal;
}

//-----------------------------------------------------
float* StEStructSupport::getNorms(TH1** histArray){

  /* not really used any more but keep */

  float* retVal=NULL;
  if(!histArray) return retVal;

  retVal=new float[6];
  for(int i=0;i<_pair_totalmax; i++) retVal[i]=histArray[i]->Integral();

  return retVal;
}

//---------------------------------------------------------
TH1** StEStructSupport::getLocalClones(const char* name){

  TH1** hset=getHists(name);
  if(!hset) return (TH1**)NULL;

  // make local clones
  TH1** hlocal=new TH1*[_pair_totalmax];
  for(int i=0;i<_pair_totalmax;i++) {
    hlocal[i]=(TH1*)hset[i]->Clone();
    hlocal[i]->Sumw2();  // trigger error propogation
  }

  return hlocal;
}

//---------------------------------------------------------
TH1** StEStructSupport::buildCommonRatios(const char* name){
  return buildCommon(name,0);
}
//---------------------------------------------------------
TH1** StEStructSupport::buildCommonCFunctions(const char* name){
  return buildCommon(name,1);
}
//---------------------------------------------------------
TH1** StEStructSupport::buildCommonRFunctions(const char* name){
  return buildCommon(name,2);
}

//---------------------------------------------------------
TH1** StEStructSupport::buildCommon(const char* name, int opt){

  /* builds hist types = ++,+-,--,++ - -- */
 
  // six input histograms ++,+-,-- for Sib and Mix 
  TH1** hlocal=getLocalClones(name);
  if(!hlocal) return hlocal;


  // four returned hists
  TH1** retVal= new TH1*[4]; // 0=++ 1=+- 2=-- 3=++ - --
  for(int i=0;i<4;i++){
    retVal[i]=(TH1*)hlocal[0]->Clone();// just get correct dimensions & names
    retVal[i]->SetName(swapIn(hlocal[i]->GetName(),"Sib",(const char*)NULL));
    retVal[i]->SetTitle(swapIn(hlocal[i]->GetTitle(),"Sibling",(const char*)NULL));
    retVal[i]->Scale(0.); // zero the hists
  }

  retVal[3]->SetName(swapIn(retVal[0]->GetName(),"pp","df"));
  retVal[3]->SetTitle(swapIn(retVal[0]->GetTitle(),"+.+","+.+ - -.-"));

// normalize by integral
  for(int i=0;i<6;i++)hlocal[i]->Scale(1.0/hlocal[i]->Integral());

// if requested, scale bg to require correlations>=0 where statistics are large
  if(1==mbgMode){
   scaleBackGround(hlocal[0],hlocal[3]);
   scaleBackGround(hlocal[1],hlocal[4]);
  }

  for(int i=0;i<3;i++){
    retVal[i]->Add(hlocal[i],hlocal[i+3],1.0,-1.0); // delta rho: if opt=1 done

    if(0==opt){ 
      retVal[i]->Divide(hlocal[i+3]);  // delta-rho/rho_mix
    } else if(2==opt){
      TH1* tmp=getSqrt(hlocal[i+3]);
      if(tmp)retVal[i]->Divide(tmp);  // delta-rho/sqrt(rho_mix);
    }

  }

  //-- now do "++ - --" ...right now implement quickly ... but correct?
  hlocal[0]->Add(hlocal[2],-1.0);   // ++ - -- Sib
  hlocal[0]->Scale(1.0/hlocal[0]->Integral());
  hlocal[3]->Add(hlocal[5],-1.0);   // ++ - -- Mix
  hlocal[3]->Scale(1.0/hlocal[3]->Integral());

  if(1==mbgMode)scaleBackGround(hlocal[0],hlocal[3]);

  retVal[3]->Add(hlocal[0],hlocal[3],1.0,-1.0);  // delta-rho

  if(0==opt){ 
    retVal[3]->Divide(hlocal[3]);  // delta-rho/rho_mix

  } else if(2==opt){
    TH1* tmp=getSqrt(hlocal[3]);   // delta-rho/sqrt(rho_mix)
    retVal[3]->Divide(tmp);
  }

  if(mNbar!=1.0)for(int i=0;i<4;i++) retVal[i]->Scale(mNbar); // Nbar scaling

  return retVal;
}

//---------------------------------------------------------
TH1** StEStructSupport::buildChargeTypeRatios(const char* name){
  return buildChargeTypes(name,0);
}
//---------------------------------------------------------
TH1** StEStructSupport::buildChargeTypeCFunctions(const char* name){
  return buildChargeTypes(name,1);
}
//---------------------------------------------------------
TH1** StEStructSupport::buildChargeTypeRFunctions(const char* name){
  return buildChargeTypes(name,2);
}

//---------------------------------------------------------
TH1** StEStructSupport::buildChargeTypes(const char* name, int opt){

  // build hist types = LS, US, CD, CI


  // six input histograms ++,+-,-- for Sib and Mix 
  TH1** hlocal=getLocalClones(name);
  if(!hlocal) return hlocal;


  // four returned hists
  TH1** retVal= new TH1*[4]; // 0=LS 1=US 2=CD=LS-US 3=CI=LS+US
  const char* nm[4]={"LS","US","CD","CI"};
  const char* tit[4]={"LS : ++ + --","US : +- + -+","CD: ++ + -- - +- - -+","CI : ++ + -- + +- + -+"};

  for(int i=0;i<4;i++){
    retVal[i]=(TH1*)new TH2F(*(TH2F*)hlocal[0]);// hlocal[0]->Clone();// just get correct dimensions & Names
    retVal[i]->SetName(swapIn(hlocal[0]->GetName(),"Sibpp",nm[i]));
    retVal[i]->SetTitle(swapIn(hlocal[0]->GetTitle(),"Sibling : +.+",tit[i])); 
    retVal[i]->Scale(0.); // zero the hists
  }

  hlocal[0]->Add(hlocal[2]);  // ++ + -- Sib
  hlocal[3]->Add(hlocal[5]);  // ++ + -- Mix

// normalize by integral
  for(int i=0;i<6;i++)hlocal[i]->Scale(1.0/hlocal[i]->Integral());

// if requested, scale bg to require correlations>=0 where statistics are large
  if(1==mbgMode){
   scaleBackGround(hlocal[0],hlocal[3]);
   scaleBackGround(hlocal[1],hlocal[4]);
  }

  if(strstr(name,"DEta"))fixDEta((TH2**)hlocal,6);
  
  retVal[0]->Add(hlocal[0]);
  retVal[1]->Add(hlocal[1]);
  
  for(int i=0;i<2;i++){
    retVal[i]->Add(hlocal[i+3],-1.0);  // delta-rho : if opt =1, we're done

    if(0==opt) {
      retVal[i]->Divide(hlocal[i+3]);  // delta-rho/rho_mix

    } else if(2==opt){                 // delta-rho/sqrt(rho_mix)
      TH1* tmp=getSqrt(hlocal[i+3]);
      retVal[i]->Divide(tmp);
    } // else opt==2                     
  }
  
  retVal[2]->Add(retVal[0],retVal[1],1.,-1.);  // CD
  retVal[3]->Add(retVal[0],retVal[1],1.,1.);   // CI

  // scale with nbar if requested 
  if(mNbar!=1.0)for(int i=0;i<4;i++) retVal[i]->Scale(mNbar); // Nbar scaling

  return retVal;
}

//---------------------------------------------------------
void StEStructSupport::scaleBackGround(TH1* sib, TH1* mix){

  float alpha=1.0;
  float hmax=sib->GetMaximum();
 
  for(int ix=1;ix<=mix->GetNbinsX();ix++){
    for(int iy=1;iy<=mix->GetNbinsY();iy++){
      float mval=mix->GetBinContent(ix,iy);
      if(mval>0.){
	float sval=sib->GetBinContent(ix,iy);
        if(sval/hmax>0.5){
	  float rval=sval/mval;
          if(rval<alpha) alpha=rval;
	}
      }
    }
  }

  mix->Scale(alpha);
  cout<<"Scaling "<<mix->GetName()<<" by "<<alpha<<endl;

}
	   
//---------------------------------------------------------
 TH1* StEStructSupport::getSqrt(TH1* h){

   TH1* retVal=(TH1*)h->Clone();
   for(int ix=1;ix<=retVal->GetNbinsX();ix++){
     for(int iy=1;iy<=retVal->GetNbinsY();iy++){
       float nv=h->GetBinContent(ix,iy);
       float env=h->GetBinError(ix,iy);
       if(nv>0.){
	 nv=sqrt(nv);
         env=env/(2.0*nv);
       };
       retVal->SetBinContent(ix,iy,nv);
       retVal->SetBinError(ix,iy,nv);
     }
   }

   return retVal;
 }


//----------------------------------------------------------------
void StEStructSupport::fixDEta(TH2** h, int numHists){

  TH1D* atmp=h[0]->ProjectionX();

  double dx=(double)(atmp->GetBinLowEdge(2)-atmp->GetBinLowEdge(1));
  double r2=sqrt(2.0);
  double xf=2.0*dx*r2;

  for(int ix=1;ix<=atmp->GetNbinsX();ix++){

    double dx1=(double)atmp->GetBinLowEdge(ix);
    double dx2=dx1+dx;
    double a1=xf*(1.0-0.5*fabs(dx1));
    double a2=xf*(1.0-0.5*fabs(dx2));
    double aval;

    if(a1==a2){
      aval=a1+0.5*(xf-a1);
    } else {
      aval=a1+0.5*(a2-a1);
    }

    if(aval>0.){ 
       aval=xf/aval;
    } else {
      aval=0;
    }
    atmp->SetBinContent(ix,aval);
  }


  for(int i=0;i<numHists;i++){
    for(int ix=1;ix<=h[i]->GetNbinsX();ix++){
     for(int iy=1;iy<=h[i]->GetNbinsY();iy++){
       float val=h[i]->GetBinContent(ix,iy);
       float eval=h[i]->GetBinError(ix,iy);
       val*=(float)atmp->GetBinContent(ix);
       eval*=(float)atmp->GetBinContent(ix);
       h[i]->SetBinContent(ix,iy,val);
       h[i]->SetBinError(ix,iy,eval);
     }
    }
  }

}

//---------------------------------------------------------
void StEStructSupport::writeAscii(TH1** h, int numHists, const char* fname){

  ofstream of(fname);
  for(int n=0;n<numHists;n++){
   int xbins=h[n]->GetNbinsX();
   int ybins=h[n]->GetNbinsY();
   TAxis * xa= h[n]->GetXaxis();
   TAxis * ya= h[n]->GetYaxis();
   of<<"# Histogram Name = "<<h[n]->GetName()<<endl;
   of<<"# X-axis: min="<<xa->GetBinLowEdge(1)<<" max="<<xa->GetBinUpEdge(xbins)<<" nbins="<<xbins<<endl;
 
   of<<"# Y-axis: min="<<ya->GetBinLowEdge(1)<<" max="<<ya->GetBinUpEdge(ybins)<<" nbins="<<ybins<<endl;
 
   of<<"# ix  iy  Value "<<endl; 
   for(int i=1;i<=xbins;i++){
     for(int j=1;j<=ybins;j++){
       of<<i<<"   "<<j<<"   "<<h[n]->GetBinContent(i,j)<<endl;
     }
   }
  of<<endl;
  }
  of.close();
}

//---------------------------------------------------------
char* StEStructSupport::swapIn(const char* name, const char* s1, const char* s2){

  // looks for s1 in name and replaces with s2
  //
  //  in perl    $name=~s/$s1/$s2/;

  if(mtmpString) delete [] mtmpString;
  mtmpString=NULL;

  if(!name) return mtmpString;

  int len=strlen(name);
  char* tmp=new char[len+1];
  strcpy(tmp,name);

  char* ptr1;
  if(!(ptr1=strstr(tmp,s1))) {
    delete [] tmp;
    return mtmpString;
  }

  int len1=strlen(s1);
  mtmpString=new char[256];

  ostringstream ts;
  char* ptr2=ptr1;
  *ptr1='\0';
  ts<<tmp; 
  if(s2)ts<<s2;
  ptr1+=len1;
  ts<<ptr1;
  *ptr2=' ';

  delete [] tmp;
  strcpy(mtmpString,(ts.str()).c_str());
  return mtmpString;
}


/***********************************************************************
 *
 * $Log: StEStructSupport.cxx,v $
 * Revision 1.1  2004/07/01 00:37:17  porter
 * new code previously my StEStructHelper. Takes hists from correltation
 * pass and builds final ressults.  Also the StEStructHAdd.h is a simple
 * replacemnt for my sumyt.C macro which could be expanded later as needed.
 *
 *
 *
 *********************************************************************/



/**********************************************************************
 *
 * $Id: StEStructSupport.cxx,v 1.2 2005/03/03 01:33:05 porter Exp $
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
#include "StEStructPool/Correlations/StEStructMaxB.h"
#include "Stiostream.h"
#include "Stsstream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"


const char* _pair_typename[] = {"Sib","Mix"};
const char* _pair_chargename[]   = {"pp","pm","mm"};
const char* _pair_ptweight[]   = {"Pr","Su"};
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

StEStructSupport::StEStructSupport(TFile* tf, int bgmode, float* npairs, float nbar): mtf(tf), mNbar(nbar), mbgMode(bgmode), mtmpString(NULL){

  //
  // npairs is a normalization for when one cuts on say two (many) different 
  // ytyt slices and wants to compare the amplitudes, the generic normalization
  // of sum of rho = 1. isn't sufficient
  //

  if(npairs){
    mnpairs = new float[6];
    for(int i=0;i<6;i++)mnpairs[i]=npairs[i];
  }

}

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
TH1** StEStructSupport::getPtHists(const char* name){

  TH1** retVal=NULL;
  if(!goodName(name)) return retVal;
  
  retVal=new TH1*[18];

  for(int i=0;i<_pair_totalmax;i++){
    TString hname(getFrontName(i)),hprname(getFrontName(i)),hsuname(getFrontName(i));
    hname+=name;
    hprname+="Pr"; hprname+=name;
    hsuname+="Su"; hsuname+=name;
    retVal[i] = (TH1*)mtf->Get(hname.Data());
    retVal[i+6] = (TH1*)mtf->Get(hprname.Data());
    retVal[i+12] = (TH1*)mtf->Get(hsuname.Data());
  }

  return retVal;
}

//---------------------------------------------------------------
TH1** StEStructSupport::getPtClones(const char* name){

  TH1** hset=getPtHists(name);
  if(!hset) return (TH1**)NULL;

  // make local clones
  TH1** hlocal=new TH1*[_pair_totalmax*3];
  for(int i=0;i<_pair_totalmax*3;i++) {
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
  const char* nm[4]={"PP","US","MM","CS"};
  const char* tit[4]={"PP : ++ + --","US : +- + -+","MM: ++ + -- - +- - -+","CS : ++ - --"};
  for(int i=0;i<4;i++){
    retVal[i]=(TH1*)hlocal[0]->Clone();// just get correct dimensions & names
    retVal[i]->SetName(swapIn(hlocal[0]->GetName(),"Sibpp",nm[i]));
    retVal[i]->SetTitle(swapIn(hlocal[0]->GetTitle(),"Sibling : +.+",tit[i])); 
    retVal[i]->Scale(0.); // zero the hists
  }

// normalize by integral
  for(int i=0;i<6;i++)hlocal[i]->Scale(1.0/hlocal[i]->Integral());

// if requested, scale bg to require correlations>=0 where statistics are large

  float sf=1.0; // sf used in buildChargeTypes... here it is set to 1.
  if(1==mbgMode){
   scaleBackGround(hlocal[0],hlocal[3],sf);
   scaleBackGround(hlocal[1],hlocal[4],sf);
   scaleBackGround(hlocal[2],hlocal[5],sf);
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

  retVal[3]->Add(retVal[0],retVal[2],1.0,-1.0);  // delta-rho
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
TH1** StEStructSupport::buildChargeTypes(const char* name, int opt, float* sf){

  // build hist types = LS, US, CD, CI


  // six input histograms ++,+-,-- for Sib and Mix 
  TH1** hlocal=getLocalClones(name);
  if(!hlocal) return hlocal;


  // four returned hists
  TH1** retVal= new TH1*[4]; // 0=LS 1=US 2=CD=LS-US 3=CI=LS+US
  const char* nm[4]={"LS","US","CD","CI"};
  const char* tit[4]={"LS : ++ + --","US : +- + -+","CD: ++ + -- - +- - -+","CI : ++ + -- + +- + -+"};

  for(int i=0;i<4;i++){
    retVal[i]=(TH1*)hlocal[0]->Clone();
    retVal[i]->SetName(swapIn(hlocal[0]->GetName(),"Sibpp",nm[i]));
    retVal[i]->SetTitle(swapIn(hlocal[0]->GetTitle(),"Sibling : +.+",tit[i])); 
    retVal[i]->Scale(0.); // zero the hists
  }

  hlocal[0]->Add(hlocal[2]);  // ++ + -- Sib
  hlocal[3]->Add(hlocal[5]);  // ++ + -- Mix

  // At this point I no longer need -- hists; hlocal[2] and hlocal[5] 
  // So I want to reuse these for alternative calculation of CD=LS-US.

  // 1st get the LS-US;
  hlocal[2]->Scale(0.);
  hlocal[5]->Scale(0.);
  hlocal[2]->Add(hlocal[0]);
  hlocal[5]->Add(hlocal[3]);

  if(strstr(name,"DEta") || strstr(name,"SEta"))fixDEta((TH2**)hlocal,6);
  if(mnpairs){
    for(int i=0;i<6;i++) hlocal[i]->Scale(1.0/mnpairs[i]);
  } else {
     for(int i=0;i<6;i++) hlocal[i]->Scale(1.0/hlocal[i]->Integral());
  }


  hlocal[2]->Add(hlocal[1],-1.0);
  hlocal[5]->Add(hlocal[4],-1.0);

  float scf1=0.;
  float scf2=0.;
    if(sf){
      scf1=sf[0];
      scf2=sf[1];
    }      

// if requested, scale bg to require correlations>=0 where statistics are large
  if(1==mbgMode){
   scaleBackGround(hlocal[0],hlocal[3],scf1);
   scaleBackGround(hlocal[1],hlocal[4],scf2);
   if(opt==3)scaleBackGround(hlocal[2],hlocal[5]);
  }

  retVal[0]->Add(hlocal[0]);
  retVal[1]->Add(hlocal[1]);
  retVal[2]->Add(hlocal[2]);
  
  int ilim=2;  
  if(opt==3)ilim=3;
  for(int i=0;i<ilim;i++){
    retVal[i]->Add(hlocal[i+3],-1.0);  // delta-rho : if opt =1, we're done
    if(0==opt) {
      retVal[i]->Divide(hlocal[i+3]);  // delta-rho/rho_mix
    } else if(opt>=2){                 // delta-rho/sqrt(rho_mix)
      TH1* tmp=getSqrt(hlocal[i+3]);
      retVal[i]->Divide(tmp);
    } // else opt==2                     
  }
 
  if(opt<3)retVal[2]->Add(retVal[0],retVal[1],1.,-1.);  // CD
  retVal[3]->Add(retVal[0],retVal[1],1.,1.);   // CI

  // scale with nbar if requested 
  if(mNbar!=1.0)for(int i=0;i<4;i++) retVal[i]->Scale(mNbar); // Nbar scaling

  return retVal;
}

//---------------------------------------------------------
TH1** StEStructSupport::buildPtChargeTypes(const char* name){

  if(!mtf) return (TH1**)NULL;
  TH1F *hptInclusive = (TH1F *)mtf->Get("pt");
  double ptHat = hptInclusive->GetMean();
  TH1F *hNEventsSame = (TH1F *)mtf->Get("NEventsSame");
  double nEventsSame = hNEventsSame->GetEntries();
  cout <<"ptHat = " << ptHat << '\t'<<"nEventsSame = " << nEventsSame << endl;

  // -- here we get 18 hists: 
  //    3 groups of 6 (Sibpp,Sibpm,Sibmm,Mixpp,Mixpm,Mixmm) 
  //    1st 6 are number, 2nd 6 are pt1*pt2 ,3rd are pt1+pt2

  TH1** hlocal=getPtClones(name);

  // four returned hists
  TH1** retVal= new TH1*[4]; // 0=LS 1=US 2=CD=LS-US 3=CI=LS+US
  const char* nm[4]={"LS","US","CD","CI"};
  const char* tit[4]={"LS : ++ + --","US : +- + -+","CD: ++ + -- - +- - -+","CI : ++ + -- + +- + -+"};

  for(int i=0;i<4;i++){
    retVal[i]=(TH1*)hlocal[0]->Clone();
    retVal[i]->SetName(swapIn(hlocal[0]->GetName(),"Sibpp",nm[i]));
    retVal[i]->SetTitle(swapIn(hlocal[0]->GetTitle(),"Sibling : +.+",tit[i])); 
    retVal[i]->Scale(0.); // zero the hists
  }

  const char* tmp[3]={"PP","PM","MM"};  
  TH1* tmpVal[3]; // ++,+-,--
  for(int i=0;i<3;i++){
    tmpVal[i]=(TH1*)hlocal[0]->Clone();
    tmpVal[i]->SetName(swapIn(hlocal[0]->GetName(),"Sibpp",tmp[i]));
    tmpVal[i]->SetTitle(swapIn(hlocal[0]->GetTitle(),"Sibling : +.+",tmp[i])); 
    tmpVal[i]->Scale(0.); // zero the hists 
  }

  if(strstr(name,"DEta") || strstr(name,"SEta"))fixDEta((TH2**)hlocal,18);

  for(int i=0;i<3;i++){
    for(int ix=0;ix<=tmpVal[i]->GetNbinsX();ix++){
     for(int iy=0;iy<=tmpVal[i]->GetNbinsY();iy++){
       double n = hlocal[i]->GetBinContent(ix, iy) / nEventsSame; // number
       double a = hlocal[i+6]->GetBinContent(ix, iy) / nEventsSame; // ptxpt
       double b = hlocal[i+12]->GetBinContent(ix, iy) / nEventsSame; // pt+pt
       double mixn = hlocal[i+3]->GetBinContent(ix, iy) / _MAXEBYEBUFFER_ / nEventsSame;// mixN
       double z = 0;
       if( mixn != 0 ) {
	    z = (a - b * ptHat + n * ptHat * ptHat ) / sqrt(double(mixn));
       }
       tmpVal[i]->SetBinContent(ix, iy, z);
        
     }
    }
  }

  retVal[0]->Add(tmpVal[0],tmpVal[2]);
  retVal[1]->Add(tmpVal[1]);
  retVal[2]->Add(retVal[0],retVal[1],1.0,-1.0);
  retVal[3]->Add(retVal[0],retVal[1],1.0,1.0);

  return retVal;

}


//---------------------------------------------------------
void StEStructSupport::scaleBackGround(TH1* sib, TH1* mix, float sf){

  float alpha=1.0;
 
  if(sf!=0.){
    alpha=sf;
  } else {
     
  float hmax=mix->GetMaximum();
  for(int ix=1;ix<=mix->GetNbinsX();ix++){
    for(int iy=1;iy<=mix->GetNbinsY();iy++){
      float mval=mix->GetBinContent(ix,iy);
      if(mval>0.){
	float sval=sib->GetBinContent(ix,iy);
        if(sval==0.)continue;
        float eval=sib->GetBinError(ix,iy)/sval;
	float dval=fabs(sval-mval)/sval;
	if(sval<mval && dval>3.0*eval && mval/hmax>0.45){
	  //       if(sval/hmax>0.5){
	  float rval=sval/mval;
          if(rval<alpha) alpha=rval;
	}
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
  double dx2=dx*dx;
  double amax=2.0*r2*dx-1.5*dx2;

  for(int ix=1;ix<=atmp->GetNbinsX();ix++){
    double dx1=(double)atmp->GetBinLowEdge(ix)+0.5*dx;
    double aval=amax;
    if(dx1!=0.)aval=r2*(2.0-fabs(dx1))*dx;
    double rval=amax/aval;
    atmp->SetBinContent(ix,rval);
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
void StEStructSupport::writeAscii(TH1** h, int numHists, const char* fname, int optErrors){

  ofstream of(fname);
  for(int n=0;n<numHists;n++){
   int xbins=h[n]->GetNbinsX();
   TAxis * xa= h[n]->GetXaxis();
   of<<"# Histogram Name = "<<h[n]->GetName()<<endl;
   of<<"# X-axis: min="<<xa->GetBinLowEdge(1)<<" max="<<xa->GetBinUpEdge(xbins)<<" nbins="<<xbins<<endl;
 
   int ybins=h[n]->GetNbinsY();
   TAxis * ya= h[n]->GetYaxis();
   if(ybins>1){
     of<<"# Y-axis: min="<<ya->GetBinLowEdge(1)<<" max="<<ya->GetBinUpEdge(ybins)<<" nbins="<<ybins<<endl;
     of<<"# ix  iy  Value "<<endl; 
     for(int i=1;i<=xbins;i++){
       for(int j=1;j<=ybins;j++){
         of<<i<<"   "<<j<<"   "<<h[n]->GetBinContent(i,j);
         if(optErrors>0)of<<"  "<<h[n]->GetBinError(i,j);
         of<<endl;
       }
     }
     of<<endl;
   } else {
     of<<"# ix Value "<<endl;
     for(int i=1;i<=xbins;i++){
         of<<i<<"   "<<h[n]->GetBinContent(i);
         if(optErrors>0)of<<"  "<<h[n]->GetBinError(i);
         of<<endl;
     }
     of<<endl;
   }

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
 * Revision 1.2  2005/03/03 01:33:05  porter
 * Added pt-correlations method to support and included
 * these histograms to the HAdd routine
 *
 * Revision 1.1  2004/07/01 00:37:17  porter
 * new code previously my StEStructHelper. Takes hists from correltation
 * pass and builds final ressults.  Also the StEStructHAdd.h is a simple
 * replacemnt for my sumyt.C macro which could be expanded later as needed.
 *
 *
 *
 *********************************************************************/



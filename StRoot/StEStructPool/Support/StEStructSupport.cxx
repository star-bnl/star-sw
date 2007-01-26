/**********************************************************************
 *
 * $Id: StEStructSupport.cxx,v 1.15 2007/01/26 17:20:58 msd Exp $
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
#include <sstream>
#include "Stsstream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"


const char* _pair_typename[] = {"Sib","Mix"};
const char* _pair_chargename[]   = {"pp","pm","mp","mm"};
const char* _pair_ptweight[]   = {"Pr","Su"};
int _pair_typemax=2;
int _pair_chargemax=4;
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

  /* for itype=0-7, returns Sibpp, Sibpm, Sibmp, Sibmm, Mixpp, Mixpm, Mixmp, Mixmm   */

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

  mapplyDEtaFix=false; // must set explicitly now

  if(npairs){
    mnpairs = new float[8];
    for(int i=0;i<8;i++)mnpairs[i]=npairs[i];
  } else {
    mnpairs = 0;
  }

}

StEStructSupport::~StEStructSupport(){ 
  if(mtmpString) delete [] mtmpString; 
  if(mnpairs) delete [] mnpairs; 
};

//---------------------------------------------------------
TH1** StEStructSupport::getHists(const char* name){

  /*
  TH1** retVal=NULL;
  if(!goodName(name)) return retVal;
  retVal=new TH1*[8];
  for(int i=0;i<_pair_totalmax;i++){
  TString hname(getFrontName(i));
  hname+=name;
  retVal[i] = (TH1*)mtf->Get(hname.Data());
  }

  return retVal;
  }
  */

  TH1** retVal=NULL;
  if(!goodName(name)) return retVal;

  retVal=new TH1*[8];

  for(int i=0;i<_pair_totalmax;i++){
    TString hname(getFrontName(i));
    hname+=name;

    TH1* tmpF=(TH1*)mtf->Get(hname.Data());
    TAxis * xa=tmpF->GetXaxis();
    TAxis * ya=tmpF->GetYaxis();

    TH1::AddDirectory(kFALSE);
    if(ya->GetNbins()==1){
      retVal[i]=(TH1*)new TH1D(tmpF->GetName(),tmpF->GetTitle(),
			       xa->GetNbins(),xa->GetXmin(),xa->GetXmax());
    } else {
      retVal[i]=(TH1*)new TH2D(tmpF->GetName(),tmpF->GetTitle(),
			       xa->GetNbins(),xa->GetXmin(),xa->GetXmax(),
			       ya->GetNbins(),ya->GetXmin(),ya->GetXmax());
    }

    for(int ix=1;ix<=xa->GetNbins();ix++){
      for(int iy=1;iy<=ya->GetNbins();iy++){
        retVal[i]->SetBinContent(ix,iy,tmpF->GetBinContent(ix,iy));
        retVal[i]->SetBinError(ix,iy,tmpF->GetBinError(ix,iy));
      }
    }
  }

  //   retVal[i] = (TH1*)mtf->Get(hname.Data());

  return retVal;
}
//-----------------------------------------------------
float* StEStructSupport::getNorms(TH1** histArray){

  /* not really used any more but keep */

  float* retVal=NULL;
  if(!histArray) return retVal;

  retVal=new float[8];
  for(int i=0;i<_pair_totalmax; i++) retVal[i]=histArray[i]->Integral();

  return retVal;
}
//-----------------------------------------------------
void StEStructSupport::rescale(TH1** hists){
  // Divides hists by bin widths, divides by Nevents, normalizes mix to sib
  
  if(!mtf) return;

  TH1F* hNEventsSame = (TH1F*)mtf->Get("NEventsSame");
  double nev = hNEventsSame->GetEntries();
  
  for(int i=0;i<4;i++){
    if(hists[i]->Integral() > 0) {
      double binFactor = 1.0;
      double dx = (hists[i]->GetXaxis())->GetBinWidth(1);
      double dy = (hists[i]->GetYaxis())->GetBinWidth(1);
      binFactor = dx*dy; 
      // divinding by ex*ey converts all input hists from counts to densities, so all operations and final result should also be density.
      if(i==0) cout << "Scaling with Nevents " << nev << " and binFactor " << binFactor << endl;
      hists[i]->Scale(1.0/(nev*binFactor));
    }
  }
  
  for(int i=0;i<4;i++){  // norm mix to sib
    double rscale=hists[i]->Integral();
    if(rscale>0) hists[i+4]->Scale(rscale/hists[i+4]->Integral()); 
    else  hists[i+4]->Scale(0);
  }
  
};
//---------------------------------------------------------
TH1** StEStructSupport::getLocalClones(const char* name){

  TH1** hset=getHists(name);
  if(!hset) return (TH1**)NULL;

  // make local clones
  TH1** hlocal=new TH1*[_pair_totalmax];
  for(int i=0;i<_pair_totalmax;i++) {
    hlocal[i]=(TH1*)hset[i]->Clone();
//    hlocal[i]->Sumw2();  // trigger error propogation
  }
  delete hset;

  return hlocal;
}

//---------------------------------------------------------
TH1** StEStructSupport::getPtHists(const char* name){

  TH1** retVal=NULL;
  if(!goodName(name)) return retVal;
  
  retVal=new TH1*[32];

  for(int i=0;i<_pair_totalmax;i++){
    TString hname(getFrontName(i)),hprname(getFrontName(i)),hpaname(getFrontName(i)),hpbname(getFrontName(i));
    hname+=name;
    hprname+="Pr"; hprname+=name;
    hpaname+="Pa"; hpaname+=name;
    hpbname+="Pb"; hpbname+=name;
    retVal[i] = (TH1*)mtf->Get(hname.Data());
    retVal[i+8] = (TH1*)mtf->Get(hprname.Data());
    retVal[i+16] = (TH1*)mtf->Get(hpaname.Data());
    retVal[i+24] = (TH1*)mtf->Get(hpbname.Data());
  }

  return retVal;
}

//---------------------------------------------------------------
TH1** StEStructSupport::getPtClones(const char* name){

  TH1** hset=getPtHists(name);
  if(!hset) return (TH1**)NULL;

  // make local clones
  TH1** hlocal=new TH1*[_pair_totalmax*4];
  for(int i=0;i<_pair_totalmax*4;i++) {
    hlocal[i]=(TH1*)hset[i]->Clone();
    hlocal[i]->Sumw2();  // trigger error propogation
  }
  delete hset;

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
// Note that I (djp) modified the analysis code so that we only fill the -+
// histograms for identified, different particles. So we get it for
// pi-K+ as an example but not for unidentified particles (as in mode 1
// and mode 3) where we don't know what the particle types are.
// That might affect the utility of this routine.
TH1** StEStructSupport::buildCommon(const char* name, int opt){

  /* builds hist types = ++,+-,-+,-- */
 
  // eight input histograms ++,+-,-+,-- for Sib and Mix 
  TH1** hlocal=getLocalClones(name);
  if(!hlocal) return hlocal;


  // four returned hists
  TH1** retVal= new TH1*[4]; // 0=++ 1=+- 2=-+ 3=--
  const char* nm[4]={"PP","PM","MP","MM"};
  const char* tit[4]={"PP : ++","PM : +-","MP: -+","MM : --"};
  for(int i=0;i<4;i++){
    retVal[i]=(TH1*)hlocal[0]->Clone();// just get correct dimensions & names
    retVal[i]->SetName(swapIn(hlocal[0]->GetName(),"Sibpp",nm[i]));
    retVal[i]->SetTitle(swapIn(hlocal[0]->GetTitle(),"Sibling : +.+",tit[i])); 
    retVal[i]->Scale(0.); // zero the hists
  }

// normalize by integral
  for(int i=0;i<8;i++) {
      if (0 != hlocal[i]->Integral()) {
          hlocal[i]->Scale(1.0/hlocal[i]->Integral());
      }
  }

// if requested, scale bg to require correlations>=0 where statistics are large

  float sf=1.0; // sf used in buildChargeTypes... here it is set to 1.
  if(1==mbgMode){
   scaleBackGround(hlocal[0],hlocal[4],sf);
   scaleBackGround(hlocal[1],hlocal[5],sf);
   scaleBackGround(hlocal[2],hlocal[6],sf);
   scaleBackGround(hlocal[3],hlocal[7],sf);
  }

  for(int i=0;i<4;i++){
    retVal[i]->Add(hlocal[i],hlocal[i+4],1.0,-1.0); // delta rho: if opt=1 done

    if(0==opt){ 
      retVal[i]->Divide(hlocal[i+4]);  // delta-rho/rho_mix
    } else if(2==opt){
      TH1* tmp=getSqrt(hlocal[i+4]);
      if(tmp)retVal[i]->Divide(tmp);  // delta-rho/sqrt(rho_mix);
      delete tmp;
    }

  }

  // Free memory of hlocal.
  for(int i=0;i<_pair_totalmax;i++) {
    delete hlocal[i];
  }
  delete hlocal;

  if(mNbar!=1.0)for(int i=0;i<4;i++) retVal[i]->Scale(mNbar); // Nbar scaling

  return retVal;
}
//---------------------------------------------------------
TH1** StEStructSupport::buildNCommon(const char* name){

  if(!mtf) return (TH1**)NULL;
  TH1F *hNEventsSame = (TH1F *)mtf->Get("NEventsSame");
  double nEventsSame = hNEventsSame->GetEntries();
  TH1F *hNEventsMixed = (TH1F *)mtf->Get("NEventsMixed");
  double nEventsMixed = hNEventsMixed->GetEntries();

  // -- here we get 8 hists: 
  //    (Sibpp,Sibpm,Sibmp,Sibmm,Mixpp,Mixpm,Mixmp,Mixmm) 
  //    All are number.

  TH1** hlocal=getLocalClones(name);

  // four returned hists
  TH1** retVal= new TH1*[4]; // 0=LS 1=US 2=CD=LS-US 3=CI=LS+US
  const char* nm[4]={"LS","US","CD","CI"};
  const char* tit[4]={"LS : ++ + --","US : +- + -+","CD: ++ + -- - +- - -+","CI : ++ + -- + +- + -+"};

  // Subtract mixed from sibling to get \Delta\rho, scaling to same
  //  number of pairs in mixed as sibling.
  // Then normalize both \Delta\rho and \rho by number of events.
  double r;
  for(int i=0;i<4;i++){
    retVal[i]=(TH1*)hlocal[i]->Clone();
    retVal[i]->SetName(swapIn(hlocal[i]->GetName(),"Sibpp",nm[i]));
    retVal[i]->SetTitle(swapIn(hlocal[i]->GetTitle(),"Sibling : +.+",tit[i])); 

    // For pid case when we have same type of particles +- and -+ are
    // both stored in +-, so -+ is empty.
    if (0 ==hlocal[i+4]->Integral()) {
        r = 0.;
    } else {
        r = hlocal[i]->Integral() / hlocal[i+4]->Integral();
    }
    retVal[i]->Add(hlocal[i+4],-r);
    retVal[i]->Scale(1.0/nEventsSame);
    hlocal[i+4]->Scale(1.0/nEventsMixed);
  }

  // Calculate \Delta\rho/\sqrt(\rho_{ref})
  for(int i=0;i<4;i++){
    for(int ix=1;ix<=retVal[i]->GetNbinsX();ix++){
      for(int iy=1;iy<=retVal[i]->GetNbinsY();iy++){
        double drho = retVal[i]->GetBinContent(ix, iy);   // \Delta\rho
        double mixn = hlocal[i+4]->GetBinContent(ix, iy); // \rho_{ref}
        if (mixn <= 0) {
            mixn = 1;
        }
        retVal[i]->SetBinContent(ix, iy, drho/sqrt(mixn) );
      }
    }
  }

  // Free local histograms.
  for(int i=0;i<_pair_totalmax;i++) {
    delete hlocal[i];
  }
  delete [] hlocal;

  return retVal;

}

//---------------------------------------------------------
// When we have imposed some type of pt cuts we may have introduced
// a covariance which will show up here. Subtract mixed pairs
// to reduce this contribution.
TH1** StEStructSupport::buildPtCommon(const char* name, int opt, int subtract){

  /* builds hist types = ++,+-,-+,-- */

  if(!mtf) return (TH1**)NULL;
  TH1F *hptInclusiveA = (TH1F *)mtf->Get("pta");
  double ptHatA = hptInclusiveA->GetMean();
  TH1F *hptInclusiveB = (TH1F *)mtf->Get("ptb");
  double ptHatB = hptInclusiveB->GetMean();
  TH1F *hNEventsSame = (TH1F *)mtf->Get("NEventsSame");
  double nEventsSame = hNEventsSame->GetEntries();
  TH1F *hNEventsMixed = (TH1F *)mtf->Get("NEventsMixed");
  double nEventsMixed = hNEventsMixed->GetEntries();
  cout <<"ptHatA = " << ptHatA << '\t'<< ", ptHatB = " << ptHatB << endl;
  cout << "nEventsSame = " << nEventsSame << ", nEventsMixed = " << nEventsMixed << endl;

  // -- here we get 32 hists: 
  //    4 groups of 8 (Sibpp,Sibpm,Sibmp,Sibmm,Mixpp,Mixpm,Mixmp,Mixmm) 
  //    1st 8 are number, 2nd 8 are pt1*pt2, 3rd are pt1 and 4th are pt2

  TH1** hlocal=getPtClones(name);

  // four returned hists
  TH1** retVal= new TH1*[4]; // 0=++ 1=+- 2=-+ 3=--
  const char* nm[4]={"PP","PM","MP","MM"};
  const char* tit[4]={"PP : ++","PM : +-","MP: -+","MM : --"};
  for(int i=0;i<4;i++){
    retVal[i]=(TH1*)hlocal[0]->Clone();// just get correct dimensions & names
    retVal[i]->SetName(swapIn(hlocal[0]->GetName(),"Sibpp",nm[i]));
    retVal[i]->SetTitle(swapIn(hlocal[0]->GetTitle(),"Sibling : +.+",tit[i])); 
    retVal[i]->Scale(0.); // zero the hists
  }

  TH1* tmpVal[4]; // ++,+-,-+,--
  for(int i=0;i<4;i++){
    tmpVal[i]=(TH1*)hlocal[0]->Clone();
    tmpVal[i]->SetName(swapIn(hlocal[0]->GetName(),"Sibpp",nm[i]));
    tmpVal[i]->SetTitle(swapIn(hlocal[0]->GetTitle(),"Sibling : +.+",nm[i])); 
    tmpVal[i]->Scale(0.); // zero the hists 
  }

  if(strstr(name,"DEta") || strstr(name,"SEta"))fixDEta((TH2**)hlocal,32);

  for(int i=0;i<4;i++){
    double r;
    if (0 == hlocal[i+4]->Integral()) {
        r = 0;
    } else {
        r = hlocal[i]->Integral() / hlocal[i+4]->Integral();
    }
    for(int ix=1;ix<=tmpVal[i]->GetNbinsX();ix++){
      for(int iy=1;iy<=tmpVal[i]->GetNbinsY();iy++){
        double sn  = hlocal[i]->GetBinContent(ix, iy); // sibling number
        double sa  = hlocal[i+8]->GetBinContent(ix, iy); // sibling ptxpt
        double sba = hlocal[i+16]->GetBinContent(ix, iy); // sibling pta
        double sbb = hlocal[i+24]->GetBinContent(ix, iy); // sibling ptb
        double mn  = hlocal[i+4]->GetBinContent(ix, iy); // mixed number
        double ma  = hlocal[i+12]->GetBinContent(ix, iy); // mixed ptxpt
        double mba = hlocal[i+20]->GetBinContent(ix, iy); // mixed pta
        double mbb = hlocal[i+28]->GetBinContent(ix, iy); // mixed ptb
        double mixn = hlocal[i+4]->GetBinContent(ix, iy) / nEventsMixed;// mixN
        double z = 0;
        if (subtract) {
            sa  = (sa  - r*ma) / nEventsSame;
            sba = (sba - r*mba) / nEventsSame;
            sbb = (sbb - r*mbb) / nEventsSame;
            sn  = (sn  - r*mn) / nEventsSame;
        }
        if( mixn != 0 ) {
	      switch(opt){
            case 0: {
              z = (sa - (sba*ptHatB + sbb*ptHatA) + sn*ptHatA*ptHatB ) / sqrt(double(mixn));
              break;
	        }
            case 1: {
              z = -1.0*(sa - (sba*ptHatB + sbb*ptHatA)) / sqrt(double(mixn));
              break;
	        }
            case 2: {
              z = ( sn*ptHatA*ptHatB ) / sqrt(double(mixn));
              break;
	        }
            default: {
              z = (sa - (sba*ptHatB + sbb*ptHatA) + sn*ptHatA*ptHatB ) / sqrt(double(mixn));
              break;
	        }
	      }
        }
        tmpVal[i]->SetBinContent(ix, iy, z);
      }
    }
  }

  retVal[0]->Add(tmpVal[0]);
  retVal[1]->Add(tmpVal[1]);
  retVal[2]->Add(tmpVal[2]);
  retVal[3]->Add(tmpVal[3]);

  // Free local histograms.
  for (int i=1;i<4;i++) {
      delete tmpVal[i];
  }
  for(int i=0;i<_pair_totalmax*4;i++) {
    delete hlocal[i];
  }
  delete [] hlocal;

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


  // eight input histograms ++,+-,-+,-- for Sib and Mix
  TH1** hlocal=getLocalClones(name);
  if(!hlocal) return hlocal;

  if(mnpairs){  // manual scaling
    for(int i=0;i<8;i++) hlocal[i]->Scale(1.0/mnpairs[i]);
  } else rescale(hlocal);  // automatic scaling, norm sib to pairs per event, norm mix to sib

  if(strstr(name,"DEta") || strstr(name,"SEta"))fixDEta((TH2**)hlocal,8); // does nothing unless mapplyDEtaFix is set

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

  hlocal[0]->Add(hlocal[3]);                // ++ + -- Sib
  if(hlocal[2]) hlocal[1]->Add(hlocal[2]);  // +- + -+ Sib, backward compatible
  hlocal[4]->Add(hlocal[7]);                // ++ + -- Mix
  if(hlocal[6]) hlocal[5]->Add(hlocal[6]);  // +- + -+ Mix

  float scf1=0.;
  float scf2=0.;
  if(sf){
    scf1=sf[0];
    scf2=sf[1];
  }
  // if requested, scale bg to require correlations>=0 where statistics are large
  if(1==mbgMode){
    scaleBackGround(hlocal[0],hlocal[4],scf1);
    scaleBackGround(hlocal[1],hlocal[5],scf2);
    if(opt==3)scaleBackGround(hlocal[3],hlocal[7]);
  }

  retVal[0]->Add(hlocal[0]);  // LS sib
  retVal[1]->Add(hlocal[1]);  // US sib

  // opt: 0 = delta-rho/rho_mix;  1 = delta-rho;  2 = delta-rho/sqrt(rho_mix)

  for(int i=0;i<2;i++){  // loop over LS and US
    retVal[i]->Add(hlocal[i+4],-1.0);  // delta-rho
  }
  retVal[2]->Add(retVal[0],retVal[1],1.,-1.);  // CD sib
  retVal[3]->Add(retVal[0],retVal[1],1.,1.);   // CI sib

  if(opt!=1) {  // retVal has LS, US, CD, CI sib; use hlocal to take ratios
    hlocal[6]->Add(hlocal[4], hlocal[5]);
    hlocal[7]->Add(hlocal[4], hlocal[5]);  // hlocal[4] starts mix for denominators

    for(int i=0;i<4;i++) {  //2
      retVal[i]->Divide(hlocal[i+4]);  // delta-rho/rho_mix
      // NOTE: now CI = (LS_sib + US_sib) / (LS_mix + US_mix), not sum of ratios

      if(opt>=2){    // delta-rho/sqrt(rho_mix)
        TH1 *href;   // should be corrected rho_ref, for now take rho_mix
        href = getSqrt(hlocal[i+4]);
        retVal[i]->Multiply(href);
        delete href;
      }
    }
  }

  // Free local histograms.
  for(int i=0;i<_pair_totalmax;i++) {
    delete hlocal[i];
  }
  delete hlocal;

  return retVal;
}
//---------------------------------------------------------
TH1** StEStructSupport::buildNChargeTypes_Old(const char* name){

  if(!mtf) return (TH1**)NULL;
  TH1F *hNEventsSame = (TH1F *)mtf->Get("NEventsSame");
  double nEventsSame = hNEventsSame->GetEntries();
  TH1F *hNEventsMixed = (TH1F *)mtf->Get("NEventsMixed");
  double nEventsMixed = hNEventsMixed->GetEntries();

  // -- here we get 8 hists: 
  //    (Sibpp,Sibpm,Sibmp,Sibmm,Mixpp,Mixpm,Mixmp,Mixmm) 
  //    All are number.

  TH1** hlocal=getLocalClones(name);

  // four returned hists
  TH1** retVal= new TH1*[4]; // 0=LS 1=US 2=CD=LS-US 3=CI=LS+US
  const char* nm[4]={"LS","US","CD","CI"};
  const char* tit[4]={"LS : ++ + --","US : +- + -+","CD: ++ + -- - +- - -+","CI : ++ + -- + +- + -+"};

  // Subtract mixed from sibling to get \Delta\rho, scaling to same
  //  number of pairs in mixed as sibling.
  // Then normalize both \Delta\rho and \rho by number of events.
  double r;
  for(int i=0;i<4;i++){
    retVal[i]=(TH1*)hlocal[i]->Clone();
    retVal[i]->SetName(swapIn(hlocal[i]->GetName(),"Sibpp",nm[i]));
    retVal[i]->SetTitle(swapIn(hlocal[i]->GetTitle(),"Sibling : +.+",tit[i])); 

    // For pid case when we have same type of particles +- and -+ are
    // both stored in +-, so -+ is empty.
    if (0 ==hlocal[i+4]->Integral()) {
        r = 0.;
    } else {
        r = hlocal[i]->Integral() / hlocal[i+4]->Integral();
    }
    retVal[i]->Add(hlocal[i+4],-r);
    retVal[i]->Scale(1.0/nEventsSame);
    hlocal[i+4]->Scale(1.0/nEventsMixed);
  }

  // Calculate \Delta\rho/\sqrt(\rho_{ref})
  for(int i=0;i<4;i++){
    for(int ix=1;ix<=retVal[i]->GetNbinsX();ix++){
      for(int iy=1;iy<=retVal[i]->GetNbinsY();iy++){
        double drho = retVal[i]->GetBinContent(ix, iy);   // \Delta\rho
        double mixn = hlocal[i+4]->GetBinContent(ix, iy); // \rho_{ref}
        if (mixn <= 0) {
            mixn = 1;
        }
        retVal[i]->SetBinContent(ix, iy, drho/sqrt(mixn) );
      }
    }
  }

  // Now form LS, US, CD and CI combinations.
  retVal[0]->Add(retVal[3]);
  retVal[1]->Add(retVal[2]);
  retVal[2]->Add(retVal[0],retVal[1],1.0,-1.0);
  retVal[3]->Add(retVal[0],retVal[1],1.0,1.0);

  // Free local histograms.
  for(int i=0;i<_pair_totalmax;i++) {
    delete hlocal[i];
  }
  delete [] hlocal;

  return retVal;

}
TH1** StEStructSupport::buildNChargeTypes(const char* name) {

  if(!mtf) return (TH1**)NULL;
  TH1F *hNEventsSame = (TH1F *)mtf->Get("NEventsSame");
  double nEventsSame = hNEventsSame->GetEntries();
  TH1F *hNEventsMixed = (TH1F *)mtf->Get("NEventsMixed");
  double nEventsMixed = hNEventsMixed->GetEntries();

  // -- here we get 8 hists: 
  //    (Sibpp,Sibpm,Sibmp,Sibmm,Mixpp,Mixpm,Mixmp,Mixmm) 
  //    All are number.

  TH1** hlocal=getLocalClones(name);

  // four temporary histograms
  TH1** tmpVal= new TH1*[4]; // 0=LS 1=US 2=CD=LS-US 3=CI=LS+US
  // four returned hists
  TH1** retVal= new TH1*[4]; // 0=LS 1=US 2=CD=LS-US 3=CI=LS+US
  const char* nm[4]={"LS","US","CD","CI"};
  const char* tit[4]={"LS : ++ + --","US : +- + -+","CD: ++ + -- - +- - -+","CI : ++ + -- + +- + -+"};
  TString hname;

  // Form LS, US, CI and CD combinations before calculating \Delta\rho
  //  and \Delta\rho/sqrt(\rho_{ref}).
  for(int i=0;i<4;i++){
    tmpVal[i]=(TH1*)hlocal[i+4]->Clone();
    retVal[i]=(TH1*)hlocal[i]->Clone();
    hname=nm[i];  hname+=name; retVal[i]->SetName(hname);
    hname=tit[i]; hname+=name; retVal[i]->SetTitle(hname);
  }
  retVal[0]->Add(retVal[3]);
  retVal[1]->Add(retVal[2]);
  retVal[2]->Add(retVal[0],retVal[1],1.0,-1.0);
  retVal[3]->Add(retVal[0],retVal[1],1.0,1.0);
  tmpVal[0]->Add(tmpVal[3]);
  tmpVal[1]->Add(tmpVal[2]);
  tmpVal[2]->Add(tmpVal[0],tmpVal[1],1.0,-1.0);
  tmpVal[3]->Add(tmpVal[0],tmpVal[1],1.0,1.0);

  // Subtract mixed from sibling to get \Delta\rho, scaling to same
  //  number of pairs in mixed as sibling.
  //  Then normalize both \Delta\rho and \rho by number of events.
  //  We want to subtract mixed CD from sibling CD but the number of
  //  pairs in CD is given by the integral of the CI histograms.
  double scale[4];
  scale[0] = retVal[0]->Integral() / tmpVal[0]->Integral();
  scale[1] = retVal[1]->Integral() / tmpVal[1]->Integral();
  scale[2] = retVal[3]->Integral() / tmpVal[3]->Integral();
  scale[3] = retVal[3]->Integral() / tmpVal[3]->Integral();
  for(int i=0;i<4;i++){
    retVal[i]->Add(tmpVal[i],-scale[i]);
    retVal[i]->Scale(1.0/nEventsSame);
    tmpVal[i]->Scale(1.0/nEventsMixed); // **TEST**
  }

  // Calculate \Delta\rho/\sqrt(\rho_{ref})
  tmpVal[2]->Scale(0);
  tmpVal[2]->Add(tmpVal[3]);
  for(int i=0;i<4;i++){
    for(int ix=1;ix<=retVal[i]->GetNbinsX();ix++){
      for(int iy=1;iy<=retVal[i]->GetNbinsY();iy++){
        double drho = retVal[i]->GetBinContent(ix, iy);   // \Delta\rho
        double mixn = tmpVal[i]->GetBinContent(ix, iy); // \rho_{ref}
        if (mixn <= 0) {
            mixn = 1;
        }
        retVal[i]->SetBinContent(ix, iy, drho/sqrt(mixn) );
      }
    }
  }

  // Free local histograms.
  for(int i=0;i<4;i++) {
    delete tmpVal[i];
  }
  for(int i=0;i<_pair_totalmax;i++) {
    delete hlocal[i];
  }
  delete [] hlocal;

  return retVal;

}
//---------------------------------------------------------
TH1** StEStructSupport::buildPtChargeTypes_Old(const char* name, int opt, int subtract){
  // I appears I made the same mistake in this routine as I did in the
  // buildNChargeTypes above, namely calculate \Delta\rho/sqrt(rho_{ref}) for
  // ++, +-, -+ and -- and then adding them together.
  // I of course want to calculate \Delta\rho and rho_{ref} for LS, US, CD and CI,
  // then calculate the ratio.
  if(!mtf) return (TH1**)NULL;
  TH1F *hptInclusiveA = (TH1F *)mtf->Get("pta");
  double ptHatA = hptInclusiveA->GetMean();
  TH1F *hptInclusiveB = (TH1F *)mtf->Get("ptb");
  double ptHatB = hptInclusiveB->GetMean();
  TH1F *hNEventsSame = (TH1F *)mtf->Get("NEventsSame");
  double nEventsSame = hNEventsSame->GetEntries();
  TH1F *hNEventsMixed = (TH1F *)mtf->Get("NEventsMixed");
  double nEventsMixed = hNEventsMixed->GetEntries();

  // -- here we get 32 hists: 
  //    4 groups of 8 (Sibpp,Sibpm,Sibmp,Sibmm,Mixpp,Mixpm,Mixmp,Mixmm) 
  //    1st 8 are number, 2nd 8 are pt1*pt2, 3rd 8 are pt1 and 4th 8 are pt2

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

  const char* tmp[4]={"PP","PM","MP","MM"};  
  TH1* tmpVal[4]; // ++,+-,-+,--
  for(int i=0;i<4;i++){
    tmpVal[i]=(TH1*)hlocal[0]->Clone();
    tmpVal[i]->SetName(swapIn(hlocal[0]->GetName(),"Sibpp",tmp[i]));
    tmpVal[i]->SetTitle(swapIn(hlocal[0]->GetTitle(),"Sibling : +.+",tmp[i])); 
    tmpVal[i]->Scale(0.); // zero the hists 
  }

  if(strstr(name,"DEta") || strstr(name,"SEta"))fixDEta((TH2**)hlocal,32);

  for(int i=0;i<4;i++){
    double r;
    if (0 == hlocal[i+4]->Integral()) {
        r = 0;
    } else {
        r = hlocal[i]->Integral() / hlocal[i+4]->Integral();
    }
    for(int ix=1;ix<=tmpVal[i]->GetNbinsX();ix++){
      for(int iy=1;iy<=tmpVal[i]->GetNbinsY();iy++){
        double sn  = hlocal[i]->GetBinContent(ix, iy); // sibling number
        double sa  = hlocal[i+8]->GetBinContent(ix, iy); // sibling ptxpt
        double sba = hlocal[i+16]->GetBinContent(ix, iy); // sibling pta
        double sbb = hlocal[i+24]->GetBinContent(ix, iy); // sibling ptb
        double mn  = hlocal[i+4]->GetBinContent(ix, iy); // mixed number
        double ma  = hlocal[i+12]->GetBinContent(ix, iy); // mixed ptxpt
        double mba = hlocal[i+20]->GetBinContent(ix, iy); // mixed pta
        double mbb = hlocal[i+28]->GetBinContent(ix, iy); // mixed ptb
        double mixn = hlocal[i+4]->GetBinContent(ix, iy) / nEventsMixed;// mixN
        double z = 0;
        if (subtract) {
            sn  = (sn  - mn*r) / nEventsSame;
            sa  = (sa  - ma*r) / nEventsSame;
            sba = (sba - mba*r) / nEventsSame;
            sbb = (sbb - mbb*r) / nEventsSame;
        }

        if( mixn != 0 ) {
          switch(opt){
            case 0: {
              z = (sa - (sba*ptHatB+sbb*ptHatA) + sn*ptHatA*ptHatB ) / sqrt(double(mixn));
              break;
            } 
            case 1: {
              z = -1.0*(sa - (sba*ptHatB+sbb*ptHatA)) / sqrt(double(mixn));
              break;
            }
            case 2: {
              z = ( sn*ptHatB*ptHatA ) / sqrt(double(mixn));
              break;
            }
            case 3: {
              //z = (sb * ptHat) / sqrt(double(mixn));
              break;
            }
            case 4: {
              z = sa/sqrt(double(mixn));
              break;
            }
            default: {
              z = (sa - (sba*ptHatB+sbb*ptHatA) + sn*ptHatA*ptHatB ) / sqrt(double(mixn));
              break;
            }
          }
          tmpVal[i]->SetBinContent(ix, iy, z );
        }
      }
    }
  }

  retVal[0]->Add(tmpVal[0],tmpVal[3]);
  retVal[1]->Add(tmpVal[1],tmpVal[2]);
  retVal[2]->Add(retVal[0],retVal[1],1.0,-1.0);
  retVal[3]->Add(retVal[0],retVal[1],1.0,1.0);

  // Free local histograms.
  for (int i=1;i<4;i++) {
      delete tmpVal[i];
  }
  for(int i=0;i<_pair_totalmax*4;i++) {
    delete hlocal[i];
  }
  delete [] hlocal;

  return retVal;

}
TH1** StEStructSupport::buildPtChargeTypes(const char* name, int opt, int subtract){

  if(!mtf) return (TH1**)NULL;
  TH1F *hptInclusiveA = (TH1F *)mtf->Get("pta");
  double ptHatA = hptInclusiveA->GetMean();
  TH1F *hptInclusiveB = (TH1F *)mtf->Get("ptb");
  double ptHatB = hptInclusiveB->GetMean();
  TH1F *hNEventsSame = (TH1F *)mtf->Get("NEventsSame");
  double nEventsSame = hNEventsSame->GetEntries();
  TH1F *hNEventsMixed = (TH1F *)mtf->Get("NEventsMixed");
  double nEventsMixed = hNEventsMixed->GetEntries();

  // -- here we get 32 hists: 
  //    4 groups of 8 (Sibpp,Sibpm,Sibmp,Sibmm,Mixpp,Mixpm,Mixmp,Mixmm) 
  //    1st 8 are number, 2nd 8 are pt1*pt2, 3rd 8 are pt1 and 4th 8 are pt2

  TH1** hlocal=getPtClones(name);

  // four returned hists
  TH1** retVal= new TH1*[4]; // 0=LS 1=US 2=CD=LS-US 3=CI=LS+US
  const char* nm[4]={"LS","US","CD","CI"};
  const char* tit[4]={"LS : ++ + --","US : +- + -+","CD: ++ + -- - +- - -+","CI : ++ + -- + +- + -+"};
  TH1* tmpVal[4]; // ++,+-,-+,--

  for(int i=0;i<4;i++){
    retVal[i]=(TH1*)hlocal[0]->Clone();
    retVal[i]->SetName(swapIn(hlocal[0]->GetName(),"Sibpp",nm[i]));
    retVal[i]->SetTitle(swapIn(hlocal[0]->GetTitle(),"Sibling : +.+",tit[i])); 
    retVal[i]->Scale(0.); // zero the hists
    tmpVal[i]=(TH1*)hlocal[0]->Clone();
    tmpVal[i]->Scale(0.); // zero the hists 
  }

  if(strstr(name,"DEta") || strstr(name,"SEta"))fixDEta((TH2**)hlocal,32);

    for(int i=0;i<4;i++){
        double r;
        if (0 == hlocal[i+4]->Integral()) {
            r = 0;
        } else {
            r = hlocal[i]->Integral() / hlocal[i+4]->Integral();
        }
        if (subtract) {
            hlocal[i]->Add(hlocal[i+4],-r);
            hlocal[i+8]->Add(hlocal[i+12],-r);
            hlocal[i+16]->Add(hlocal[i+20],-r);
            hlocal[i+24]->Add(hlocal[i+28],-r);
        }
        hlocal[i]->Scale(1.0/nEventsSame);
        hlocal[i+8]->Scale(1.0/nEventsSame);
        hlocal[i+16]->Scale(1.0/nEventsSame);
        hlocal[i+24]->Scale(1.0/nEventsSame);
        hlocal[i+4]->Scale(1.0/nEventsMixed);
        switch(opt){
            case 0: {
                retVal[i]->Add(hlocal[i+8]);
                retVal[i]->Add(hlocal[i+16],-ptHatB);
                retVal[i]->Add(hlocal[i+24],-ptHatA);
                retVal[i]->Add(hlocal[i],ptHatA*ptHatB);
                break;
            } 
            case 1: {
                retVal[i]->Add(hlocal[i+8]);
                break;
            }
            case 2: {
                retVal[i]->Add(hlocal[i+16],-ptHatB);
                retVal[i]->Add(hlocal[i+24],-ptHatA);
                break;
            }
            case 4: {
                retVal[i]->Add(hlocal[i],ptHatA*ptHatB);
                break;
            }
            default: {
                retVal[i]->Add(hlocal[i+8]);
                retVal[i]->Add(hlocal[i+16],-ptHatB);
                retVal[i]->Add(hlocal[i+24],-ptHatA);
                retVal[i]->Add(hlocal[i],ptHatA*ptHatB);
                break;
            }
        }
    }

    retVal[0]->Add(retVal[3]);
    retVal[1]->Add(retVal[2]);
    retVal[2]->Add(retVal[0],retVal[1],1.0,-1.0);
    retVal[3]->Add(retVal[0],retVal[1],1.0,1.0);
    tmpVal[0]->Add(hlocal[4],hlocal[7]);
    tmpVal[1]->Add(hlocal[5],hlocal[6]);
    tmpVal[2]->Add(tmpVal[0],tmpVal[1],1.0,1.0);
    tmpVal[3]->Add(tmpVal[0],tmpVal[1],1.0,1.0);

    for(int i=0;i<4;i++){
        for(int ix=1;ix<=retVal[i]->GetNbinsX();ix++){
            for(int iy=1;iy<=retVal[i]->GetNbinsY();iy++){
                double drho = retVal[i]->GetBinContent(ix, iy); // \Delta\rho
                double mixn = tmpVal[i]->GetBinContent(ix, iy); // \rho_{ref}
                if (mixn <= 0) {
                    mixn = 1;
                }
                retVal[i]->SetBinContent(ix, iy, drho/sqrt(mixn) );
            }
        }
    }


  // Free local histograms.
  for (int i=1;i<4;i++) {
      delete tmpVal[i];
  }
  for(int i=0;i<_pair_totalmax*4;i++) {
    delete hlocal[i];
  }
  delete [] hlocal;

  return retVal;

}

//---------------------------------------------------------
void StEStructSupport::scaleBackGround(TH1* sib, TH1* mix, float sf) {

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
  //  cout<<"Scaling "<<mix->GetName()<<" by "<<alpha<<endl;

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

  if(!mapplyDEtaFix) return;

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
void StEStructSupport::writeAscii(TH1** h, int iHist, const char* fname, int optErrors){

  ofstream of(fname);
   int xbins=h[iHist]->GetNbinsX();
   TAxis * xa= h[iHist]->GetXaxis();
   of<<"# Histogram Name = "<<h[iHist]->GetName()<<endl;
   of<<"# X-axis: min="<<xa->GetBinLowEdge(1)<<" max="<<xa->GetBinUpEdge(xbins)<<" nbins="<<xbins<<endl;
 
   int ybins=h[iHist]->GetNbinsY();
   TAxis * ya= h[iHist]->GetYaxis();
   if(ybins>1){
     of<<"# Y-axis: min="<<ya->GetBinLowEdge(1)<<" max="<<ya->GetBinUpEdge(ybins)<<" nbins="<<ybins<<endl;
     of<<"# ix  iy  Value "<<endl; 
     for(int i=1;i<=xbins;i++){
       for(int j=1;j<=ybins;j++){
         of<<i<<"   "<<j<<"   "<<h[iHist]->GetBinContent(i,j);
         if(optErrors>0)of<<"  "<<h[iHist]->GetBinError(i,j);
         of<<endl;
       }
     }
     of<<endl;
   } else {
     of<<"# ix Value "<<endl;
     for(int i=1;i<=xbins;i++){
         of<<i<<"   "<<h[iHist]->GetBinContent(i);
         if(optErrors>0)of<<"  "<<h[iHist]->GetBinError(i);
         of<<endl;
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
 * Revision 1.15  2007/01/26 17:20:58  msd
 * Updated HAdd for new binning scheme.
 * Improved Support::buildChargeTypes.
 *
 * Revision 1.14  2006/12/14 20:07:11  prindle
 *   I was calculating \Delta\rho/sqrt(rho) for ++, +-, -+ and --
 * and then combining those into LS, US, CD and CI. The was wrong
 * and now I am doing it correctly. For CI this makes only a slight
 * change, it seems the amplitude is decreased a little. For CD
 * this is a bigger change. I left the old versions (with _Old appended)
 * for now.
 *
<<<<<<< StEStructSupport.cxx
=======
 * Revision 1.13  2006/10/27 00:05:32  prindle
 *   Modified buildChargeTypes to handle case where the -+ histogram is
 * empty. Also tried making buildCommonTypes work, but there one of the
 * output histograms was intended to be -+ and most of the time that
 * will be empty.
 *
>>>>>>> 1.13
 * Revision 1.11  2006/04/26 18:52:12  dkettler
 *
 * Added reaction plane determination for the analysis
 *
 * Added reaction plane angle calculation
 *
 * Case 3 in buildPtChargeTypes needs to be corrected
 *
 * Revision 1.10  2006/04/25 21:04:39  msd
 * Added Jeff's patch for getHists to create doubles instead of floats
 *
 * Revision 1.9  2006/04/06 01:09:49  prindle
 *   Calculating pt for each cut bin caused changes in HAdd.
 * The splitting of +- into +- and -+ caused changes in Support.
 *
 * Revision 1.8  2006/04/04 22:14:10  porter
 * fixdeta is now NOT default but included in StEStruct2ptCorrelations
 *
 * Revision 1.7  2005/09/14 17:25:37  msd
 * minor tweak
 *
 * Revision 1.6  2005/09/07 20:26:16  prindle
 *
 *
 *     Support: Fixed some meory leaks.
 *
 * Revision 1.4  2005/03/08 21:56:42  porter
 * fixed bug in StEStructHAdd.cxx and added diagnostic option in ptcorrelations to
 * view individual terms separately
 *
 * Revision 1.3  2005/03/08 20:16:34  msd
 * included <sstream> library
 *
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



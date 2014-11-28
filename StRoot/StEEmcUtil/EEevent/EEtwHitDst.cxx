#include <cassert>
#include "EEtwHitDst.h"

ClassImp(EEtwHitDst);

//---------------------------------
//---------------------------------
//---------------------------------
EEtwHitDst::EEtwHitDst(){mEnergy=-1; mEta=-1;};

//---------------------------------
//---------------------------------
//---------------------------------
EEtwHitDst::~EEtwHitDst() {};


//---------------------------------
//---------------------------------
//---------------------------------
void EEtwHitDst::get(char &sub, int &eta, Float_t &ener) {
  sub=mSubSec;
  eta=mEta;
  ener=mEnergy;
}

//---------------------------------
//---------------------------------
//---------------------------------
void EEtwHitDst::set(char sub, int eta, Float_t ener) {
  //  printf("add sub/int=%d eta=%d ener=%f\n",sub,eta,ener); 
  mSubSec=sub; assert(mSubSec>='A' && mSubSec<='E');
  mEta=eta; assert(mEta>=1 && mEta<=12);
  mEnergy=ener;
}

//---------------------------------
//---------------------------------
//---------------------------------
void EEtwHitDst::print(){
  printf("sub=%c etaBin=%d Energy=%f \n",mSubSec,mEta,mEnergy);
}








#include "StFwdDataFcs.h"

ClassImp(StFcsPhotonCandidate)

StFcsPhotonCandidate::StFcsPhotonCandidate()
{
  for( int i=0; i<5; ++i ){
    mEpdHitNmip[i] = -1;
    mEpdMatch[i] = 0;
  }
}

StFcsPhotonCandidate::~StFcsPhotonCandidate()
{}

void StFcsPhotonCandidate::ConvertEpdKeyToPpTt(Short_t key, Short_t &pp, Short_t &tt)
{
  pp = key/100;
  tt = key-100*pp;
}

TLorentzVector StFcsPhotonCandidate::lvRaw()
{
  TLorentzVector v;
  v.SetPxPyPzE(mPxRaw,mPyRaw,mPzRaw,mEn);
  return v;
}

TLorentzVector StFcsPhotonCandidate::lvVert()
{
  TLorentzVector v;
  v.SetPxPyPzE(mPxVert,mPyVert,mPzVert,mEn);
  return v;
}

Float_t StFcsPhotonCandidate::magPosition()
{ return sqrt( mX*mX + mY*mY + mZ*mZ ); }

Bool_t StFcsPhotonCandidate::IsEqual(const TObject* obj) const
{
  StFcsPhotonCandidate* other = (StFcsPhotonCandidate*) obj;
  if( obj==0 ){ return kFALSE; }
  //else{ return (this->mInvMass==other->mInvMass); }
  else{
    if( this->mFromCluster!=other->mFromCluster ){ return kFALSE; }
    else{ return ( this->mEn==other->mEn ); }
  }
}

Int_t StFcsPhotonCandidate::Compare(const TObject* obj) const
{
  StFcsPhotonCandidate* other = 0;
  other = (StFcsPhotonCandidate*)obj;
  if( other==0 ){ std::cout << "ERROR - Not an StFcsPairCandidate" << std::endl; return 0; }
  if( this->mFromCluster==other->mFromCluster ){
    //if( this->mFromCluster < other->mFromCluster ){ return -1; }
    //if( this->mFromCluster==other->mFromCluster ){
    if(      this->mEn < other->mEn ){ return  1; } //This way TClonesArray sorts in descending order
    else if( this->mEn > other->mEn ){ return -1; } //This way TClonesArray sorts in descending order
    else{ return 0; }
  }
  else{
    return 1;
    //if( this->mFromCluster==false ){ return 1; } //points are greater than clusters
    //else{ this->mFromCluster==true ){ return -1; }
  }
}

void StFcsPhotonCandidate::Copy(TObject& object) const
{
  ((StFcsPhotonCandidate&)object).mFromCluster = mFromCluster;
  ((StFcsPhotonCandidate&)object).mDetId = mDetId;
  ((StFcsPhotonCandidate&)object).mX = mX;
  ((StFcsPhotonCandidate&)object).mY = mY;
  ((StFcsPhotonCandidate&)object).mZ = mZ;
  
  ((StFcsPhotonCandidate&)object).mEn = mEn;
  ((StFcsPhotonCandidate&)object).mPxRaw = mPxRaw;
  ((StFcsPhotonCandidate&)object).mPyRaw = mPyRaw;
  ((StFcsPhotonCandidate&)object).mPzRaw = mPzRaw;
  
  ((StFcsPhotonCandidate&)object).mPxVert = mPxVert;
  ((StFcsPhotonCandidate&)object).mPyVert = mPyVert;
  ((StFcsPhotonCandidate&)object).mPzVert = mPzVert;

  ((StFcsPhotonCandidate&)object).mEpdFoundRegion = mEpdFoundRegion;
  for( int i=0; i<5; ++i ){
    (((StFcsPhotonCandidate&)object)).mEpdHitNmip[i] = mEpdHitNmip[i];
    (((StFcsPhotonCandidate&)object)).mEpdMatch[i] = mEpdMatch[i];
  }

  ((StFcsPhotonCandidate&)object).mEpdHitNmipSum = mEpdHitNmipSum;
  ((StFcsPhotonCandidate&)object).mEpdHitAdjMax = mEpdHitAdjMax;
}


void StFcsPhotonCandidate::Clear(Option_t* opt)
{
  //std::cout <<"=====|StFcsPhotonCandidate::Clear()::DEEPDEBUG" << std::endl;
  mFromCluster = false;
  mDetId = -1;
  mX = 0;
  mY = 0;
  mZ = 0;
  
  mEn = 0;
  mPxRaw = 0;
  mPyRaw = 0;
  mPzRaw = 0;
  
  mPxVert = 0;
  mPyVert = 0;
  mPzVert = 0;

  mEpdFoundRegion = -2;
  for( int i=0; i<5; ++i ){
    mEpdHitNmip[i] = -1;
    mEpdMatch[i] = 0;

    //std::cout << "=====|StFcsPhotonCandidate::Clear()::DEEPDEBUG||hitnmip:"<<mEpdHitNmip[i] << "|hitmatch:"<<mEpdMatch[i] << std::endl;
  }

  mEpdHitNmipSum = 0;
  mEpdHitAdjMax = 0;
}

void StFcsPhotonCandidate::Print(Option_t* opt) const
{
  TString option(opt);
  option.ToLower();
  std::cout << "|Clus:"<<mFromCluster << "|Pos:("<<mX<<","<<mY<<","<<mZ<<")|En:"<<mEn << "|PRaw:"<<mPxRaw<<","<<mPyRaw<<","<<mPzRaw<<","<<"|PVert:"<<mPxVert<<","<<mPyVert<<","<<mPzVert << std::endl;  
  if( option.Contains("epd") ){
    std::cout << "  - |FoundRegion:"<<mEpdFoundRegion;
    for( short i=0; i<5; ++i ){
      if( mEpdMatch[i]!=0 ){
	std::cout << "|i:"<<i << "|key:"<<mEpdMatch[i] << "|nmip:"<<mEpdHitNmip[i];
      }
    }
    std::cout << std::endl << "     - |Sum3x3:"<<mEpdHitNmipSum << "|Max3x3:"<<mEpdHitAdjMax /*<< "|RedMip:"<<RedMip() << "|RedMax:"<<RedMax()*/ << std::endl;
  }
}

ClassImp(StFcsPairCandidate)

StFcsPairCandidate::StFcsPairCandidate()
{}

StFcsPairCandidate::~StFcsPairCandidate()
{}

TLorentzVector StFcsPairCandidate::lv()
{
  TLorentzVector v;
  v.SetPxPyPzE(mPx,mPy,mPz,mEn);
  return v;
}

Float_t StFcsPairCandidate::eta() const
{ if( mEta<0 ){ return asinh(mPz/pt()); }else{ return mEta; } }

Float_t StFcsPairCandidate::phi() const
{ return atan2(mPy,mPx); }

Float_t StFcsPairCandidate::pt() const
{ return sqrt( mPx*mPx + mPy*mPy ); }

Float_t StFcsPairCandidate::ptot() const
{ return sqrt( mPx*mPx + mPy*mPy + mPz*mPz ); }

Float_t StFcsPairCandidate::theta() const
{ return 2.0*atan(exp(-1.0*eta())); }

Float_t StFcsPairCandidate::mass() const
{ return sqrt(mEn*mEn - (mPx*mPx + mPy*mPy + mPz*mPz)); }

Bool_t StFcsPairCandidate::IsEqual(const TObject* obj) const
{
  StFcsPairCandidate* other = (StFcsPairCandidate*) obj;
  if( obj==0 ){ return kFALSE; }
  //else{ return (this->mInvMass==other->mInvMass); }
  else{ return ( fabs(this->mInvMass-Pi0Mass())==fabs(other->mInvMass-Pi0Mass()) ); }
}

Int_t StFcsPairCandidate::Compare(const TObject* obj) const
{
  StFcsPairCandidate* other = 0;
  other = (StFcsPairCandidate*)obj;
  if( other==0 ){ std::cout << "ERROR - Not an StFcsPairCandidate" << std::endl; return 0; }
  if(      fabs(this->mInvMass-Pi0Mass()) < fabs(other->mInvMass-Pi0Mass()) ){ return -1; }
  else if( fabs(this->mInvMass-Pi0Mass()) > fabs(other->mInvMass-Pi0Mass()) ){ return  1; }
  //if(      (this->mEn) < (other->mEn) ){ return -1; }
  //else if( (this->mEn) > (other->mEn) ){ return  1; }
  else { return 0; }
}

void StFcsPairCandidate::Clear(Option_t* opt)
{
  mFromCluster = false;
  mFromPh = -1;
  mPhoton1Idx = -1;
  mPhoton2Idx = -1;
  mPx = 0;
  mPy = 0;
  mPz = 0;
  mEn = 0;  
  mEta = -1;
  mDgg = 0;
  mZgg = 0;
  mAlpha = 0;
  mInvMass = -1;  
}

void StFcsPairCandidate::Print(Option_t* opt) const
{
  std::cout << "|Clus:"<<mFromCluster <<"|PhCut:"<<mFromPh << "|Ph1Idx:"<<mPhoton1Idx << "|Ph2Idx:"<<mPhoton2Idx << "|En:"<<mEn << "|P:("<<mPx<<","<<mPy<<","<<mPz<<")|Pt:"<<pt()<<"|Eta:"<<mEta << "|Dgg:"<<mDgg << "|Zgg:"<<mZgg << "|Alpha:"<<mAlpha << "|InvMass:"<<mInvMass << std::endl;
}

Float_t StFcsPairCandidate::zgg(StFcsPhotonCandidate& ph1, StFcsPhotonCandidate& ph2)
{return fabs(ph1.mEn-ph2.mEn)/(ph1.mEn+ph2.mEn);}

Float_t StFcsPairCandidate::dgg(StFcsPhotonCandidate& ph1, StFcsPhotonCandidate& ph2)
{ return sqrt( (ph1.mX-ph2.mX)*(ph1.mX-ph2.mX) + (ph1.mY-ph2.mY)*(ph1.mY-ph2.mY) + (ph1.mZ-ph2.mZ)*(ph1.mZ-ph2.mZ) ); }

Float_t StFcsPairCandidate::alpha(StFcsPhotonCandidate& ph1, StFcsPhotonCandidate& ph2)
{
  Float_t ph1dotph2 = ph1.mX*ph2.mX + ph1.mY*ph2.mY + ph1.mZ*ph2.mZ; //dot product of vectors for the current cluster position and cluster j position
  Float_t ph1mag = ph1.magPosition(); //magnitude of position vector for candidate 1
  Float_t ph2mag = ph2.magPosition(); //magnitude of position vector for candidate 2
  return acos( ph1dotph2 / (ph1mag*ph2mag) );
}

void StFcsPairCandidate::DiscriminateCharge(TClonesArray* photonarr, Double_t epdnmipcut)
{
  TString photonarr_classname(photonarr->GetClass()->GetName());
  if( !photonarr_classname.EqualTo("StFcsPhotonCandidate") ){ return; }
  StFcsPhotonCandidate* ph1 = (StFcsPhotonCandidate*)photonarr->At(mPhoton1Idx);
  StFcsPhotonCandidate* ph2 = (StFcsPhotonCandidate*)photonarr->At(mPhoton2Idx);
  if( ph1==0 || ph2==0 ){ return; }
  //Only include candidates who have their nmip value set
  if( ph1->mEpdMatch[0]==0 || ph2->mEpdMatch[0]==0 ){ return; }
  //These if statements should be mutually exclusive but just in case give a -2 in case neither is satisfied
  if( ph1->mEpdHitNmip[0]<=epdnmipcut && ph2->mEpdHitNmip[0]<=epdnmipcut )     { mFromPh = 0; }
  else if( ph1->mEpdHitNmip[0]<=epdnmipcut && ph2->mEpdHitNmip[0]>epdnmipcut  ){ mFromPh = 1; }
  else if( ph1->mEpdHitNmip[0]>epdnmipcut  && ph2->mEpdHitNmip[0]<=epdnmipcut ){ mFromPh = 2; }
  else if( ph1->mEpdHitNmip[0]>epdnmipcut  && ph2->mEpdHitNmip[0]>epdnmipcut  ){ mFromPh = 3; }
  else{ mFromPh = -2; }
}



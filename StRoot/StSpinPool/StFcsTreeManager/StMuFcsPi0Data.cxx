#include "StMuFcsPi0Data.h"

ClassImp(FcsEventInfo);

FcsEventInfo::FcsEventInfo()
{}

FcsEventInfo::~FcsEventInfo()
{}

/*Not sure if needed below code is not right
Double_t FcsEventInfo::EpdTacDiffEarly()const
{
  if( mEpdEarlyW>50 && mEpdEarlyE>0 ){ return mEpdEarlyW-mEpdEarlyE;}
  else{ return -999; }
}
Double_t FcsEventInfo::EpdTacDiffAvg()const{ return mEpdAvgW-mEpdAvgE; }  
*/

Short_t FcsEventInfo::BlueSpin(Int_t spinbit)
{
  if( spinbit==10 || spinbit==9 ){ return 1; }
  else if( spinbit==6 || spinbit==5 ){ return -1; }
  else{ return 0; }
}

Short_t FcsEventInfo::YellowSpin(Int_t spinbit)
{
  if( spinbit==10 || spinbit==6 ){ return 1; }
  else if( spinbit==9 || spinbit==5 ){ return -1; }
  else{ return 0; }
}

void FcsEventInfo::Clear(Option_t* option)
{
  mRunTime = -1;
  mRunNum = -1;
  mFill = 0;
  mEvent = -1;
  mBx48Id = -1;
  mBx7Id = -1;
  mSpin = 0;

  mTofMultiplicity = -1;

  mPrimVertRanking = -1;
  mPrimVx = -999;
  mPrimVy = -999;
  mPrimVz = -999;
  mVpdVz = -999;
  mBbcVz = -999;
  mBbcTacDiff = 0;
  mEpdTacEarlyW = 0;
  mEpdTacEarlyE = 0;
  mEpdAvgW = 0;
  mEpdAvgE = 0;
  mEpdVz = -999;
  mZdcVz = -999;
  mFoundVertex = 0;

  mClusterSize = 0;
}

void FcsEventInfo::Print(Option_t* option) const
{
  std::cout <<"## FcsEventInfo|ClusterSize:"<<mClusterSize << std::endl;
  std::cout << " + |RunTime:"<<mRunTime << "|RunNum:"<<mRunNum << "|mFill:"<<mFill << "|mEvent:"<<mEvent << "|Bx48Id:"<<mBx48Id << "|Bx7Id:"<<mBx7Id << "|mSpin:"<<mSpin << "|mTofMult:"<<mTofMultiplicity << std::endl;

  std::cout << " + |VertexBit:"<<mFoundVertex << "|VpdVz:"<<mVpdVz << "|ZdcVz:"<<mZdcVz << "|BbcTDiff:"<<mBbcTacDiff << "|BbcVz:"<<mBbcVz << std::endl;
  std::cout << " + |EpdTacEarlyW:"<<mEpdTacEarlyW << "|EpdTacEarlyE:"<<mEpdTacEarlyE << "|EpdTacAvgW:"<<mEpdAvgW << "|EpdTacAvgE:"<<mEpdAvgE << "|EpdVz:"<<mEpdVz << std::endl;
}

ClassImp(FcsPhotonCandidate)

FcsPhotonCandidate::FcsPhotonCandidate()
{
  for( int i=0; i<5; ++i ){
    mEpdHitNmip[i] = -1;
    mEpdMatch[i] = 0;
  }
}

FcsPhotonCandidate::~FcsPhotonCandidate()
{}

void FcsPhotonCandidate::ConvertEpdKeyToPpTt(Short_t key, Short_t &pp, Short_t &tt)
{
  pp = key/100;
  tt = key-100*pp;
}

TLorentzVector FcsPhotonCandidate::lvRaw()
{
  TLorentzVector v;
  v.SetPxPyPzE(mPxRaw,mPyRaw,mPzRaw,mEn);
  return v;
}

TLorentzVector FcsPhotonCandidate::lvVert()
{
  TLorentzVector v;
  v.SetPxPyPzE(mPxVert,mPyVert,mPzVert,mEn);
  return v;
}

Float_t FcsPhotonCandidate::magPosition()
{ return sqrt( mX*mX + mY*mY + mZ*mZ ); }

Bool_t FcsPhotonCandidate::IsEqual(const TObject* obj) const
{
  FcsPhotonCandidate* other = (FcsPhotonCandidate*) obj;
  if( obj==0 ){ return kFALSE; }
  //else{ return (this->mInvMass==other->mInvMass); }
  else{
    if( this->mFromCluster!=other->mFromCluster ){ return kFALSE; }
    else{ return ( this->mEn==other->mEn ); }
  }
}

Int_t FcsPhotonCandidate::Compare(const TObject* obj) const
{
  FcsPhotonCandidate* other = 0;
  other = (FcsPhotonCandidate*)obj;
  if( other==0 ){ std::cout << "ERROR - Not an FcsPairCandidate" << std::endl; return 0; }
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

void FcsPhotonCandidate::Copy(TObject& object) const
{
  ((FcsPhotonCandidate&)object).mFromCluster = mFromCluster;
  ((FcsPhotonCandidate&)object).mDetId = mDetId;
  ((FcsPhotonCandidate&)object).mX = mX;
  ((FcsPhotonCandidate&)object).mY = mY;
  ((FcsPhotonCandidate&)object).mZ = mZ;
  
  ((FcsPhotonCandidate&)object).mEn = mEn;
  ((FcsPhotonCandidate&)object).mPxRaw = mPxRaw;
  ((FcsPhotonCandidate&)object).mPyRaw = mPyRaw;
  ((FcsPhotonCandidate&)object).mPzRaw = mPzRaw;
  
  ((FcsPhotonCandidate&)object).mPxVert = mPxVert;
  ((FcsPhotonCandidate&)object).mPyVert = mPyVert;
  ((FcsPhotonCandidate&)object).mPzVert = mPzVert;

  for( int i=0; i<5; ++i ){
    (((FcsPhotonCandidate&)object)).mEpdHitNmip[i] = mEpdHitNmip[i];
    (((FcsPhotonCandidate&)object)).mEpdMatch[i] = mEpdMatch[i];
  }

  ((FcsPhotonCandidate&)object).mEpdHitNmipSum = mEpdHitNmipSum;
  ((FcsPhotonCandidate&)object).mEpdHitAdjMax = mEpdHitAdjMax;
}


void FcsPhotonCandidate::Clear(Option_t* opt)
{
  //std::cout <<"=====|FcsPhotonCandidate::Clear()::DEEPDEBUG" << std::endl;
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
  for( int i=0; i<5; ++i ){
    mEpdHitNmip[i] = -1;
    mEpdMatch[i] = 0;

    //std::cout << "=====|FcsPhotonCandidate::Clear()::DEEPDEBUG||hitnmip:"<<mEpdHitNmip[i] << "|hitmatch:"<<mEpdMatch[i] << std::endl;
  }

  mEpdHitNmipSum = 0;
  mEpdHitAdjMax = 0;
}

void FcsPhotonCandidate::Print(Option_t* opt) const
{
  TString option(opt);
  option.ToLower();
  std::cout << "|Clus:"<<mFromCluster << "|Pos:("<<mX<<","<<mY<<","<<mZ<<")|En:"<<mEn << "|PRaw:"<<mPxRaw<<","<<mPyRaw<<","<<mPzRaw<<","<<"|PVert:"<<mPxVert<<","<<mPyVert<<","<<mPzVert << std::endl;  
  if( option.Contains("epd") ){
    std::cout << "  - ";
    for( short i=0; i<5; ++i ){
      if( mEpdMatch[i]!=0 ){
	std::cout << "|i:"<<i << "|key:"<<mEpdMatch[i] << "|nmip:"<<mEpdHitNmip[i];
      }
    }
    std::cout << std::endl << "     - |Sum:"<<mEpdHitNmipSum << "|Max:"<<mEpdHitAdjMax << "|RedMip:"<<RedMip() << "|RedMax:"<<RedMax() << std::endl;
  }
}

ClassImp(FcsPairCandidate)

FcsPairCandidate::FcsPairCandidate()
{}

FcsPairCandidate::~FcsPairCandidate()
{}

TLorentzVector FcsPairCandidate::lv()
{
  TLorentzVector v;
  v.SetPxPyPzE(mPx,mPy,mPz,mEn);
  return v;
}

Float_t FcsPairCandidate::eta() const
{ if( mEta<0 ){ return asinh(mPz/pt()); }else{ return mEta; } }

Float_t FcsPairCandidate::phi() const
{ return atan2(mPy,mPx); }

Float_t FcsPairCandidate::pt() const
{ return sqrt( mPx*mPx + mPy*mPy ); }

Float_t FcsPairCandidate::ptot() const
{ return sqrt( mPx*mPx + mPy*mPy + mPz*mPz ); }

Float_t FcsPairCandidate::theta() const
{ return 2.0*atan(exp(-1.0*eta())); }

Float_t FcsPairCandidate::mass() const
{ return sqrt(mEn*mEn - (mPx*mPx + mPy*mPy + mPz*mPz)); }

Bool_t FcsPairCandidate::IsEqual(const TObject* obj) const
{
  FcsPairCandidate* other = (FcsPairCandidate*) obj;
  if( obj==0 ){ return kFALSE; }
  //else{ return (this->mInvMass==other->mInvMass); }
  else{ return ( fabs(this->mInvMass-Pi0Mass())==fabs(other->mInvMass-Pi0Mass()) ); }
}

Int_t FcsPairCandidate::Compare(const TObject* obj) const
{
  FcsPairCandidate* other = 0;
  other = (FcsPairCandidate*)obj;
  if( other==0 ){ std::cout << "ERROR - Not an FcsPairCandidate" << std::endl; return 0; }
  if(      fabs(this->mInvMass-Pi0Mass()) < fabs(other->mInvMass-Pi0Mass()) ){ return -1; }
  else if( fabs(this->mInvMass-Pi0Mass()) > fabs(other->mInvMass-Pi0Mass()) ){ return  1; }
  //if(      (this->mEn) < (other->mEn) ){ return -1; }
  //else if( (this->mEn) > (other->mEn) ){ return  1; }
  else { return 0; }
}

void FcsPairCandidate::Clear(Option_t* opt)
{
  mFromCluster = false;
  mFromPh = 0;
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

void FcsPairCandidate::Print(Option_t* opt) const
{
  std::cout << "|Clus:"<<mFromCluster <<"|PhCut:"<<mFromPh << "|Ph1Idx:"<<mPhoton1Idx << "|Ph2Idx:"<<mPhoton2Idx << "|En:"<<mEn << "|P:("<<mPx<<","<<mPy<<","<<mPz<<")|Pt:"<<pt()<<"|Eta:"<<mEta << "|Dgg:"<<mDgg << "|Zgg:"<<mZgg << "|Alpha:"<<mAlpha << "|InvMass:"<<mInvMass << std::endl;
}

Float_t FcsPairCandidate::zgg(FcsPhotonCandidate& ph1, FcsPhotonCandidate& ph2)
{return fabs(ph1.mEn-ph2.mEn)/(ph1.mEn+ph2.mEn);}

Float_t FcsPairCandidate::dgg(FcsPhotonCandidate& ph1, FcsPhotonCandidate& ph2)
{ return sqrt( (ph1.mX-ph2.mX)*(ph1.mX-ph2.mX) + (ph1.mY-ph2.mY)*(ph1.mY-ph2.mY) + (ph1.mZ-ph2.mZ)*(ph1.mZ-ph2.mZ) ); }

Float_t FcsPairCandidate::alpha(FcsPhotonCandidate& ph1, FcsPhotonCandidate& ph2)
{
  Float_t ph1dotph2 = ph1.mX*ph2.mX + ph1.mY*ph2.mY + ph1.mZ*ph2.mZ; //dot product of vectors for the current cluster position and cluster j position
  Float_t ph1mag = ph1.magPosition(); //magnitude of position vector for candidate 1
  Float_t ph2mag = ph2.magPosition(); //magnitude of position vector for candidate 2
  return acos( ph1dotph2 / (ph1mag*ph2mag) );
}



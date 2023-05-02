#include "StRHICfRecoEnergy.h"

StRHICfRecoEnergy::StRHICfRecoEnergy()
{
  init();
}

StRHICfRecoEnergy::~StRHICfRecoEnergy()
{
  for(int it=0; it<kRHICfNtower; it++){
    delete mLeakOutTableNeutron[it];
    mLeakOutTableNeutron[it] = nullptr;
    for(int ip=0; ip<kRHICfNplate; ip++){
      delete mLeakInTablePhoton[it][ip];
      delete mLeakOutTablePhoton[it][ip];
      mLeakInTablePhoton[it][ip] = nullptr;
      mLeakOutTablePhoton[it][ip] = nullptr;
    }
  }
}

void StRHICfRecoEnergy::init()
{
  mRunType = 999;
  std::fill(mRecoHitNum, mRecoHitNum+kRHICfNtower, -999);

  memset(mPlateE, 0, sizeof(mPlateE));
  memset(mPlateECorr, 0, sizeof(mPlateECorr));
  memset(mOverlap, 0, sizeof(mOverlap));
  memset(mRecoHitPos, 0, sizeof(mRecoHitPos));
  memset(mSumEnergy, 0, sizeof(mSumEnergy));
  memset(mResultEnergy, 0, sizeof(mResultEnergy));
  memset(mEnergyRatio, 0, sizeof(mEnergyRatio));
  memset(mMultiHitPos, 0, sizeof(mMultiHitPos));
  memset(mMultiPeakHeight, 0, sizeof(mMultiPeakHeight));
}

bool StRHICfRecoEnergy::calculate()
{
  for(int it=0; it<kRHICfNtower; it++){
    mSumEnergy[it][0] = getSumEnergy(it, 0, 15, mPlateE[it]);
    mSumEnergy[it][1] = getSumEnergy(it, 1, 11, mPlateE[it]);
    double mParticleEnergy = 0.;
    recoPhotonEnergySimple(it, mParticleEnergy);

    if(mParticleEnergy < mEnergyThreshold){setResultHitNum(it, 0);}  
  }

  // Type-I pi0.
  if(getResultHitNum(0)>0 && getResultHitNum(1)>0){
    recoPhotonEnergyDouble();

    for(int it=0; it<2; it++){
      if(mResultEnergy[it][kRHICfPhoton] < mEnergyThreshold){setResultHitNum(it, 0);}
    }
  }

  // Single photon and Type-II pi0.
	if(getResultHitNum(0)>0 && getResultHitNum(1)==0){  // for TS tower
		correctLightYield(); 
    recoPhotonEnergySingle(0);
	}
  if(getResultHitNum(0)==0 && getResultHitNum(1)>0){  // for TL tower
		correctLightYield();
		recoPhotonEnergySingle(1);
	}

  // Hadron
  for(int it=0; it<kRHICfNtower; it++){
		if(getResultHitNum(it) == 0){continue;}
		recoHadronEnergy(it);
	}

  std::cout << "StRHICfRecoEnergy::calculate() -- Done" << std::endl;
  return kRHICfOk;
}

void StRHICfRecoEnergy::setRunType(int runType){mRunType = runType;}

void StRHICfRecoEnergy::setPlateEnergy(int tower, int layer, double val){mPlateE[tower][layer] = val;}

void StRHICfRecoEnergy::setResultHitPos(int tower, int xy, double val){mRecoHitPos[tower][xy] = val;}

void StRHICfRecoEnergy::setResultHitNum(int tower, int val){mRecoHitNum[tower] = val;}

void StRHICfRecoEnergy::setMultiHitPos(int tower, int layer, int xy, int order, double val){mMultiHitPos[tower][layer][xy][order] = val;}

void StRHICfRecoEnergy::setMultiPeakHeight(int tower, int layer, int xy, int order, double val){mMultiPeakHeight[tower][layer][xy][order] = val;}

void StRHICfRecoEnergy::setOverlap(int tower, int xy, bool val){mOverlap[tower][xy] = val;}

void StRHICfRecoEnergy::setLeakageInTable(int tower, int layer, TH2D* table){mLeakInTablePhoton[tower][layer] = table;}

void StRHICfRecoEnergy::setLeakageOutTablePhoton(int tower, int layer, TH2D* table){mLeakOutTablePhoton[tower][layer] = table;}

void StRHICfRecoEnergy::setLeakageOutTableNeutron(int tower, TH2D* table){mLeakOutTableNeutron[tower] = table;}

int StRHICfRecoEnergy::getResultHitNum(int tower){return mRecoHitNum[tower];}

double StRHICfRecoEnergy::getPlateSumEnergy(int tower, bool all){
  if(all){return mSumEnergy[tower][0];}
  else{return mSumEnergy[tower][1];}
  return 0.;
}

double StRHICfRecoEnergy::getResultEnergy(int tower, int particle){return mResultEnergy[tower][particle];}

double StRHICfRecoEnergy::getEnergyRatio(int tower, int order){return mEnergyRatio[tower][order];}

void StRHICfRecoEnergy::recoPhotonEnergySimple(int tower, double& energy)
{
  double posX = mRecoHitPos[tower][0]; 
  double posY = mRecoHitPos[tower][1]; 
  double caliSumEnergy = 0.;

  for(int ip=1; ip<=11; ip++){
    double efficiency = getLeakageOutPhoton(tower, ip, posX, posY);   
    if(efficiency<0.1 || efficiency>2.){

      double gsobarSize = checkGSOBarSize(tower);
      double edgeSize = (ip<12 ? 1.0 : 2.0);

      if(posX < edgeSize){posX = edgeSize;}          
      if(posX > gsobarSize-edgeSize){posX = gsobarSize-edgeSize;}
      if(posY < edgeSize){posY = edgeSize;}
      if(posY > gsobarSize-edgeSize){posY = gsobarSize-edgeSize;}

      efficiency = getLeakageOutPhoton(tower, ip, posX, posY);   
    }
    caliSumEnergy += mPlateE[tower][ip]/efficiency; 
  }
  energy = getPhotonEnergyConvert(tower, caliSumEnergy);
}

void StRHICfRecoEnergy::recoPhotonEnergySingle(int tower)
{
  for(int it=0; it<kRHICfNtower; it++){
    if(it==tower){
      mSumEnergy[it][0] = getSumEnergy(it, 0, 15, mPlateECorr[it]);
      mSumEnergy[it][1] = getSumEnergy(it, 1, 11, mPlateECorr[it]);
      mResultEnergy[it][kRHICfPhoton] = getPhotonEnergyConvert(it, mSumEnergy[it][1]);
    }
    else{
      mSumEnergy[it][0] = getSumEnergy(it, 0, 15, mPlateE[it]);
      mSumEnergy[it][1] = 0.;
      mResultEnergy[it][kRHICfPhoton] = getPhotonEnergyConvert(it, mSumEnergy[it][1]);
    }
  }
}

void StRHICfRecoEnergy::recoPhotonEnergyDouble()
{
  double mLeakInEffi[kRHICfNtower][kRHICfNplate];
  double mLeakOutEffi[kRHICfNtower][kRHICfNplate];

  memset(mLeakInEffi, 0, sizeof(mLeakInEffi));
  memset(mLeakOutEffi, 0, sizeof(mLeakOutEffi));

  for(int it=0; it<kRHICfNtower; it++){
    double posX = mRecoHitPos[it][0]; 
    double posY = mRecoHitPos[it][1]; 
    double gsobarSize = checkGSOBarSize(it);
    
    if(posX < 0.){posX = 0.1;}
    if(posY < 0.){posY = 0.1;}
    if(posX > gsobarSize){posX = gsobarSize-0.1;}
    if(posY > gsobarSize){posY = gsobarSize-0.1;}

    for(int ip=0; ip<kRHICfNplate; ip++){
      mLeakOutEffi[it][ip] = getLeakageOutPhoton(it, ip, posX, posY);
      mLeakInEffi[it][ip] = getLeakageInPhoton(it, ip,  posX, posY);

      if(ip >= 11){mLeakOutEffi[it][ip] = mLeakOutEffi[it][10];}
      if(ip >= 9){mLeakInEffi[it][ip] = mLeakInEffi[it][8];}
    }
  }

  for(int ip=0; ip<kRHICfNplate; ip++){
    double leakInFactor = mLeakInEffi[1][ip] * mLeakInEffi[0][ip] - 1.;
    mPlateECorr[0][ip] = ((mLeakInEffi[1][ip] * mPlateE[1][ip] - mPlateE[0][ip]) / leakInFactor) / mLeakOutEffi[0][ip];		
    mPlateECorr[1][ip] = ((mLeakInEffi[0][ip] * mPlateE[0][ip] - mPlateE[1][ip]) / leakInFactor) / mLeakOutEffi[1][ip];
  }

  for(int it=0; it<kRHICfNtower; it++){
    mSumEnergy[it][0] = getSumEnergy(it, 0, 15, mPlateECorr[it]);
    mSumEnergy[it][1] = getSumEnergy(it, 1, 11, mPlateECorr[it]);
    mResultEnergy[it][kRHICfPhoton] = getPhotonEnergyConvert(it, mSumEnergy[it][1]);
  }
}

void StRHICfRecoEnergy::recoHadronEnergy(int tower)
{
  double posX = mRecoHitPos[tower][0]; 
  double posY = mRecoHitPos[tower][1]; 
	double efficiency = getLeakageOutNeutron(tower, posX, posY);
	double sumEnergy = getSumEnergy(tower, 0, 15, mPlateE[tower]);
  double sumECorr = sumEnergy/efficiency;
	mResultEnergy[tower][kRHICfHadron] = getHadronEnergyConvert(tower, sumECorr); 
}

void StRHICfRecoEnergy::correctLightYield()
{
  for(int it=0; it<kRHICfNtower; it++){
    // single photon
    if(getResultHitNum(it)==1){
      double posX = mRecoHitPos[it][0]; 
      double posY = mRecoHitPos[it][1]; 
      getCalibrationEnergy(it, posX, posY);
    }

    // Type-II pi0.
    if(getResultHitNum(it) > 1){
      double posX1 = mMultiHitPos[it][0][0][0];
      double posY1 = mMultiHitPos[it][0][1][0];
      double posX2 = mMultiHitPos[it][0][0][1];
      double posY2 = mMultiHitPos[it][0][1][1];
      double firstRatio = 0.;
      double secondRatio = 0.;
      
      if(!mOverlap[it][0]){firstRatio += mMultiPeakHeight[it][0][0][0];} 
      if(!mOverlap[it][1]){firstRatio += mMultiPeakHeight[it][0][1][0];} 
      if(!mOverlap[it][0]){firstRatio += mMultiPeakHeight[it][1][0][0];} 
      if(!mOverlap[it][1]){firstRatio += mMultiPeakHeight[it][1][1][0];} 
      if(!mOverlap[it][0]){secondRatio += mMultiPeakHeight[it][0][0][1];}
      if(!mOverlap[it][1]){secondRatio += mMultiPeakHeight[it][0][1][1];}
      if(!mOverlap[it][0]){secondRatio += mMultiPeakHeight[it][1][0][1];}
      if(!mOverlap[it][1]){secondRatio += mMultiPeakHeight[it][1][1][1];}

      mEnergyRatio[it][0] = firstRatio/(firstRatio + secondRatio);
      mEnergyRatio[it][1] = 1. - mEnergyRatio[it][0];

      getCalibrationEnergy(it, posX1, posY1, posX2, posY2, mEnergyRatio[it][0], mEnergyRatio[it][1]);
    }
  }
}

void StRHICfRecoEnergy::getCalibrationEnergy(int tower, double posX1, double posY1, double posX2, double posY2, double firstRatio, double secondRatio)
{
  for(int ip=0; ip<kRHICfNplate; ip++){
		double efficiency1 = getLeakageOutPhoton(tower, ip, posX1, posY1);
    double efficiency2 = getLeakageOutPhoton(tower, ip, posX2, posY2);
        
		if(efficiency1 < 0.1 || efficiency1 > 2.){
      double gsobarSize = checkGSOBarSize(tower);
      double edgeSize = (ip<12 ? 1.0 : 2.0);

      if(posX1 < edgeSize){posX1 = edgeSize;}          
      if(posX1 > gsobarSize-edgeSize){posX1 = gsobarSize-edgeSize;}
      if(posY1 < edgeSize){posY1 = edgeSize;}
      if(posY1 > gsobarSize-edgeSize){posY1 = gsobarSize-edgeSize;}

			efficiency1 = getLeakageOutPhoton(tower, ip, posX1, posY1);
		}
    if(efficiency2 < 0.1 || efficiency2 > 2.){
      double gsobarSize = checkGSOBarSize(tower);
      double edgeSize = (ip<12 ? 1.0 : 2.0);

      if(posX2 < edgeSize){posX2 = edgeSize;}          
      if(posX2 > gsobarSize-edgeSize){posX2 = gsobarSize-edgeSize;}
      if(posY2 < edgeSize){posY2 = edgeSize;}
      if(posY2 > gsobarSize-edgeSize){posY2 = gsobarSize-edgeSize;}

			efficiency2 = getLeakageOutPhoton(tower, ip, posX2, posY2);
    }

    if(firstRatio <= -100. || secondRatio <= -100.){
      mPlateECorr[tower][ip] = mPlateE[tower][ip]/efficiency1;
    }
    if(firstRatio >= 0. || secondRatio >= 0.){
      double calibFactor = efficiency1*firstRatio + efficiency2*secondRatio;
      mPlateECorr[tower][ip] = mPlateE[tower][ip]/calibFactor;
    }
	}
}

double StRHICfRecoEnergy::getSumEnergy(int tower, int startIdx, int endIdx, double* energy)
{
  double mSumEnergy = 0.;
  for(int ip=startIdx; ip<=endIdx; ip++){
    if(ip<11){mSumEnergy += energy[ip];}
    else{mSumEnergy += energy[ip]*2.;}
  }
  return mSumEnergy;
}

double StRHICfRecoEnergy::getPhotonEnergyConvert(int tower, double sumEnergy)
{
	if(mRunType==kRHICfTL){
    if(tower==0) return 31.1323*sumEnergy+1.42462;
    if(tower==1) return 27.3429*sumEnergy+1.11522;
	}
  if(mRunType==kRHICfTS){
    if(tower==0) return 31.1666*sumEnergy+1.39715; 
    if(tower==1) return 27.3389*sumEnergy+1.12892;
	}
	if(mRunType==kRHICfTOP){
    if(tower==0) return 31.1749*sumEnergy+1.41172;
    if(tower==1) return 27.3434*sumEnergy+1.12205;
	}
	return 0;
}

double StRHICfRecoEnergy::getHadronEnergyConvert(int tower, double sumEnergy)
{
	if(tower==0) return 87.383*sumEnergy-11.1752;
	if(tower==1) return 68.472*sumEnergy-15.0983;
	return 0;
}

double StRHICfRecoEnergy::getLeakageInPhoton(int tower, int layer, double x, double y)
{
  return mLeakInTablePhoton[tower][layer]->Interpolate(x, y);
}

double StRHICfRecoEnergy::getLeakageOutPhoton(int tower, int layer, double x, double y)
{
  return mLeakOutTablePhoton[tower][layer]->Interpolate(x, y);
}

double StRHICfRecoEnergy::getLeakageOutNeutron(int tower, double x, double y)
{
  return mLeakOutTableNeutron[tower]->Interpolate(x, y);
}
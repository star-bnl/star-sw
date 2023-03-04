/***************************************************************************
 * Author: Seunghwan Lee
 ***************************************************************************
 *
 * Description: RHICf Useful class for calibration and reconstruction
 *
 ***************************************************************************
 * $Id: StRHICfFunction.cxx,v 1.0 2022/08/25 2:51:00 SeunghwanLee Exp $
 * $Log: StRHICfFunction.cxx,v $
 ***************************************************************************/

#include "StRHICfFunction.h"

StRHICfFunction::StRHICfFunction()
{
  initChecker();
}

void StRHICfFunction::initChecker()
{
  mRunType = 999;
  mGSOBarNum = 0;
  mGSOBarTableNum = 0;
  mPlateNum = 0;
  
  for(int i=0; i<5; i++){
    if(i<3){mLeakageNum[i] = 0;}
    mValueForRecoNum[i] = 0;
  }
}

StRHICfFunction::~StRHICfFunction()
{
}

int StRHICfFunction::checkRunTypeForRHICf2017(int runNum)
{
  static const int starNoTL[17] = {18175022, 18175023, 18175024, 18175025, 18175026, 
                                    18175027, 18175029, 18175030, 18176011, 18176012, 
                                    18176014, 18176016, 18176017, 18176018, 18176019, 
                                    18176020, 18176021};

  static const int starNoTS[29] = {18176033, 18176034, 18176035, 18176040, 18176042, 
                                    18176043, 18177001, 18177002, 18177003, 18177005, 
                                    18177043, 18177046, 18177047, 18177049, 18177050, 
                                    18177052, 18178002, 18178003, 18178004, 18178005, 
                                    18178006, 18178007, 18178008, 18178009, 18178011, 
                                    18178012, 18178015, 18178016, 18178017};

  static const int starNoTOP[19] = {18177011, 18177012, 18177014, 18177015, 18177016, 
                                      18177017, 18177018, 18177019, 18177020, 18177024, 
                                      18177025, 18177026, 18177027, 18177028, 18177029, 
                                      18177031, 18177032, 18177034, 18177036};

    
  for(int idx=0; idx<29; idx++){
    if(idx < 17){ // for starNoTL array
      if(starNoTL[idx] == runNum){
        mRunType = kRHICfTL;
        return mRunType;
      }
    }

    // for starNoTS array
    if(starNoTS[idx] == runNum){
      mRunType = kRHICfTS;
      return mRunType;
    }

    if(idx < 19){ // for starNoTOP array
      if(starNoTOP[idx] == runNum){
        mRunType = kRHICfTOP;
        return mRunType;
      }
    }
  }

  return kRHICfFatal;
}

int StRHICfFunction::checkGSOBarSize(int tower)
{
  if(tower==0){return kRHICfNbarSmall;}
  else if(tower==1){return kRHICfNbarLarge;}
  else{return kRHICfFatal;}
}

float StRHICfFunction::rescaleEnergyFactor(int tower, int layer)
{
  if(mRunType == 999){
    cout << "StRHICfFunction::rescaleFactorByTower() - Not set the RunType !!!" << endl;
    return kRHICfFatal;
  }

  float rescaleFactor = 0.;

  // energy rescale factor by tower each runtpye.
  static const float factorByTowerTL[2] = {1.043, 1.042};
  static const float factorByTowerTS[2] = {1.042, 1.061};
  static const float factorByTowerTOP[2] = {1.055, 1.067};

  // energy rescale factor by layer
	static const float factorByLayer[2][16] = {{0.9687, 1.006, 0.9749, 1.06, 1.01,
                                              0.9476, 1.069, 0.9761, 1.046, 1.055,
                                              1.178, 1.349, 1.996, 2.593, 4.576,
                                              4.966},
                                            {0.8955, 0.8995, 1.032, 1.032, 1.082,
                                              0.9758, 0.9511, 1.008, 0.999, 1.023,
                                              1.011, 1.2, 2.4, 2.946, 5.13,
                                              10.94}};

	if(mRunType == kRHICfTL){
    rescaleFactor = factorByTowerTL[tower] / factorByLayer[tower][layer];
    if(layer > 8){rescaleFactor = factorByTowerTL[tower];}
  }
	else if(mRunType == kRHICfTS){
    rescaleFactor = factorByTowerTS[tower] / factorByLayer[tower][layer];
    if(layer > 8){rescaleFactor = factorByTowerTS[tower];}
  }
	else if(mRunType == kRHICfTOP){
    rescaleFactor = factorByTowerTOP[tower] / factorByLayer[tower][layer];
    if(layer > 8){rescaleFactor = factorByTowerTOP[tower];}
	}
  return rescaleFactor;
}

bool StRHICfFunction::checkGSOBarEnergy(float val)
{
  if(val > -900.){mGSOBarNum += 1;}
  if(mGSOBarNum == kRHICfNtower*kRHICfNlayer*kRHICfNxy*kRHICfNbarLarge-kRHICfNlayer*kRHICfNxy*kRHICfNbarSmall){
    return kRHICfOk;
  }
  else{return kRHICfFatal;}
}

bool StRHICfFunction::checkGSOBarTable(float val)
{
  if(val > -900.){mGSOBarTableNum += 1;}
  if(mGSOBarTableNum == kRHICfNtower*kRHICfNlayer*kRHICfNxy*kRHICfNbarLarge-kRHICfNlayer*kRHICfNxy*kRHICfNbarSmall){
    return kRHICfOk;
  }
  else{return kRHICfFatal;}
}

bool StRHICfFunction::checkPlateEnergy(float val)
{
  if(val > -900.){mPlateNum += 1;}
  if(mPlateNum == kRHICfNtower*kRHICfNplate){
    return kRHICfOk;
  }
  else{return kRHICfFatal;}
}

bool StRHICfFunction::checkRecoValue(string opt, float val)
{
  transform(opt.begin(), opt.end(), opt.begin(), ::tolower);
  if(opt=="resulthitpos"){
    if(val > -900.){mValueForRecoNum[0] += 1;}
    if(mValueForRecoNum[0] == kRHICfNtower*kRHICfNxy){return kRHICfOk;}
    else{return kRHICfFatal;}
  }
  else if(opt=="resulthitnum"){
    if(val > -900.){mValueForRecoNum[1] += 1;}
    if(mValueForRecoNum[1] == kRHICfNtower){return kRHICfOk;}
    else{return kRHICfFatal;}
  }
  else if(opt=="multihitpos"){
    if(val > -900.){mValueForRecoNum[2] += 1;}
    if(mValueForRecoNum[2] == kRHICfNtower*2*kRHICfNxy*2){return kRHICfOk;}
    else{return kRHICfFatal;}
  }
  else if(opt=="multipeakheight"){
    if(val > -900.){mValueForRecoNum[3] += 1;}
    if(mValueForRecoNum[3] == kRHICfNtower*2*kRHICfNxy*2){return kRHICfOk;}
    else{return kRHICfFatal;}
  }
  else if(opt=="overlap"){
    if(val > -900.){mValueForRecoNum[4] += 1;}
    if(mValueForRecoNum[4] == kRHICfNtower*kRHICfNxy){return kRHICfOk;}
    else{return kRHICfFatal;}
  }
  else if(opt=="runtype"){
    if(val < 3){return kRHICfOk;}
    else{return kRHICfFatal;}
  }
  return kRHICfFatal;
}

bool StRHICfFunction::checkLeakageTable(string opt, bool val)
{
  transform(opt.begin(), opt.end(), opt.begin(), ::tolower);
  if(opt=="photonin"){
    if(val == true){mLeakageNum[0] += 1;}
    if(mLeakageNum[0] == kRHICfNtower*kRHICfNplate){return kRHICfOk;}
    else{return kRHICfFatal;}
  }
  else if(opt=="photonout"){
    if(val == true){mLeakageNum[1] += 1;}
    if(mLeakageNum[1] == kRHICfNtower*kRHICfNplate){return kRHICfOk;}
    else{return kRHICfFatal;}
  }
  else if(opt=="neutronout"){
    if(val == true){mLeakageNum[2] += 1;}
    if(mLeakageNum[2] == kRHICfNtower){return kRHICfOk;}
    else{return kRHICfFatal;}
  }
  return kRHICfFatal;
}

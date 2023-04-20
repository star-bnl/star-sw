#include "StRHICfRecoPos.h"

StRHICfRecoPos::StRHICfRecoPos()
{
  memset(mGSOBarSTable, 0, sizeof(mGSOBarSTable));
  memset(mGSOBarTTable, 0, sizeof(mGSOBarTTable));

  for(int it=0; it<kRHICfNtower; it++){
    for(int il=0; il<kRHICfNlayer; il++){
      for(int ixy=0; ixy<kRHICfNxy; ixy++){
        int gsobarSize = checkGSOBarSize(it);
        
        mGSOBarHist[it][il][ixy] = new TH1D("", "", gsobarSize, 0, gsobarSize);
        mGSOBarHistExpend[it][il][ixy] = new TH1D("", "", gsobarSize+4, -2, gsobarSize+2);
        mGSOBarGraph[it][il][ixy] = new TGraphErrors(gsobarSize);

        mSingleFit[it][il][ixy] = new TF1("SingleFit", this, &StRHICfRecoPos::getLorenzianSingle, 0., double(gsobarSize), 6);
        mMultiFit[it][il][ixy] = new TF1("MultiFit", this, &StRHICfRecoPos::getLorenzianMulti, 0., double(gsobarSize), 11);
        mEachFit[it][il][ixy][0] = new TF1("EachFit1", this, &StRHICfRecoPos::getLorenzianSingle, 0., double(gsobarSize), 6);
        mEachFit[it][il][ixy][1] = new TF1("EachFit2", this, &StRHICfRecoPos::getLorenzianSingle, 0., double(gsobarSize), 6);

        mSpectrum[it][il][ixy] = new TSpectrum();
      }
    }
  }
}

StRHICfRecoPos::~StRHICfRecoPos()
{
  for(int it=0; it<kRHICfNtower; it++){
    for(int il=0; il<kRHICfNlayer; il++){
      for(int ixy=0; ixy<kRHICfNxy; ixy++){ 
        delete mGSOBarHist[it][il][ixy];
        delete mGSOBarHistExpend[it][il][ixy];
        delete mGSOBarGraph[it][il][ixy];
        delete mSingleFit[it][il][ixy];
        delete mMultiFit[it][il][ixy];
        delete mEachFit[it][il][ixy][0];
        delete mEachFit[it][il][ixy][1];

        mGSOBarHist[it][il][ixy] = nullptr;
        mGSOBarHistExpend[it][il][ixy] = nullptr;
        mGSOBarGraph[it][il][ixy] = nullptr;
        mSingleFit[it][il][ixy] = nullptr;
        mMultiFit[it][il][ixy] = nullptr;
        mEachFit[it][il][ixy][0] = nullptr;
        mEachFit[it][il][ixy][1] = nullptr;
      }
    }
  }
}

void StRHICfRecoPos::init()
{
  memset(mWorthy, false, sizeof(mWorthy));
  memset(mOverlap, false, sizeof(mOverlap));
  memset(mGSOMaxLayer, 0, sizeof(mGSOMaxLayer));
  memset(mGSOBarSE, 0, sizeof(mGSOBarSE));
  memset(mGSOBarTE, 0, sizeof(mGSOBarTE));

  for(int it=0; it<kRHICfNtower; it++){
    for(int il=0; il<kRHICfNlayer; il++){
      for(int ixy=0; ixy<kRHICfNxy; ixy++){
        mGSOBarHist[it][il][ixy] -> Reset();
        mGSOBarHistExpend[it][il][ixy] -> Reset();
        mGSOBarGraph[it][il][ixy] -> Set(0);

        for(int p=0; p<11; p++){
          mMultiFit[it][il][ixy] -> SetParameter(p, 0);
          if(p < 6){
            mSingleFit[it][il][ixy] -> SetParameter(p, 0);
            mEachFit[it][il][ixy][0] -> SetParameter(p, 0);
            mEachFit[it][il][ixy][1] -> SetParameter(p, 0);
          }
        }
      }
    }
  }
}

void StRHICfRecoPos::setGSOBarEnergy(int tower, int layer, int xy, int bar, Double_t val)
{
  if(tower==0){mGSOBarSE[layer][xy][bar] = val;}
  if(tower==1){mGSOBarTE[layer][xy][bar] = val;}
}

void StRHICfRecoPos::setGSOBarTable(int tower, int layer, int xy, int bar, Double_t val)
{
  if(tower==0){mGSOBarSTable[layer][xy][bar] = val;}
  if(tower==1){mGSOBarTTable[layer][xy][bar] = val;}
}

bool StRHICfRecoPos::fillData()
{
  // For worthy
  int tmpWorthy[kRHICfNxy][kRHICfNtower];
  memset(tmpWorthy, 0, sizeof(tmpWorthy));

  Double_t pedeRMSGeV = mPedADCRMS/(m1000Vto600VFactor*mSpecialFactor*mAvgConvFactor);

  for(int it=0; it<kRHICfNtower; it++){
    for(int il=0; il<kRHICfNlayer; il++){
      for(int ixy=0; ixy<kRHICfNxy; ixy++){
        mGSOBarHistExpend[it][il][ixy] -> SetBinContent(1, 0.);
        mGSOBarHistExpend[it][il][ixy] -> SetBinContent(2, 0.);

        int gsobarSize = checkGSOBarSize(it);
        for(int ich=0; ich<gsobarSize; ich++){
          Double_t gsobarE = 0;
          Double_t gsobarTable = 0;
          if(it==0){
            gsobarE = mGSOBarSE[il][ixy][ich];
            gsobarTable = mGSOBarSTable[il][ixy][ich];
          }
          if(it==1){
            gsobarE = mGSOBarTE[il][ixy][ich];
            gsobarTable = mGSOBarTTable[il][ixy][ich];
          }

          mGSOBarHist[it][il][ixy] -> SetBinContent(ich+1, gsobarE);
          mGSOBarHistExpend[it][il][ixy] -> SetBinContent(ich+3, gsobarE);
          mGSOBarGraph[it][il][ixy] -> SetPoint(ich, gsobarTable, gsobarE);
          mGSOBarGraph[it][il][ixy] -> SetPointError(ich, 0., mGSOBarMapError * gsobarE + sqrt(pedeRMSGeV));   

          // For worthy
          double edep = mGSOBarHist[it][il][ixy] -> GetBinContent(ich+1);
          if(edep > mNoiseThreshold){tmpWorthy[ixy][it] += 1;}
        }
        mGSOBarHistExpend[it][il][ixy] -> SetBinContent(gsobarSize+3, 0.);
        mGSOBarHistExpend[it][il][ixy] -> SetBinContent(gsobarSize+4, 0.);
      }
    }
  }
  // For worthy
  for(int it=0; it<kRHICfNtower; it++){if(tmpWorthy[0][it]>0 && tmpWorthy[1][it]>0) mWorthy[it] = true;}

  // Remove the dead channal
  mGSOBarGraph[0][3][0]->SetPoint(12, mGSOBarSTable[3][0][12],  0.);
  mGSOBarGraph[0][3][0]->RemovePoint(12);

  findMaxLayer();

  return kRHICfOk;
}

bool StRHICfRecoPos::calculate()
{   
  initSetParamaters();
  fitting();

  std::cout << "StRHICfRecoPos::calculate() -- Done " << std::endl;
  return kRHICfOk;
}

bool StRHICfRecoPos::separateMultiFit(int tower)
{
  for(int il=0; il<kRHICfNlayer; il++){
    for(int ixy=0; ixy<kRHICfNxy; ixy++){
      Double_t parMulti[11];
      mMultiFit[tower][il][ixy] -> GetParameters(parMulti);
      mEachFit[tower][il][ixy][0] -> SetParameters(parMulti[0], parMulti[1], parMulti[2], parMulti[3], parMulti[4], parMulti[5]/2.);
      mEachFit[tower][il][ixy][1] -> SetParameters(parMulti[6], parMulti[7], parMulti[8], parMulti[9], parMulti[10], parMulti[5]/2.);
    }
  }
  return kRHICfOk;
}

int StRHICfRecoPos::getGSOMaxLayer(int tower, int order){return mGSOMaxLayer[tower][order];}

int StRHICfRecoPos::getMaximumBin(int tower, int layer, int xy)
{
  int bin = 0;
	double tmpVal = 0;
  double tmpE = 0;

  for(int ich=0; ich<checkGSOBarSize(tower); ich++){
    if(tower==0){tmpE = mGSOBarSE[layer][xy][ich];}
    if(tower==1){tmpE = mGSOBarTE[layer][xy][ich];}
    if(tmpE > tmpVal){
      tmpVal = tmpE;
      bin = ich;
    }
  }
  return bin+1;
}

int StRHICfRecoPos::getEvalHitNum(int tower)
{
  int nHit[kRHICfNlayer][kRHICfNxy];
  for(int il=0; il<kRHICfNlayer; il++){
    for(int ixy=0; ixy<kRHICfNxy; ixy++){
      nHit[il][ixy] = getEvalHitNum(tower, il, ixy);
    }
  }

  bool isNullData = true;
  for(int il=0; il<4; ++il){
    if(mGSOMaxLayer[tower][0]==il && nHit[il][0] > 0 && nHit[il][1] > 0){isNullData = false;}
  }

  mOverlap[tower][0] = false;
	mOverlap[tower][1] = false;

	if(isNullData == true){return 0;}

  if(nHit[mGSOMaxLayer[tower][0]][0]==2 && nHit[mGSOMaxLayer[tower][0]][1]==2){return 2;}

  if(mMultiFit[tower][mGSOMaxLayer[tower][0]][0]->GetParameter(1) == mMultiFit[tower][mGSOMaxLayer[tower][0]][0]->GetParameter(7) && nHit[mGSOMaxLayer[tower][0]][1] == 2){
		mOverlap[tower][0] = true;
    return 2;
	}
  if(mMultiFit[tower][mGSOMaxLayer[tower][0]][1]->GetParameter(1) == mMultiFit[tower][mGSOMaxLayer[tower][0]][1]->GetParameter(7) &&  nHit[mGSOMaxLayer[tower][0]][0] == 2){
    mOverlap[tower][1] = true;
    return 2;
	}
  return 1;
}

int StRHICfRecoPos::getEvalHitNum(int tower, int layer, int xy)
{
  int nHitSingleFit = 0;
  int nHitMultiFit = 0;

  // Check nHit to fitting result for single
  if(getSinglePeakHeight(tower, layer, xy) > mNoiseThreshold){nHitSingleFit = 1;}

  // Check nHit to fitting result for multi
  if(getMultiPeakHeight(tower, layer, xy, 0) > mNoiseThreshold){nHitMultiFit++;}
  if(getMultiPeakHeight(tower, layer, xy, 1) > mNoiseThreshold){nHitMultiFit++;}

  // Check nHit to TSpectrum
  if(mSpectrum[tower][layer][xy]->GetNPeaks() < 2){
    nHitMultiFit = 1;
    mMultiFit[tower][layer][xy] -> SetParameter(7, mMultiFit[tower][layer][xy]->GetParameter(1));
  }

  // Check nHit to distance 
  Double_t parMulti[11];
  mMultiFit[tower][layer][xy] -> GetParameters(parMulti);
  if(abs(parMulti[1] - parMulti[7]) < mPeakDistThreshold){
    nHitMultiFit = 1;
    mMultiFit[tower][layer][xy] -> SetParameter(7, mMultiFit[tower][layer][xy]->GetParameter(1));
  }

	// Check nHit to peak height ratio.
  double heightRatio = getMultiPeakHeight(tower, layer, xy, 1)/getMultiPeakHeight(tower, layer, xy, 0);
  if(heightRatio < mTSpecRatioThreshold){nHitMultiFit = 1;}

  double chi2Ratio = getSingleChi2(tower, layer, xy)/getMultiChi2(tower, layer, xy);

  if(nHitMultiFit > 1 && chi2Ratio > 1.0){return 2;}
	if(nHitSingleFit > 0){return 1;}
  return 0;
}

double StRHICfRecoPos::getDepositEnergy(int tower, int layer)
{
  double edepX = mGSOBarHist[tower][layer][0] -> Integral();
  double edepY = mGSOBarHist[tower][layer][1] -> Integral();
  return edepX+edepY;
}

double StRHICfRecoPos::getMultiEnergySum(int tower, int layer, int xy, int order){return mEachFit[tower][layer][xy][order] -> Integral(0, checkGSOBarSize(tower));}

Double_t StRHICfRecoPos::getSinglePeakPos(int tower, int layer, int xy){return mSingleFit[tower][layer][xy] -> GetParameter(1);}

Double_t StRHICfRecoPos::getMultiPeakPos(int tower, int layer, int xy, int order)
{
  if(order==0){return mMultiFit[tower][layer][xy] -> GetParameter(1);}
  else{return mMultiFit[tower][layer][xy] -> GetParameter(7);}
}

double StRHICfRecoPos::getSinglePeakHeight(int tower, int layer, int xy){return mSingleFit[tower][layer][xy] ->Eval(getSinglePeakPos(tower, layer, xy));}

double StRHICfRecoPos::getMultiPeakHeight(int tower, int layer, int xy, int order)
{
  double peak[2];
  Double_t multiPar[11];
  memset(peak, 0, sizeof(peak));
  memset(multiPar, 0, sizeof(multiPar));

  mMultiFit[tower][layer][xy] -> GetParameters(multiPar);
  
  peak[0] = multiPar[2]*(multiPar[4]/pow(multiPar[0], 0.5) + (1 - multiPar[4])/pow(multiPar[3], 0.5))/2. + multiPar[5];
  peak[1] = multiPar[8]*(multiPar[10]/pow(multiPar[6], 0.5) + (1 - multiPar[10])/pow(multiPar[9], 0.5))/2. + multiPar[5];

  if(order==0){return std::max(peak[0], peak[1]);}
  else{return std::min(peak[0], peak[1]);}
}

double StRHICfRecoPos::getMultiPeakRaw(int tower, int layer, int xy, int order)
{
	Int_t posBin = mGSOBarHist[tower][layer][xy] -> FindBin(getMultiPeakPos(tower, layer, xy, order));
	double candidateBins[3] = {mGSOBarHist[tower][layer][xy]->GetBinContent(posBin-1), mGSOBarHist[tower][layer][xy]->GetBinContent(posBin), mGSOBarHist[tower][layer][xy]->GetBinContent(posBin+1)};
	int maxIdx = TMath::LocMax(3, candidateBins);
	return candidateBins[maxIdx];
}

double StRHICfRecoPos::getEvalPeakHeight(int tower, int layer, int xy, int order){return mEachFit[tower][layer][xy][order] -> Eval(getMultiPeakPos(tower, layer, xy, order));}

double StRHICfRecoPos::getSingleChi2(int tower, int layer, int xy)
{
  double chi2 = mSingleFit[tower][layer][xy]->GetChisquare();
	double dof  = (double) mSingleFit[tower][layer][xy]->GetNDF();
  return chi2/dof;
}

double StRHICfRecoPos::getMultiChi2(int tower, int layer, int xy)
{
  double chi2 = mMultiFit[tower][layer][xy]->GetChisquare();
	double dof  = (double) mMultiFit[tower][layer][xy]->GetNDF();
  return chi2/dof;
}

bool StRHICfRecoPos::getWorthy(int tower){return mWorthy[tower];}

bool StRHICfRecoPos::getOverlap(int tower, int xy){return mOverlap[tower][xy];}

void StRHICfRecoPos::findMaxLayer()
{
  double maxSumBar[kRHICfNtower][kRHICfNlayer];
  for(int it=0; it<kRHICfNtower; it++){
    for(int il=0; il<kRHICfNlayer; il++){maxSumBar[it][il] = getDepositEnergy(it, il);}
    mGSOMaxLayer[it][0] = TMath::LocMax(4, maxSumBar[it]);
    maxSumBar[it][mGSOMaxLayer[it][0]] = 0.;
    mGSOMaxLayer[it][1] = TMath::LocMax(4, maxSumBar[it]);
  }
}

void StRHICfRecoPos::initSetParamaters()
{
  Double_t parInMaxLayer[2][kRHICfNtower][kRHICfNxy][2];
  Double_t parSingle[kRHICfNtower][kRHICfNlayer][kRHICfNxy][2];

  memset(parInMaxLayer, 0, sizeof(parInMaxLayer));
  memset(parSingle, 0, sizeof(parSingle));

  for(int it=0; it<kRHICfNtower; it++){
    for(int il=0; il<kRHICfNlayer; il++){
      for(int ixy=0; ixy<kRHICfNxy; ixy++){
        Double_t posPrime = 0.;
        Double_t valuePrime = 0.;
        Double_t posSub = 0.;
        Double_t valueSub = 0.;

        // Search the peak 
        mSpectrum[it][il][ixy]->Search(mGSOBarHistExpend[it][il][ixy], mTSpecSigma, "nodraw goff", mTSpecRatioThreshold);
        int numHit = mSpectrum[it][il][ixy]->GetNPeaks();
				Double_t* tSpecPeak = mSpectrum[it][il][ixy]->GetPositionX();

        // Setting prime, sub position and hight
        if(numHit==0){
          Int_t maxBin = mGSOBarHist[it][il][ixy]->GetMaximumBin();
          Int_t subBin = checkGSOBarSize(it)/2;

          posPrime = mGSOBarHist[it][il][ixy]->GetBinCenter(maxBin);
          valuePrime = mGSOBarHist[it][il][ixy]->GetBinContent(maxBin);
          posSub = Double_t(subBin);
          valueSub = mGSOBarHist[it][il][ixy]->GetBinContent(subBin);
        }
        else if(numHit==1){
          Int_t maxBin = mGSOBarHist[it][il][ixy]->GetXaxis()->FindBin(tSpecPeak[0]);
          Int_t subBin = checkGSOBarSize(it)/2;
          
          posPrime = tSpecPeak[0];
          valuePrime = mGSOBarHist[it][il][ixy]->GetBinContent(maxBin);
          posSub = Double_t(subBin);
          valueSub = mGSOBarHist[it][il][ixy]->GetBinContent(subBin);
        }
        else if(numHit>1){
          double bufferPeakValue = 0;
          int primeIdx = -1;

          // searching for prime peak
          for(int in=0; in<numHit; in++){
            Int_t bin = mGSOBarHist[it][il][ixy]->GetXaxis()->FindBin(tSpecPeak[in]);
            Double_t value = mGSOBarHist[it][il][ixy]->GetBinContent(bin);
                
            if(value > bufferPeakValue){
              bufferPeakValue = value;
              posPrime = tSpecPeak[in];
              valuePrime = value;
              primeIdx = in;
            }
          }

          bufferPeakValue = 0;
          //searching for sub peak
          for(int in=0; in<numHit; in++){
            if(in == primeIdx){continue;}

            Int_t bin = mGSOBarHist[it][il][ixy]->GetXaxis()->FindBin(tSpecPeak[in]);
            Double_t value = mGSOBarHist[it][il][ixy]->GetBinContent(bin);
              
            if(value > bufferPeakValue){
              bufferPeakValue = value;
              posSub = tSpecPeak[in];
              valueSub = value;
            }
          } 
        }

        if(valuePrime<0){valuePrime = -valuePrime;}
				if(valueSub<0){valueSub = -valueSub;}

        if(getGSOMaxLayer(it, 0) == il){
          parInMaxLayer[0][it][ixy][0] = posPrime;
          parInMaxLayer[0][it][ixy][1] = valuePrime;
          parInMaxLayer[1][it][ixy][0] = posSub;
          parInMaxLayer[1][it][ixy][1] = valueSub;
        }
        parSingle[it][il][ixy][0] = posPrime;
        parSingle[it][il][ixy][1] = valuePrime;
      }
    }
  }

  // Parameters setup
  for(int it=0; it<kRHICfNtower; it++){
    for(int il=0; il<kRHICfNlayer; il++){
      for(int ixy=0; ixy<kRHICfNxy; ixy++){
        Double_t initParSingle[6] = {mParWidth1, parSingle[it][il][ixy][0], parSingle[it][il][ixy][1], mParWidth2, mParRatio, mParBaseLine};
        mSingleFit[it][il][ixy] -> SetParameters(initParSingle);

        mSingleFit[it][il][ixy] -> SetParLimits(0, mParWidth1Min, mParWidth1Max);
        mSingleFit[it][il][ixy] -> SetParLimits(1, -1., 1. + double(checkGSOBarSize(it)));
        mSingleFit[it][il][ixy] -> SetParLimits(2, mParHeightMin, mParHeightMax);
        mSingleFit[it][il][ixy] -> SetParLimits(3, mParWidth2Min, mParWidth2Max);
        mSingleFit[it][il][ixy] -> SetParLimits(4, mParRatioMin, mParRatioMax);

        Double_t initParMulti[11] = {mParWidth1, parInMaxLayer[0][it][ixy][0], parInMaxLayer[0][it][ixy][1], mParWidth2, mParRatio, mParBaseLine, mParWidth1, parInMaxLayer[1][it][ixy][0], parInMaxLayer[1][it][ixy][1], mParWidth2, mParRatio};
        mMultiFit[it][il][ixy] -> SetParameters(initParMulti);

        mMultiFit[it][il][ixy] -> SetParLimits(0, mParWidth1Min, mParWidth1Max);
        mMultiFit[it][il][ixy] -> SetParLimits(1, parInMaxLayer[0][it][ixy][0]-2, parInMaxLayer[0][it][ixy][0]+2);
        mMultiFit[it][il][ixy] -> SetParLimits(2, mParHeightMin, mParHeightMax);
        mMultiFit[it][il][ixy] -> SetParLimits(3, mParWidth2Min, mParWidth2Max);
        mMultiFit[it][il][ixy] -> SetParLimits(4, mParRatioMin, mParRatioMax);
        mMultiFit[it][il][ixy] -> SetParLimits(6, mParWidth1Min, mParWidth1Max);
        mMultiFit[it][il][ixy] -> SetParLimits(7, parInMaxLayer[1][it][ixy][0]-2, parInMaxLayer[1][it][ixy][0]+2);
        mMultiFit[it][il][ixy] -> SetParLimits(8, mParHeightMin, mParHeightMax);
        mMultiFit[it][il][ixy] -> SetParLimits(9, mParWidth2Min, mParWidth2Max);
        mMultiFit[it][il][ixy] -> SetParLimits(10, mParRatioMin, mParRatioMax);
      }
    }
  }
}

void StRHICfRecoPos::fitting()
{
  for(int it=0; it<kRHICfNtower; it++){
    for(int il=0; il<kRHICfNlayer; il++){
      for(int ixy=0; ixy<kRHICfNxy; ixy++){
        mGSOBarGraph[it][il][ixy] -> Fit(mSingleFit[it][il][ixy], "QR");
        mGSOBarGraph[it][il][ixy] -> Fit(mMultiFit[it][il][ixy], "QR");
      }
    }
  }
}

Double_t StRHICfRecoPos::getLorenzianSingle(Double_t* x, Double_t* par)
{
  Double_t val = par[4]*par[0]/pow(((x[0]-par[1])*(x[0]-par[1])+par[0]), 1.5)/2.;
  Double_t val2 = (1-par[4])*(par[3]/2.)/pow(((x[0]-par[1])*(x[0]-par[1])+par[3]), 1.5);

  return par[2]*(val+val2) + par[5];
}

Double_t StRHICfRecoPos::getLorenzianMulti(Double_t* x, Double_t* par)
{
  Double_t p1val = par[4]*par[0]/pow(((x[0]-par[1])*(x[0]-par[1])+par[0]), 1.5)/2.;
  Double_t p1val2 = (1.-par[4])*par[3]/pow(((x[0]-par[1])*(x[0]-par[1])+par[3]), 1.5)/2.;
  Double_t peak1 = par[2]*(p1val+p1val2);

  Double_t p2val = par[10]*par[6]/pow(((x[0]-par[7])*(x[0]-par[7])+par[6]), 1.5)/2.;
  Double_t p2val2 = (1.-par[10])*par[9]/pow(((x[0]-par[7])*(x[0]-par[7])+par[9]), 1.5)/2.;
  Double_t peak2 = par[8]*(p2val+p2val2);

  return peak1 + peak2 + par[5];
}

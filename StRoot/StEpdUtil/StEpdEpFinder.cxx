#include "StEpdEpFinder.h"
#include "TVector3.h"
#include "TH2D.h"
#include "TFile.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "TClonesArray.h"
#include "StEpdGeom.h"
#include "StPicoEvent/StPicoEpdHit.h"
#include "TMath.h"
#include <cassert>
#include <iostream>
//#include <fstream>
using namespace std;


StEpdEpFinder::StEpdEpFinder(int nCentBins, char const* CorrectionFile) : mFormatUsed(2), mThresh(0.3), mMax(2.0), mWeightingScheme(0)
{

  mEpdGeom = new StEpdGeom();

  for (int iOrder=0; iOrder<_EpOrderMax; iOrder++){
    mEtaWeights[iOrder] = 0;
    for (int iRing=0; iRing<16; iRing++){
      mRingWeights[iRing][iOrder]=1.0;
    }
  }

  // ----------------------------------- Stuff read from the "Correction File" -----------------------------------------                                                    
  // "Shift correction" histograms that we INPUT and apply here                                                                                                             
  mCorrectionInputFile = new TFile(CorrectionFile,"READ");
  if (mCorrectionInputFile->IsZombie()) {
    std::cout << "Error opening file with Correction Histograms" << std::endl;
    std::cout << "I will use no weighting at all." << std::endl;
    for (int ewFull=0; ewFull<3; ewFull++){
      for (int order=1; order<_EpOrderMax+1; order++){
	mEpdShiftInput_sin[ewFull][order-1] = 0;
	mEpdShiftInput_cos[ewFull][order-1] = 0;
      }
    }
    for (int ew=0; ew<2; ew++){
      mPhiWeightInput[ew] = 0;
    }
  }
  else{
    for (int ew=0; ew<3; ew++){
      for (int order=1; order<_EpOrderMax+1; order++){
	mEpdShiftInput_sin[ew][order-1] = (TProfile2D*)mCorrectionInputFile->Get(Form("EpdShiftEW%dPsi%d_sin",ew,order));
	mEpdShiftInput_cos[ew][order-1] = (TProfile2D*)mCorrectionInputFile->Get(Form("EpdShiftEW%dPsi%d_cos",ew,order));
      }
      mPhiWeightInput[ew] = (TH2D*)mCorrectionInputFile->Get(Form("PhiWeightEW%d",ew));
    }
    for (int order=1; order<_EpOrderMax+1; order++){
      mEpdShiftInput_sin[2][order-1] = (TProfile2D*)mCorrectionInputFile->Get(Form("EpdShiftFullEventPsi%d_sin",order));
      mEpdShiftInput_cos[2][order-1] = (TProfile2D*)mCorrectionInputFile->Get(Form("EpdShiftFullEventPsi%d_cos",order));
    }
  }
  // ----------------------------------- Stuff written to the "Correction File" -----------------------------------------                                                   
  // "Shift correction" histograms that we produce and OUTPUT                                                                                                               
  mCorrectionOutputFile = new TFile("./StEpdEpFinderCorrectionHistograms_OUTPUT.root","RECREATE");
  for (int ew=0; ew<2; ew++){
    for (int order=1; order<_EpOrderMax+1; order++){
      mEpdShiftOutput_sin[ew][order-1] = new TProfile2D(Form("EpdShiftEW%dPsi%d_sin",ew,order),Form("EpdShiftEW%dPsi%d_sin",ew,order),
							_EpTermsMax,0.5,1.0*_EpTermsMax+.5,nCentBins,-0.5,(double)nCentBins-0.5,-1.0,1.0);
      mEpdShiftOutput_cos[ew][order-1] = new TProfile2D(Form("EpdShiftEW%dPsi%d_cos",ew,order),Form("EpdShiftEW%dPsi%d_cos",ew,order),
							_EpTermsMax,0.5,1.0*_EpTermsMax+.5,nCentBins,-0.5,(double)nCentBins-0.5,-1.0,1.0);
    }
    mPhiWeightOutput[ew]   = new TH2D(Form("PhiWeightEW%d",ew),Form("Tile Weight divided by Averaged EW=%d",ew),12,0.5,12.5,31,0.5,31.5); // bins are PP,TT 
    mPhiAveraged[ew]       = new TH2D(Form("PhiAveraged%d",ew),Form("Average for this phi EW=%d",ew),12,0.5,12.5,31,0.5,31.5); // just for normalization. discard after use
  }
  for (int order=1; order<_EpOrderMax+1; order++){
    mEpdShiftOutput_sin[2][order-1] = new TProfile2D(Form("EpdShiftFullEventPsi%d_sin",order),
						     Form("EpdShiftFullEventPsi%d_sin",order),
						     _EpTermsMax,0.5,1.0*_EpTermsMax+.5,nCentBins,-0.5,(double)nCentBins-0.5,-1.0,1.0);
    mEpdShiftOutput_cos[2][order-1] = new TProfile2D(Form("EpdShiftFullEventPsi%d_cos",order),
						     Form("EpdShiftFullEventPsi%d_cos",order),
						     _EpTermsMax,0.5,1.0*_EpTermsMax+.5,nCentBins,-0.5,(double)nCentBins-0.5,-1.0,1.0);
  }

  // now stuff for the "Resolution File"
  mResolutionOutputFile = new TFile("EpResolutions.root","RECREATE");
  for (int order=1; order<_EpOrderMax; order++){
    mAveCosDeltaPsi[order-1] = new TProfile(Form("AveCosDeltaPsi%d",order),Form("#langle cos(%d#Psi_{E,%d}-%d#Psi_{W,%d} #rangle",order,order,order,order),
				    nCentBins,-0.5,(double)nCentBins-0.5,-1.0,1.0);
  }
}

void StEpdEpFinder::SetEtaWeights(int order, TH2D EtaWeights){
  mWeightingScheme = 1;
  if (order>_EpOrderMax){
    std::cout << "You are setting weights for EP of order " << order << " which is too high!  Increase _EpOrderMax in StEpdEpFinder.h\n";
    assert(0);
  }
  mEtaWeights[order-1] = new TH2D;
  EtaWeights.Copy(*mEtaWeights[order-1]);
}

void StEpdEpFinder::SetRingWeights(int order, double* RingWeights){
  mWeightingScheme = 2;
  if (order>_EpOrderMax){
    std::cout << "You are setting weights for EP of order " << order << " which is too high!  Increase _EpOrderMax in StEpdEpFinder.h\n";
    assert(0);
  }
  for (int iring=0; iring<16; iring++){
    mRingWeights[iring][order-1] = RingWeights[iring];
  }
}

void StEpdEpFinder::Finish(){

  mCorrectionInputFile->Close();

  for (int ew=0; ew<2; ew++){
    mPhiWeightOutput[ew]->Divide(mPhiAveraged[ew]);
    delete mPhiAveraged[ew];
  }
  mCorrectionOutputFile->Write();
  mCorrectionOutputFile->Close();

  mResolutionOutputFile->Write();
  mResolutionOutputFile->Close();

  cout << "StEpdEpFinder out!\n\n";
}

StEpdEpInfo StEpdEpFinder::Results(TClonesArray* EpdHits, TVector3 primVertex, int CentId){
  StEpdEpInfo result;

  double pi = TMath::Pi();

  //--------------------------------- begin loop over hits ---------------------------------------
  for (int hit=0; hit<EpdHits->GetEntries(); hit++){
    int tileId,ring,TT,PP,EW,ADC;
    float nMip;
    switch(mFormatUsed) {
    case 2 :
      {                               // interesting.  If a declaration is inside a case, it MUST be explicitly scoped with brackets {} or else error.
	                              // https://en.cppreference.com/w/cpp/language/switch
	StPicoEpdHit* epdHit = (StPicoEpdHit*)((*EpdHits)[hit]);
	tileId = epdHit->id();
	EW = (tileId<0)?0:1;
	ring = epdHit->row();
	TT = epdHit->tile();
	PP = epdHit->position();
	ADC = epdHit->adc();
	//      nMip = epdHit->nMIP();  <--- when calibrated!
	nMip = (TT<10)?(double)ADC/160.0:(double)ADC/115.0;
	break;
      }
    default :
      std::cout << "You are requesting a format other than picoDst.  It is not implemented\n";
      std::cout << "YOU do it!\n";
      assert(0);
    }
    if (nMip<mThresh) continue;
    double TileWeight = (nMip<mMax)?nMip:mMax;
    TVector3 StraightLine = mEpdGeom->TileCenter(tileId) - primVertex;
    double phi = StraightLine.Phi();
    double eta = StraightLine.Eta();

    //---------------------------------
    // fill Phi Weight histograms to be used in next iteration (if desired)
    // Obviously, do this BEFORE phi weighting!
    //---------------------------------
    mPhiWeightOutput[EW]->Fill(PP,TT,TileWeight);
    if (TT==1){ for (int pp=1; pp<13; pp++) mPhiAveraged[EW]->Fill(pp,1,TileWeight/12.0);}
    else{
      for (int pp=1; pp<13; pp++){
	for (int tt=2*(ring-1); tt<2*ring; tt++) mPhiAveraged[EW]->Fill(pp,tt,TileWeight/24.0);
      }
    }
    // now calculate Q-vectors
    for (int order=1; order<_EpOrderMax+1; order++){
      double TotalWeight =  RingOrEtaWeight(ring,eta,order,CentId) * TileWeight;
      double WtSine   = TotalWeight*sin(phi*(double)order);
      double WtCosine = TotalWeight*cos(phi*(double)order);
      result.QrawOneSide[EW][order-1][0]      += WtCosine;
      result.QrawOneSide[EW][order-1][1]      += WtSine;
      result.QringRaw[EW][order-1][0][ring-1] += WtCosine;
      result.QringRaw[EW][order-1][1][ring-1] += WtSine;
      if (mPhiWeightInput[EW]){
	WtSine   /= mPhiWeightInput[EW]->GetBinContent(PP,TT);
	WtCosine /= mPhiWeightInput[EW]->GetBinContent(PP,TT);
      }
      result.QphiWeightedOneSide[EW][order-1][0]      += WtCosine;
      result.QphiWeightedOneSide[EW][order-1][1]      += WtSine;
      result.QringPhiWeighted[EW][order-1][0][ring-1] += WtCosine;
      result.QringPhiWeighted[EW][order-1][1][ring-1] += WtSine;
    }
  }
  //--------------------------------- end loop over hits ---------------------------------------

  // Before going any farther, flip the sign of the 1st-order Q-vector on the East side.
  //  I want the rapidity-odd first-order event plane.  If you want something else, then
  //  write your own code, you weirdo ;-)
  for (int xy=0; xy<2; xy++){
    result.QrawOneSide[0][0][xy]                *= -1.0;
    result.QphiWeightedOneSide[0][0][xy]        *= -1.0;
    for (int iring=0; iring<16; iring++){
      result.QringRaw[0][0][xy][iring]          *= -1.0;
      result.QringPhiWeighted[0][0][xy][iring]  *= -1.0;
    }
  }

  //---------------------------------
  // Calculate unshifted EP angles
  // * note in this part, first we loop over East/West, then we do Full
  //---------------------------------
  for (int order=1; order<_EpOrderMax+1; order++){
    for (int ew=0; ew<2; ew++){
      result.PsiRaw[ew][order-1]                       = GetPsiInRange(result.QrawOneSide[ew][order-1][0],result.QrawOneSide[ew][order-1][1],order);
      result.PsiPhiWeighted[ew][order-1]               = GetPsiInRange(result.QphiWeightedOneSide[ew][order-1][0],result.QphiWeightedOneSide[ew][order-1][1],order);
      for (int ring=1; ring<17; ring++){
	result.PsiRingRaw[ew][order-1][ring-1]         = GetPsiInRange(result.QringRaw[ew][order-1][0][ring-1],result.QringRaw[ew][order-1][1][ring-1],order);
	result.PsiRingPhiWeighted[ew][order-1][ring-1] = GetPsiInRange(result.QringPhiWeighted[ew][order-1][0][ring-1],result.QringPhiWeighted[ew][order-1][1][ring-1],order);
      } // loop over ring
    }   // loop over EW
    // now the "Full Event" angles - these have the "ew index" = 2.
    double Qx,Qy;
    Qx = result.QrawOneSide[0][order-1][0] + result.QrawOneSide[1][order-1][0];
    Qy = result.QrawOneSide[0][order-1][1] + result.QrawOneSide[1][order-1][1];
    result.PsiRaw[2][order-1] = GetPsiInRange(Qx,Qy,order);

    Qx = result.QphiWeightedOneSide[0][order-1][0] + result.QphiWeightedOneSide[1][order-1][0];
    Qy = result.QphiWeightedOneSide[0][order-1][1] + result.QphiWeightedOneSide[1][order-1][1];
    result.PsiPhiWeighted[2][order-1] = GetPsiInRange(Qx,Qy,order);
  }     // loop over order

  //---------------------------------
  // Now shift
  //---------------------------------

  for (int order=1; order<_EpOrderMax+1; order++){
    for (int ewFull=0; ewFull<3; ewFull++){  // 0,1,2 means East,West,Full
      result.PsiPhiWeightedAndShifted[ewFull][order-1] = result.PsiPhiWeighted[ewFull][order-1];
      if (mEpdShiftInput_sin[ewFull][order-1] != 0){
	for (int i=1; i<=_EpTermsMax; i++){
	  double tmp = (double)(order*i);
	  double sinAve = mEpdShiftInput_sin[ewFull][order-1]->GetBinContent(i,CentId+1);    /// note the "+1" since CentId begins at zero
	  double cosAve = mEpdShiftInput_cos[ewFull][order-1]->GetBinContent(i,CentId+1);    /// note the "+1" since CentId begins at zero
	  result.PsiPhiWeightedAndShifted[ewFull][order-1] +=
	    2.0*(cosAve*sin(tmp*result.PsiPhiWeighted[ewFull][order-1]) - sinAve*cos(tmp*result.PsiPhiWeighted[ewFull][order-1]))/tmp;
	}
	double AngleWrapAround = 2.0*pi/(double)order;
	if (result.PsiPhiWeightedAndShifted[ewFull][order-1]<0) result.PsiPhiWeightedAndShifted[ewFull][order-1] += AngleWrapAround;
	else if (result.PsiPhiWeightedAndShifted[ewFull][order-1]>AngleWrapAround) result.PsiPhiWeightedAndShifted[ewFull][order-1] -= AngleWrapAround;
      }
    }
  }
  //---------------------------------
  // Now calculate shift histograms for a FUTURE run (if you want it)
  //---------------------------------
  for (int i=1; i<=_EpTermsMax; i++){
    for (int ewFull=0; ewFull<3; ewFull++){
      for (int order=1; order<_EpOrderMax+1; order++){
	double tmp = (double)(order*i);
	mEpdShiftOutput_sin[ewFull][order-1]->Fill(i,CentId,sin(tmp*result.PsiPhiWeighted[ewFull][order-1]));
	mEpdShiftOutput_cos[ewFull][order-1]->Fill(i,CentId,cos(tmp*result.PsiPhiWeighted[ewFull][order-1]));
      }
    }
  }

  return result;
}

double StEpdEpFinder::RingOrEtaWeight(int ring, double eta, int order, int centId){
  switch (mWeightingScheme){
  case 0:                                         // no weighting
    return 1.0;
    break;
  case 1:                                         // eta-based weights
    { // must explicitly scope with {} if there is a declaration statement in a case
      if (mEtaWeights[order-1]==0) return 1.0;      // user never defined weights for this order
      int etaBin = mEtaWeights[order-1]->GetXaxis()->FindBin(fabs(eta));
      return mEtaWeights[order-1]->GetBinContent(etaBin,centId+1);      // note the "+1" since centId starts at zero
      break;
    }
  case 2:                                         // ring-based weights
    return mRingWeights[ring-1][order-1];
    break;
  default:
    return 1.0;
  }
}

double StEpdEpFinder::GetPsiInRange(double Qx, double Qy, int order){
  double temp;
  if ((Qx==0.0)||(Qy==0.0)) temp=-999;
  else{
    temp = atan2(Qy,Qx)/((double)order);
    double AngleWrapAround = 2.0*TMath::Pi()/(double)order;
    if (temp<0.0) temp+= AngleWrapAround;
    else if (temp>AngleWrapAround) temp -= AngleWrapAround;
  }
  return temp;
}


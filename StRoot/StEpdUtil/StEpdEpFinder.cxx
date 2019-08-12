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

ClassImp(StEpdEpFinder)

StEpdEpFinder::StEpdEpFinder(int nEventTypeBins, char const* OutFileName, char const* CorrectionFile) : mFormatUsed(2), mThresh(0.3), mMax(2.0), mWeightingScheme(0)
{

  cout << "\n**********\n*  Welcome to the EPD Event Plane finder.\n"
       << "*  Please note that when you specify 'order' as an argument to a method,\n"
       << "*  then 1=first-order plane, 2=second-order plane, etc.\n"
       << "*  This code is currently configured to go up to order=" << _EpOrderMax << "\n"
       << "*  To include higher orders, edit StEpdUtil/StEpdEpInfo.h\n**********\n";


  mNumberOfEventTypeBins = nEventTypeBins;

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
    for (int ew=0; ew<2; ew++){
      for (int order=1; order<_EpOrderMax+1; order++){
	mEpdShiftInput_sin[ew][order-1] = (TProfile2D*)mCorrectionInputFile->Get(Form("EpdShiftEW%dPsi%d_sin",ew,order));
	mEpdShiftInput_cos[ew][order-1] = (TProfile2D*)mCorrectionInputFile->Get(Form("EpdShiftEW%dPsi%d_cos",ew,order));
      }
      mPhiWeightInput[ew] = (TH3D*)mCorrectionInputFile->Get(Form("PhiWeightEW%d",ew));
    }
    for (int order=1; order<_EpOrderMax+1; order++){
      mEpdShiftInput_sin[2][order-1] = (TProfile2D*)mCorrectionInputFile->Get(Form("EpdShiftFullEventPsi%d_sin",order));
      mEpdShiftInput_cos[2][order-1] = (TProfile2D*)mCorrectionInputFile->Get(Form("EpdShiftFullEventPsi%d_cos",order));
    }
  }
  // ----------------------------------- Stuff written to the "Correction File" -----------------------------------------                                                   
  // "Shift correction" histograms that we produce and OUTPUT                                                                                                               
  mCorrectionOutputFile = new TFile(OutFileName,"RECREATE");
  for (int ew=0; ew<2; ew++){
    for (int order=1; order<_EpOrderMax+1; order++){
      mEpdShiftOutput_sin[ew][order-1] = new TProfile2D(Form("EpdShiftEW%dPsi%d_sin",ew,order),Form("EpdShiftEW%dPsi%d_sin",ew,order),
							_EpTermsMax,0.5,1.0*_EpTermsMax+.5,nEventTypeBins,-0.5,(double)nEventTypeBins-0.5,-1.0,1.0);
      mEpdShiftOutput_cos[ew][order-1] = new TProfile2D(Form("EpdShiftEW%dPsi%d_cos",ew,order),Form("EpdShiftEW%dPsi%d_cos",ew,order),
							_EpTermsMax,0.5,1.0*_EpTermsMax+.5,nEventTypeBins,-0.5,(double)nEventTypeBins-0.5,-1.0,1.0);
    }
    mPhiWeightOutput[ew]   = new TH3D(Form("PhiWeightEW%d",ew),Form("Tile Weight divided by Averaged EW=%d",ew),12,0.5,12.5,31,0.5,31.5,nEventTypeBins,-0.5,(double)nEventTypeBins-0.5); // bins are PP,TT,EventType
    mPhiAveraged[ew]       = new TH3D(Form("PhiAveraged%d",ew),Form("Average for this phi EW=%d",ew),12,0.5,12.5,31,0.5,31.5,nEventTypeBins,-0.5,(double)nEventTypeBins-0.5); // just for normalization. discard after use
  }
  for (int order=1; order<_EpOrderMax+1; order++){
    mEpdShiftOutput_sin[2][order-1] = new TProfile2D(Form("EpdShiftFullEventPsi%d_sin",order),
						     Form("EpdShiftFullEventPsi%d_sin",order),
						     _EpTermsMax,0.5,1.0*_EpTermsMax+.5,nEventTypeBins,-0.5,(double)nEventTypeBins-0.5,-1.0,1.0);
    mEpdShiftOutput_cos[2][order-1] = new TProfile2D(Form("EpdShiftFullEventPsi%d_cos",order),
						     Form("EpdShiftFullEventPsi%d_cos",order),
						     _EpTermsMax,0.5,1.0*_EpTermsMax+.5,nEventTypeBins,-0.5,(double)nEventTypeBins-0.5,-1.0,1.0);
  }

  // now stuff for the "Resolution File"
  TString ResolutionFileName = "Resolution_";
  ResolutionFileName += OutFileName;
  mResolutionOutputFile = new TFile(ResolutionFileName,"RECREATE");
  for (int order=1; order<=_EpOrderMax; order++){
    mAveCosDeltaPsi[order-1] = new TProfile(Form("AveCosDeltaPsi%d",order),
					    Form("#LT cos(%d#Psi_{E,%d}-%d#Psi_{W,%d} #GT (weighted and shifted)",order,order,order,order),
					    nEventTypeBins,-0.5,(double)nEventTypeBins-0.5,-1.0,1.0);
  }
}
//------------------------------------------------------------------------------------------------------------
void StEpdEpFinder::SetEtaWeights(int order, TH2D EtaWeights){
  if (OrderOutsideRange(order)){
    cout << "Order outside range - Ignoring your eta weights\n";
    return;
  }
  if (EtaWeights.GetYaxis()->GetNbins() != mNumberOfEventTypeBins){
    cout << "Wrong number of EventType bins - Ignoring your eta weights\n";
    return;
  }
  mWeightingScheme = 1;
  mEtaWeights[order-1] = new TH2D;
  EtaWeights.Copy(*mEtaWeights[order-1]);
}

void StEpdEpFinder::SetRingWeights(int order, double* RingWeights){
  if (OrderOutsideRange(order)){
    cout << "Ignoring your ring weights\n";
    return;
  }
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

//==================================================================================================================
StEpdEpInfo StEpdEpFinder::Results(TClonesArray* EpdHits, TVector3 primVertex, int EventTypeId){

  if ((EventTypeId<0)||(EventTypeId>=mNumberOfEventTypeBins)){
    cout << "You are asking for an undefined EventType - fail!\n";
    assert(0);
  }

  StEpdEpInfo result;

  double pi = TMath::Pi();


  // This below is for normalizing Q-vectors
  double TotalWeight4Ring[2][16][2];                // for normalizing Q-vector: indices EW, ring,  (nonPhiWeighted or PhiWeighted)
  double TotalWeight4Side[2][_EpOrderMax][2];       // for normalizing Q-vector: indices EW, order, (nonPhiWeighted or PhiWeighted)  ** depends on Order because eta-weight depends on order
  for (int iew=0; iew<2; iew++){
    for (int phiWeightedOrNo=0; phiWeightedOrNo<2; phiWeightedOrNo++){
      for (int order=1; order<_EpOrderMax+1; order++){
	TotalWeight4Side[iew][order-1][phiWeightedOrNo] = 0;
      }
      for (int iring=0; iring<16; iring++){
	TotalWeight4Ring[iew][iring][phiWeightedOrNo] = 0;
      }
    }
  }


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
	nMip = epdHit->nMIP();   // malisa 20feb2019 - I have finally made the transition from ADC (next line) to truly nMip, now that calibrations are done.
	//	nMip = (TT<10)?(double)ADC/160.0:(double)ADC/115.0;
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
    mPhiWeightOutput[EW]->Fill(PP,TT,EventTypeId,TileWeight);
    if (TT==1){ for (int pp=1; pp<13; pp++) mPhiAveraged[EW]->Fill(pp,1,EventTypeId,TileWeight/12.0);}
    else{
      for (int pp=1; pp<13; pp++){
	for (int tt=2*(ring-1); tt<2*ring; tt++) mPhiAveraged[EW]->Fill(pp,tt,EventTypeId,TileWeight/24.0);
      }
    }
    //--------------------------------
    // now calculate Q-vectors
    //--------------------------------

    double PhiWeightedTileWeight = TileWeight;
    if (mPhiWeightInput[EW]) PhiWeightedTileWeight /= mPhiWeightInput[EW]->GetBinContent(PP,TT,EventTypeId);
    TotalWeight4Ring[EW][ring-1][0] += TileWeight;
    TotalWeight4Ring[EW][ring-1][1] += PhiWeightedTileWeight;

    for (int order=1; order<_EpOrderMax+1; order++){
      double etaWeight = RingOrEtaWeight(ring,eta,order,EventTypeId);
      TotalWeight4Side[EW][order-1][0] += fabs(etaWeight) * TileWeight;             // yes the fabs() makes sense.  The sign in the eta weight is equivalent to a trigonometric phase.
      TotalWeight4Side[EW][order-1][1] += fabs(etaWeight) * PhiWeightedTileWeight;  // yes the fabs() makes sense.  The sign in the eta weight is equivalent to a trigonometric phase.

      double Cosine = cos(phi*(double)order);
      double Sine   = sin(phi*(double)order);

      result.QrawOneSide[EW][order-1][0]      += etaWeight * TileWeight * Cosine;
      result.QrawOneSide[EW][order-1][1]      += etaWeight * TileWeight * Sine;
      result.QringRaw[EW][order-1][0][ring-1] += TileWeight * Cosine;
      result.QringRaw[EW][order-1][1][ring-1] += TileWeight * Sine;

      result.QphiWeightedOneSide[EW][order-1][0]      += etaWeight * PhiWeightedTileWeight * Cosine;
      result.QphiWeightedOneSide[EW][order-1][1]      += etaWeight * PhiWeightedTileWeight * Sine;
      result.QringPhiWeighted[EW][order-1][0][ring-1] += PhiWeightedTileWeight * Cosine;
      result.QringPhiWeighted[EW][order-1][1][ring-1] += PhiWeightedTileWeight * Sine;
    }
  }  // loop over hits
  //--------------------------------- end loop over hits ---------------------------------------

  // Xiaoyu needs the weights used for each ring, so she can "un-normalize" the ring-by-ring Q-vectors.  Okay...
  for (int iew=0; iew<2; iew++){
    for (int iring=1; iring<17; iring++){
      result.RingSumWeightsRaw[iew][iring-1]         = TotalWeight4Ring[iew][iring-1][0];
      result.RingSumWeightsPhiWeighted[iew][iring-1] = TotalWeight4Ring[iew][iring-1][1];
    }
    for (int order=1; order<_EpOrderMax+1; order++){
      result.WheelSumWeightsRaw[iew][order-1]         = TotalWeight4Side[iew][order-1][0];
      result.WheelSumWeightsPhiWeighted[iew][order-1] = TotalWeight4Side[iew][order-1][1];
    }
  }


  // at this point, we are finished with the detector hits, and deal only with the Q-vectors,

  // first, normalize the Q's...
  for (int EW=0; EW<2; EW++){
    for (int order=1; order<_EpOrderMax+1; order++){
      if (TotalWeight4Side[EW][order-1][0]>0.0001){
	result.QrawOneSide[EW][order-1][0] /= TotalWeight4Side[EW][order-1][0];
	result.QrawOneSide[EW][order-1][1] /= TotalWeight4Side[EW][order-1][0];
      }
      if (TotalWeight4Side[EW][order-1][1]>0.0001){
	result.QphiWeightedOneSide[EW][order-1][0] /= TotalWeight4Side[EW][order-1][1];
	result.QphiWeightedOneSide[EW][order-1][1] /= TotalWeight4Side[EW][order-1][1];
      }
      for (int ring=0; ring<16; ring++){
	if (TotalWeight4Ring[EW][ring][0]>0.001){
	  result.QringRaw[EW][order-1][0][ring] /= TotalWeight4Ring[EW][ring][0];
	  result.QringRaw[EW][order-1][1][ring] /= TotalWeight4Ring[EW][ring][0];
	}
	if (TotalWeight4Ring[EW][ring][1]>0.001){
	  result.QringPhiWeighted[EW][order-1][0][ring] /= TotalWeight4Ring[EW][ring][1];
	  result.QringPhiWeighted[EW][order-1][1][ring] /= TotalWeight4Ring[EW][ring][1];
	}
      }
    }
  }

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

  // at this point, we are finished with the Q-vectors and just use them to get angles Psi

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
	  double sinAve = mEpdShiftInput_sin[ewFull][order-1]->GetBinContent(i,EventTypeId+1);    /// note the "+1" since EventTypeId begins at zero
	  double cosAve = mEpdShiftInput_cos[ewFull][order-1]->GetBinContent(i,EventTypeId+1);    /// note the "+1" since EventTypeId begins at zero
	  result.PsiPhiWeightedAndShifted[ewFull][order-1] +=
	    2.0*(cosAve*sin(tmp*result.PsiPhiWeighted[ewFull][order-1]) - sinAve*cos(tmp*result.PsiPhiWeighted[ewFull][order-1]))/tmp;
	}
	double AngleWrapAround = 2.0*pi/(double)order;
	if (result.PsiPhiWeightedAndShifted[ewFull][order-1]<0) result.PsiPhiWeightedAndShifted[ewFull][order-1] += AngleWrapAround;
	else if (result.PsiPhiWeightedAndShifted[ewFull][order-1]>AngleWrapAround) result.PsiPhiWeightedAndShifted[ewFull][order-1] -= AngleWrapAround;
      }
    }
  }

  // for the Resolutions file.
  for (int order=1; order<=_EpOrderMax; order++){
    mAveCosDeltaPsi[order-1]->Fill((double)EventTypeId,
				   cos(((double)order)*(result.PsiPhiWeightedAndShifted[0][order-1]-result.PsiPhiWeightedAndShifted[1][order-1])));
  }

  //---------------------------------
  // Now calculate shift histograms for a FUTURE run (if you want it)
  //---------------------------------
  for (int i=1; i<=_EpTermsMax; i++){
    for (int ewFull=0; ewFull<3; ewFull++){
      for (int order=1; order<_EpOrderMax+1; order++){
	double tmp = (double)(order*i);
	mEpdShiftOutput_sin[ewFull][order-1]->Fill(i,EventTypeId,sin(tmp*result.PsiPhiWeighted[ewFull][order-1]));
	mEpdShiftOutput_cos[ewFull][order-1]->Fill(i,EventTypeId,cos(tmp*result.PsiPhiWeighted[ewFull][order-1]));
      }
    }
  }

  return result;
}

//==================================================================================================================
double StEpdEpFinder::RingOrEtaWeight(int ring, double eta, int order, int EventTypeId){
  switch (mWeightingScheme){
  case 0:                                         // no weighting
    return 1.0;
    break;
  case 1:                                         // eta-based weights
    { // must explicitly scope with {} if there is a declaration statement in a case
      if (mEtaWeights[order-1]==0) return 1.0;      // user never defined weights for this order
      int etaBin = mEtaWeights[order-1]->GetXaxis()->FindBin(fabs(eta));
      return mEtaWeights[order-1]->GetBinContent(etaBin,EventTypeId+1);      // note the "+1" since EventTypeId starts at zero
      break;
    }
  case 2:                                         // ring-based weights
    return mRingWeights[ring-1][order-1];
    break;
  default:
    return 1.0;
  }
}

//==================================================================================================================
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

//==================================================================================================================
bool StEpdEpFinder::OrderOutsideRange(int order){
  if (order < 1) {
    cout << "\n*** Invalid order specified ***\n";
    cout << "order must be 1 (for first-order plane) or higher\n";
    return true;
  }
  if (order > _EpOrderMax) {
    cout << "\n*** Invalid order specified ***\n";
    cout << "Maximum order=" << _EpOrderMax << ". To change, edit StEpdUtil/StEpdEpInfo.h\n";
    return true;
  }
  return false;
}


//==================================================================================================================
TString StEpdEpFinder::Report(){
  TString rep = Form("This is the StEpdEpFinder Report:\n");
  rep += Form("Number of EventType bins = %d\n",mNumberOfEventTypeBins);
  rep += Form("Format of input data = %d  [note: 0=StEvent, 1=StMuDst, 2=StPicoDst]\n",mFormatUsed);
  rep += Form("Threshold (in MipMPV units) = %f  and MAX weight = %f\n",mThresh,mMax);
  rep += Form("Weighting scheme used=%d  [note: 0=none, 1=eta-based weighting, 2=ring-based weighting]\n",mWeightingScheme);
  if (mWeightingScheme==1){
    for (int order=1; order<_EpOrderMax+1; order++){
      if (mEtaWeights[order-1]==0){
        rep += Form("No eta weighing for order n=%d\n",order);
      }
      else{
        rep += Form("Weights for order %d  W[j] means weight for EventType bin j\n",order);
        rep += Form("eta");
	for (int EventTypeId=0; EventTypeId<mNumberOfEventTypeBins; EventTypeId++){rep += Form("\t\tW[%d]",EventTypeId);}
        rep += Form("\n");
        for (int iEtaBin=1; iEtaBin<=mEtaWeights[order-1]->GetXaxis()->GetNbins(); iEtaBin++){
          rep += Form("%f",mEtaWeights[order-1]->GetXaxis()->GetBinCenter(iEtaBin));
	  for (int EventTypeId=0; EventTypeId<mEtaWeights[order-1]->GetYaxis()->GetNbins(); EventTypeId++){rep += Form("\t%f",mEtaWeights[order-1]->GetBinContent(iEtaBin,EventTypeId+1));}
          rep += Form("\n");
        }
      }
    }
  }
  else{
    if (mWeightingScheme==2){
      rep += Form("Here is the weighting: W[n] is weight for EP of order n\n");
      rep += Form("ring");
      for (int order=1; order<=_EpOrderMax; order++){rep += Form("\tW[%d]",order);}
      rep += Form("\n");
      for (int iRing=1; iRing<=16; iRing++){
        rep += Form("%d",iRing);
        for (int order=1; order<=_EpOrderMax; order++){rep += Form("\t%f",mRingWeights[iRing-1][order-1]);}
        rep += Form("\n");
      }
    }
  }
  return rep;
}


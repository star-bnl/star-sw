/***************************************************************************
 *
 * $Id: BPLCMSFrame3DCorrFctn.cxx,v 1.7 2003/01/31 19:20:54 magestro Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   3D Bertsch-Pratt decomposition in the LCMS frame
 *
 ***************************************************************************
 *
 * $Log: BPLCMSFrame3DCorrFctn.cxx,v $
 * Revision 1.7  2003/01/31 19:20:54  magestro
 * Cleared up simple compiler warnings on i386_linux24
 *
 * Revision 1.6  2002/06/07 22:51:38  lisa
 * Widely used BPLCMSFrame3DCorrFctn class now accumulates UNcorrected denominator and has a WriteOutHistos method
 *
 * Revision 1.5  2001/05/23 00:19:04  lisa
 * Add in Smearing classes and methods needed for momentum resolution studies and correction
 *
 * Revision 1.4  2000/10/26 19:48:49  rcwells
 * Added functionality for Coulomb correction of <qInv> in 3D correltions
 *
 * Revision 1.3  2000/09/14 18:36:53  lisa
 * Added Qinv and ExitSep pair cuts and BPLCMSFrame3DCorrFctn_SIM CorrFctn
 *
 * Revision 1.2  2000/08/23 19:43:43  lisa
 * added alternate normalization algorithm to 3d CorrFctns in case normal one fails
 *
 * Revision 1.1  2000/08/17 20:48:39  lisa
 * Adding correlationfunction in LCMS frame
 *
 *
 **************************************************************************/

#include "StHbtMaker/CorrFctn/BPLCMSFrame3DCorrFctn.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(BPLCMSFrame3DCorrFctn)
#endif

//____________________________
BPLCMSFrame3DCorrFctn::BPLCMSFrame3DCorrFctn(char* title, const int& nbins, const float& QLo, const float& QHi){

  // set some stuff...
  mQinvNormLo = 0.15;
  mQinvNormHi = 0.18;
  mNumRealsNorm = 0;
  mNumMixedNorm = 0;
  mCorrection = 0;  // pointer to Coulomb Correction object

  mPairCut = 0; // added Sept2000 - CorrFctn-specific PairCut

  mSmearPair = 0; // no resolution correction unless user sets SmearPair

  // set up numerator
  char TitNum[100] = "Num";
  strcat(TitNum,title);
  mNumerator = new StHbt3DHisto(TitNum,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  // set up denominator
  char TitDen[100] = "Den";
  strcat(TitDen,title);
  mDenominator = new StHbt3DHisto(TitDen,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  // set up uncorrected denominator
  char TitDenUncoul[100] = "DenNoCoul";
  strcat(TitDenUncoul,title);
  mUncorrectedDenominator = new StHbt3DHisto(TitDenUncoul,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  // set up ratio
  char TitRat[100] = "Rat";
  strcat(TitRat,title);
  mRatio = new StHbt3DHisto(TitRat,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  // set up ave qInv
  char TitQinv[100] = "Qinv";
  strcat(TitQinv,title);
  mQinvHisto = new StHbt3DHisto(TitQinv,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);

  // to enable error bar calculation...
  mNumerator->Sumw2();
  mDenominator->Sumw2();
  mUncorrectedDenominator->Sumw2();
  mRatio->Sumw2();

  // Following histos are for the momentum resolution correction
  // they are filled only if a StHbtSmear object is plugged in
  // here comes the "idea" numerator and denominator and ratio...
  char TitNumID[100] = "IDNum";
  strcat(TitNumID,title);
  mIDNumHisto = new StHbt3DHisto(TitNumID,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  char TitDenID[100] = "IDDen";
  strcat(TitDenID,title);
  mIDDenHisto = new StHbt3DHisto(TitDenID,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  char TitRatID[100] = "IDRat";
  strcat(TitRatID,title);
  mIDRatHisto = new StHbt3DHisto(TitRatID,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);

  mIDNumHisto->Sumw2();
  mIDDenHisto->Sumw2();
  mIDRatHisto->Sumw2();

  //
  // here comes the "smeared" numerator and denominator...
  char TitNumSM[100] = "SMNum";
  strcat(TitNumSM,title);
  mSMNumHisto = new StHbt3DHisto(TitNumSM,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  char TitDenSM[100] = "SMDen";
  strcat(TitDenSM,title);
  mSMDenHisto = new StHbt3DHisto(TitDenSM,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  char TitRatSM[100] = "SMRat";
  strcat(TitRatSM,title);
  mSMRatHisto = new StHbt3DHisto(TitRatSM,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  //
  mSMNumHisto->Sumw2();
  mSMDenHisto->Sumw2();
  mSMRatHisto->Sumw2();
  //
  // here comes the correction factor (which is just ratio of ideal ratio to smeared ratio)
  char TitCorrection[100] = "CorrectionFactor";
  strcat(TitCorrection,title);
  mCorrectionHisto = new StHbt3DHisto(TitCorrection,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);  
  mCorrectionHisto->Sumw2();
  // here comes the fully corrected correlation function
  char TitCorrCF[100] = "CorrectedCF";
  strcat(TitCorrCF,title);
  mCorrCFHisto = new StHbt3DHisto(TitCorrCF,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  mCorrCFHisto->Sumw2();

  // user can (and should) override these defaults...
  mLambda = 0.6;
  mRout2 = 6.0*6.0;
  mRside2 = 6.0*6.0;
  mRlong2 = 7.0*7.0;

}

//____________________________
BPLCMSFrame3DCorrFctn::~BPLCMSFrame3DCorrFctn(){
  delete mNumerator;
  delete mDenominator;
  delete mRatio;
  delete mQinvHisto;
  delete mIDNumHisto;
  delete mIDDenHisto;
  delete mIDRatHisto;
  delete mSMNumHisto;
  delete mSMDenHisto;
  delete mSMRatHisto;
  delete mCorrectionHisto;
  delete mCorrCFHisto;
}

//_________________________
void BPLCMSFrame3DCorrFctn::WriteOutHistos(){

  mNumerator->Write();
  mDenominator->Write();
  mUncorrectedDenominator->Write();
  mRatio->Write();
  mQinvHisto->Write();

  if (mSmearPair){
    mIDNumHisto->Write();
    mIDDenHisto->Write();
    mIDRatHisto->Write();
    //
    mSMNumHisto->Write();
    mSMDenHisto->Write();
    mSMRatHisto->Write();
    //
    mCorrectionHisto->Write();
    mCorrCFHisto->Write();
  }
}

//_________________________
void BPLCMSFrame3DCorrFctn::Finish(){
  // here is where we should normalize, fit, etc...
  double NumFact,DenFact;
  if ((mNumRealsNorm !=0) && (mNumMixedNorm !=0)){
    NumFact = double(mNumRealsNorm);
    DenFact = double(mNumMixedNorm);
  }
  // can happen that the mNumRealsNorm and mNumMixedNorm = 0 if you do non-standard
  //   things like making a new CorrFctn and just setting the Numerator and Denominator
  //   from OTHER CorrFctns which you read in (like when doing parallel processing) 
  else{
    cout << "Warning! - no normalization constants defined - I do the best I can..." << endl;
    int nbins = mNumerator->GetNbinsX();
    int half_way = nbins/2;
    NumFact = mNumerator->Integral(half_way,nbins,half_way,nbins,half_way,nbins);
    DenFact = mDenominator->Integral(half_way,nbins,half_way,nbins,half_way,nbins);
  }

  mRatio->Divide(mNumerator,mDenominator,DenFact,NumFact);
  mQinvHisto->Divide(mUncorrectedDenominator);

  // now do all the resolution correction stuff..
  if (mSmearPair){  // but only do it if we have been working with a SmearPair
    mIDRatHisto->Divide(mIDNumHisto,mIDDenHisto);
    mSMRatHisto->Divide(mSMNumHisto,mSMDenHisto);
    mCorrectionHisto->Divide(mIDRatHisto,mSMRatHisto);
    mCorrCFHisto->Multiply(mRatio,mCorrectionHisto);
  }

}

//____________________________
StHbtString BPLCMSFrame3DCorrFctn::Report(){
  string stemp = "LCMS Frame Bertsch-Pratt 3D Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenominator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in ratio:\t%E\n",mRatio->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Normalization region in Qinv was:\t%E\t%E\n",mQinvNormLo,mQinvNormHi);
  stemp += ctemp;
  sprintf(ctemp,"Number of pairs in Normalization region was:\n");
  stemp += ctemp;
  sprintf(ctemp,"In numerator:\t%lu\t In denominator:\t%lu\n",mNumRealsNorm,mNumMixedNorm);
  stemp += ctemp;
  if (mCorrection)
    {
      float radius = mCorrection->GetRadius();
      sprintf(ctemp,"Coulomb correction used radius of\t%E\n",radius);
    }
  else
    {
      sprintf(ctemp,"No Coulomb Correction applied to this CorrFctn\n");
    }
  stemp += ctemp;

  if (mPairCut){
    sprintf(ctemp,"Here is the PairCut specific to this CorrFctn\n");
    stemp += ctemp;
    stemp += mPairCut->Report();
  }
  else{
    sprintf(ctemp,"No PairCut specific to this CorrFctn\n");
    stemp += ctemp;
  }

  //  
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void BPLCMSFrame3DCorrFctn::AddRealPair(const StHbtPair* pair){

  if (mPairCut){
    if (!(mPairCut->Pass(pair))) return;
  }

  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...
  if ((Qinv < mQinvNormHi) && (Qinv > mQinvNormLo)) mNumRealsNorm++;
  double qOut = fabs(pair->qOutCMS());
  double qSide = fabs(pair->qSideCMS());
  double qLong = fabs(pair->qLongCMS());

  mNumerator->Fill(qOut,qSide,qLong);
}
//____________________________
void BPLCMSFrame3DCorrFctn::AddMixedPair(const StHbtPair* pair){

  if (mPairCut){
    if (!(mPairCut->Pass(pair))) return;
  }

  double CoulombWeight = (mCorrection ? mCorrection->CoulombCorrect(pair) : 1.0);

  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...
  if ((Qinv < mQinvNormHi) && (Qinv > mQinvNormLo)) mNumMixedNorm++;
  double qOut = fabs(pair->qOutCMS());
  double qSide = fabs(pair->qSideCMS());
  double qLong = fabs(pair->qLongCMS());

  mDenominator->Fill(qOut,qSide,qLong,CoulombWeight);
  mUncorrectedDenominator->Fill(qOut,qSide,qLong,1.0);
  mQinvHisto->Fill(qOut,qSide,qLong,Qinv);

  // now for the momentum resolution stuff...
  if (mSmearPair){
    double CorrWeight =  1.0 + 
      mLambda*exp((-qOut*qOut*mRout2 -qSide*qSide*mRside2 -qLong*qLong*mRlong2)/0.038936366329);
    CorrWeight *= CoulombWeight;  // impt.

    mIDNumHisto->Fill(qOut,qSide,qLong,CorrWeight);
    mIDDenHisto->Fill(qOut,qSide,qLong,CoulombWeight);

    mSmearPair->SetUnsmearedPair(pair);
    double qOut_prime = fabs(mSmearPair->SmearedPair().qOutCMS());
    double qSide_prime = fabs(mSmearPair->SmearedPair().qSideCMS());
    double qLong_prime = fabs(mSmearPair->SmearedPair().qLongCMS());

    mSMNumHisto->Fill(qOut_prime,qSide_prime,qLong_prime,CorrWeight);

    double SmearedCoulombWeight = ( mCorrection ? 
				    mCorrection->CoulombCorrect(&(mSmearPair->SmearedPair())) : 
				    1.0);

    mSMDenHisto->Fill(qOut_prime,qSide_prime,qLong_prime,SmearedCoulombWeight);
  }
}


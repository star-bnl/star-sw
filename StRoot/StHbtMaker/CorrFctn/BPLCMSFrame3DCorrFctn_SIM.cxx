/***************************************************************************
 *
 * $Id: BPLCMSFrame3DCorrFctn_SIM.cxx,v 1.6 2003/09/02 17:58:20 perev Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   3D Bertsch-Pratt decomposition in the LCMS frame
 *   THIS IS A SIMULATION CORRELATION FUNCTION CLASS !!!
 *
 *
 ***************************************************************************
 *
 * $Log: BPLCMSFrame3DCorrFctn_SIM.cxx,v $
 * Revision 1.6  2003/09/02 17:58:20  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.5  2003/01/31 19:21:09  magestro
 * Cleared up simple compiler warnings on i386_linux24
 *
 * Revision 1.4  2001/05/23 00:19:05  lisa
 * Add in Smearing classes and methods needed for momentum resolution studies and correction
 *
 * Revision 1.3  2000/10/08 17:11:07  lisa
 * now toggle between Num and Den at BEGINNING of BPLCMSFrame3DCorrFctn_SIM::AddMixedPair()
 *
 * Revision 1.2  2000/10/05 23:08:59  lisa
 * Added kT-dependent radii to mixed-event simulator AND implemented AverageSeparation Cut and CorrFctn
 *
 * Revision 1.1  2000/09/14 18:36:53  lisa
 * Added Qinv and ExitSep pair cuts and BPLCMSFrame3DCorrFctn_SIM CorrFctn
 *
 *
 *
 **************************************************************************/


#include "StHbtMaker/CorrFctn/BPLCMSFrame3DCorrFctn_SIM.h"
#include "StHbtMaker/Infrastructure/StHbtSmearPair.h"

#include <cstdio>

#ifdef __ROOT__ 
ClassImp(BPLCMSFrame3DCorrFctn_SIM)
#endif

//____________________________
BPLCMSFrame3DCorrFctn_SIM::BPLCMSFrame3DCorrFctn_SIM(char* title, const int& nbins, const float& QLo, const float& QHi){

  // set some stuff...
  mQinvNormLo = 0.15;
  mQinvNormHi = 0.18;
  mNumRealsNorm = 0;
  mNumMixedNorm = 0;
  mCorrection = 0;  // pointer to Coulomb Correction object

  mPairCut = 0; // added Sept2000 - CorrFctn-specific PairCut

  mToggleNumDen = 0; // this toggles between a pair being given to numerator or denominator.

  mLambda=0.;
  mRside2=25.0;
  mRout2=25.0;
  mRlong2=25.0;

  mRout_alpha = mRside_alpha = mRlong_alpha = 0;  // set these zero by default

  // set up numerator
  char TitNum[100] = "Num";
  strcat(TitNum,title);
  mNumerator = new StHbt3DHisto(TitNum,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  // set up denominator
  char TitDen[100] = "Den";
  strcat(TitDen,title);
  mDenominator = new StHbt3DHisto(TitDen,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
  // set up ratio
  char TitRat[100] = "Rat";
  strcat(TitRat,title);
  mRatio = new StHbt3DHisto(TitRat,title,nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);

  // to enable error bar calculation...
  mNumerator->Sumw2();
  mDenominator->Sumw2();
  mRatio->Sumw2();

  // 20feb2001 - add some more histograms that are filled if there is any
  //             momentum smearing
  
  char titInv[100] = "QinvRes";
  strcat(titInv,title);
  mResolutionHistos[0] = new StHbt2DHisto(titInv,title,10,0.0,0.1,50,-0.025,0.025);
  //
  char titOut[100] = "QoutRes";
  strcat(titOut,title);
  mResolutionHistos[1] = new StHbt2DHisto(titOut,title,10,0.0,0.1,50,-0.025,0.025);
  //
  char titSid[100] = "QsidRes";
  strcat(titSid,title);
  mResolutionHistos[2] = new StHbt2DHisto(titSid,title,10,0.0,0.1,50,-0.025,0.025);
  //
  char titLon[100] = "QlonRes";
  strcat(titLon,title);
  mResolutionHistos[3] = new StHbt2DHisto(titLon,title,10,0.0,0.1,50,-0.025,0.025);

}

//____________________________
BPLCMSFrame3DCorrFctn_SIM::~BPLCMSFrame3DCorrFctn_SIM(){
  delete mNumerator;
  delete mDenominator;
  delete mRatio;
}
//_________________________
void BPLCMSFrame3DCorrFctn_SIM::Finish(){

  mRatio->Divide(mNumerator,mDenominator);
}

//____________________________
StHbtString BPLCMSFrame3DCorrFctn_SIM::Report(){
  string stemp = "LCMS Frame Bertsch-Pratt 3D Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"    THIS IS A SIMULATION CORRELATION FUNCTION CLASS!!!\n");
  stemp += ctemp;
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
void BPLCMSFrame3DCorrFctn_SIM::AddRealPair(const StHbtPair* pair){

  // note that in this SIMULATION correlation function class, we do NOTHING with real
  // pairs.  The numerator is just a weighted denominator, filled in AddMixedPair (below)

  /* no-op */
  return;

}




//____________________________
void BPLCMSFrame3DCorrFctn_SIM::AddMixedPair(const StHbtPair* pair){

  mToggleNumDen = !mToggleNumDen;  // toggle at the BEGINNING of call instead of end
                                   // so that SAME pairs go into Num and Den as long as they pass cut


  if (mPairCut){
    if (!(mPairCut->Pass(pair))){
//       // mike is checking stuff here
//       double qOut = fabs(pair->qOutCMS());
//       double qSide = fabs(pair->qSideCMS());
//       double qLong = fabs(pair->qLongCMS());
//       //      if ((qSide > 0.035)||(qLong > 0.035)){
//       if ((qSide > 0.0)||(qLong > 0.0)){
// 	cout << qOut << " " << qSide << " " << qLong << " " << 
// 	  pair->NominalTpcEntranceSeparation() << " " <<
// 	  pair->track1()->NominalTpcEntrancePoint() << " " <<
// 	  pair->track2()->NominalTpcEntrancePoint() << endl;
// 	int ijunk;
// 	cout << "Enter a junk integer : ";
// 	cin >> ijunk;
//       }
//      // end of checking stuff
      return;
    }
  }

  double weight=1.0;

  // these are the values we use to assign correlation WEIGHT
  double qOut = fabs(pair->qOutCMS());
  double qSide = fabs(pair->qSideCMS());
  double qLong = fabs(pair->qLongCMS());

  // these are the values we BIN in - same as above if no smearing turned on...
  double qOut_bin,qSide_bin,qLong_bin;  // scope 'em!
  if (mSmearPair){
    mSmearPair->SetUnsmearedPair(pair);
    qOut_bin = fabs(mSmearPair->SmearedPair().qOutCMS());
    qSide_bin = fabs(mSmearPair->SmearedPair().qSideCMS());
    qLong_bin = fabs(mSmearPair->SmearedPair().qLongCMS());
    // fill resolution histos...
    double Qinv = fabs(pair->qInv());
    if (Qinv < 0.1){
      double Qinv_smear = fabs(mSmearPair->SmearedPair().qInv());
      mResolutionHistos[0]->Fill(Qinv,Qinv_smear-Qinv);
      mResolutionHistos[1]->Fill(qOut,qOut_bin-qOut);
      mResolutionHistos[2]->Fill(qSide,qSide_bin-qSide);
      mResolutionHistos[3]->Fill(qLong,qLong_bin-qLong);
    }
  }
  else{
    qOut_bin  = qOut;
    qSide_bin = qSide;
    qLong_bin = qLong;
  }


  // note the Coulomb bit below...
  // for the *suppression* ("numerator" pairs), we use the TRUE momentum, as Nature would
  // for the *correction* ("denominator" pairs), we use the SMEARED momentum, as an experimenter would

  if (mToggleNumDen){
    if (mCorrection){
      if (mSmearPair){
	weight = mCorrection->CoulombCorrect(&(mSmearPair->SmearedPair()));
      }
      else{
	weight = mCorrection->CoulombCorrect(pair);
      }
    }
    mDenominator->Fill(qOut_bin,qSide_bin,qLong_bin,weight);
  }
  else{

    if (mCorrection) weight = mCorrection->CoulombCorrect(pair);

    // update 1oct2000 - now allow mT dependent evolution of Radii

    float mT2 = (pair->fourMomentumSum().mt2())/4.0;  // divide by 4.0 because want mT of (p1+p2)/2 
                                                       // and factor of two gets squared in mt2

    float Rout2,Rside2,Rlong2;

    if (mRout_alpha!=0){Rout2 = mRout2*::pow(mT2,mRout_alpha);}
    else{Rout2 = mRout2;}

    if (mRside_alpha!=0){Rside2 = mRside2*::pow(mT2,mRside_alpha);}
    else{Rside2 = mRside2;}

    if (mRlong_alpha!=0){Rlong2 = mRlong2*::pow(mT2,mRlong_alpha);}
    else{Rlong2 = mRlong2;}
      
    double CorrWeight = 1.0 + 
      mLambda*exp((-qOut*qOut*Rout2 -qSide*qSide*Rside2 -qLong*qLong*Rlong2)/0.038936366329);
    CorrWeight *= weight;
    mNumerator->Fill(qOut_bin,qSide_bin,qLong_bin,CorrWeight);
  }

}




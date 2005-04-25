#include "StHbtMaker/CorrFctn/QoslCMSCorrFctnRPkT.h"
#include <cstdio>
#include "StHbtMaker/Infrastructure/StHbtReactionPlaneAnalysis.h"
#include "PhysicalConstants.h"
#include "TString.h"
#include "TH3S.h"

#ifdef __ROOT__ 
ClassImp(QoslCMSCorrFctnRPkT)
#endif
//____________________________
QoslCMSCorrFctnRPkT::QoslCMSCorrFctnRPkT(char* title, const int& nbinso, const float& QoLo, const float& QoHi,
			   const int& nbinss, const float& QsLo, const float& QsHi,
			   const int& nbinsl, const float& QlLo, const float& QlHi, const int& rpBins){

  mCorrection = new StHbtCoulomb;
  mCorrection->SetRadius(5.0);
  mCorrection->SetChargeProduct(1.0);

	qMax = QoHi;

  nRPbins = rpBins;
  nKtBins = 4;
  
  for(int i=0; i<nRPbins; i++) {
   	TString TitAngle=Form("_phi%i",i*180/nRPbins);

   	for(int j=0; j<nKtBins; j++) {
      TString TitKt=Form("_kt%i",j);

      // set up numerator
      TString TitNum = "Num";
      TitNum += title;
      TitNum += TitAngle.Data();
      TitNum += TitKt.Data();
      mNumerator[i][j] = new TH3S(TitNum.Data(),TitNum.Data(),nbinso,QoLo,QoHi,
				    nbinss,QsLo,QsHi,
				    nbinsl,QlLo,QlHi);

      // set up denominator
      TString TitDen = "Den";
      TitDen += title;
      TitDen += TitAngle.Data();
      TitDen += TitKt.Data();
      mDenominator[i][j] = new TH3S(TitDen.Data(),TitDen.Data(),nbinso,QoLo,QoHi,
				      nbinss,QsLo,QsHi,
				      nbinsl,QlLo,QlHi);

      //// set up ave qInv
      //char TitQinv[100] = "Qinv";
      //strcat(TitQinv,title);
      //strcat(TitQinv,TitAngle.Data());
      //strcat(TitQinv,TitKt.Data());
      //mQinvHisto[i][j] = new StHbt3DHisto(TitQinv,TitQinv,nbinso,QoLo,QoHi,nbinss,QsLo,QsHi,nbinsl,QlLo,QlHi);

      // set up ave qInv
      TString TitCoul = "Coul";
      TitCoul += title;
      TitCoul += TitAngle.Data();
      TitCoul += TitKt.Data();
 	   	mCoulHisto[i][j] = new StHbt3DHisto(TitCoul.Data(),TitCoul.Data(),nbinso,QoLo,QoHi,nbinss,QsLo,QsHi,nbinsl,QlLo,QlHi);

      // to enable error bar calculation...
      //mCoulHisto[i][j]->Sumw2();

    }
  }
  
}

//____________________________
QoslCMSCorrFctnRPkT::~QoslCMSCorrFctnRPkT(){
  for(int i=0; i<nRPbins; i++) {
   for(int j=0; j<nKtBins; j++) {
    delete mNumerator[i][j];
    delete mDenominator[i][j];
    delete mCoulHisto[i][j];
   }
  }
}
//_________________________
void QoslCMSCorrFctnRPkT::Finish(){
  // here is where we should normalize, fit, etc...
//  for(int i=0; i<nRPbins; i++) {
//   for(int j=0; j<nKtBins; j++) {
//    mQinvHisto[i][j]->Divide(mDenominator[i][j]);
//   }
//  }

}

//____________________________
StHbtString QoslCMSCorrFctnRPkT::Report(){
  string stemp = "QoslCMS Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator[0][0]->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenominator[0][0]->GetEntries());
  stemp += ctemp;
  //  stemp += mCoulombWeight->Report();

  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void QoslCMSCorrFctnRPkT::AddRealPair(const StHbtPair* pair){

  int rpBin, ktBin;
  rpBin = GetRPBin(pair);
  ktBin = GetKtBin(pair);
  if(ktBin<0) return;
  
  double Qo = pair->qOutCMS();
  double Qs = pair->qSideCMS();
  double Ql = pair->qLongCMS();

	if(fabs(Qo)>qMax || fabs(Qs)>qMax || fabs(Ql)>qMax) return; 

	Ql = fabs(Ql);
  if ( Qs<0.0 ) {
		Qs*=-1.0;
		Qo*=-1.0;
  }
  mNumerator[rpBin][ktBin]->Fill(Qo,Qs,Ql);
}
//____________________________
void QoslCMSCorrFctnRPkT::AddMixedPair(const StHbtPair* pair){

  int rpBin, ktBin;
  rpBin = GetRPBin(pair);
  ktBin = GetKtBin(pair);
  if(ktBin<0) return;

  //double Qinv = fabs(pair->qInv());   

  double weight = 1.0;
  if (mCorrection) weight = mCorrection->CoulombCorrect(pair);

  double Qo = pair->qOutCMS();
  double Qs = pair->qSideCMS();
  double Ql = pair->qLongCMS();

	if(fabs(Qo)>qMax || fabs(Qs)>qMax || fabs(Ql)>qMax) return; 

	Ql = fabs(Ql);
  if ( Qs<0.0 ) {
		Qs*=-1.0;
		Qo*=-1.0;
  }
  mDenominator[rpBin][ktBin]->Fill(Qo,Qs,Ql);
  mCoulHisto[rpBin][ktBin]->Fill(Qo,Qs,Ql,weight);

}
//____________________________
int QoslCMSCorrFctnRPkT::GetRPBin(const StHbtPair* pair) {

  // Get pair angle, put it between 0 and 360
  double pxTotal = pair->fourMomentumSum().x();
  double pyTotal = pair->fourMomentumSum().y();
  double angle = atan2(pyTotal,pxTotal)*180.0/pi;
  if (angle<0.0) angle+=360.0;
  
  // Get RP angle
  StHbtReactionPlaneAnalysis* RPanal = (StHbtReactionPlaneAnalysis*) myAnalysis;
  double RPangle = RPanal->ReactionPlane()*180.0/pi;
  
  // Get angle difference, put it within full rp bin range
  // (low edge of '0' bin to upper edge of highest bin)
  // for example: if nRPbins=3, then -30 < angleDiff < 150
  // This assumes we're looking at 2nd harmonic of RP only
  double angleDifference = angle-RPangle;
  if (angleDifference<0.0) angleDifference+=360.0;
  if (angleDifference>=180.0) angleDifference-=180.0;
  if (angleDifference>=(180.0-90.0/nRPbins)) angleDifference-=180.0;
  
  // Determine rp bin ( 0 <= rpBin <= nRPbins-1 )
  int rpBin;
  rpBin = (int) ((angleDifference*nRPbins/180.0)+0.5);
  if(rpBin>=nRPbins || ((angleDifference*nRPbins/180.0)+0.5)<0.0)
    cout << endl << endl << endl << endl << "PROBLEM!!!!" << endl << endl << endl
        << endl << endl << endl;
  return rpBin;
       
}
//____________________________
int QoslCMSCorrFctnRPkT::GetKtBin(const StHbtPair* pair) {

  double kT = fabs(pair->kT());
  int ktBin;

  if(kT<0.15 || kT>0.6) return -1;
  
  if(kT<0.25) 
    ktBin = 0;
  else if( kT >= 0.25 && kT < 0.35 ) 
    ktBin = 1;
  else if( kT >= 0.35 && kT < 0.45 ) 
    ktBin = 2;
  else if( kT >= 0.45 && kT <= 0.6 ) 
    ktBin = 3;

  return ktBin;

}
//____________________________
void QoslCMSCorrFctnRPkT::SetCorrection(StHbtCoulomb* coulomb) {
  mCorrection = coulomb;
}



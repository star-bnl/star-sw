#include "StHbtMaker/CorrFctn/ParityTwistCorrFctn.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(ParityTwistCorrFctn) 
#endif

//____________________________
ParityTwistCorrFctn::ParityTwistCorrFctn(){
  mTagWriter = StHbtTagWriter::Instance();  // get the singleton

  // histograms
  mSameTzz = new StHbt1DHisto("SameTzz","Parity Same Twist zz",100,-0.0010,0.0010);
  mMixedTzz = new StHbt1DHisto("MixedTzz","Parity Mixed Twist zz",100,-0.0010,0.0010);

  // end histograms

  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
  mSameTzz->SetDirectory(0);
  mMixedTzz->SetDirectory(0);

  mSameTzz->Sumw2();
  mMixedTzz->Sumw2();

}   
 
//____________________________
ParityTwistCorrFctn::~ParityTwistCorrFctn(){
  // histograms
  delete mSameTzz;
  delete mMixedTzz ;

  // end histograms;
}
//_________________________
void ParityTwistCorrFctn::Finish(){
  cout << " alive in finish " << endl;

}    
//____________________________
StHbtString ParityTwistCorrFctn::Report(){
  string stemp = "Parity Correlation Function Report:\n";
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
inline void ParityTwistCorrFctn::AddRealPair(const StHbtPair* pair){
  cout << "WARNING ParityTwistCorrFctn::AddRealPair shouldn't be called" << endl;
}
//_________________________


//_________________________
inline void ParityTwistCorrFctn::AddMixedPair(const StHbtPair* pair){
  cout << "WARNING ParityTwistCorrFctn::AddMixedPair shouldn't be called" << endl;
}

//_________________________
inline void ParityTwistCorrFctn::ParityCompute(ParityBuff *Plus, ParityBuff *Minus, int mxd){


      StHbtLorentzVector    PlusTrack; 
      StHbtLorentzVector    MinusTrack; 

      double  Tzz =0;
      double  sumXpos = 0;
      double  sumYpos = 0;
      double  sumZpos = 0;
      double  sumXXpos = 0;
      double  sumYYpos = 0;
      double  sumZZpos = 0;
      double  sumXYpos = 0;
      double  sumXZpos = 0;
      double  sumYZpos = 0;

      double  sumXneg = 0;
      double  sumYneg = 0;
      double  sumZneg = 0;
      double  sumXXneg = 0;
      double  sumYYneg = 0;
      double  sumZZneg = 0;
      double  sumXYneg = 0;
      double  sumXZneg = 0;
      double  sumYZneg = 0;
	 
      if (mxd == SAME) {
	cout << "********  we got to the ParityComputeII for SAME event" << endl;
      }
      if (mxd == MIXED) {
	cout << "********  we got to the ParityComputeII for MIXED event" << endl;
      }

    int plusSize = Plus->size();
    int minusSize = Minus->size();

       {for (int jjj = 0; jjj < plusSize; jjj++){
	 StHbtThreeVector TempV = (*Plus)[jjj].vect();
	 sumXpos  += TempV.x();
	 sumYpos  += TempV.y();
	 sumZpos  += TempV.z();
	 sumXXpos  += TempV.x()*TempV.x();
	 sumYYpos  += TempV.y()*TempV.y();
	 sumZZpos  += TempV.z()*TempV.z();
	 sumXYpos  += TempV.x()*TempV.y();
	 sumXZpos  += TempV.x()*TempV.z();
	 sumYZpos  += TempV.y()*TempV.z();
       }}

       {for (int jjj = 0; jjj < minusSize; jjj++){
	 StHbtThreeVector TempV = (*Minus)[jjj].vect();
	 sumXneg  += TempV.x();
	 sumYneg  += TempV.y();
	 sumZneg  += TempV.z();
	 sumXXneg  += TempV.x()*TempV.x();
	 sumYYneg  += TempV.y()*TempV.y();
	 sumZZneg  += TempV.z()*TempV.z();
	 sumXYneg  += TempV.x()*TempV.y();
	 sumXZneg  += TempV.x()*TempV.z();
	 sumYZneg  += TempV.y()*TempV.z();
       }}

       Tzz = (  (sumXZpos * sumYneg ) + (sumYpos  * sumXZneg)
		- (sumXpos  * sumYZneg) - (sumYZpos * sumXneg ) )  /  (plusSize*minusSize);

        cout << "Tzz is ..................."<< Tzz <<endl;

      if (mxd == SAME) {
                   mSameTzz->Fill(Tzz);
      }
      if (mxd == MIXED) {
                   mMixedTzz->Fill(Tzz);
      }

}
//_________________________

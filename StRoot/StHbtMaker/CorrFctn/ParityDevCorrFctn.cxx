#include "StHbtMaker/CorrFctn/ParityDevCorrFctn.h"
#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"
#include "StHbtMaker/Cut/mikesEventCut.h"

#include <cstdio>

#ifdef __ROOT__
ClassImp(ParityDevCorrFctn) 
#endif

//____________________________
ParityDevCorrFctn::ParityDevCorrFctn(){
  //mTagWriter = StHbtTagWriter::Instance();  // get the singleton

  // histograms
  mKMiSamex = new StHbt1DHisto("SameKMinusX","Parity Same KMinus x",100,-0.4,0.4);
  mKMiSamey = new StHbt1DHisto("SameKMinusY","Parity Same KMinus y",100,-0.4,0.4);
  mKMiSamez = new StHbt1DHisto("SameKMinusZ","Parity Same KMinus z",100,-0.4,0.4);
  mJcSamex = new StHbt1DHisto("SameJcX","Parity Same Jc X",100,-0.2,0.2);
  mJcSamey = new StHbt1DHisto("SameJcY","Parity Same Jc Y",100,-0.2,0.2);
  mJcSamez = new StHbt1DHisto("SameJcZ","Parity Same Jc Z",100,-0.2,0.2);
  mJcKtSame = new StHbt1DHisto("SameJcKt","Parity Same JcKt",100,-0.005,0.005);
  mKtwistKtSame = new StHbt1DHisto("SameKtwistKt","Parity Same KtwistKt",100,-0.005,0.005);

  mKMiMixedx = new StHbt1DHisto("MixedKMinusX","Parity Mixed KMinus x",100,-0.4,0.4);
  mKMiMixedy = new StHbt1DHisto("MixedKMinusY","Parity Mixed KMinus y",100,-0.4,0.4);
  mKMiMixedz = new StHbt1DHisto("MixedKMinusZ","Parity Mixed KMinus z",100,-0.4,0.4);
  mJcMixedx = new StHbt1DHisto("MixedJcX","Parity Mixed Jc X",100,-0.2,0.2);
  mJcMixedy = new StHbt1DHisto("MixedJcY","Parity Mixed Jc Y",100,-0.2,0.2);
  mJcMixedz = new StHbt1DHisto("MixedJcZ","Parity Mixed Jc Z",100,-0.2,0.2);
  mJcKtMixed = new StHbt1DHisto("MixedJcKt","Parity Mixed JcKt",100,-0.005,0.005);
  mKtwistKtMixed = new StHbt1DHisto("MixedKtwistKt","Parity Mixed KtwistKt",100,-0.005,0.005);

  mJcKtBinomial = new StHbt1DHisto("JcKtBin","Number JcKt positive",100,-0.10,0.10);
  mNumPairsBin = new StHbt1DHisto("Numpairs","Number of Pairs",2000,000.,2000.);


  // end histograms

  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
  mKMiSamex->SetDirectory(0);
  mKMiSamey->SetDirectory(0);
  mKMiSamez->SetDirectory(0);
  mJcSamex->SetDirectory(0);
  mJcSamey->SetDirectory(0);
  mJcSamez->SetDirectory(0);
  mJcKtSame->SetDirectory(0);
  mKtwistKtSame->SetDirectory(0);
  mKMiMixedx->SetDirectory(0);
  mKMiMixedy->SetDirectory(0);
  mKMiMixedz->SetDirectory(0);
  mJcMixedx->SetDirectory(0);
  mJcMixedy->SetDirectory(0);
  mJcMixedz->SetDirectory(0);
  mJcKtMixed->SetDirectory(0);
  mKtwistKtMixed->SetDirectory(0);
  mJcKtBinomial->SetDirectory(0);
  mNumPairsBin->SetDirectory(0); 

  mKMiMixedx->Sumw2();
  mKMiMixedy->Sumw2();
  mKMiMixedz->Sumw2();
  mJcMixedx->Sumw2();
  mJcMixedy->Sumw2();
  mJcMixedz->Sumw2();
  mJcKtMixed->Sumw2();
  mKtwistKtMixed->Sumw2();
  mKMiSamex->Sumw2();
  mKMiSamey->Sumw2();
  mKMiSamez->Sumw2();
  mJcSamex->Sumw2();
  mJcSamey->Sumw2();
  mJcSamez->Sumw2();
  mJcKtSame->Sumw2();
  mKtwistKtSame->Sumw2();
  mJcKtBinomial->Sumw2();
  mNumPairsBin->Sumw2();

}   
 
//____________________________
ParityDevCorrFctn::~ParityDevCorrFctn(){
  // histograms
  delete mKMiSamex;
  delete mKMiSamey;
  delete mKMiSamez;
  delete mJcSamex ;
  delete mJcSamey ;
  delete mJcSamez ;
  delete mJcKtSame ;
  delete mKtwistKtSame ;

  delete mKMiMixedx;
  delete mKMiMixedy;
  delete mKMiMixedz;
  delete mJcMixedx ;
  delete mJcMixedy ;
  delete mJcMixedz ;
  delete mJcKtMixed ;
  delete mKtwistKtMixed ;
  // end histograms;
}
//_________________________
void ParityDevCorrFctn::Finish(){
  cout << " alive in finish " << endl;

}    
//____________________________
StHbtString ParityDevCorrFctn::Report(){
  string stemp = "Parity Correlation Function Report:\n";
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
inline void ParityDevCorrFctn::AddRealPair(const StHbtPair* pair){
  cout << "WARNING ParityDevCorrFctn::AddRealPair shouldn't be called" << endl;
}
//_________________________

//_________________________
inline void ParityDevCorrFctn::AddMixedPair(const StHbtPair* pair){
  cout << "WARNING ParityDevCorrFctn::AddMixedPair shouldn't be called" << endl;
}

//_________________________
inline void ParityDevCorrFctn::ParityCompute(ParityBuff *Plus, ParityBuff *Minus, int mxd){

      StHbtLorentzVector    PlusTrack; 
      StHbtLorentzVector    MinusTrack; 
      StHbtThreeVector      KMinusPos;
      StHbtThreeVector      KMinusNeg;
      StHbtThreeVector      KMinus;
      StHbtThreeVector      Kt;

      double  JcKt =0;
      double  PairJcKt = 0;
      double  JcKtBinom = 0;
      double  KtwistKt = 0;	 

    int plusSize = Plus->size();
    int minusSize = Minus->size();

     if (mxd == SAME) {
         cout << "********  we got to the ParityCompute for SAME event" << endl;
         cout << " size of same event plus is "  << Plus->size() << "  ";
         cout << " size of same event minus is " << Minus->size() << endl;
      }
      if (mxd == MIXED) {
         cout << "********  we got to the ParityCompute for MIXED event" << endl;
         cout << " size of mixed event plus is "  << Plus->size() << "  ";
         cout << " size of mixed event minus is " << Minus->size() << endl;
      }
    //     ****** Compute K- **********

       {for (int jjj = 0; jjj <plusSize; jjj++){
	 KMinusPos += (*Plus)[jjj].vect().unit(); 
       }}
       {for (int jjj = 0; jjj < minusSize; jjj++){
	 KMinusNeg += (*Minus)[jjj].vect().unit(); 
       }}
       KMinus = (KMinusPos/plusSize) - (KMinusNeg/minusSize);
       Kt = KMinus;
       Kt.setZ(0.);
       cout << "Kt is " << Kt.x() << "," << Kt.y() << endl ; 

       // now calculate Ktwist*Kt

       double plusSum = 0.;
       {for (int jjj = 0; jjj < plusSize; jjj++){
	 StHbtThreeVector TempV = (*Plus)[jjj].vect();
	 double step1 = (TempV.y() * Kt.x()-TempV.x() * Kt.y() );  // this is y-component rotated to Kt coordinates
	 double step2 = step1*TempV.z();                                                                     // multiply by z-component
	 double step3 = step2/(TempV.mag2() * Kt.mag());                         // normalize
	 plusSum += step3;
       }}
       double minusSum = 0.;
       {for (int jjj = 0; jjj < minusSize; jjj++){
	 StHbtThreeVector TempV = (*Minus)[jjj].vect();
	 double step1 = (TempV.y() * Kt.x()-TempV.x() * Kt.y() );  // this is y-component rotated to Kt coordinates
	 double step2 = step1*TempV.z();                                                                     // multiply by z-component
	 double step3 = step2/(TempV.mag2() * Kt.mag());                         // normalize
	 minusSum += step3;
       }}

       double Ktwist      = (plusSum/ plusSize) - (minusSum/ minusSize);
       KtwistKt = Ktwist*Kt.mag();	 

       cout << "KtwistKt  is " << KtwistKt << endl;
       // end Ktwist

       // now JcKt -here we use  unique pairs since it's approx. as sensitive as using all pairs (?)

       int smallSize = plusSize;
       int numPairs = 0;
       int numJcKtPlus = 0;
       if (minusSize < plusSize) smallSize = minusSize;
       StHbtThreeVector Jc;
        {for (int jjj = 0; jjj < smallSize; jjj++){
	 PlusTrack = (*Plus)[jjj];
	 MinusTrack = (*Minus)[jjj];
	      StHbtThreeVector vOne  = PlusTrack.vect();
	      StHbtThreeVector vTwo  = MinusTrack.vect();
                         StHbtThreeVector PairJc = vOne.unit().cross(vTwo.unit());
                          if ( (vOne.z()*vTwo.z()) < 0.){
                           PairJc.setX(-PairJc.x());
	       }
                          if ( (vOne.x()*vTwo.x()) < 0.){
                           PairJc.setY(-PairJc.y());
                          }
                          if ( (vOne.y()*vTwo.y()) < 0.){
                           PairJc.setZ(-PairJc.z());
                          }
	       Jc += PairJc;
	       //this is for binomial only
	       numPairs++;
	       PairJcKt = PairJc.dot(Kt);
	       if (PairJcKt > 0){
		 numJcKtPlus++;
	       }
	       // end binomial
       }}
       Jc = Jc/(smallSize); // remember to change normalization if we go back to all pairs
       JcKt = Jc.dot(Kt);
       cout << "JcKt  is " << JcKt << endl;
       JcKtBinom = ( double(numJcKtPlus)/double(numPairs) ) -1./2. ; 
       cout << "JcKt bin = " << numJcKtPlus << "/" <<numPairs << "-1/2  ="<< JcKtBinom << endl; 
      // end JcKt

      if (mxd == SAME) {
                   mKMiSamex->Fill(KMinus.x());
                   mKMiSamey->Fill(KMinus.y());
                   mKMiSamez->Fill(KMinus.z());
                   mJcSamex->Fill(Jc.x());
                   mJcSamey->Fill(Jc.y());
                   mJcSamez->Fill(Jc.z());
                   mJcKtSame->Fill(JcKt);
                   mKtwistKtSame->Fill(KtwistKt);

                   mJcKtBinomial->Fill(JcKtBinom); // binomial
                   mNumPairsBin->Fill(numPairs); // binomial		   
      }

      if (mxd == MIXED) {
                   mKMiMixedx->Fill(KMinus.x());
                   mKMiMixedy->Fill(KMinus.y());
                   mKMiMixedz->Fill(KMinus.z());
                   mJcMixedx->Fill(Jc.x());
                   mJcMixedy->Fill(Jc.y());
                   mJcMixedz->Fill(Jc.z());
                   mJcKtMixed->Fill(JcKt);
                   mKtwistKtMixed->Fill(KtwistKt);
      }


}
//_________________________

#include "StHbtMaker/Cut/evansPairCut.h"
#include <string>
#include <cstdio>

#ifdef __ROOT__
ClassImp(evansPairCut)
#endif

//__________________
evansPairCut::evansPairCut(){
  mNPairsPassed = mNPairsFailed = 0;
  AngleCut = 0.020;
  pCutDistance = 0.010;
  Bfield = 0.5;  // B-field in Tesla
}
//__________________
//evansPairCut::~evansPairCut(){
//  /* no-op */
//}
//__________________
bool evansPairCut::Pass(const StHbtPair* pair){

  return true;
}
void evansPairCut::ParityPairCuts(ParityBuff *Plus, ParityBuff *Minus){
  // _________________________________________________________________________
  // here we compare three quantities for each pair-
  //     The first is do the pair have the same sign
  // for p(z)-if not we don't make the other comparisons (this is to save time as the other cuts are
  // more time consuming.
  //     The second is a comparison of the two tracks momentum.  Two tracks with identical production angle
  //but different transverse momentum (p1 and p2 in GeV/c) will by traversing a magnetic field of B Tesla 
  //be seperated at a radius of .5 meters by (to first order) DIST =  ([(.5)**][.3*B]/2)(1/p1-1/p2).  The effective
  // DIST is the parameter which is set (assuming the B-field is know) as pCutDistance
  //    If the tracks are close enough in momentum, we go on to the third (and slowest) cut.  If the opening angle
  //between the tracks is less than the adjustable parameter 'AngleCut' the tracks are rejected.
  //    Any two tracks that fail all three cuts are removed from the sample entirely.
  // ______________________________________________________________________

      StHbtThreeVector FirstTrack;
      StHbtThreeVector SecondTrack;
      vector <int> PlusIsGood;
      vector <int> MinusIsGood;

      double cosSqAngleCut = cos(AngleCut)*cos(AngleCut);

      int newPlusSize = Plus->size();
      int newMinusSize = Minus->size();

      double pCut = (pCutDistance*2.0)/( (0.5)*(0.5)*(0.3)*Bfield);

      cout <<"begin evans Pair Cut "<< newPlusSize<< " plus and "<< newMinusSize<<" minus tracks"<< endl; 

      {for (int jjj = 0; jjj < newPlusSize; jjj++){
                   PlusIsGood.push_back(1);
      }}     
      {for (int jjj = 0; jjj < newMinusSize; jjj++){
                   MinusIsGood.push_back(1);
      }}     

      // loop over all combinations of plus and minus tracks, flagging any pairs that are too close 
      double FdotS;
      {for (int bbb = 0; bbb < newPlusSize; bbb++){
	FirstTrack = ((*Plus)[bbb]).vect();
	{for (int hhh = 0; hhh < newMinusSize; hhh++){
	  SecondTrack = ((*Minus)[hhh]).vect();
	  if (FirstTrack.z()*SecondTrack.z() > 0. ){  
	    FdotS = FirstTrack.dot(SecondTrack);
	    if ( ( (FdotS*FdotS) / (FirstTrack.mag2()* SecondTrack.mag2()) > cosSqAngleCut ) && (FdotS > 0 ) ){  
	       if (fabs(1.0/FirstTrack.mag()-1.0/SecondTrack.mag()) < pCut){
		PlusIsGood[bbb] = 0;
		MinusIsGood[hhh] = 0;
	       }		     
	    }
	  }
	}}
      }}

      // now check plus vectors with other plus vectors

      {for (int bbb = 0; bbb < newPlusSize; bbb++){
	FirstTrack = ((*Plus)[bbb]).vect();
	{for (int hhh = (bbb+1); hhh < newPlusSize; hhh++){
	  SecondTrack = ((*Plus)[hhh]).vect();
	  if (FirstTrack.z()*SecondTrack.z() > 0. ){  
	    FdotS = FirstTrack.dot(SecondTrack);
	    if ( ( (FdotS*FdotS) / (FirstTrack.mag2()* SecondTrack.mag2()) > cosSqAngleCut ) && (FdotS > 0 ) ){  
	       if (fabs(1.0/FirstTrack.mag()-1.0/SecondTrack.mag()) < pCut){
		PlusIsGood[bbb] = 0;
		PlusIsGood[hhh] = 0;
	       }		     
	    }
	  }
	}}
      }}

      // now check minus vectors with other minus vectors

      {for (int bbb = 0; bbb < newMinusSize; bbb++){
	FirstTrack = ((*Minus)[bbb]).vect();
	{for (int hhh = (bbb+1); hhh < newMinusSize; hhh++){
	  SecondTrack = ((*Minus)[hhh]).vect();
	  if (FirstTrack.z()*SecondTrack.z() > 0. ){  
	    FdotS = FirstTrack.dot(SecondTrack);
	    if ( ( (FdotS*FdotS) / (FirstTrack.mag2()* SecondTrack.mag2()) > cosSqAngleCut ) && (FdotS > 0 ) ){  
	       if (fabs(1.0/FirstTrack.mag()-1.0/SecondTrack.mag()) < pCut){
		MinusIsGood[bbb] = 0;
		MinusIsGood[hhh] = 0;
	       }		     
	    }
	  }
	}}
      }}

      // now remove the 'bad' tracks from the 'plus' and 'minus' vectors

           unsigned int jjj = 0;
           while (jjj < (*Plus).size()){
	     if (PlusIsGood[jjj]==0){
	       (*Plus).erase((*Plus).begin()+jjj);
	       PlusIsGood.erase(PlusIsGood.begin()+jjj);
	     }
	     else{
	       jjj++;
	     }
           }
           jjj = 0;
           while (jjj < (*Minus).size()){
	     if (MinusIsGood[jjj]==0){
	       (*Minus).erase((*Minus).begin()+jjj);
	       MinusIsGood.erase(MinusIsGood.begin()+jjj);
	     }
	     else{
	       jjj++;
	     }
           }

           // clean up

           PlusIsGood.clear();
           MinusIsGood.clear();

           cout << "end evans Pair Cut with "<<  (*Plus).size()<< " plus and "<< (*Minus).size() <<" minus tracks"<< endl; 

}
//__________________
StHbtString evansPairCut::Report(){
  string Stemp = "evans Pair Cut Report\n";
  //  char Ctemp[100];
  //  sprintf(Ctemp,"Open angle Cut of \t%f radians and distance cut o:\t%f (assuming B field of \t%f Tesla \n",AngleCut,pCutDistance,Bfield);
  //  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
//__________________

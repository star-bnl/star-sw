/***************************************************************************
 *
 * $Id: MinvCorrFctn.cxx,v 1.11 2011/04/03 15:46:42 fisyak Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     A simple invariant-mass correlation function
 *
 ***************************************************************************
 *
 * $Log: MinvCorrFctn.cxx,v $
 * Revision 1.11  2011/04/03 15:46:42  fisyak
 * Warn off
 *
 * Revision 1.10  2003/01/31 19:59:21  magestro
 * Removed irrelevant include statement
 *
 * Revision 1.9  2000/08/08 23:39:21  laue
 * Updated for standalone version
 *
 * Revision 1.8  2000/06/15 18:52:42  willson
 * HbtAnalysis() method must be cast to specific analysis
 * rotateEventCut installed
 *
 * Revision 1.7  2000/06/09 14:21:21  laue
 * check of specific analysis type added
 *
 * Revision 1.6  2000/03/17 17:22:40  laue
 * Roberts new three particle correlations implemented.
 *
 * Revision 1.4  2000/01/25 17:34:44  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.3  1999/07/29 02:47:08  lisa
 * 1) add OpeningAngle correlation function 2) add StHbtMcEventReader 3) make histos in CorrFctns do errors correctly
 *
 * Revision 1.2  1999/07/06 22:33:19  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/
//#ifndef __CINT__
//#include "fortranc.h"
//#define fortrantest F77_NAME(fortrantest,FORTRANTEST)
//extern "C" {int type_of_call F77_NAME(fortrantest,FORTRANTEST)(int*);}
//#endif

#include "StHbtMaker/CorrFctn/MinvCorrFctn.h"

#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"
#include "StHbtMaker/Cut/mikesEventCut.h"

#include <cstdio>

#ifdef __ROOT__
ClassImp(MinvCorrFctn) 
#endif

//____________________________
MinvCorrFctn::MinvCorrFctn(char* title, const int& nbins, const float& MinvLo, const float& MinvHi){
  //mTagWriter = StHbtTagWriter::Instance();  // get the singleton

  char theTitle[100];
  // set up numerator
  const char *TitNum = "MinvCorrFctn_Num";
  sprintf(theTitle,"Num %s\n",title);
  mNumerator = new StHbt1DHisto(TitNum,theTitle,nbins,MinvLo,MinvHi);
  // set up denominator
  const char *TitDen= "MinvCorrFctn_Den";
  sprintf(theTitle,"Den %s\n",title);
  mDenominator = new StHbt1DHisto(TitDen,theTitle,nbins,MinvLo,MinvHi);
  // set up difference
  const char *TitDif = "MinvCorrFctn_Dif";
  sprintf(theTitle,"Dif %s\n",title);
  mDifference = new StHbt1DHisto(TitDif,theTitle,nbins,MinvLo,MinvHi);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
  mNumerator->SetDirectory(0);
  mDenominator->SetDirectory(0);
  mDifference->SetDirectory(0);

  mNumerator->Sumw2();
  mDenominator->Sumw2();
  mDifference->Sumw2();

  //  for (int i=0; i < 100; i++) {
  //    int j = fortrantest( &i );
  //  }
}   
 
//____________________________
MinvCorrFctn::~MinvCorrFctn(){
  delete mNumerator;
  delete mDenominator;
  delete mDifference;
}
//_________________________
void MinvCorrFctn::Finish(){
  long NEvents = 1;
  if (   dynamic_cast<StHbtAnalysis*>( HbtAnalysis() )   ) {
    if (   dynamic_cast<mikesEventCut*>( ((StHbtAnalysis*)HbtAnalysis())->EventCut() )   )
      NEvents = ((mikesEventCut*)((StHbtAnalysis*)HbtAnalysis())->EventCut())->NEventsPassed();
  }

  mNumerator->Scale(1./NEvents);
  mDenominator->Scale(1./NEvents);
  mDifference->Scale(1./NEvents);

  double NumeratorInt = mNumerator->Integral();
  double DenominatorInt = mDenominator->Integral();
  mDifference->Add(mNumerator,mDenominator,1.0,-1*NumeratorInt/DenominatorInt);

}    
//____________________________
StHbtString MinvCorrFctn::Report(){
  string stemp = "Minv Correlation Function Report:\n";
  char ctemp[100];  
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenominator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in difference:\t%E\n",mDifference->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
inline void MinvCorrFctn::AddRealPair(const StHbtPair* pair){
  mNumerator->Fill(pair->mInv());
  //mTagWriter->SetTag("positiveKaonsMeans",2, (float)pair->mInv() );  // <-- this is how to fill the tag
}
//____________________________
inline void MinvCorrFctn::AddMixedPair(const StHbtPair* pair){
  mDenominator->Fill(pair->mInv());
}



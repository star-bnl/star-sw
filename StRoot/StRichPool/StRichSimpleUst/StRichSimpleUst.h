// $Id: StRichSimpleUst.h,v 1.1 2002/11/19 18:27:21 dunlop Exp $
//

#ifndef StRichSimpleUst_HH
#define StRichSimpleUst_HH

///////////////////////////////////////////////////////////////////////////////
//
// StRicResidualMaker
//
// Description: 
//  Selects a subset of StGlobalTrack's from StEvent.
//  The subset of StGlobalTrack's are the tpc tracks
//  that come from origin and intersect a plane
//  with a given normal vector and a point lying in the plane
//  occupied by the RICH's radiator
//
// Environment:
//  Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Author List: 
//  Matt Horsley, Yale University
//
// History:
//
///////////////////////////////////////////////////////////////////////////////

#include "StMaker.h"
#include <TFile.h>
#include <TTree.h>
#include <vector>
#include <map>
class StRichGeometryDb;
class StRichUstStruct;
class StTrack;
class StEvent;
class StV0MuDst;



class StRichSimpleUst: public StMaker {

 private:

    TTree* mSimpleUstTree;
    StRichUstStruct* mRichUstStruct;
    TBranch* mSuperBranch;
    
    

    TString          mEventFile;
    TString          mEventFileOld;
    TString          mEventDir;

    TFile      *mFile;         //!

  Int_t mEventCounter;

  StRichGeometryDb* mGeometryDb;//!


    
 protected:
    map<unsigned int,StV0MuDst*> getLambdas(StEvent*);//!
    void initMicroEventFile();
    
    
 public:
  StRichSimpleUst(const Char_t *name="RICHMIPFINDER");
  virtual ~StRichSimpleUst();
  virtual void Clear(Option_t *option="");
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

   
  ClassDef(StRichSimpleUst, 1)
    };
    
#endif










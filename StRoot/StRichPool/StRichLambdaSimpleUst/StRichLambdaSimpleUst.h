// $Id: StRichLambdaSimpleUst.h,v 1.1 2002/11/19 18:33:19 dunlop Exp $
//

#ifndef StRichLambdaSimpleUst_HH
#define StRichLambdaSimpleUst_HH

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
class StRichLambdaUstStruct;
class StTrack;
class StEvent;
class StV0MuDst;



class StRichLambdaSimpleUst: public StMaker {

private:

    TTree* mSimpleUstTree;
    StRichLambdaUstStruct* mRichUstStruct;
    TBranch* mSuperBranch;
    
    

    TFile* mOutput1;
    Int_t mEventCounter;

    StRichGeometryDb* mGeometryDb;//!

    Char_t mFileName[2000];
    
    
protected:
    map<unsigned int,StV0MuDst*> getLambdas(StEvent*);//!
    map<unsigned int,StV0MuDst*> getAntiLambdas(StEvent*);//!
    map<unsigned int,StV0MuDst*> getK0Shorts(StEvent*);//!
    
    
public:
    StRichLambdaSimpleUst(const Char_t *name="RICHMIPFINDER");
    virtual ~StRichLambdaSimpleUst();
    virtual void Clear(Option_t *option="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
    void SetFileName(const Char_t *in="lambdaUst.root");
    
   
    ClassDef(StRichLambdaSimpleUst, 1)
	};
    
#endif










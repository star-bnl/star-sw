#ifndef STAR_StBFChainOpt
#define STAR_StBFChainOpt

//////////////////////////////////////////////////////////////////////////
/*!

 \class  StBFChainOpt
 \author Victor Perev, 
 \date   2006/04/17 

 Overload of abstract StChainOpt for interface with StBFChain

*/
//////////////////////////////////////////////////////////////////////////
#include "StChainOpt.h"
class StBFChain;
class StBFChainOpt : public StChainOpt {
public:
StBFChainOpt(StBFChain *bfc);
virtual ~StBFChainOpt(){};
//Int_t kOpt(const char *Tag)	const;
const TString &GetFileIn()  	const;
const TString &GetFileOut() 	const;
      TFile   *GetTFile()	const;
      TString  GetGeometry()  	const;
private:
StBFChain *fBFChain;

   ClassDef(StBFChainOpt, 0)
};
#endif

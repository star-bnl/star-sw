#ifndef STAR_StChainOpt
#define STAR_StChainOpt

//////////////////////////////////////////////////////////////////////////
/*!

 \class  StChainOpt
 \author Victor Perev, 
 \date   2006/04/17 

 Abstract Class to provide chain options to user StMaker

*/
//////////////////////////////////////////////////////////////////////////
#include "TNamed.h"
#include "TString.h"
class TString;
class StChainOpt : public TNamed {
 public:
  StChainOpt(const char *name="StChainOpt"):TNamed(name,""){;}
   virtual ~StChainOpt(){};
//   virtual Int_t kOpt(const char *Tag)	const =0;
   virtual const TString &GetFileIn()  		const =0;
   virtual const TString &GetFileOut() 		const =0;
   virtual TFile *GetTFile()                    const =0;
   virtual TString GetGeometry()  	        const {return TString("");}
// ClassDef(StChainOpt, 0)
};
#endif

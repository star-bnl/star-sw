// $Id: StarMCHit.h,v 1.3 2009/03/31 02:29:51 perev Exp $
// $Log: StarMCHit.h,v $
// Revision 1.3  2009/03/31 02:29:51  perev
// Riostream.h added
//
// Revision 1.2  2007/01/05 21:37:08  potekhin
// Add CVS tags
//
#ifndef StarMCHit_h
#define StarMCHit_h

#include "Riostream.h"
#include "TLorentzVector.h"
#include "TDataSet.h"
#include "TGeoMatrix.h"
#include "TArrayS.h"

class StarMCHit : public TDataSet {
 public:
  StarMCHit(const Char_t *name="StarMCHit",const Char_t *title="") : cumulativeStep(0.0), cumulativeDedx(0.0) {
		      active=true;
		      if(LastHit()!=NULL && LastHit()->isActive()) {
			cout<<"Warning: hit not finalized"<<endl;
		      }
		      path.Set(HIT_PATH_DEPTH);

		      lastHit=this;
		      AddCount();
		    };

  void    setTrack(Int_t t)   {track=t;}
  Int_t   getTrack(void)      {return track;}

  void    setPid(Int_t p)     {pid=p;}
  Int_t   getPid(void)        {return pid;}

  void    AddStep(Double_t s) {cumulativeStep+=s;}
  void    AddDedx(Double_t e) {cumulativeDedx+=e;}

  void    setActive(Bool_t a) {active=a;}
  Bool_t  isActive(void)      {return active;}

  void    setPathAt(Int_t index, Int_t n) {path[index]=n;}
  Short_t getPathAt(Int_t index)          {return path[index];}

  static Int_t Count(void)   {return count;}
  static void  AddCount(void){count++;}
  static Int_t MaxDepth(void)   {return HIT_PATH_DEPTH;}

  static StarMCHit* LastHit(void) {return lastHit;}

  void   setEntry(void) {
    entryPointGlobal=currentPointGlobal;
    entryPointLocal=currentPointLocal;
  }

  void   setExit(void) {
    exitPointGlobal=currentPointGlobal;
    exitPointLocal=currentPointLocal;
  }

  TLorentzVector    currentPointGlobal;
  TLorentzVector    currentPointLocal;

  TLorentzVector    entryPointGlobal;
  TLorentzVector    entryPointLocal;

  TLorentzVector    exitPointGlobal;
  TLorentzVector    exitPointLocal;

  virtual ~StarMCHit() {};

  void getLocal(TGeoHMatrix  *matrixC) {
    Double_t xm[3] = {currentPointGlobal.X(), currentPointGlobal.Y(), currentPointGlobal.Z()};
    Double_t xd[3];
    matrixC->MasterToLocal(xm,xd);
    currentPointLocal = TLorentzVector(xd[0],xd[1],xd[2],currentPointGlobal.T());
  }


 private:
  static const Int_t HIT_PATH_DEPTH = 16;
  static Int_t       count;
  static StarMCHit*  lastHit;

  Int_t              track;
  Int_t              pid;
  TArrayS            path;

  Double_t           cumulativeStep;
  Double_t           cumulativeDedx;
  Bool_t             active;


  ClassDef(StarMCHit,1)
};
#endif

#ifndef STAR_StSpinSortMaker
#define STAR_StSpinSortMaker

//////////////////////////////////////////////////////////////////////////
//
//  returns ID>0 of the spin configuration extracted from
//  the DB scheme table according to the event time stamp &
//
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif

class TH1F;
class St_ppDbSpinConf;
class St_ppDbSpinAvr;

class StSpinSortMaker : public StMaker {
 private: //....................................................
  
  TH1F *hi, *ha;
  TH1F *h[6];
  float tstart,tstop; // for histograms
  int ntbin;
  TString sVersion; // remembers version of the SpinSort-scheme in DB
  St_ppDbSpinConf *spinConf;
  St_ppDbSpinAvr *spinAvr;
  
  void readDB();
  int time0; // (sec) GMT of the first event
  
 protected: //....................................................
  
 public:  //....................................................
  void Setup(const char * vvv, int nb, float t1, float t2){
    sVersion=vvv;
    ntbin=nb;
    tstart=t1;
    tstop=t2;
  }
  
  int spinID;  // resulting SpinSortID
  StSpinSortMaker(const char *name="AAB");
  virtual       ~StSpinSortMaker();
  virtual Int_t Init();
  virtual Int_t  Make(); 
  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
  // static Char_t  m_VersionCVS = "$Id: StSpinSortMaker.h,v 1.1 2001/04/13 18:04:35 balewski Exp $";
  
  virtual const char *GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StSpinSortMaker.h,v 1.1 2001/04/13 18:04:35 balewski Exp $ built "__DATE__" "__TIME__ ; return cvs;}
  
  ClassDef(StSpinSortMaker, 0)   //StAF chain virtual base class for Makers
    };
    
#endif
    
    
    

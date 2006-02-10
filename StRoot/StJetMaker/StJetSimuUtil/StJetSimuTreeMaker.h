//  StJetSimuTreeMaker.h

#ifndef STAR_StJetSimuTreeMaker
#define STAR_StJetSimuTreeMaker
                                                                   
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "StJetMaker/StJetMaker.h"
#include <string>
#include <iostream>
#include <map>
#include <algorithm>
using namespace std;

class TTree;
class StChain;
class StJetSimuTrigMaker;
class StJetSimuWeightMaker;
class StMcEventMaker;

class StJetSimuTreeMaker : public StMaker {
 private:
 
 protected:

 public: 
  StJetSimuTreeMaker(const char *name,const char *outputName);
  virtual  ~StJetSimuTreeMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t Finish();

  //Event Tree
  TTree *jTree;
  TFile *outfile;
  
  //pointers to makers
  StJetSimuTrigMaker *trigMaker;
  StJetSimuWeightMaker *weightMaker;

  int evtid; //event number from MuDst
  int pid;   //subprocess id
  int BHTmax;//BEMC HT max
  int BJPmax;//BEMC max JPsum 
  int BJPsum;//BEMC total sum
  int EHTmax;//EEMC HT max
  int EJPmax;//EEMC max JPsum 
  int EJPsum;//EEMC total sum
  int BJP[6];//BEMC ADC for each JP
  int EJP[6];//EEMC ADC for each JP
  int bbc;   //bbc=true passes BBC cut
  int Badc[48];// holds bbc adcs
  float TowHtEt[20];//holds Highest Et tower per eta ring in BEMC
  float s,t,u,cos_th,hard_p,x1,x2;
 
  //asymmetry
  double partonic_all,df1,df2,f1,f2,Q2,weight;
  int flavor1, flavor2, flavor3, flavor4;
  //trigger 
  float Alex_ht_Et;
  int Alex_ht_id,Alex_ht_DSM;
  int JP1_2004,JP1_2004_Patch,JP1_2004_DSM;
  int HT1_2004,HT1_2004_Tow,HT1_2004_DSM;
  void setPrintOption(int p)
    { print = p;
    cout <<"Print option = "<<print<<endl;
    };
    
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StJetSimuTreeMaker.h,v 1.2 2006/02/10 18:08:32 mmiller Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

 private:
  bool print;
  TString m;

  ClassDef(StJetSimuTreeMaker,0)   //StAF chain virtual base class for Makers
};


#endif


// $Log: StJetSimuTreeMaker.h,v $
// Revision 1.2  2006/02/10 18:08:32  mmiller
// Added Renee's modifications to incorporate 2005 Jet patch trigger.
//
// Revision 1.1  2004/10/12 18:49:12  mmiller
// Added StJetSimuUtil (should have added before, not sure why it didn't)
//
// Revision 1.1  2004/09/24 13:50:08  rfatemi
// Jet Simulation Makers
//
// Revision 1.15  2003/09/10 19:47:43  perev
// ansi corrs
//
// Revision 1.14  2002/11/26 23:49:40  jeromel
// Small modif after Art's note ... doxygen issue + cleanup
//
// Revision 1.13  2002/04/28 01:28:36  jeromel
// Reshaped comments for doxygen. Hopefully, users will propagate this good
// habit.
//
// Revision 1.12  1999/09/24 22:03:09  perev
// Add InitRun & FinishRun to template maker
//
// Revision 1.11  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.10  1999/07/10 22:59:17  fine
// Some comments have been introduced to show html docs
//
// Revision 1.9  1999/03/11 03:33:16  perev
// new schema
//
// Revision 1.8  1999/03/10 15:02:07  fine
// HTML link to STAR problem report form has been introduced
//
// Revision 1.7  1998/10/31 00:25:45  fisyak
// Makers take care about branches
//
// Revision 1.6  1998/10/06 18:00:31  perev
// cleanup
//
// Revision 1.5  1998/08/26 12:15:13  fisyak
// Remove asu & dsl libraries
//
// Revision 1.4  1998/08/14 15:25:58  fisyak
// add options
//
// Revision 1.3  1998/08/10 02:32:07  fisyak
// Clean up
//
// Revision 1.2  1998/07/20 15:08:15  fisyak
// Add tcl and tpt
//


//  StJetSimuWeightMaker.h,v 1.15 2004/08/02 19:47:43 

#ifndef STAR_StJetSimuWeightMaker
#define STAR_StJetSimuWeightMaker

                                                                    
//class  StJetSimuWeightMaker
//author R.Fatemi
//date   2004/08/01
//This maker accesses tables which hold pythia event record
//as well as partonic mandelstam s,t,u,cos(theta),pT,x1 and x2
//pass along what you need in simu tree to StJetSimuTreeMaker


#ifndef StMaker_H
#include "StMaker.h"
extern "C" void polar_(int*,double*,double*,double*,int*);
extern "C" void parpol2_(int*, double*, double*, double* ,double* , double*, double*, double*, double*, double*, double*);
extern "C" void grv98pa_(int*, double*, double*, double*, double*, double*, double*, double*, double*);
extern "C" void grv98f2_(int*, double*, double*, double*, double*, double*, double*);
extern "C" void unpolar_(int*, double*, double*, double*, int*);
extern "C" void num_(double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
extern "C" void denom_(double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
extern "C" Double_t ctq5pd_(int*,int*,double*,double*,int*);
#endif

class StJetSimuTrigMaker;
class StChain;
class St_particle;
class St_g2t_event;
class St_g2t_pythia;
class StMcEventMaker;

class StJetSimuWeightMaker : public StMaker
{

 private:
  bool print;
  int polset;
  int unpolset;

 protected:

 public: 
  StJetSimuWeightMaker(const char *name="SimuWeight");
  virtual  ~StJetSimuWeightMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  void Zero();

  //pointers to makers
  StMcEventMaker *mcEventMaker;
  StJetSimuTrigMaker *trigMaker;
  St_particle *particleTabPtr;
  St_g2t_event *Pg2t_event;
  St_g2t_pythia *Pg2t_pythia;

  int geantPID;//sub process id from GEANT table
  int geantID;//event number from GEANT table
  int evtid; //event number from MuDst
  int pid;   //subprocess id from StMcEvent
  float parton1[11],parton2[11];
  int flavor1,flavor2,flavor3,flavor4;
  float s,t,u,hard_p,cos_th,x1,x2;
  int polid, unpolid, pol_id_flag, unpol_id_flag;
  double partonic_all,df1,df2,f1,f2,Q2,weight;

  Double_t getPolPDF(int x1, double d1, double d2);
  Double_t getUnPolPDF(int x1, double d1, double d2);
  Double_t getPartonicALL(double a, double b, double c, int d, int e, int f, int g, int h);
  void setPrintOption(int p){
    print = p;
  } 
  void setUnpolPDF(int p){
    unpolset=p;
  }
  void setPolPDF(int p){
    polset=p;
  }

  
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StJetSimuWeightMaker.h,v 1.2 2006/02/10 18:08:32 mmiller Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }
  
  
  ClassDef(StJetSimuWeightMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StJetSimuWeightMaker.h,v $
// Revision 1.2  2006/02/10 18:08:32  mmiller
// Added Renee's modifications to incorporate 2005 Jet patch trigger.
//
// Revision 1.1  2004/10/12 18:49:12  mmiller
// Added StJetSimuUtil (should have added before, not sure why it didn't)
//
// Revision 1.2  2004/10/12 18:20:14  mmiller
// Add StJetSimuUtil
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

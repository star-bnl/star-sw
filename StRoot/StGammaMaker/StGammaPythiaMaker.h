//class  StGammaPythiaMaker
//author R.Fatemi
//date   2007/May/24 
//This class allows access to the pythia record and the isolatation of prompt and decay photons. In addition, this code provides 
//the calculation of "asymmetries" in MC data by using partonic kinematic information from the pythia record along with polarized 
//and unpolarized pdfs and LO partonic asymmetries. Documentation on the Method of Asymmetry Weights (MAW) can be found in the 
//preprint on SPHINX (hep-0005320)

#ifndef STAR_StGammaPythiaMaker
#define STAR_StGammaPythiaMaker

class StPythiaEvent;
                                                                  
#ifndef StMaker_H
#include "StMaker.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEventTypes.hh"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcTrack.hh"
#include "StMcEvent/StMcVertex.hh"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#include "TLorentzVector.h"
#include "StMessMgr.h"
#include <vector>

extern "C" void polar_(int*,double*,double*,double*,int*);
extern "C" Double_t ctq5pd_(int*,int*,double*,double*,int*);
extern "C" void num_(double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
extern "C" void denom_(double*,double*,double*,double*,double*,double*,double*,double*,double*,double*,double*);
#endif

class StGammaPythiaMaker : public StMaker
{
private:

  vector<StMcTrack *> filterMcTracks ( StMcVertex *v, Int_t geantId );
  void collectDecayPhotons();
  void collectDecayPhotons(StMcVertex* vertex);
  
  bool display_flag;
  int n_prompt;
  
public: 

  StGammaPythiaMaker(const char *name="GammaPythia");
  ~StGammaPythiaMaker();

  void  Clear(Option_t* option = "");
  Int_t Init();
  Int_t Make();
  
  //pointers to makers
  StMuDstMaker *muDstMaker;
  StMuEvent *muEvent;
  StMcEvent *mcEvent;
  St_particle *particleTabPtr;
  St_g2t_event *Pg2t_event;
  St_g2t_pythia *Pg2t_pythia;
  
  int geantPID;                        //sub process id from GEANT table
  int geantID;                         //event number from GEANT table
  int evtid;                           //event number from MuDst
  int pid;                             //subprocess id 
  int flavor1,flavor2,flavor3,flavor4; //flavor of scattered partons before/after hard interaction
  float s,t,u,hard_p,cos_th,x1,x2;     //partonic kinematics
  int pol_id_flag, unpol_id_flag;      //flags for *.F 
  double partonic_all,Q2;              //partonic a_LL from LO calculations and hard scale
  
  vector<TLorentzVector> Pion04Mom;    //4 momentum of pions in pythia record which may or may not decay in geant record
  vector<TLorentzVector> Prompt4Mom;   //4 momentum of prompt photons in pythia record
  vector<TLorentzVector> Decay4Mom;    //4 momentum of decay photon from pythia and geant record
  vector<TLorentzVector> Frag4Mom;     // Four momentum of fragmentation photons
  vector<TLorentzVector> Initial4Mom;  // Four momentum of initial state radiation
  vector<TLorentzVector> Final4Mom;    // Four momentum of final state radiation  

  double df1_LO,df2_LO,f1_LO,f2_LO,weight_LO;         //Leading Order polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
  double df1_NLO,df2_NLO,f1_NLO,f2_NLO,weight_NLO;    //NLO STD polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
  double df1_NLO_g0,df2_NLO_g0,weight_NLO_g0;         //NLO G0  polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
  double df1_NLO_gmax,df2_NLO_gmax,weight_NLO_gmax;   //NLO GMAX polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
  double df1_NLO_gmin,df2_NLO_gmin,weight_NLO_gmin;   //NLO GMIN polarized pdf, unpolarized pdf and weight = df1*df2*partonic_all/f1/f2
  
  Double_t get_polPDF_LO(int x1, double d1, double d2);
  Double_t get_polPDF_NLO(int x1, double d1, double d2);
  Double_t get_polPDF_NLO_g0(int x1, double d1, double d2);
  Double_t get_polPDF_NLO_gmax(int x1, double d1, double d2);
  Double_t get_polPDF_NLO_gmin(int x1, double d1, double d2);
  
  Double_t get_unpolPDF_LO(int x1, double d1, double d2); 
  Double_t get_unpolPDF_NLO(int x1, double d1, double d2);
  
  Double_t getPartonicALL(double a, double b, double c, int d, int e, int f, int g, int h);

  int numberOfPion0() const;
  int numberOfPrompt() const;
  int numberOfDecay() const;
  int numberOfFrag() const;
  int numberOfInitial() const;
  int numberOfFinal() const;

  TLorentzVector& pion0(int i);
  TLorentzVector& prompt(int i);
  TLorentzVector& decay(int i);
  TLorentzVector& frag(int i);
  TLorentzVector& initial_rad(int i);
  TLorentzVector& final_rad(int i);

  void fillPythiaEvent(StPythiaEvent* pythia);
  
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StGammaPythiaMaker.h,v 1.5 2007/07/14 01:47:22 pibero Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }  
  
  ClassDef(StGammaPythiaMaker,0)   //StAF chain virtual base class for Makers
};

inline int StGammaPythiaMaker::numberOfPion0() const { return Pion04Mom.size(); }
inline int StGammaPythiaMaker::numberOfPrompt() const { return Prompt4Mom.size(); }
inline int StGammaPythiaMaker::numberOfDecay() const { return Decay4Mom.size(); }
inline int StGammaPythiaMaker::numberOfFrag() const { return Frag4Mom.size(); }
inline int StGammaPythiaMaker::numberOfInitial() const { return Initial4Mom.size(); }
inline int StGammaPythiaMaker::numberOfFinal() const { return Final4Mom.size(); }

inline TLorentzVector& StGammaPythiaMaker::pion0(int i) { return Pion04Mom[i]; }
inline TLorentzVector& StGammaPythiaMaker::prompt(int i) { return Prompt4Mom[i]; }
inline TLorentzVector& StGammaPythiaMaker::decay(int i) { return Decay4Mom[i]; }
inline TLorentzVector& StGammaPythiaMaker::frag(int i) { return Frag4Mom[i]; }
inline TLorentzVector& StGammaPythiaMaker::initial_rad(int i) { return Initial4Mom[i]; }
inline TLorentzVector& StGammaPythiaMaker::final_rad(int i) { return Final4Mom[i]; }

#endif

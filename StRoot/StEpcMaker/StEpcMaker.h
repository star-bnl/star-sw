//
// $Id: StEpcMaker.h,v 1.1 2000/05/15 21:18:32 subhasis Exp $
//
// $Log: StEpcMaker.h,v $
// Revision 1.1  2000/05/15 21:18:32  subhasis
// initial version
//
// tower-smd-psd-track matching Maker for EMC
//
//
// Authors: Subhasis Chattopadhyay.
//    

#ifndef STAR_StEpcMaker
#define STAR_StEpcMaker

#include "StMaker.h"

//For CC5 compatibility
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif 

#ifdef ST_NO_TEMPLATE_DEF_ARGS
// Syntax currently required by Solaris compiler
#define StVector(T) vector<T, allocator<T> >
typedef vector<int, allocator<int> > intVector;
typedef vector<Float_t,allocator<Float_t> > FloatVector;
#else
#define StVector(T) vector<T>
typedef vector<int> intVector;
typedef vector<Float_t> FloatVector;
#endif

#include <TH1.h>
#include <TH2.h>
#include "StPreEclMaker/StEmcPreClusterCollection.h"
#include "StPreEclMaker/StBemcPreClusterCollection.h"
#include "StPreEclMaker/StBsmdePreClusterCollection.h"
#include "StPreEclMaker/StBsmdpPreClusterCollection.h"
#include "St_emc_Maker/StEmcHitCollection.h"
#include "emc_def.h"
#include "tables/St_tpt_track_Table.h"
#include "tables/St_dst_track_Table.h"
#include "tables/St_dst_vertex_Table.h"
typedef StVector(StEmcPreCluster*) StMatchVec;
typedef StVector(StEmcPreCluster*)::iterator StMatchVecIter;

class StEpcMaker : public StMaker {
private:
  void MakeHistograms();   // Filling QA Histograms

protected:
  //Point Energy spectra
  TH1F *m_point_energy;
  //Point Eta spectra
  TH1F *m_point_eta;
  //Point Phi spectra
  TH1F *m_point_phi;
  //Point SigmaEta spectra
  TH1F *m_point_sigeta;
  //Point SigmaPhi spectra
  TH1F *m_point_sigphi;
  //Point DeltaEta spectra
  TH1F *m_point_deleta;
  //Point DeltaPhi spectra
  TH1F *m_point_delphi;
  //Point TrMom spectra
  TH1F *m_point_trmom;
  //Point Flag spectra
  TH1F *m_point_flag;

public: 
  StEpcMaker(const char *name="epc");
  virtual ~StEpcMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  virtual Int_t fillStEvent();
  
  virtual const char *GetCVS()
  {static const char cvs[]="Tag $Name:  $ $Id: StEpcMaker.h,v 1.1 2000/05/15 21:18:32 subhasis Exp $ built "__DATE__" "__TIME__ ; return cvs;}  

  ClassDef(StEpcMaker, 1)// EMC-Track match maker
};
#include "StPointCollection.h"

#endif









/***************************************************************************
 *
 * $Id: CTH.hh,v 1.1 2000/09/05 14:21:10 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *              Histogram classes (inherited from TH?Ds) which 
 *              will be listed by the StHbtHistoCollector
 *
 ***************************************************************************
 *
 * $Log: CTH.hh,v $
 * Revision 1.1  2000/09/05 14:21:10  laue
 * NEW !! A histogram class (CTH, inherited from TH?Ds) that puts itself into
 * a list (StHbtHistoCollector) at instantiation time. This provides an easy
 * way to write out all the histograms.
 *
 **************************************************************************/
#ifndef CTH_hh
#define CTH_hh

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"

#include "StHbtMaker/Infrastructure/StHbtHistoCollector.h"
#include "StChain.h"

class CTH1D : public TH1D {
public:
  CTH1D(const char* name, const char* title, 
	Int_t nbinsx, Axis_t xlow, Axis_t xup) : TH1D(name, title, nbinsx, xlow, xup){
      StHbtHistoCollector* collector = StHbtHistoCollector::Instance();//!
    collector->Add(this);
  }
  ClassDef(CTH1D,1)
};

class CTH2D : public TH2D {
public:
  CTH2D(const char* name, const char* title, 
	Int_t nbinsx, Axis_t xlow, Axis_t xup, 
	Int_t nbinsy, Axis_t ylow, Axis_t yup) : TH2D(name, title, nbinsx, xlow, xup, nbinsy, ylow, yup){
    StHbtHistoCollector* collector = StHbtHistoCollector::Instance();
    collector->Add(this);
  }
  ClassDef(CTH2D,1)
};

class CTH3D : public TH3D {
public:
  CTH3D(const char* name, const char* title, 
	Int_t nbinsx, Axis_t xlow, Axis_t xup, 
	Int_t nbinsy, Axis_t ylow, Axis_t yup, 
	Int_t nbinsz, Axis_t zlow, Axis_t zup) : TH3D(name, title, nbinsx, xlow, xup, nbinsy, ylow, yup, nbinsz, zlow, zup){
    StHbtHistoCollector* collector = StHbtHistoCollector::Instance();
    collector->Add(this);
  }
  ClassDef(CTH3D,1)
};



#endif
 

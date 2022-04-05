/***************************************************************************
 *
 * $Id: StHbtHistoCollector.h,v 1.2 2001/09/05 20:41:42 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *              Class to keep a list of all histograms
 *
 ***************************************************************************
 *
 * $Log: StHbtHistoCollector.h,v $
 * Revision 1.2  2001/09/05 20:41:42  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 * Revision 1.1  2000/09/05 14:21:09  laue
 * NEW !! A histogram class (CTH, inherited from TH?Ds) that puts itself into
 * a list (StHbtHistoCollector) at instantiation time. This provides an easy
 * way to write out all the histograms.
 *
 **************************************************************************/

#ifndef StHbtHistoCollector_h
#define StHbtHistoCollector_h

#include "StHbtMaker/Infrastructure/CTHCollection.hh"
#include "StChain.h"

class CTH1D;
class CTH2D;
class CTH3D;


class StHbtHistoCollector {
public:
    static StHbtHistoCollector* Instance();
    void Clear();
    void Add(CTH1D*);  
    void Add(CTH2D*);  
    void Add(CTH3D*);  
    void Write();  

    friend class nobody;
protected: 
  StHbtHistoCollector();
  virtual ~StHbtHistoCollector() { /* no-op */ };
private:
  static StHbtHistoCollector* _instance;
  
  CTH1DCollection m1DList;
  CTH2DCollection m2DList;
  CTH3DCollection m3DList;

 public:  
#ifdef __ROOT__
  ClassDef(StHbtHistoCollector,0)
#endif

};

#endif
 

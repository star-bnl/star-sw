/***************************************************************************
 *
 * $Id: StHbtHistoCollector.cxx,v 1.3 2001/04/25 18:03:41 perev Exp $
 *
 * Author: Frank Laue, Ohio State, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *              Class to keep a list of all histograms
 *
 ***************************************************************************
 *
 * $Log: StHbtHistoCollector.cxx,v $
 * Revision 1.3  2001/04/25 18:03:41  perev
 * HPcorrs
 *
 * Revision 1.2  2000/09/05 14:43:21  laue
 * cast changed from TH1D to TH1D&
 *
 * Revision 1.1  2000/09/05 14:21:10  laue
 * NEW !! A histogram class (CTH, inherited from TH?Ds) that puts itself into
 * a list (StHbtHistoCollector) at instantiation time. This provides an easy
 * way to write out all the histograms.
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtHistoCollector.h"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"


#ifdef __ROOT__
ClassImp(StHbtHistoCollector)
#endif

StHbtHistoCollector* StHbtHistoCollector::_instance=0;

StHbtHistoCollector* StHbtHistoCollector::Instance() {
    if (_instance == 0 ) _instance = new StHbtHistoCollector();
    return _instance;
}

StHbtHistoCollector::StHbtHistoCollector() { 
  cout << " StHbtHistoCollector::StHbtHistoCollector() " << endl; 
}


void StHbtHistoCollector::Add(CTH1D* h) { m1DList.push_back(h); }
void StHbtHistoCollector::Add(CTH2D* h) { m2DList.push_back(h); }
void StHbtHistoCollector::Add(CTH3D* h) { m3DList.push_back(h); }


void StHbtHistoCollector::Clear() { 
  m1DList.clear();
  m2DList.clear();
  m3DList.clear();
}

void StHbtHistoCollector::Write() { 
  {for (CTH1DIterator iter=m1DList.begin(); iter!=m1DList.end(); iter++) {
    TH1D temp( (TH1D&)(**iter) );
      temp.SetDirectory(0);
      temp.Write();
  }}
  {for (CTH2DIterator iter=m2DList.begin(); iter!=m2DList.end(); iter++) {
    TH2D temp( (TH2D&)(**iter) );
      temp.SetDirectory(0);
      temp.Write();
  }}
  {for (CTH3DIterator iter=m3DList.begin(); iter!=m3DList.end(); iter++) {
    TH3D temp( (TH3D&)(**iter) );
      temp.SetDirectory(0);
      temp.Write();
  }}
}


/***************************************************************************
 *
 * $Id: CTH.cc,v 1.1 2000/09/05 14:21:10 laue Exp $
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
 * $Log: CTH.cc,v $
 * Revision 1.1  2000/09/05 14:21:10  laue
 * NEW !! A histogram class (CTH, inherited from TH?Ds) that puts itself into
 * a list (StHbtHistoCollector) at instantiation time. This provides an easy
 * way to write out all the histograms.
 *
 **************************************************************************/
#include "StHbtMaker/Infrastructure/CTH.hh"

ClassImp(CTH1D)
ClassImp(CTH2D)
ClassImp(CTH3D)
 

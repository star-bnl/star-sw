/***************************************************************************
 *
 * $Id: StHbtHisto.hh,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *    these are the HBT histogram object.
 *    While working with root, we simply typedef it to a TH1D
 *
 ***************************************************************************
 *
 * $Log: StHbtHisto.hh,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtHisto_hh
#define StHbtHisto_hh
#ifndef ROOT_TH1
#include "TH1.h"
#endif
#ifndef ROOT_TH2
#include "TH2.h"
#endif
#ifndef ROOT_TH3
#include "TH3.h"
#endif

typedef TH1D StHbt1DHisto;
typedef TH2D StHbt2DHisto;
typedef TH3D StHbt3DHisto;

#endif

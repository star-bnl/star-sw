/***************************************************************************
 *
 * $Id: CTHCollection.hh,v 1.1 2000/09/05 14:21:10 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *              Class to keep a list of all histograms
 *
 ***************************************************************************
 *
 * $Log: CTHCollection.hh,v $
 * Revision 1.1  2000/09/05 14:21:10  laue
 * NEW !! A histogram class (CTH, inherited from TH?Ds) that puts itself into
 * a list (StHbtHistoCollector) at instantiation time. This provides an easy
 * way to write out all the histograms.
 *
 **************************************************************************/

#ifndef CTHCollection_hh
#define CTHCollection_hh

class CTH1D;
class CTH2D;
class CTH3D;


#include <list>
#if !defined(ST_NO_NAMESPACES)
using std::list;
#endif

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<CTH1D*, allocator<CTH1D*> >            CTH1DCollection;
typedef list<CTH1D*, allocator<CTH1D*> >::iterator  CTH1DIterator;
typedef list<CTH2D*, allocator<CTH2D*> >            CTH2DCollection;
typedef list<CTH2D*, allocator<CTH2D*> >::iterator  CTH2DIterator;
typedef list<CTH3D*, allocator<CTH3D*> >            CTH3DCollection;
typedef list<CTH3D*, allocator<CTH3D*> >::iterator  CTH3DIterator;
#else
typedef list<CTH1D*>            CTH1DCollection;
typedef list<CTH1D*>::iterator  CTH1DIterator;
typedef list<CTH2D*>            CTH2DCollection;
typedef list<CTH2D*>::iterator  CTH2DIterator;
typedef list<CTH3D*>            CTH3DCollection;
typedef list<CTH3D*>::iterator  CTH3DIterator;
#endif

#endif

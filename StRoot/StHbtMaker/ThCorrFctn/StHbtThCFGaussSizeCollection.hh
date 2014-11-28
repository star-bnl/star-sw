/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description: 
 *   The ThGaussZiseCollection contains pointers to all ThCFGaussSize 
 *   that are used by StHbtThCFGaussFit
 *
 ***************************************************************************
 *
 * $Log: 
 *
 **************************************************************************/

#ifndef StHbtThCFGaussSizeCollection_hh
#define StHbtThCFGaussSizeCollection_hh


#include <list>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StHbtThCFGaussSize;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtThCFGaussSize*, allocator<StHbtThCFGaussSize*> >            StHbtThCFGaussSizeCollection;
typedef list<StHbtThCFGaussSize*, allocator<StHbtThCFGaussSize*> >::iterator  StHbtThCFGaussSizeIterator;
#else
typedef list<StHbtThCFGaussSize*>            StHbtThCFGaussSizeCollection;
typedef list<StHbtThCFGaussSize*>::iterator  StHbtThCFGaussSizeIterator;
#endif

#endif

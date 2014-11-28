/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description: 
 *   The ThCorrFctnCollection contains pointers to all Theoretical Correlation 
 *   Functions that are plugged in a ThCorrFctnManager
 *
 ***************************************************************************
 *
 * $Log: 
 *
 **************************************************************************/

#ifndef StHbtThCorrFctnCollection_hh
#define StHbtThCorrFctnCollection_hh


#include <list>
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif
class StHbtThCorrFctn;

#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef list<StHbtThCorrFctn*, allocator<StHbtThCorrFctn*> >            StHbtThCorrFctnCollection;
typedef list<StHbtThCorrFctn*, allocator<StHbtThCorrFctn*> >::iterator  StHbtThCorrFctnIterator;
#else
typedef list<StHbtThCorrFctn*>            StHbtThCorrFctnCollection;
typedef list<StHbtThCorrFctn*>::iterator  StHbtThCorrFctnIterator;
#endif

#endif

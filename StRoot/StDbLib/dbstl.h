/***************************************************************************
 *
 * $Id: dbstl.h,v 1.1 2000/03/28 17:03:19 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  some definitions for build with Solaris CC4.2 + STL 
 *               offline use(d) object space, online uses RogueWave
 *
 ***************************************************************************
 *
 * $Log: dbstl.h,v $
 * Revision 1.1  2000/03/28 17:03:19  porter
 * Several upgrades:
 * 1. configuration by timestamp for Conditions
 * 2. query by whereClause made more systematic
 * 3. conflict between db-stored comments & number lists resolved
 * 4. ensure endtime is correct for certain query falures
 * 5. dbstl.h->handles ObjectSpace & RogueWave difference (Online vs Offline)
 *
 *
 **************************************************************************/

#ifndef DBSTL_HH
#define DBSTL_HH

#ifdef ST_NO_TEMPLATE_DEF_ARGS
#ifndef ONL_solaris
#include <ospace/config.h>
#else
#include <stdcomp.h>
#endif
#endif

#endif


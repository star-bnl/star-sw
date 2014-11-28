/***************************************************************************
 *
 * $Id: dbCollection.h,v 1.1 2000/03/28 17:03:19 porter Exp $
 *
 * Author: R. Jeff Porter
 ***************************************************************************
 *
 * Description:  Simple table definition of a named collection for 
 *               timestamped collection access (e.g. in Conditions)
 *
 ***************************************************************************
 *
 * $Log: dbCollection.h,v $
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
#ifndef DBCOLLECTION_HH
#define DBCOLLECTION_HH


struct dbCollection {

  char name[64]; // name of the collection

};

#endif


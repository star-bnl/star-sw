/*****************************************
 *
 * $Id: StMcParameterDB.h,v 1.3 1999/10/01 14:08:59 calderon Exp $
 * $Log: StMcParameterDB.h,v $
 * Revision 1.3  1999/10/01 14:08:59  calderon
 * Added Local Hit resolution Histogram. It is made by default
 * without any requirement of association, to serve
 * as a diagnostic.
 * Before building track multimap, check the size of the
 * tpc hit map.  If it is too small, print out a warning
 * and exit.
 *
 * Revision 1.2  1999/09/23 21:25:22  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 * Changed extension to .h so it would
 * be accessible from the Root macro
 *
 *  
 *****************************************/
#ifndef ParameterDB_hh
#define ParameterDB_hh

#ifndef StMaker_H
#include "StMaker.h"
#endif



class StMcParameterDB {
public:
    static StMcParameterDB* instance(); // *MENU*
    
    float xCut() const; // *MENU*
    float zCut() const; // *MENU*
    unsigned int reqCommonHits() const; // *MENU*
    
    void setXCut(float); // *MENU*
    void setZCut(float); // *MENU*
    void setReqCommonHits(unsigned int); // *MENU*

private:
    static StMcParameterDB *mParamDB;   
    StMcParameterDB();
    // also copy constructor
    StMcParameterDB(const StMcParameterDB&);
    // also assignment operator
    StMcParameterDB& operator= (const StMcParameterDB&);
    
    float mXCut;
    float mZCut;
    unsigned int mReqCommonHits;

    ClassDef(StMcParameterDB, 1)
    
};
ostream& operator<<(ostream &, const StMcParameterDB&);

inline float StMcParameterDB::xCut() const { return mXCut; }

inline float StMcParameterDB::zCut() const { return mZCut; }

inline unsigned int StMcParameterDB::reqCommonHits() const { return mReqCommonHits; }

#endif

//
//  StMcParameterDB::instance()->xCut()
//  or
//  StMcParameterDB* mydb =  StMcParameterDB::instance();
//  mydb->xCut();
//

/*****************************************
 *
 * StMcParameterDB.h
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
    
    double xCut() const; // *MENU*
    double zCut() const; // *MENU*
    unsigned int reqCommonHits() const; // *MENU*
    
    void setXCut(double); // *MENU*
    void setZCut(double); // *MENU*
    void setReqCommonHits(unsigned int); // *MENU*

private:
    static StMcParameterDB *mParamDB;   
    StMcParameterDB();
    // also copy constructor
    StMcParameterDB(const StMcParameterDB&);
    // also assignment operator
    StMcParameterDB& operator= (const StMcParameterDB&);
    
    double mXCut;
    double mZCut;
    unsigned int mReqCommonHits;

    ClassDef(StMcParameterDB, 1)
    
};

// inline double StMcParameterDB::xCut() const { return mXCut; }

// inline double StMcParameterDB::zCut() const { return mZCut; }

// inline unsigned int StMcParameterDB::reqCommonHits() const { return mReqCommonHits; }

#endif

//
//  StMcParameterDB::instance()->xCut()
//  or
//  StMcParameterDB* mydb =  StMcParameterDB::instance();
//  mydb->xCut();
//

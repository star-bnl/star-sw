/*****************************************
 *
 * $Id: StMcParameterDB.h,v 1.9 2015/03/13 18:44:44 perev Exp $
 * $Log: StMcParameterDB.h,v $
 * Revision 1.9  2015/03/13 18:44:44  perev
 * Roll back
 *
 * Revision 1.7  2005/11/22 21:44:16  fisyak
 * Add Ssd to Associator, add IdTruth options for Svt and Ssd
 *
 * Revision 1.6  2003/06/27 03:01:19  calderon
 * The z cut now depends on z_mc.
 * The parameterization is done in the parameter DB
 * with a linearly increasing rms, symmetric in +/- z.
 *
 * Revision 1.5  1999/12/14 07:07:41  calderon
 * Added Ratio Number of Common Hits / Number of Reconstructed Hits for
 * each detector.
 * Numbering scheme from StEvent & StMcEvent as per SVT request
 * Added Kink, V0 and Xi vertex associations.
 *
 * Revision 1.4  1999/12/08 00:00:25  calderon
 * New version of StAssociationMaker.
 * -Uses new StEvent / StMcEvent
 * -Includes maps using reconstructed and monte carlo objects as keys for:
 *   TPC Hits
 *   SVT Hits
 *   SSD Hits
 *   FTPC Hits
 *   Tracks (using all 3 hit multimaps)
 *
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

    float xCutTpc() const; // *MENU*
    float yCutTpc() const; // *MENU*
    float zCutTpc() const; // *MENU*
    float zCutTpc(float z) const;
    unsigned int reqCommonHitsTpc() const; // *MENU*

    float xCutSvt() const; // *MENU*
    float yCutSvt() const; // *MENU*
    float zCutSvt() const; // *MENU*

    unsigned int reqCommonHitsSvt() const; // *MENU*

    float xCutSsd() const; // *MENU*
    float yCutSsd() const; // *MENU*
    float zCutSsd() const; // *MENU*
    unsigned int reqCommonHitsSsd() const; // *MENU*

    float rCutFtpc() const; // *MENU*
    float phiCutFtpc() const; // *MENU*
    unsigned int reqCommonHitsFtpc() const; // *MENU*
    
    void setXCutTpc(float); // *MENU*
    void setYCutTpc(float); // *MENU*
    void setZCutTpc(float); // *MENU*
    void setReqCommonHitsTpc(unsigned int); // *MENU*

    void setXCutSvt(float); // *MENU*
    void setYCutSvt(float); // *MENU*
    void setZCutSvt(float); // *MENU*
    void setReqCommonHitsSvt(unsigned int); // *MENU*
    
    void setXCutSsd(float); // *MENU*
    void setYCutSsd(float); // *MENU*
    void setZCutSsd(float); // *MENU*
    void setReqCommonHitsSsd(unsigned int); // *MENU*
    
    void setRCutFtpc(float); // *MENU*
    void setPhiCutFtpc(float); // *MENU*
    void setReqCommonHitsFtpc(unsigned int); // *MENU*
    
private:
    static StMcParameterDB *mParamDB;   
    StMcParameterDB();
    StMcParameterDB(const StMcParameterDB&);
    StMcParameterDB& operator= (const StMcParameterDB&);
    
    float mXCutTpc;
    float mYCutTpc;
    float mZCutTpc;
    unsigned int mReqCommonHitsTpc;

    float mXCutSvt;
    float mYCutSvt;
    float mZCutSvt;
    unsigned int mReqCommonHitsSvt;

    float mXCutSsd;
    float mYCutSsd;
    float mZCutSsd;
    unsigned int mReqCommonHitsSsd;

    float mRCutFtpc;
    float mPhiCutFtpc;
    unsigned int mReqCommonHitsFtpc;

    ClassDef(StMcParameterDB, 1)
    
};
ostream& operator<<(ostream &, const StMcParameterDB&);

inline float StMcParameterDB::xCutTpc() const { return mXCutTpc; }

inline float StMcParameterDB::yCutTpc() const { return mYCutTpc; }

inline float StMcParameterDB::zCutTpc() const { return mZCutTpc; }

inline unsigned int StMcParameterDB::reqCommonHitsTpc() const { return mReqCommonHitsTpc; }

inline float StMcParameterDB::xCutSvt() const { return mXCutSvt; }

inline float StMcParameterDB::yCutSvt() const { return mYCutSvt; }

inline float StMcParameterDB::zCutSvt() const { return mZCutSvt; }

inline unsigned int StMcParameterDB::reqCommonHitsSvt() const { return mReqCommonHitsSvt; }

inline float StMcParameterDB::xCutSsd() const { return mXCutSsd; }

inline float StMcParameterDB::yCutSsd() const { return mYCutSsd; }

inline float StMcParameterDB::zCutSsd() const { return mZCutSsd; }

inline unsigned int StMcParameterDB::reqCommonHitsSsd() const { return mReqCommonHitsSsd; }

inline float StMcParameterDB::rCutFtpc() const { return mRCutFtpc; }

inline float StMcParameterDB::phiCutFtpc() const { return mPhiCutFtpc; }

inline unsigned int StMcParameterDB::reqCommonHitsFtpc() const { return mReqCommonHitsFtpc; }

#endif

//
//  StMcParameterDB::instance()->xCutTpc()
//  or
//  StMcParameterDB* mydb =  StMcParameterDB::instance();
//  mydb->xCutTpc();
//

/***************************************************************************
 *
 * $Id: StEventClusteringHints.h,v 2.3 2001/04/20 00:50:49 ullrich Exp $
 *
 * Author: Thomas Ullrich, Apr 2001
 ***************************************************************************
 *
 * Description: Singeleton class to provide clustering hints for
 *              StEvent I/O. The class can distinguish between two modes
 *              miniDST and DST mode in which clustering hints
 *              are different.
 *              With clustering hints we mean the association
 *              of a class stored *directly* in the StEvent class
 *              (StEvent::mContent) and the name of the branch it
 *              will be written to.
 *
 ***************************************************************************
 *
 * $Log: StEventClusteringHints.h,v $
 * Revision 2.3  2001/04/20 00:50:49  ullrich
 * Added new query methods.
 *
 * Revision 2.2  2001/04/11 03:44:07  ullrich
 * Added namespaces for Sun CC5.
 *
 * Revision 2.1  2001/04/06 17:47:20  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEventClusteringHints_hh
#define StEventClusteringHints_hh

#include <map>
#include <string>
#include <vector>
#include <iostream.h>
#include "Rtypes.h"
#if !defined(ST_NO_NAMESPACES)
using std::map;
using std::string;
using std::vector;
#endif

class StEventClusteringHints {
public:
    static StEventClusteringHints* instance();  // get one and only instance (singleton)
    
    void setDstMode();                          // switch to DST mode
    void setMiniDstMode();                      // switch to miniDST mode
    const char* branchName(const char*) const;  // get branch name for given class name

#if !defined(__CINT__)    
    vector<string> listOfBranches() const;      // list of all branches for given mode (miniDST or DST)         
    vector<string> listOfClasses() const;       // list of all top level classes known     
    vector<string> listOfClasses(const char*) const;  // list of all top level classes for a given branch
#endif    
    
    void setBranch(const char*, const char*);   // assign classname with a branch name
    void print(ostream& = cout);                // print current configuration
    
    friend class nobody;                        // makes g++ warning disappear

private:
    StEventClusteringHints();
    virtual ~StEventClusteringHints();
    StEventClusteringHints(const StEventClusteringHints&);

private:
    static StEventClusteringHints *mSelf;   //!
    map<string,string> *mNameMap;           //!
    map<string,string> mDstMap;             //!
    map<string,string> mMiniDstMap;         //!
 
    ClassDef(StEventClusteringHints,1)
};
#endif

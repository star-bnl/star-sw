/***************************************************************************
 *
 * $Id: StPidAmpPath.hh,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             A net is divide into multiple paths along BetheBlock curve.
 *             StPidAmpPath is for describing such a path
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpPath.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpPath_hh
#define StPidAmpPath_hh

#include <iostream.h>
#include <strstream.h>
#include <string>

#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif


#include "TGraph.h"
#include "TH1.h"


#include "StPidAmpMaker/Infrastructure/StPidAmpParticle.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpSliceVector.hh"
#include "StPidAmpMaker/Infrastructure/StPidParamVector.hh"


//should be no fitting in Path.
//fitting should be taken care at Net level.

class StPidAmpPath {

public:

  StPidAmpPath();
  StPidAmpPath(const StPidAmpPath&);

  StPidAmpPath(int& idx,StPidAmpParticle* particleType,StPidAmpWindow& theWindow,string aNetName );
  
  ~StPidAmpPath();
  
  void fillPath(StPidAmpSliceVector& slices);

  void fillPathGraph(); 

  void fillFittedPath2Slices(StPidAmpSliceVector& slices); 
  //fill StPidAmpSlice::mPathFittedSlice

  void adjudgePathWindow(); 

  void                  setIndex(int& i);
  int                   index();
  TGraph*               pathGraph(); //return graph to net for fitting.
  StPidParamVector* pathParams();
  string               name() const;
  TH1D*                 pathFittedHisto();



private:

  int                   mIndex;
  string                mName;// netname+path idex
  TH1D*                 mPathHisto;
  TH1D*                 mPathFittedHisto; //histo. of values at fitted path curve
  TGraph*               mPathGraph;
  StPidAmpParticle*     mParticleType;
  StPidParamVector* mPathParams;
  StPidAmpWindow        mPathWindow;
  
};

ostream& operator<<(ostream& s, const StPidAmpPath& path);

inline void                  StPidAmpPath::setIndex(int& i){mIndex=i;}
inline int                   StPidAmpPath::index(){return mIndex;}
inline TGraph*               StPidAmpPath::pathGraph() { return mPathGraph;}
inline StPidParamVector* StPidAmpPath::pathParams() {return mPathParams;}
inline string                StPidAmpPath::name() const {return mName;}

#endif

/***************************************************************************
 *
 * $Id: StPidAmpChannelInfo.hh,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             class for identify a channel  
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpChannelInfo.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpChannelInfo_hh
#define StPidAmpChannelInfo_hh

#include <iostream.h>
#include <strstream.h>
#include <string>

#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif

#include "StPidAmpMaker/Infrastructure/StPidAmpTrk.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpCutVector.hh"

class StPidAmpChannelInfo{

public:

      StPidAmpChannelInfo();//default no cut at all. for mBGNet's sake.
      StPidAmpChannelInfo(StPidAmpCutVector cutCollect);

      ~StPidAmpChannelInfo();

      bool      isInChannel(StPidAmpTrk* trk);
      string name(); 
      StPidAmpCutVector cutVector();


private:

      void makeName();

      
      string mName;
   //name is consist of various cuts.
  //would not find two identical channel names in this package.

      StPidAmpCutVector mCutVector;

};
// bydefault,
// mCutVector[0] is Nhits cut.
// mCutVector[1] is PtCut
// Never change the order of Cuts in StPidAmpCutVector!

ostream& operator<<(ostream& s,const StPidAmpChannelInfo& info);

inline string StPidAmpChannelInfo::name(){ return mName;}
inline StPidAmpCutVector StPidAmpChannelInfo::cutVector(){return mCutVector;} 
#endif

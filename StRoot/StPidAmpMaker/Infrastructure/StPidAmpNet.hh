/***************************************************************************
 *
 * $Id: StPidAmpNet.hh,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             Abstract class for nets
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpNet.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpNet_hh
#define StPidAmpNet_hh


#include <iostream.h>
#include <string>

#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif


#include "TGraph.h"
#include "TH1.h"
#include "TH3.h"

#include "StPidAmpMaker/Infrastructure/StPidParamVector.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpChannelInfo.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpTrk.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpWindow.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpPath.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpSlice.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpParticle.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpSliceVector.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpPathVector.hh"
#include "StPidAmpMaker/StPidAmpTrkVector.h"
#include "StPidAmpMaker/Include/StPidAmpConst.hh"
#include "StPidAmpMaker/StPidAmpNetOut.h"


class StPidAmpChannelCollection;

// note: To avoid confusing conversion between +/- particles,
// all net position, path position, slice position were designed by
// assumming there were for positive particles type.
// that means StPidAmpSlice::mMeanRig (etc)>0 always!!


class StPidAmpNet {

public:

  StPidAmpNet();
  StPidAmpNet(StPidAmpParticle def, StPidAmpChannelInfo channelInfo); 
  StPidAmpNet(const StPidAmpNet&);
  virtual ~StPidAmpNet();

  
  void fillNet(StPidAmpTrkVector* trks,StPidAmpChannelCollection* set=0); 
  void fillAmp();//fill amp graph.
  void fillReso(); //fill linr graph. 
  void adjudgeWindow();
  void setBandParams(StPidParamVector& pars);
  void fillBetaGammaNet(StPidAmpTrkVector* trks,StPidAmpChannelCollection* set); 
  void fitPaths(StPidAmpTrkVector* trks,TH3D* histo);
  int  getSliceIndex(StPidAmpTrk* trk);

  virtual double dedxAtBandCenter(double rig);
  virtual int    getSliceIndex(double x);
  virtual void   fitBand(TH3D* histo);//fit band. put result in mBandParams.
  virtual void   fitAmp(StPidAmpTrkVector* trks,TH3D* histo);
  virtual void   fillPathFittedSlices();
  virtual void   fitReso(); 
  virtual void   drawNetFittings();
  virtual void   setUp(); 
  virtual void   fillBand(); //fill band graph.
  virtual void   processNet(StPidAmpTrkVector* trks=0,TH3D* histo=0);
  virtual void   fillNetOut();//fill mNetOut. 
  virtual void   fitAPath(StPidAmpPath& path, StPidAmpTrkVector* trks,TH3D* histo);

  virtual ostream& put(ostream& s) const;



  TGraph*                     bandGraph() const;
  TGraph*                      ampGraph() const;
  TGraph*                     linrGraph() const;
  StPidParamVector            ampParams() const;
  StPidParamVector           bandParams() const;
  StPidAmpSliceVector*      sliceVector() const;
  StPidAmpPathVector*        pathVector() const;
  string                           name() const;
  StPidAmpParticle         particleType() const;
  StPidAmpNetOut                 netOut() const;

  void setSlicesFitDraw(bool br);
  void setPathsFitDraw(bool br);
  void setAmpFitDraw(bool br);
  void setBandFitDraw(bool br);
  void setResoFitDraw(bool br);
  void setParticleType(StPidAmpParticle p);

  void setFitBand(bool br);
  void setFitPath(bool br);
  void setFitAmp(bool br);
  void setFitReso(bool br);



  static void setDefaultBandParams(StPidParamVector& pars);


private:

  
  void pushATrk(StPidAmpTrk* theTrack, StPidAmpChannelCollection* set=0);
  void fillSlices(StPidAmpTrkVector* trks,StPidAmpChannelCollection* set=0);
  void fillPaths(); //use slices to fill paths.
  void push2BGNet(StPidAmpNet& net, StPidAmpTrk* trk);

  StPidAmpSliceVector* mSliceCollect; //reserve(1000)
  StPidAmpPathVector*  mPathCollect;

  TGraph*         mBandGraph;
  TGraph*         mAmpGraph;
  TGraph*         mResoGraph;

  bool      mDrawPathsFit;
  bool      mDrawAmpFit;
  bool      mDrawResoFit;

  bool      mFitPath;
  bool      mFitAmp;
  bool      mFitReso;

  static StPidParamVector mDefaultBandParams;


protected:


  double ( *funcBandPt) (double *, double *);
  void     fitSlices(); // gauss fitting for all slices.
  void     drawSlicesInASegment(strstream& st, double theBegin, double theEnd);
  double   maxPoint(TGraph* gr,bool value);//return the max dedx value of points in gr.


  bool      mFitBand;
  bool      mDrawSlicesFit;
  bool      mDrawBandFit;

  StPidAmpNetOut  mNetOut;//the obj. that will be written to disk.


  string          mName;//=cannelInfo::mName+particleType::mName.

  StPidParamVector     mAmpParams;
  StPidParamVector     mResoParams;
  StPidParamVector     mBandParams;
  //if fitOpt.Contains("B") mBandParams will be parameters for betaGamma.

  StPidAmpParticle         mParticleType;
  StPidAmpChannelInfo      mChannelInfo;
  StPidAmpWindow           mNetWindow;


    
};

ostream& operator<<(ostream& s, const StPidAmpNet& net);

inline TGraph* StPidAmpNet::bandGraph() const { return mBandGraph; }
inline TGraph* StPidAmpNet::ampGraph()  const { return mAmpGraph; }
inline TGraph* StPidAmpNet::linrGraph() const { return mResoGraph; }
inline string  StPidAmpNet::name()      const { return mName; }

inline StPidParamVector StPidAmpNet::ampParams() const {return mAmpParams;}
inline StPidParamVector StPidAmpNet::bandParams() const {return mBandParams;}
inline StPidAmpSliceVector* StPidAmpNet::sliceVector() const {return mSliceCollect;}
inline StPidAmpPathVector* StPidAmpNet::pathVector() const {return mPathCollect; }
inline StPidAmpParticle StPidAmpNet::particleType() const {return mParticleType;}
inline StPidAmpNetOut StPidAmpNet::netOut() const {return mNetOut;}

inline void StPidAmpNet::setSlicesFitDraw(bool br){ mDrawSlicesFit=br;}
inline void StPidAmpNet::setPathsFitDraw(bool br){ mDrawPathsFit=br;}
inline void StPidAmpNet::setAmpFitDraw(bool br){ mDrawAmpFit=br;}
inline void StPidAmpNet::setBandFitDraw(bool br){ mDrawBandFit=br;}
inline void StPidAmpNet::setResoFitDraw(bool br){ mDrawResoFit=br;}
inline void StPidAmpNet::setParticleType(StPidAmpParticle p){mParticleType=p;}

inline void StPidAmpNet::setFitBand(bool br){mFitBand=br;}
inline void StPidAmpNet::setFitPath(bool br){mFitPath=br;}
inline void StPidAmpNet::setFitAmp(bool br){mFitAmp=br;}
inline void StPidAmpNet::setFitReso(bool br){mFitReso=br;}

#endif

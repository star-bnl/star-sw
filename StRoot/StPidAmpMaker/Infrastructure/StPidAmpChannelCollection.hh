/***************************************************************************
 *
 * $Id: StPidAmpChannelCollection.hh,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpChannelCollection is a complete ensemble of 
 *             StPidAmpChannels. Can not find two identical channels in
 *             StPidAmpChannelCollection
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpChannelCollection.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpChannelCollection_hh
#define StPidAmpChannelCollection_hh

#include <vector>
#include <iostream.h>
#include <strstream.h>
#include <string>

#if !defined(ST_NO_NAMESPACES)
using std::string;
using std::vector;
#endif


#include "TString.h"

#include "TH1.h"
#include "TH3.h"

#include "StPidAmpMaker/Infrastructure/StPidAmpChannel.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpChannelVector.hh"
#include "StPidAmpMaker/Infrastructure/StPidParamVector.hh"
#include "StPidAmpMaker/Infrastructure/StPidAmpTrk.hh"
#include "StPidAmpMaker/Include/StPidAmpEnum.hh"
//#include "StPidAmpMaker/StPidAmpTrkVector.h"



class StPidAmpChannelCollection {
 
public:

    StPidAmpChannelCollection();
    
    StPidAmpChannelCollection(int n, int* nhitsAry,int p, double* ptAry, StPidAmpNetType theNetType,TString fitOpt, TString drawOpt);

    ~StPidAmpChannelCollection();

      
    void process(StPidAmpTrkVector* trks,TH3D* histo); 
  //do all stuff for output parameters, and save them to channel


    string                     name();
    void                       setDrawOpt(TString s);
    void                       setWritePars2Disk(bool br);//write out?
    void                       setNHits4BGNet(int n);
    int                        nhits4BGNet();
 
  StPidAmpChannelVector* channelVector();
    
  StPidAmpNet* bgNet();//a net for dedx~betaGamma fitting
  StPidAmpNet* bgElectronNet();//a net for dedx~betaGamma fitting vi e-
  StPidAmpNet* bgPositronNet();//a net for dedx~betaGamma fitting vi e+

  StPidAmpNet* bgPionPlusNet();//a net for dedx~betaGamma fitting vi pi+
  StPidAmpNet* bgPionMinusNet();//a net for dedx~betaGamma fitting vi pi-
  StPidAmpNet* bgKaonPlusNet();//a net for dedx~betaGamma fitting vi k+
  StPidAmpNet* bgKaonMinusNet();//a net for dedx~betaGamma fitting vi k-
  StPidAmpNet* bgProtonNet();//a net for dedx~betaGamma fitting vi proton 
  StPidAmpNet* bgAntiProtonNet();//a net for dedx~betaGamma fitting vi anti

  StPidAmpNetType netType();


private:

      void filterOptions(StPidAmpNetType theNetType,TString fitOpt, TString drawOpt);
      void setUpChannels(int n, int* nhitsAry,int p, double* ptAry,StPidAmpNetType theNetType);
      void setDefaultBandParameters();
      void setDrawOpt(); //parse mDrawOpt, pass to channels.
      void processBGNet(bool fitBand, bool fitPath, bool fitAmp, bool fitReso,bool drawSlicesFit, bool drawPathsFit, bool drawAmpFit, bool drawBandFit, bool drawResoFit,StPidAmpNet& net);
      void drawMultiBGNets2Gether();
      void setUpBGNets();
      void writePars2Disk();//write out .root file
      void writeAmp2Disk();
      void writeReso2Disk();
      void writeBands2Disk();
      void writeBGBands2Disk();
      void outputBGBand4Debug();

      string                     mName; 
      TString                    mDrawOpt;
      TString                    mFitOpt;
      StPidAmpChannelVector* mChannelCollect;
      StPidAmpTrkVector*     mTrks;
      StPidParamVector       mBGParams; //fitting result of mBetaGamma.??????
      StPidAmpNetType            mNetType;
      bool                       mWritePars2Disk;
      bool                       mDrawBGNet;
      int                        mNHitsCut4BGNet;//if track's nhits < this, 
                                     //that track won't be filled into BGNets.
        

  StPidAmpNet* mBGNet;//a net for dedx~betaGamma fitting
  StPidAmpNet* mBGElectronNet;//a net for dedx~betaGamma fitting vi e-
  StPidAmpNet* mBGPositronNet;//a net for dedx~betaGamma fitting vi e+

  StPidAmpNet* mBGPionPlusNet;//a net for dedx~betaGamma fitting vi pi+
  StPidAmpNet* mBGPionMinusNet;//a net for dedx~betaGamma fitting vi pi-


  StPidAmpNet* mBGKaonPlusNet;//a net for dedx~betaGamma fitting vi k+
  StPidAmpNet* mBGKaonMinusNet;//a net for dedx~betaGamma fitting vi k-

  StPidAmpNet* mBGProtonNet;//a net for dedx~betaGamma fitting vi proton 
  StPidAmpNet* mBGAntiProtonNet;//a net for dedx~betaGamma fitting viantiproton




};

ostream& operator<<(ostream& s, StPidAmpChannelCollection& set);

inline string StPidAmpChannelCollection::name(){return mName;}
inline void StPidAmpChannelCollection::setDrawOpt(TString s){mDrawOpt=s;}
inline void StPidAmpChannelCollection::setWritePars2Disk(bool br){ mWritePars2Disk=br;} 
inline void StPidAmpChannelCollection::setNHits4BGNet(int n){mNHitsCut4BGNet=n;}

inline int  StPidAmpChannelCollection::nhits4BGNet(){ return mNHitsCut4BGNet;} 

inline StPidAmpNetType StPidAmpChannelCollection::netType(){return mNetType;}
inline StPidAmpChannelVector* StPidAmpChannelCollection::channelVector(){return mChannelCollect;}

inline StPidAmpNet* StPidAmpChannelCollection::bgNet(){return mBGNet;}
inline StPidAmpNet* StPidAmpChannelCollection::bgElectronNet(){return mBGElectronNet;}
inline StPidAmpNet* StPidAmpChannelCollection::bgPositronNet(){return mBGPositronNet;}
inline StPidAmpNet* StPidAmpChannelCollection::bgPionPlusNet(){return mBGPionPlusNet;}
inline StPidAmpNet* StPidAmpChannelCollection::bgPionMinusNet(){return mBGPionMinusNet;}
inline StPidAmpNet* StPidAmpChannelCollection::bgKaonPlusNet(){return mBGKaonPlusNet;}
inline StPidAmpNet* StPidAmpChannelCollection::bgKaonMinusNet(){return mBGKaonMinusNet;}
inline StPidAmpNet* StPidAmpChannelCollection::bgProtonNet(){return mBGProtonNet;}
inline StPidAmpNet* StPidAmpChannelCollection::bgAntiProtonNet(){return mBGAntiProtonNet;}





#endif

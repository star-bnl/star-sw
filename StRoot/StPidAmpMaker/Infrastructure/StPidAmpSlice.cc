/***************************************************************************
 *
 * $Id: StPidAmpSlice.cc,v 1.1.1.1 2000/03/09 17:48:35 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             Verticle slice along rigidity(p/z)
 ***************************************************************************
 *
 * $Log: StPidAmpSlice.cc,v $
 * Revision 1.1.1.1  2000/03/09 17:48:35  aihong
 * Installation of package
 *
 **************************************************************************/


#include "TF1.h"

#include "StPidAmpMaker/Infrastructure/StPidAmpSlice.hh"
#include "StPidAmpMaker/Include/StPidAmpConst.hh"

//--------------------------------
StPidAmpSlice::StPidAmpSlice(){
  /* no-op */
}

//--------------------------------
StPidAmpSlice::StPidAmpSlice(int idx,double meanRig, double lowBound, double highBound, double width, string aNetName, StPidAmpParticle* particleType):mUsePathFitResult(false){

    setSlice(idx,meanRig,lowBound,highBound,width);

    mName.append(aNetName);
    mName.append(" rig: ");
    strstream tempStream;
    tempStream<<float(mMeanRig);//float(int(float(mMeanRig)*100)/100.0);
    mName.append(tempStream.str());

    mParticleType = particleType;

    // a slice's name is channelinfo+particletype+rig

    // would not find two slices with the same name in this package.
    // same for path's name. net's name. channel's name.

    strstream statisSlice;
    strstream fittedSlice;
    strstream reconSlice;
    strstream diffSlice;

    statisSlice<<mName.c_str()<<" statSlice ";
    fittedSlice<<mName.c_str()<<" pthFitSlice ";
     reconSlice<<mName.c_str()<<" reconSlice ";
      diffSlice<<mName.c_str()<<" diffSlice ";
 

        mSlice=new TH1D(statisSlice.str(),statisSlice.str(),NPaths,mLowBound,mHighBound);
        mPathFittedSlice=new TH1D(fittedSlice.str(), fittedSlice.str(),NPaths,mLowBound,mHighBound);
        mReconstructedSlice=new TH1D(reconSlice.str(),reconSlice.str(),NPaths,mLowBound,mHighBound);
        mDiffSlice=new TH1D(diffSlice.str(),diffSlice.str(),NPaths,mLowBound,mHighBound);

        mSliceInfo=new StPidAmpSliceInfo();
}







//--------------------------------
StPidAmpSlice::StPidAmpSlice(const StPidAmpSlice& s) //copy constructor
{  
      mIndex=s.mIndex;
      mLowBound=s.mLowBound;
      mHighBound=s.mHighBound;
      mMidBound=s.mMidBound;
      mMeanRig=s.mMeanRig;
      mName=s.mName;
      mWidth=s.mWidth;
 
      mSlice=new TH1D();
      *mSlice=TH1D(*(s.mSlice));
//currently there is a problem with such format of assignment.
      //see ~aihong/newtpcpid/test9.C 

      mPathFittedSlice=new TH1D();
      *mPathFittedSlice=TH1D(*(s.mPathFittedSlice));
   
      mReconstructedSlice=new TH1D();
      *mReconstructedSlice=TH1D(*(s.mReconstructedSlice));

      mDiffSlice=new TH1D();
      *mDiffSlice=TH1D(*(s.mDiffSlice));

   
      *mParticleType=*(s.mParticleType);
      
  //mSliceInfo=StPidAmpSliceInfo(s.mSliceInfo); //be careful here, might not 
                                     //work. see ~aihong/newtpcpid/test9.C 

      mSliceInfo=new StPidAmpSliceInfo();
      *mSliceInfo=*(s.mSliceInfo);

        mUsePathFitResult=s.mUsePathFitResult;

}

//--------------------------------
StPidAmpSlice::~StPidAmpSlice()
{ /* no-op */}

//--------------------------------
void StPidAmpSlice::fill(StPidAmpTrk *trk, double x){
 
  if (fabs(trk->rig())>mLeftEdge&&
      fabs(trk->rig())<mRightEdge) {

    mSlice->Fill(x);
  }
}



//--------------------------------
void StPidAmpSlice::fit(){

  if (mSlice->GetMaximum()>0){//filter out empty slices.

        double vary1=0.2; //center fix range
        double vary2=0.3; //reso fix range
        double reso=0.15; //reso reference
        double sliceHeight=fabs(mHighBound-mLowBound);
    
        TF1 *mGaussFcn = new TF1("mGaussFcn", "gaus",mLowBound,mHighBound);
        mGaussFcn->SetParLimits(1, mMidBound-vary1*sliceHeight,mMidBound+vary1*sliceHeight);
        mGaussFcn->SetParLimits(2,reso*mMidBound*(1.0-vary2),reso*mMidBound*(1.0+vary2));

        if (mUsePathFitResult) {
        mPathFittedSlice->Fit("mGaussFcn","NR");
	}else {
        mSlice->Fit("mGaussFcn","NR");
	}
        mSliceInfo->setAmp(mGaussFcn->GetParameter(0)); 
        mSliceInfo->setMeanDedx(mGaussFcn->GetParameter(1));        
        mSliceInfo->setSigma(mGaussFcn->GetParameter(2)); 





        
        delete mGaussFcn;
  }
}


//--------------------------------
void StPidAmpSlice::fillReonstructedSlice(StPidAmpSliceInfo* sliceInfo){
        TF1 *mGaussFcn = new TF1("mGaussFcn", "gaus",mLowBound,mHighBound);
        mGaussFcn->SetParameter(0,sliceInfo->amp());
        mGaussFcn->SetParameter(1,sliceInfo->meanDedx());
        mGaussFcn->SetParameter(2,sliceInfo->sigma());

        for (int i=1; i<mReconstructedSlice->GetNbinsX()+1; i++){
        mReconstructedSlice->SetBinContent(i, mGaussFcn->Eval(mReconstructedSlice->GetBinCenter(i),0,0));
	}

}

//--------------------------------
TH1D* StPidAmpSlice::produceDiffSlice(){

        TH1D* diff=new TH1D();
          *diff=*mSlice;
        for (int i=1; i<diff->GetNbinsX()+1; i++){
        diff->SetBinContent(i, (mSlice->GetBinContent(i)-mReconstructedSlice->GetBinContent(i)));
	}

        return diff;
}



//--------------------------------
ostream& operator<<(ostream& s, const StPidAmpSlice& slice){

  return s<<*(slice.sliceInfo())<<endl;
}

/***************************************************************************
 *
 * $Id: StPidAmpPath.cc,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             A net is divide into multiple paths along BetheBlock curve.
 *             StPidAmpPath is for describing such a path
 ***************************************************************************
 *
 * $Log: StPidAmpPath.cc,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#include "StPidAmpMaker/Infrastructure/StPidAmpPath.hh"


//-----------------------------------
StPidAmpPath::StPidAmpPath()
{ /*no-op */ }

//-----------------------------------
StPidAmpPath::StPidAmpPath(const StPidAmpPath& path){ //copy constr
    mIndex=path.mIndex;
    mName=path.mName;
    mPathHisto=new TH1D();
    *mPathHisto=TH1D(*(path.mPathHisto));
    mPathFittedHisto=new TH1D();
    *mPathFittedHisto=TH1D(*(path.mPathFittedHisto));
    mPathGraph=new TGraph();
    *mPathGraph=TGraph(*(path.mPathGraph));
    *mParticleType=*(path.mParticleType);
    
    //    mPathParams->clear();
    //   for (int j=0; j<path.mPathParams.size(); j++){
    //    mPathParams->push_back(path.mPathParams->at(j));
    //    }

    mPathParams=new StPidParamVector();
    *mPathParams=*(path.mPathParams); //if this does not work, try above.

    mPathWindow=path.mPathWindow;

}


//-----------------------------------
StPidAmpPath::StPidAmpPath(int& idx,StPidAmpParticle* particleType, StPidAmpWindow& theWindow, string aNetName):mIndex(idx){

     
     mPathHisto      =new TH1D();
     mPathFittedHisto=new TH1D();
     mPathGraph      =new TGraph();
     mPathParams     =new StPidParamVector();

     mParticleType=particleType;
     mPathWindow  =theWindow;

     mName.append(aNetName);
     mName.append("Pthidx: ");
     strstream tempStream;
     tempStream<<idx<<" ";
     mName.append(tempStream.str());

     

}
     

//-----------------------------------
StPidAmpPath::~StPidAmpPath()
{ /*no-op*/ }


//-----------------------------------
void StPidAmpPath::fillPath(StPidAmpSliceVector& slices){


   int ii;
   strstream sta,stb;
   sta<<mName.c_str()<<"hist ";
   stb<<mName.c_str()<<"PthFit ";

  for (ii=1; ii<mPathHisto->GetNbinsX()+1; ii++) mPathHisto->SetBinContent(ii,0.0);

   *mPathHisto=TH1D(sta.str(), sta.str(),slices.size(),slices.front()->leftEdge(), slices.back()->rightEdge());

   *mPathFittedHisto=TH1D(stb.str(), stb.str(),slices.size(),slices.front()->leftEdge(), slices.back()->rightEdge());
 //construct a sibling TH1D. but do not fill it.


   for (ii=1; ii<mPathHisto->GetNbinsX()+1; ii++) {

     if (ii>slices.size()) break;

     mPathHisto->SetBinContent(ii,slices[(ii-1)]->slice()->GetBinContent(mIndex+1));
  //beaware of that the first in-range bin in TH1 is as "1" instead of "0"
  //but our path index begin with 0 !!
   }
}





//-----------------------------------
void StPidAmpPath::fillPathGraph(){
  //use window to filter points from TH1D, put them into TGrah and fit.

  //has not considered the neat cut edge of pt cut.
  //that will result a bunch of points along x axis of PathGraph
  //those dots should be excluded.

     int ii;
  
     for (ii=1; ii<mPathHisto->GetNbinsX()+1; ii++) {

       if (mPathWindow.isInWindow(mPathHisto->GetBinCenter(ii))) {
         mPathGraph->SetPoint(mPathGraph->GetN(),mPathHisto->GetBinCenter(ii),mPathHisto->GetBinContent(ii));
	 //in TGraph, the index of the first point is "0" , not "1"
       }

     }

         mPathGraph->SetPoint(mPathGraph->GetN(),4.0,0.0);


}









//-----------------------------------
void StPidAmpPath::fillFittedPath2Slices(StPidAmpSliceVector& slices){

  //values of pixel position on Fitted path curve is in mPathFittedHisto.
  //transfer them to StPidAmpSlice::mPathFittedSlice

   
   int ii;
   
      for (ii=1; ii<mPathFittedHisto->GetNbinsX()+1; ii++) {

     if (ii>slices.size()) break;

     slices[(ii-1)]->pathFittedSlice()->SetBinContent(mIndex+1,mPathFittedHisto->GetBinContent(ii));
  //beaware of that the first in-range bin in TH1 is as "1" instead of "0"
  //but our path index begin with 0 !!
   }
   
}
//-----------------------------------
void StPidAmpPath::adjudgePathWindow(){
  //adjudge the window specifically to a path.
  //if histo. of betaGamma in NetSet is chosen to be filled, we should not
  //adjudgePath Window!!  
   
  //window has dependence on path.
  //re-adjudge mPathWindow for this purpose.

  //but this func should not be called when fill mBetaGamma in NetSet.


  // no op. yet

}

//-----------------------------------
ostream& operator<<(ostream& s, const StPidAmpPath& path){

  return s<<"operator<<(ostream& s, const StPidAmpPath& path) not yet implemented"<<(path.name()).c_str()<<endl;//path.pathParams()<<endl;
 }




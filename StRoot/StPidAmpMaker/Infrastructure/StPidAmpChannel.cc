/***************************************************************************
 *
 * $Id: StPidAmpChannel.cc,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             StPidAmpChannel contains multiple StPidAmpNet.
 *             Only tracks satisfying channel condition can be filled into 
 *             nets inside that channel
 ***************************************************************************
 *
 * $Log: StPidAmpChannel.cc,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/



#include <strstream.h>

#include "TCanvas.h"

#include "StPidAmpMaker/Infrastructure/StPidAmpChannel.hh"
#include "StPidAmpMaker/Include/StPidAmpConst.hh"


//----------------------------------
StPidAmpChannel::StPidAmpChannel(){

  /*-no-op*/
   //channel should not be copied!!
  //because it has base class pointer
  //as member. 
  //but we want call its derived 
  //func of fit() etc.if copied, 
  //the actual content will be auto.
  // casted to base, and we resulted 
  //in a base version fit() calling.
}

//----------------------------------
StPidAmpChannel::~StPidAmpChannel(){
  /*-no-op*/

}

//----------------------------------
StPidAmpChannel::StPidAmpChannel(StPidAmpChannelInfo& channelInfo,StPidAmpNetType netType){

    mNetCollect=new StPidAmpNetVector();

    mDrawBandFits=true;
    mDrawAmpFits =true;
    mChannelInfo =channelInfo;

    setUp(netType); //setup nets.
}



//----------------------------------
void StPidAmpChannel::fillChannel(StPidAmpTrkVector* trks){


     StPidAmpNetIter iter;
     StPidAmpNet*    theNet;

     for (iter=mNetCollect->begin(); iter!=mNetCollect->end(); iter++){
          theNet=*iter;
          theNet->fillNet(trks);
     }
  

}


//----------------------------------
void StPidAmpChannel::drawFittings(){
       bool saveCanvas=false;

       int NPads=9;
       strstream stBandFit, stAmpFit,stResoFit;
       stBandFit<<"bandFits_"<<mChannelInfo.name().c_str();
       stAmpFit <<"ampFits_" <<mChannelInfo.name().c_str();   
       stResoFit<<"resoFits_"<<mChannelInfo.name().c_str(); 

       if (mDrawBandFits){
          
  TCanvas* theBandFitCanvas=new TCanvas(stBandFit.str(), stBandFit.str(),20,10,800,600);
  theBandFitCanvas->Divide(3,3);
  
  for(int j=0; j<mNetCollect->size();j++){
     if ((j+1)>NPads) break;
     theBandFitCanvas->cd(j+1);
     ((*mNetCollect)[j])->bandGraph()->Draw("A*");
  }//note that idex of pads of canvas begin with 1.
   //but our mNetCollect begin with 0.

  theBandFitCanvas->cd();
  if (saveCanvas) theBandFitCanvas->SaveAs("");

       }

       if (mDrawAmpFits){
  TCanvas* theAmpFitCanvas=new TCanvas(stAmpFit.str(), stAmpFit.str(),20,10,800,600);
  theAmpFitCanvas->Divide(3,3);
  
  for(int jj=0; jj<mNetCollect->size();jj++){
     if ((jj+1)>NPads) break;
     theAmpFitCanvas->cd(jj+1);
     ((*mNetCollect)[jj])->ampGraph()->Draw("A*");
  }//note that idex of pads of canvas begin with 1.
   //but our mNetCollect begin with 0.

  theAmpFitCanvas->cd();
  if (saveCanvas) theAmpFitCanvas->SaveAs("");
       }


       if (mDrawResoFits){
  TCanvas* theResoFitCanvas=new TCanvas(stResoFit.str(), stResoFit.str(),20,10,800,600);
  theResoFitCanvas->Divide(3,3);

  for ( int k=0; k<mNetCollect->size(); k++){

       if ((k+1)>NPads) break;
       theResoFitCanvas->cd(k+1);
      ((*mNetCollect)[k])->linrGraph()->Draw("A*");
  }
      theResoFitCanvas->cd();
     if (saveCanvas) theResoFitCanvas->SaveAs("");
       }



}


//----------------------------------
void StPidAmpChannel::fillBGNet(StPidAmpTrkVector* trks, StPidAmpChannelCollection* set){


     StPidAmpNetIter iter;
     StPidAmpNet*    theNet;
 
     for (iter=mNetCollect->begin(); iter!=mNetCollect->end(); iter++){
          theNet=*iter;
          theNet->fillBetaGammaNet(trks,set);
     }
     
}

//----------------------------------
void StPidAmpChannel::setBandParams4Nets(StPidParamVector& pars){
//put the fitted result of dedx~betagamma into nets.
     StPidAmpNetIter iter;
     StPidAmpNet* theNet;
     for (iter=mNetCollect->begin(); iter!=mNetCollect->end(); iter++){
          theNet=*iter;
          theNet->setBandParams(pars);
          //Be aware that StPidAmpNet::setBandParams(pars)
	  //just store the first three params of pars.
	  //because the StPidAmpNet::mBandParams wont be refreshed 
	  //unless the fitBand() function get called.
	  //so to avoid all mBandParams comes to be idential with 
	  //mBGNet's mBandParams,
	  //we choose just pick out the first "real" band parameters.
	  //the other 4 will be filled by a net's own "mass, charge etc.."
     }

}
//----------------------------------
void StPidAmpChannel::processChannel(StPidAmpTrkVector* trks,TH3D* histo,bool fitBd, bool fitPth, bool fitAp, bool fitLr){
  //assume getagamma fittings has finished.

     StPidAmpNetIter iter;
     StPidAmpNet* theNet;
     for (iter=mNetCollect->begin(); iter!=mNetCollect->end(); iter++){
          theNet=*iter;
          theNet->setFitBand(fitBd);
          theNet->setFitPath(fitPth);
          theNet->setFitAmp(fitAp);
          theNet->setFitReso(fitLr);

          theNet->processNet(trks,histo);
     }

      drawFittings();
      fillChannelInfoOut();
}

//----------------------------------
void StPidAmpChannel::setDefaultBandParams(StPidParamVector& bandPars){
  StPidAmpNet::setDefaultBandParams(bandPars);
}
//----------------------------------
void StPidAmpChannel::setUp(StPidAmpNetType netType){ //set up nets.


  if (netType==noDependent){	   

  StPidAmpDefaultNet*      electronNet=new StPidAmpDefaultNet(StPidAmpParticle::mElectron,   mChannelInfo);
  StPidAmpDefaultNet*       piMinusNet=new StPidAmpDefaultNet(StPidAmpParticle::mPiMinus,    mChannelInfo);
  StPidAmpDefaultNet*     kaonMinusNet=new StPidAmpDefaultNet(StPidAmpParticle::mKaonMinus,  mChannelInfo);
  StPidAmpDefaultNet*    antiProtonNet=new StPidAmpDefaultNet(StPidAmpParticle::mAntiProton, mChannelInfo);
  StPidAmpDefaultNet*      positronNet=new StPidAmpDefaultNet(StPidAmpParticle::mPositron,   mChannelInfo);
  StPidAmpDefaultNet*        piPlusNet=new StPidAmpDefaultNet(StPidAmpParticle::mPiPlus,     mChannelInfo);
  StPidAmpDefaultNet*      kaonPlusNet=new StPidAmpDefaultNet(StPidAmpParticle::mKaonPlus,   mChannelInfo);
  StPidAmpDefaultNet*        protonNet=new StPidAmpDefaultNet(StPidAmpParticle::mProton,     mChannelInfo);
  StPidAmpDefaultNet*      deuteronNet=new StPidAmpDefaultNet(StPidAmpParticle::mDeuteron,   mChannelInfo);

  //negative particles first



  mNetCollect->clear();
  mNetCollect->reserve(NParticleTypes);

  mNetCollect->push_back(electronNet);
  mNetCollect->push_back(piMinusNet);
  mNetCollect->push_back(kaonMinusNet);
  mNetCollect->push_back(antiProtonNet);
  mNetCollect->push_back(positronNet);
  mNetCollect->push_back(piPlusNet);
  mNetCollect->push_back(kaonPlusNet);
  mNetCollect->push_back(protonNet); 
  mNetCollect->push_back(deuteronNet);

  }else if (netType==nhitsDependent){

  StPidAmpNHitsNet*      electronNet=new StPidAmpNHitsNet(StPidAmpParticle::mElectron,   mChannelInfo);
  StPidAmpNHitsNet*       piMinusNet=new StPidAmpNHitsNet(StPidAmpParticle::mPiMinus,    mChannelInfo);
  StPidAmpNHitsNet*     kaonMinusNet=new StPidAmpNHitsNet(StPidAmpParticle::mKaonMinus,  mChannelInfo);
  StPidAmpNHitsNet*    antiProtonNet=new StPidAmpNHitsNet(StPidAmpParticle::mAntiProton, mChannelInfo);
  StPidAmpNHitsNet*      positronNet=new StPidAmpNHitsNet(StPidAmpParticle::mPositron,   mChannelInfo);
  StPidAmpNHitsNet*        piPlusNet=new StPidAmpNHitsNet(StPidAmpParticle::mPiPlus,     mChannelInfo);
  StPidAmpNHitsNet*      kaonPlusNet=new StPidAmpNHitsNet(StPidAmpParticle::mKaonPlus,   mChannelInfo);
  StPidAmpNHitsNet*        protonNet=new StPidAmpNHitsNet(StPidAmpParticle::mProton,     mChannelInfo);
  StPidAmpNHitsNet*      deuteronNet=new StPidAmpNHitsNet(StPidAmpParticle::mDeuteron,   mChannelInfo);

  //negative particles first

 
  


  mNetCollect->clear();
  mNetCollect->reserve(NParticleTypes);

  mNetCollect->push_back(electronNet);
  mNetCollect->push_back(piMinusNet);
  mNetCollect->push_back(kaonMinusNet);
  mNetCollect->push_back(antiProtonNet);
  mNetCollect->push_back(positronNet);
  mNetCollect->push_back(piPlusNet);
  mNetCollect->push_back(kaonPlusNet);
  mNetCollect->push_back(protonNet); 
  mNetCollect->push_back(deuteronNet);
  }







}
//---------------------------------
void StPidAmpChannel::fillChannelInfoOut(){

     mChannelInfoOut.SetNHitsRange(((mChannelInfo.cutVector())[0]).lowEdge(), ((mChannelInfo.cutVector())[0]).highEdge());
     mChannelInfoOut.SetPtRange(((mChannelInfo.cutVector())[1]).lowEdge(),((mChannelInfo.cutVector())[1]).highEdge());

}


//----------------------------------
ostream& operator<<(ostream& s, StPidAmpChannel& channel){
  
     StPidAmpNetIter iter;
     StPidAmpNet* theNet;
     for (iter=(channel.netVector())->begin(); iter!=(channel.netVector())->end(); iter++){
     theNet=*iter;
     s<<*theNet<<endl;
     }

     return s;
}
     

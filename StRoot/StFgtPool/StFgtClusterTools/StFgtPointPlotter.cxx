
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtPointCollection.h"
#include "StFgtPointPlotter.h"

#include "StRoot/StEvent/StFgtHit.h"
#include "StRoot/StEvent/StFgtPoint.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StEventInfo.h"
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StarClassLibrary/StThreeVectorF.hh"

#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <utility>
#include <TArc.h>
#include <TLine.h>
#include <set>



Int_t StFgtPointPlotter::Init()
{
  outRootFile=new TFile("pointPlots.root","RECREATE");

  histos=new TH2D*[6*4];
  tbHistos=new TH1D*[6*4];
  tbCorrelation=new TH2D("tbCorrelation","tbCorrelation",15,0,15,15,0,15);


  char buffer[200];
  for(int i=0;i<6;i++)
    {
      for(int iq=0;iq<4;iq++)
	{
	  sprintf(buffer,"disk_%d_quad_%d",i,iq);
	  cout <<"init histo: " << i*4+iq << ", " << buffer <<endl;
	  histos[i*4+iq]=new TH2D(buffer,buffer,100,-40,40,100,-40,40);
	  sprintf(buffer,"MaxTimeBin_disk_%d_quad_%d",i,iq);
	  tbHistos[i*4+iq]=new TH1D(buffer,buffer,15,0,14);
	}
    }
  return kStOk;
}

Int_t StFgtPointPlotter::Make()
{

   Int_t ierr = kStOk;

   StEvent* eventPtr = 0;
   eventPtr = (StEvent*)GetInputDS("StEvent");

   if( !eventPtr ) {
      LOG_ERROR << "Error getting pointer to StEvent from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };

   StFgtCollection* fgtCollectionPtr = 0;

   if( eventPtr ) {
      fgtCollectionPtr=eventPtr->fgtCollection();
   };

   if( !fgtCollectionPtr) {
      LOG_ERROR << "Error getting pointer to StFgtCollection from '" << ClassName() << "'" << endm;
      ierr = kStErr;
   };
   StFgtPointCollection *pointCollectionPtr = fgtCollectionPtr->getPointCollection();

   if( !pointCollectionPtr ){
      LOG_ERROR << "Error getting pointer to StFgtPointCollection from StFgtCollection" << endm;
      ierr = kStErr;
   };
   Int_t numPoints=pointCollectionPtr->getNumPoints();

   cout <<"point plotter: we have " << numPoints << " points " <<endl;
   const StSPtrVecFgtPoint& pointVec=pointCollectionPtr->getPointVec();
   StSPtrVecFgtPointConstIterator iter1;
   for(iter1 = pointVec.begin();iter1!=pointVec.end();++iter1)
     {
       Float_t R=(*iter1)->getPositionR();
       Float_t Phi=(*iter1)->getPositionPhi();

       Int_t disk=(*iter1)->getDisc();
       Int_t quad=(*iter1)->getQuad();

       Float_t x=R*cos(Phi);
       Float_t y=R*sin(Phi);

       Int_t tbR=(*iter1)->getHitR()->getMaxTimeBin();
       Int_t tbPhi=(*iter1)->getHitPhi()->getMaxTimeBin();

       cout <<"disk: " << disk <<" quad: " << quad << ", point at (" <<x << ", " << y << ") "<<endl;
       histos[disk*4+quad]->Fill(x,y);
       tbHistos[disk*4+quad]->Fill(tbR);
       tbHistos[disk*4+quad]->Fill(tbPhi);

       tbCorrelation->Fill(tbR,tbPhi);
     }

  return ierr;
}

Int_t StFgtPointPlotter::Finish()
{
  for(int i=0;i<6;i++)
    {
      for(int iq=0;iq<4;iq++)
	{
	  histos[i*4+iq]->Write();
	  tbHistos[i*4+iq]->Write();
	}
    }
  tbCorrelation->Write();
  outRootFile->Write();
  return kStOk;
}

StFgtPointPlotter::~StFgtPointPlotter()
{

}

StFgtPointPlotter::StFgtPointPlotter( const Char_t* name): StMaker( name )
{

}


ClassImp(StFgtPointPlotter);

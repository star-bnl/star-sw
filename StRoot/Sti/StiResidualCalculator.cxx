//StiResidualCalculator.cxx
/***************************************************************************
 *
 * $Id: StiResidualCalculator.cxx,v 2.2 2003/04/29 18:48:33 pruneau Exp $
 *
 * \class  StiResidualCalculator provides a utility for determining the
 *         track residuals.
 * \author Andrew Rose, Wayne State University 
 * \date   October 2002
 ***************************************************************************
 * $Log: StiResidualCalculator.cxx,v $
 * Revision 2.2  2003/04/29 18:48:33  pruneau
 * *** empty log message ***
 *
 * Revision 2.1  2003/04/29 14:59:01  andrewar
 * Modified to conform to naming convention. Added
 * initDetectors(StiDetectorBuilder) to switch desired detectors 'off' during
 * tracking, so residual will be unbiased.
 *
 */

#include <math.h>
#include <float.h>

//STL
#include <algorithm>
using std::transform;
#include <numeric>
using std::accumulate;
#include <functional>
using std::less;


//ROOT
#include "TH3.h"
#include "TH2.h"
#include "TFile.h"

//StEvent includes
#include "StDetectorId.h"
#include "StHit.h"

//Sti Includes
#include "Sti/StiNeverActiveFunctor.h"
#include "Sti/StiHit.h"
#include "Sti/StiTrack.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiTrackContainer.h"
#include "Sti/StiDetectorBuilder.h"

#include "Sti/StiResiduals.h"
#include "Sti/StiResidualCalculator.h"

class TH3D;
class TFile;

//Constructor
StiResidualCalculator::StiResidualCalculator(StiHitContainer *hitC, StiDetectorBuilder *detBuilder)
  :candidateHits(hitC)
{
  candidates.clear();
  
  cout <<"StiResidualCalculator::StiResidualCalculator created "
       <<endl;

  //Okay detector, init hists
  int check=Init();
  if(check<1) cout <<"StiResidualCalculator::StiResidualCalculator "
		   <<"Error while initializing histograms."<<endl;

  if(detBuilder) initDetector(detBuilder);
  
  return;
}

void StiResidualCalculator::initDetector(StiDetectorBuilder *detBuilder)
{
  cout <<"StiResidualCalculator::initDetector"<<endl;
  if (detBuilder)
    {
      cout <<"Group ID: "<<detBuilder->getGroupId()<<endl;
      int rows = detBuilder->getNRows();
      cout <<"Rows: "<<rows;
      int sectors = detBuilder->getNSectors(0);
      cout <<" Sectors "<<sectors<<endl;

      isNotActiveFunc = new StiNeverActiveFunctor();
      

      for(int i=4, j=0; j<sectors;j++)
	{
	  StiDetector* det = detBuilder->getDetector(i,j);
	  //mark detector as unused

	  det->setIsActive(isNotActiveFunc);
	  //store detector pointer in vector
	  candidates.push_back(det);
	  cout <<"Residuals for "<<det->getName()
	       <<" set"<<endl;
	  mDetectorHist->Fill(i,j);
	}
    }
  else
    cout << "StiResidualCalculator::No valid Detector Builder found."<<endl;


  return;
}

//Methods
int StiResidualCalculator::Init()
{

  cout <<"StiResidualCalculator::Init"<<endl;

  //!
  //! Initialize histograms (with detector info).
  //! Returns 0 if fails, 1 if success.
  //!


  string angleYBaseName = "yResidualCrossDip";
  string coordYBaseName = "yResidualZYRow";
  string angleZBaseName = "zResidualCrossDip";
  string coordZBaseName = "zResidualZYRow";
  string angleBaseName = "ResidualCrossDip";
  string coordBaseName = "ResidualZYRow";
  string detectorBaseName;
  string incrimentName;

  //determine detector
//    switch(mDetector)
//    {
//      case kTpcId:
//         //TPC
//         detectorBaseName="Tpc";
//         incrimentName="Row %dd";
//         numLayers=45;
//         break;
//      case kSvtId:
//        //SVT
//         detectorBaseName="Svt";
//         incrimentName="Layer %dd";
//         numLayers=3;
//         break;
//      default:
//        //NOT a tracking detector???!?!? (at least, one I want to deal with)
//         break;
//    }
//      //make hist name strings from base names. This makes the pointer
    //name unique.
       detectorBaseName="Tpc";
       incrimentName="Row %dd";
    angleYBaseName+=detectorBaseName;
    coordYBaseName+=detectorBaseName;
    angleZBaseName+=detectorBaseName;
    coordZBaseName+=detectorBaseName;


  //setup hists
  mYResidualCrossDip = new TH3D(angleYBaseName.c_str(),"Residual in Y vs. Cross, Dip", 128, -1.6, 1.6, 128, -1.6, 1.6, 128, -2.,2.);
  mYResidualZY=new TH3D(coordYBaseName.c_str(),"",128,-2.,2.,128,-2.,2.,128,-2.,2.);
  mZResidualCrossDip = new TH3D(angleZBaseName.c_str(),"Residual in Z vs. Cross, Dip", 128, -1.6, 1.6, 128, -1.6, 1.6, 128, -2.,2.);
  mZResidualZY=new TH3D(coordZBaseName.c_str(),"",128,-2.,2.,128,-2.,2.,128,-2.,2.);
  mResidualCrossDip = new TH3D(angleBaseName.c_str(),"Residual in Z vs. Cross, Dip", 128, -1.6, 1.6, 128, -1.6, 1.6, 128, -2.,2.);
  mResidualZY=new TH3D(coordBaseName.c_str(),"",128,-2.,2.,128,-2.,2.,128,-2.,2.);


  mCross = new TH1D("mCross","",30,-1.7,1.7);
  mDip = new TH1D("mDip","",30,-1.7,1.7);
  mPt = new TH1D("mPt","",30,0.,6.);
  mDrift = new TH1D("mDrift","",100,-200.,200.);

  mDetectorHist = new TH2D("mDetectorHist","Detectors in Study", 
		       50,0.,50.,12,0.,12.);

  return 1;
}

void StiResidualCalculator::calcResiduals(StiTrackContainer *tracks)
{

  //Do some checking to make sure the next call isn't pointless
  if(candidates.size()==0)
    {
      cout<<"StiResidualCalculator::trackResiduals"
	  <<" no detectors initialized for residuals."<<endl;
      return;
    }  

  //get iterator from track container
  TrackToTrackMap::const_iterator trackIt = tracks->begin();
  //trackIterator

  //loop over tracks
  int check=0;
  while(trackIt!=tracks->end())
    {
      check= trackResidue((*trackIt).second);
      trackIt++;
    }

  return;
}

int StiResidualCalculator::trackResidue(const StiTrack *track)
{
  //Take a general StiTrack. Since this maker is written
  //specifically for StiKalmanTracks (needs handles in
  //Kalman tracks), check for castness to Kalman track.
  //If fails, exit. If success, call trackResidue(StiKalmanTrack).



  //cast to Kalman Track; if fail, end

       const StiKalmanTrack* kTrack = 
	 static_cast<const StiKalmanTrack*>(track);  
      if(!kTrack)
	{
	  cout <<"Error: Could not cast to Kalman Track!"<<endl;
	  return -10;
	}  

     int check = trackResidue(kTrack);
     return check;
}

int StiResidualCalculator::trackResidue(const StiKalmanTrack *track)
{

  //! Retrieves the node list for the input KalmanTrack,
  //! calls a subroutine to fill the hists with the
  //! pertinant values (fillHist).


  double nHits =0;
  double otherHits=0;
     
  StiKalmanTrackNode* leaf = track->getLastNode();
  StiKTNForwardIterator iT(leaf);
  StiKTNForwardIterator end = iT.end();

  double pt=track->getPt();

  while(iT != end)
    {
      
      StiKalmanTrackNode iNode = (*iT);
      
      //if the node has no vail detector pointer, go on to next node
      if(!iNode.getDetector()) {iT++;continue;}


      //Residuals are calculated uner following conditions:
      //1.) The node's detector is *not* active
      //2.) The detector has been entered into the list of
      //    detectors to examine (should be an exclusive subset of 1) 
      if(!(iNode.getDetector()->isActive())
	 && (find(candidates.begin(), candidates.end(),iNode.getDetector())
  	     !=candidates.end()))
	{

	  if(iNode.getHit())
	    { 
	      cout <<"Warning! Node has a hit!"<<endl
	           <<"Detector: "<<iNode.getDetector()->getName()
		   <<endl
		   <<"Is Active?: "<<iNode.getDetector()->isActive()<<endl
		   <<"Hit det?: "<<iNode.getHit()->detector()->getName()
		   <<endl;
	      ++iT;
	      continue;
	    }
	  //get node values
	  double cross = iNode.crossAngle();
	  double dip   = iNode.pitchAngle();
	  double nodeZ = iNode.getZ();
	  double nodeY = iNode.getY();
	  double dy,dz;

	  fillTrackHist(cross,dip,pt, nodeZ);

	  //Examine a 20cm x 20cm window around track projection.
  	  candidateHits->setDeltaD(20.);
  	  candidateHits->setDeltaZ(20.);
 	  candidateHits->setRefPoint(iNode);
	

	  StiHit* hit;
	  //cout <<"Hit container size: "<<candidateHits.size()<<endl;

	  while(candidateHits->hasMore())
	    {
	      hit = candidateHits->getHit();
	
	      //Fill local Y residuals
	      dy = hit->y() - nodeY;
	      //Fill local Z residuals
	      dz = hit->z() - nodeZ;
	  
	      FillHist(nodeZ,nodeY,cross,dip,dz,dy);
	      nHits++;
	    }//end loop over hits in detector
	}//end if iT valid

      
      iT++;
    }//end while over nodes


  return 1;
}


void StiResidualCalculator::Write(char* outfile)
{

  //! Writes the residual hists to a file.

  cout <<"StiResidualCalculator::Write to file "<<outfile<<endl;

  //open output file
  TFile *f = new TFile(outfile,"RECREATE");

  //dump hists to file
  mYResidualCrossDip->Write();
  mYResidualZY->Write();
  mZResidualCrossDip->Write();
  mZResidualZY->Write();

  mResidualCrossDip->Write();
  mResidualZY->Write();

  mCross->Write();
  mDip->Write();
  mPt->Write();
  mDrift->Write();

  mDetectorHist->Write();

  //close file
  f->Close();
}

void StiResidualCalculator::FillHist(double z, double y,
				     double cross, double dip,
				     double dz, double dy)
{
  mYResidualCrossDip->Fill(cross, dip, dy);
  mYResidualZY->Fill(z, y, dy);

  mZResidualCrossDip->Fill(cross, dip, dz);
  mZResidualZY->Fill(z, y, dz);


  double diff = sqrt(dy*dy+dz*dz);
  mResidualCrossDip->Fill(cross, dip, diff);
  mResidualZY->Fill(z, y, diff);
}

void StiResidualCalculator::fillTrackHist(double cross, double dip,
					  double pt, double z)
{
  mCross->Fill(cross);
  mDip->Fill(dip);
  mPt->Fill(pt);
  mDrift->Fill(z);
}

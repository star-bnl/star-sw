//StiResidualMaker.cxx
/***************************************************************************
 *
 * $Id: StiResidualMaker.cxx,v 2.3 2003/03/16 21:57:33 andrewar Exp $
 *
 * \class  StiResidualMaker provides a utility for determining the
 *         track residuals.
 * \author Andrew Rose, Wayne State University 
 * \date   October 2002
 ***************************************************************************
 * $Log: StiResidualMaker.cxx,v $
 * Revision 2.3  2003/03/16 21:57:33  andrewar
 * Fixed filling bug, removed couts and improved functionality
 *
 * Revision 2.2  2003/03/16 16:52:12  andrewar
 * Add histograms, removed redundant checks on hits
 *
 * Revision 2.1  2003/01/08 21:17:57  pruneau
 * Addind class StiSortedHitIterator to work in the seed finder
 * and StiDummyVertex finder to provide an StEvent based vertex
 * retrieval mechanism.
 *
 * Revision 2.0  2002/12/10 21:59:58  pruneau
 * Introducing version 2.0
 *
 * Revision 1.3  2002/11/19 22:32:26  andrewar
 * Fixed bug with vector of node pointers.
 *
 * Revision 1.1  2002/10/16 18:42:04  andrewar
 * Initial commit. Methods for derived class StiResidualMaker.
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
#include "TFile.h"

//StEvent includes
#include "StDetectorId.h"
#include "StHit.h"

//Sti Includes
#include "Sti/StiHit.h"
#include "Sti/StiTrack.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"
#include "Sti/StiTrackContainer.h"

#include "Sti/StiResiduals.h"
#include "Sti/StiResidualMaker.h"

class TH3D;
class TFile;

//Constructor
StiResidualMaker::StiResidualMaker(StDetectorId det)
                  :mDetector(det)
{

  cout <<"StiResidualMaker::StiResidualMaker set for detector "
       <<det<<endl;
  
  //check for user sanity
  if(mDetector==kUnknownId)
    {//initialize with detector==0
      cout<<"StiResidualMaker::StiResidualMaker ERROR-"
	  <<"Initialize with detector = kUnknownId."
	  <<endl
	  <<"\tAll Residual histograms will be empty " <<endl
	  <<"\t and future calls to this maker will be ignored."<<endl
	  <<endl;
      return;
    }


  //check for valid detector pointer?

  //Okay detector, init hists
  int check=Init();

  if (check==0)
    {
      cout <<"StiResidualMaker::StiResidualMaker ERROR-"
	   <<"\tInit returned error value. Disabeling histogramming"
	   <<"\tfrom this maker."<<endl;
      mDetector=kUnknownId;  //this will disable all future references
    }

  return;
}


//Methods
int StiResidualMaker::Init()
{

  cout <<"StiResidualMaker::Init"<<endl;

  //!
  //! Initialize histograms (with detector info).
  //! Returns 0 if fails, 1 if success.
  //!


  int numLayers;
  //float zLow, zHigh;
  //float yLow, yHigh;
  string angleYBaseName = "yResidualCrossDip";
  string coordYBaseName = "yResidualZYRow";
  string angleZBaseName = "zResidualCrossDip";
  string coordZBaseName = "zResidualZYRow";
  string angleBaseName = "ResidualCrossDip";
  string coordBaseName = "ResidualZYRow";
  string detectorBaseName;
  string incrimentName;

  //determine detector
  switch(mDetector)
  {
    case kTpcId:
       //TPC
       detectorBaseName="Tpc";
       incrimentName="Row %dd";
       numLayers=45;
       break;
    case kSvtId:
      //SVT
       detectorBaseName="Svt";
       incrimentName="Layer %dd";
       numLayers=3;
       break;
    default:
      //NOT a tracking detector???!?!? (at least, one I want to deal with)
       return 0; //return error value
       break;
  }
    //make hist name strings from base names. This makes the pointer
    //name unique.
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

  return 1;
}

int StiResidualMaker::calcResiduals(StiTrackContainer *tracks)
{
  

  if(mDetector==kUnknownId) 
    {
      cout <<"StiResidualMaker::calcResiduals ERROR -"
	   <<"reference to null detector. Continuing..."<<endl;
      return 0; //invlaid detector
    }

  //get iterator from track container
  TrackMap::const_iterator trackIt = tracks->begin();
  //trackIterator

  //loop over tracks
  int check=0;
  while(trackIt!=tracks->end())
    {
      check= trackResidue((*trackIt).second);
      trackIt++;
    }

  return 1;
}

int StiResidualMaker::trackResidue(const StiTrack *track)
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

int StiResidualMaker::trackResidue(const StiKalmanTrack *track)
{

  //! Retrieves the node list for the input KalmanTrack,
  //! calls a ssubroutine to fill the hists with the
  //! pertinant values (fillHist).

  
  if (track->getPointCount()<15) return 1;

  //Get nodes in the detector 
  //(only returns nodes with hits in requested detector)
    vector<StiKalmanTrackNode*> nodes = track->getNodes(mDetector);

    cout <<"got node vect"<<endl;
  if(nodes.size()==0)
    {
      cout << "StiResidualMaker:: Warning - track has no hits"
	   << " in requested detector (Detector Id "
	   << mDetector << ")" << endl;
      return -5;
    }

  //fill hists with residuals
  vector<StiKalmanTrackNode*>::iterator iT = nodes.begin();


   
  StiKalmanTrackNode *iNode;
  while(iT != nodes.end())
    {
      cout <<"In while loop"<<endl;
      //pass node to FillHist
      if((StiKalmanTrackNode*)iT) 
	{
	  iNode=*iT;
	
	  FillHist(iNode);
	}
      else
	{
	  cout <<"Error! failed to get node!"<<endl;
	}
      iT++;
    }


  return 1;
}


void StiResidualMaker::Write(char* outfile)
{

  //! Writes the residual hists to a file.

  cout <<"StiResidualMaker::Write to file "<<outfile<<endl;

  if(mDetector==kUnknownId) 
    {
      cout <<"StiResidualMaker::calcResiduals ERROR -"
	   <<"reference to null detector. Returning..."<<endl;
      return; //invlaid detector
    }

  //open output file
  TFile *f = new TFile(outfile,"RECREATE");

  //dump hists to file
  mYResidualCrossDip->Write();
  mYResidualZY->Write();
  mZResidualCrossDip->Write();
  mZResidualZY->Write();


  //close file
  f->Close();
}

void StiResidualMaker::FillHist(StiKalmanTrackNode* node)
{

  //! Takes a Kalman Track Node and fills histograms with the
  //! residuals. Residuals are calculated as Hit I - Node I,
  //! where I is either the coordinate along the beam axis (Z),
  //! or along the plane of the detector (Y).

  cout <<"StiResidualMaker::FillHist"<<endl;
  //get node values
  double cross = node->crossAngle();
  double dip   = node->pitchAngle();
  double nodeZ = node->getZ();
  double nodeY = node->getY();
  double diffy,diffz, diff;
  double row = 10;

  //get hit pointer
  StiHit *hit=node->getHit();

  //trap if no hit with node - should now be done in getNodes()
  
  //Fill local Y residuals
  diffy = hit->y() - nodeY;

  if(diff!=0.) mYResidualCrossDip->Fill(cross, dip, diffy);
  if(diff!=0.) mYResidualZY->Fill(nodeZ, nodeY, diffy);



  //Fill local Z residuals
  diffz = hit->z() - nodeZ;
  if (diff!=0.)  mZResidualCrossDip->Fill(cross, dip, diffz);
  if (diff!=0.)  mZResidualZY->Fill(nodeZ, nodeY, diffz);


  if(diffz!=0 && diffy!=0)
    {
      diff = sqrt(diffy*diffy+diffz*diffz);
      mYResidualCrossDip->Fill(cross, dip, diff);
      mYResidualZY->Fill(nodeZ, nodeY, diff);
    }  


}


//StiResidualMaker.cxx
/***************************************************************************
 *
 * $Id: StiResidualMaker.cxx,v 1.3 2002/11/19 22:32:26 andrewar Exp $
 *
 * \class  StiResidualMaker provides a utility for determining the
 *         track residuals.
 * \author Andrew Rose, Wayne State University 
 * \date   October 2002
 ***************************************************************************
 * $Log: StiResidualMaker.cxx,v $
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

  cout <<"StiResidualMaker::StiResidualMaker"<<endl;
  
  //check for user sanity
  if(mDetector==kUnknownId)
    {//initialize with detector==0
      cout<<"StiResidualMaker::StiResidualMaker ERROR-"
	  <<"Initialize with detector = kUnknownId."
	  <<endl
	  <<"\tAll Residual histograms will be empty " <<endl
	  <<"\t and future calls to this maker will be ignored."<<endl
	  <<"\t Exiting (and you should probably be ashamed of yourself)."
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
  float zLow, zHigh;
  float yLow, yHigh;
  string angleYBaseName = "yResidualCrossDip";
  string coordYBaseName = "yResidualZYRow";
  string angleZBaseName = "zResidualCrossDip";
  string coordZBaseName = "zResidualZYRow";
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
  mYResidualCrossDipZ = new TH3D(angleYBaseName.c_str(),"Residual in Y vs. Cross, Dip, and Z", 128, -45., 45., 128, -45., 45., 128, -2.,2.);
  mYResidualZYRow=new TH3D(coordYBaseName.c_str(),"",128,-2.,2.,128,-2.,2.,45,0.,44.);
  mZResidualCrossDipZ = new TH3D(angleZBaseName.c_str(),"Residual in Z vs. Cross, Dip, and Z", 128, -45., 45., 128, -45., 45., 128, -2.,2.);
  mZResidualZYRow=new TH3D(coordZBaseName.c_str(),"",128,-2.,2.,128,-2.,2.,45,0.,44.);

  return 1;
}

int StiResidualMaker::calcResiduals(StiTrackContainer *tracks)
{
  
  cout <<"StiResidualMaker::calcResiduals"<<endl;

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

      //abort if track not okay (1=all fine, -5 no hits)
      if (check!=1 || check!=-5 ) return check;
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

  cout <<"StiResidualMaker::trackResidue"<<endl;

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

  cout <<"StiResidualMaker::trackResidue(Kalman Track)"<<endl;
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
  cout <<"StiResidualMaker::trackResidue - got nodes"<<endl;

  //fill hists with residuals
  vector<StiKalmanTrackNode*>::iterator iT = nodes.begin();


  
  cout <<"StiResidualMaker::trackResidue- got iterator"<<endl;
 
  cout <<"Begining node: "<<*(nodes.begin())<<endl;
   
  StiKalmanTrackNode *iNode;
  while(iT != nodes.end())
    {
      cout <<"In while loop"<<endl;
      //pass node to FillHist
      if((StiKalmanTrackNode*)iT) 
	{
	  iNode=*iT;
	  cout <<"trackResidue: "<<*iNode<<endl;
	  FillHist(iNode);
	}
      else
	{
	  cout <<"Error! failed to get node!"<<endl;
	}
      iT++;
    }

  cout <<"StiResidualMaker::trackResidue - done"<<endl;

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
  TFile *f = new TFile(outfile);

  //dump hists to file
  mYResidualCrossDipZ->Write();
  mYResidualZYRow->Write();
  mZResidualCrossDipZ->Write();
  mZResidualZYRow->Write();


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
  double nodeZ = node->fP1;
  double nodeY = node->fP0;
  double diff;
  double row = 10;

  cout <<"StiResidualMaker:: Node: "<<*node<<endl;

  //get hit pointer
  StiHit *hit=node->getHit();

  //trap if no hit with node
  if(hit==(StiHit*)NULL) return;
  if(hit->detector()==NULL) cout <<"No DETECTOR!!"<<endl;
  cout <<"Got hit"<<endl;

  //Fill local Y residuals
  diff = hit->y() - nodeY;

  mYResidualCrossDipZ->Fill(diff, cross, dip, nodeZ);
  mYResidualZYRow->Fill(diff, nodeZ, nodeY, row);
  cout <<"Got Y values"<<endl;
  cout <<"Y: "<<nodeY <<" Z: "<<nodeZ
       <<"Hit Y: "<<hit->y() <<" Z: "<<hit->z()
       <<"diff: "<< diff<<endl;


  //Fill local Z residuals
  diff = hit->z() - nodeZ;
  mZResidualCrossDipZ->Fill(diff, cross, dip, nodeZ);
  mZResidualZYRow->Fill(diff, nodeZ, nodeY, row);
  cout <<"Got Z Values"<<endl;

}


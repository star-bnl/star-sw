//StiResidualMaker.cxx
/***************************************************************************
 *
 * $Id: StiResidualMaker.cxx,v 1.1 2002/10/16 18:42:04 andrewar Exp $
 *
 * /author Andrew Rose, Wayne State University 
 * October 2002
 ***************************************************************************
 * $Log: StiResidualMaker.cxx,v $
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
  //!
  //! Initialize histograms (with detector info).
  //! Returns 0 if fails, 1 if success.
  //!


  int numLayers;
  float zLow, zHigh;
  float yLow, yHigh;
  char* angleYBaseName = "yResidualCrossDip";
  char* coordYBaseName = "yResidualZYRow";
  char* angleZBaseName = "zResidualCrossDip";
  char* coordZBaseName = "zResidualZYRow";
  char* detectorBaseName;
  char* incrimentName;

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
    (void)strcat(angleYBaseName,detectorBaseName);
    (void)strcat(coordYBaseName,detectorBaseName);
    (void)strcat(angleZBaseName,detectorBaseName);
    (void)strcat(coordZBaseName,detectorBaseName);


  //setup hists
  mYResidualCrossDipZ = new TH3D(angleYBaseName,"Residual in Y vs. Cross, Dip, and Z", 128, -45., 45., 128, -45., 45., 128, -2.,2.);
  mYResidualZYRow=new TH3D(coordYBaseName,"",128,-2.,2.,128,-2.,2.,45,0.,44.);
  mZResidualCrossDipZ = new TH3D(angleZBaseName,"Residual in Z vs. Cross, Dip, and Z", 128, -45., 45., 128, -45., 45., 128, -2.,2.);
  mZResidualZYRow=new TH3D(coordZBaseName,"",128,-2.,2.,128,-2.,2.,45,0.,44.);

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
      const StiKalmanTrack* kTrack = 
	static_cast<const StiKalmanTrack*>((*trackIt).second);  
      if(kTrack==(const StiKalmanTrack*)NULL)
	{
	  cout <<"Error: Could not cast to Kalman Track!"<<endl;
	  return -10;
	}  

      check=trackResidue(kTrack);
      //abort if track not okay (1=all fine, -5 no hits)
      if (check!=1 || check!=-5 ) return check;
      trackIt++;
    }

  return 1;
}

int StiResidualMaker::trackResidue(const StiKalmanTrack *track)
{

  //Get nodes in the detector 
  //(only returns nodes with hits in requested detector)
    vector<StiKalmanTrackNode*> nodes = 
      const_cast<StiKalmanTrack*>(track)->getNodes(mDetector);

  if(nodes.size()==0)
    {
      cout <<"StiResidualMaker:: Warning - track has no hits"
	   <<" in requested detector (Detector Id "
	   <<mDetector<<")"<<endl;
      return -5;
    }

  //fill hists with residuals
  vector<StiKalmanTrackNode*>::iterator iT = nodes.begin();
  while(iT!=nodes.end())
    {
      FillHist((StiKalmanTrackNode*)iT);
      iT++;
    }

  return 1;
}


void StiResidualMaker::Write(char* outfile)
{
  if(mDetector==kUnknownId) 
    {
      cout <<"StiResidualMaker::calcResiduals ERROR -"
	   <<"reference to null detector. Returning..."<<endl;
      return; //invlaid detector
    }

  //open output file
  TFile *f = new TFile(outfile);

  //dump hists to file


  //close file
  f->Close();
}

void StiResidualMaker::FillHist(StiKalmanTrackNode* node)
{
  //get node values
  double cross = asin((node->fP3)*(node->fX)-(node->fP2));
  double dip   = atan(node->fP4);
  double nodeZ = node->fP0;
  double nodeY = node->fP1;
  double diff;
  double row;

  //get hit pointer
  StiHit *hit=node->getHit();

  //trap if no hit with node
  if(!hit) return;

  //Fill local Y residuals
  diff = hit->y() - nodeY;
  mYResidualCrossDipZ->Fill(diff, cross,dip,nodeZ);
  mYResidualZYRow->Fill(diff, nodeZ,nodeY,row);

  //Fill local Z residuals
  diff = hit->z() - nodeZ;
  mZResidualCrossDipZ->Fill(diff, cross,dip,nodeZ);
  mZResidualZYRow->Fill(diff, nodeZ,nodeY,row);
  
}


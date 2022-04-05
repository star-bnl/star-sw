/*!\class StEEmcMatchMaker
Author Wei-Ming Zhang              KSU    4/27/2005
An example maker to read maps from StEEmcAssociationMaker for efficiency study
*/
#include <Stiostream.h>
#include "StEventTypes.h"
#include "StMcEventTypes.hh"
#include "StEEmcPool/StEEmcAssociationMaker/StEEmcAssociationMaker.h"
#include "StEEmcMatchMaker.h"
                                                  
ClassImp(StEEmcMatchMaker)

//_____________________________________________________________________________
StEEmcMatchMaker::StEEmcMatchMaker(const char *name):StMaker(name)
{
}
//_____________________________________________________________________________
StEEmcMatchMaker::~StEEmcMatchMaker()
{
}
//_____________________________________________________________________________
Int_t StEEmcMatchMaker::Init()
{
  return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t StEEmcMatchMaker::Make()
{
#if 0
 const TString detname[] = {"etow", "eprs", "esmdu", "esmdv"};
 const TString part[] =    {"none", "gamma", "e+",   "e-",  "nu", 
                                    "mu+",   "mu-",  "pi0", "pi+", 
                                    "pi-",   "k0l",  "k+",  "k-", 
                                    "n",     "p",    "p-",  "k0s"};

  StEEmcAssociationMaker *assoc = 
                         (StEEmcAssociationMaker*)GetMaker("EEmcAssoc");
  if(assoc) {

// Analyze with clusters  
    for(int i = 0; i < 4; i++) {
      cout <<"-----------------------------------------------------\n";
      cout <<"ASSOCIATION FOR DETECTOR "<<detname[i].Data()<<endl;

// Check TrackCluster map
      multiEEmcTrackCluster* tMap=assoc->getTrackClusterMap(detname[i].Data());
      if(tMap)
      {
        cout <<"\nTrack->Cluster Association Map\n";
        StMcTrack* oldTrack;
        int nTrack = 0;
        int nCluster;
        for(multiEEmcTrackClusterIter j=tMap->begin(); j!=tMap->end(); j++)
        {
          StMcTrack* track = (StMcTrack*)(*j).first;
          if(!track) continue;

// Skip duplicated information of track associated with multiple clusters
          if(track != oldTrack) {  
            if(track->geantId() > 16) continue;//check particles up to k0 short 
            nTrack++;
            cout <<"\n"<< nTrack 
               << " McTrack" << "(" <<part[track->geantId()].Data() << ")"
               << " pt = " << track->pt()
               << ", TReta = " <<track->pseudoRapidity() << endl;
               nCluster = 0;
          }

          oldTrack = (StMcTrack*)(*j).first;

          StEEmcClusterAssociation* value = 
                               (StEEmcClusterAssociation*)(*j).second;
          if(value)
          {
            StEmcCluster *cluster = (StEmcCluster*) value->getCluster();

            if(cluster)
            {
              nCluster++;
              cout << "  " << nCluster << " Cluster " << endl;
              cout << "    E=" << cluster->energy()
                 << ", eta=" << cluster->eta()
                 << ", phi=" << cluster->phi()
                 << ", FrTr=" << value->getFractionTrack()
                 << ", FrCl=" << value->getFractionCluster()<<endl;

// Add efficiency analysis code here
            }
          }
        }
      }

// Check ClusterTrack map
      multiEEmcClusterTrack* cMap=assoc->getClusterTrackMap(detname[i].Data());
      if(cMap)
      {
        cout <<"\nCluster->Track Association Map\n";
        StEmcCluster *oldCluster; 
        int nCluster = 0;
        int nTrack;
        for(multiEEmcClusterTrackIter j=cMap->begin(); j!=cMap->end(); j++)
        {
          StEmcCluster *cluster = (StEmcCluster*)(*j).first;
          if(!cluster) continue;

// Skip duplicated information of cluster associated with multiple tracks 
          if(cluster != oldCluster) {  
            nTrack = 0;
            nCluster++;
            cout <<"\n"<< nCluster << " Cluster" 
                 << " E=" << cluster->energy()
                 << ", eta=" << cluster->eta()
                 << ", phi="<< cluster->phi() << endl;
          }

          oldCluster = (StEmcCluster*)(*j).first;

          StEEmcClusterAssociation* value = 
                        (StEEmcClusterAssociation*)(*j).second;
          if(value)
          {
            StMcTrack* track = (StMcTrack*)value->getTrack();
            if(track)
            {
              nTrack++;
              cout << "  " << nTrack 
              << " McTrack" << "(" <<part[track->geantId()].Data() << ")"<<endl
              << "    pt=" << track->pt()
              << ", TReta=" <<track->pseudoRapidity()
              << ", FrTr = " <<value->getFractionTrack()
              <<", FrCl = " <<value->getFractionCluster()<<endl;

// Add efficiency analysis code here
            } 
          } 
        } 
      } 
    }
// Check TrackPoint map
    multiEEmcTrackPoint* tMap1 = assoc->getTrackPointMap();
    if(tMap1)
    {
      cout <<"\n===========================================================\n";
      cout <<"\nTrack->Point Association Map\n";
      StMcTrack* oldTrack;
      int nTrack = 0;
      int nPoint;
      for(multiEEmcTrackPointIter j=tMap1->begin(); j!=tMap1->end(); j++)
      {
        StMcTrack* track = (StMcTrack*)(*j).first;
        if(!track) continue;

// Skip duplicated information of track associated with multiple points
        if(track != oldTrack) {  
          if(track->geantId() > 16) continue; // check particles up to k0 short 
          nTrack++;
          cout <<"\n"<< nTrack 
               << " McTrack" << "(" <<part[track->geantId()].Data() << ")"
               << " pt = " << track->pt()
               << ", TReta = " <<track->pseudoRapidity() << endl;
               nPoint = 0;
        }

        oldTrack = (StMcTrack*)(*j).first;

        StEEmcPointAssociation* value = 
                               (StEEmcPointAssociation*)(*j).second;
        if(value)
        {
          StEmcPoint *point = (StEmcPoint*) value->getPoint();

          if(point)
          {
            nPoint++;
            cout << "  " << nPoint << " Point "  
                 << "  E=" << point->energy()
                 << ", Assoc.=" << value->getAssociation()
                 << ", det:";  
            for(int i = 0; i< 4; i++) 
              if (value->getAssociation(i+1) == 1) 
                               cout << " " << detname[i].Data();  
            cout << endl;   

// Add efficiency analysis code here
          }
        }
      }
    } 

// Check PointTrack map
    multiEEmcPointTrack* pMap = assoc->getPointTrackMap();
    if(pMap)
    { 
      cout <<"\nPoint->Track Association Map\n";
      StEmcPoint *oldPoint; 
      int nPoint = 0;
      int nTrack;
      for(multiEEmcPointTrackIter j=pMap->begin(); j!=pMap->end(); j++)
      {
        StEmcPoint *point = (StEmcPoint*)(*j).first;
        if(!point) continue;

        StEEmcPointAssociation* value = 
                        (StEEmcPointAssociation*)(*j).second;

// Skip duplicated information of point associated with multiple tracks 
        if(point != oldPoint) {  
          nTrack = 0;
          nPoint++;
          cout <<"\n"<< nPoint << " Point" 
               << "  E=" << point->energy()
               << ", Assoc.=" << value->getAssociation();
          for(int i = 0; i< 4; i++) {
            if (value->getAssociation(i+1) == 1) 
              cout << " det: " << detname[i].Data();  
          }
          cout << endl;   
        }

        oldPoint = (StEmcPoint*)(*j).first;

        if(value)
        {
          StMcTrack* track = (StMcTrack*)value->getTrack();
          if(track)
          {
            nTrack++;
            cout << "  " << nTrack 
            << " McTrack" << "(" <<part[track->geantId()].Data() << ")" 
            << "    pt=" << track->pt()
            << ", TReta=" <<track->pseudoRapidity() << endl;

// Add efficiency analysis code here
          } 
        } 
      } 
    } 
  }
#endif
  return kStOk;
}

//_____________________________________________________________________________
Int_t StEEmcMatchMaker::Finish() 
{
  return StMaker::Finish();
}

///////////////////////////////////////////////////////////////////////////
//
// $Id: StEEmcMatchMaker.cxx,v 1.3 2007/07/12 19:27:17 fisyak Exp $
// $Log: StEEmcMatchMaker.cxx,v $
// Revision 1.3  2007/07/12 19:27:17  fisyak
// Add includes for TMath for ROOT 5.16
//
// Revision 1.2  2005/06/04 23:40:36  balewski
// temporary disabled - it is Saturde evening - Jan
//
// Revision 1.1.1.1  2005/05/31 18:54:40  wzhang
// First version
//
//
///////////////////////////////////////////////////////////////////////////

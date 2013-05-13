///////estimate efficiencies of disks using straight line
#include "StFgtStraightTrackMaker.h"
#include "StRoot/StEvent/StFgtCollection.h"
#include "StRoot/StEvent/StFgtHitCollection.h"
#include "StRoot/StEvent/StFgtHit.h"
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

//#define PRINT_1D
//max num clusters any disk is allowed to have

//#define COSMIC
#include "StFgtCosmicAlignment.h"
#define CHARGE_MEASURE clusterCharge
#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"
//#define LEN_CONDITION


#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
//disk for which I want to calculate efficieny


#define MAX_CHARGE_RATIO
#define MIN_CHARGE_RATIO

//#define DISK_EFF 2
#define MY_PI 3.14159265359

#define DISK_DIM 40
#define NUM_EFF_BIN 100


pair<double,double> StFgtStraightTrackMaker::getDca(  vector<AVTrack>::iterator it)
{

  float oldDist=100000;
  float zStep=1.0;
  float optZ=-200;
  float dist=0;
  for(float z=-100;z<100;z=z+zStep)
    {
      float x=it->mx*z+it->ax;
      float y=it->my*z+it->ay;
      dist=x*x+y*y;
      if(dist>oldDist)
	{
	  ///we go away
	  dist=oldDist;
	  optZ=z-zStep;//last z was better
	  break;
	}
      else
	oldDist=dist;
    }
  return pair<double,double>(optZ,sqrt(dist));
}



//if required only return clusters with matches...
pair<Double_t,Double_t> StFgtStraightTrackMaker::findCluChargeSize(Int_t iD,Char_t layer, Double_t ordinate)
{
  if(iD<0 || iD >5)
    {
      return pair<Double_t,Double_t>(-99999,-99999);
    }
  vector<generalCluster> &hitVec=*(pClusters[iD]);
  Double_t charge=-99999;
  Double_t cluSize=-9999;
  Double_t minDist=99999;
  for(vector<generalCluster>::iterator it=hitVec.begin();it!=hitVec.end();it++)
    {
      if(it->layer!=layer)
	continue;
      Float_t ord=-9999;
      if(layer=='R')
	{
	  ord=it->posR;
	}
      else
	{
	  ord=it->posPhi;
	  if(fabs(ordinate-(ord+MY_PI))<fabs(ord-ordinate))
	    ord=ord+MY_PI;
	  if(fabs(ordinate-(ord-MY_PI))<fabs(ord-ordinate))
	    ord=ord-MY_PI;
	}

      if((fabs(ordinate-ord)<minDist) && (!useChargeMatch || it->hasMatch))
	{
	  charge=it->clusterCharge;
	  cluSize=it->clusterSize;
	  minDist=fabs(ordinate-ord);
	}
    }
  return pair<Double_t,Double_t>(charge,cluSize);
}

Double_t StFgtStraightTrackMaker::findClosestPoint(float mx, float bx, float my, float by, double xE, double yE, Int_t iD)
{
  //  cout <<"expecting point at " << xE <<", " <<yE <<endl;
  if(iD<0 || iD >5)
    {
      return 99999;
    }
  vector<generalCluster> &hitVec=*(pClusters[iD]);
  Double_t dist2=99999;
  for(vector<generalCluster>::iterator it=hitVec.begin();it!=hitVec.end();it++)
    {
      for(vector<generalCluster>::iterator it2=hitVec.begin();it2!=hitVec.end();it2++)
	{
	  //
	  if(useChargeMatch && !StFgtGeneralBase::arePointsMatched(it,it2))
	    continue;

	  if(it->layer==it2->layer)
	    continue;
	  Float_t r=it->posR;
	  Float_t phi=it2->posPhi;
	  if(it->layer!='R')
	    {
	      phi=it->posPhi;
	      r=it2->posR;
	    }

	  Float_t x=r*cos(phi);
	  Float_t y=r*sin(phi);
	  //	  cout <<"we have " << x <<", " << y <<endl;
	  Double_t mDist=(x-xE)*(x-xE)+(y-yE)*(y-yE);
	  if(mDist<dist2)
	    {
	      dist2=mDist;
	      //recalculate distance with proper alignment
	      float tmpX, tmpY,tmpZ,tmpP,tmpR;
	      if(isCosmic)
		{

		  getAlign(iD,phi,r,tmpX,tmpY,tmpZ,tmpP,tmpR);
		  Double_t xExpUpdate=mx*tmpZ+bx;
		  Double_t yExpUpdate=my*tmpZ+by;
		  //	      cout<<"tmpx: " << tmpX <<" old: " << x <<" xE old: " << xE << " updated: " << xExpUpdate;
		  //	      cout<<"tmpy: " << tmpY <<" old: " << y <<" yE old: " << yE << " updated: " << yExpUpdate<<endl;
		  mDist=(tmpX-xExpUpdate)*(tmpX-xExpUpdate)+(tmpY-yExpUpdate)*(tmpY-yExpUpdate);
		  dist2=mDist;

		}
	      ///Double_t xExp=mx*StFgtGeom::getDiscZ(i)+bx;
	      //	    Double_t yExp=my*StFgtGeom::getDiscZ(i)+by;


	      //	      (*outTxtFile) <<"point found, x: " << x <<" y: " << y << " dist: " << dist2 <<endl;
	    }
	}
    }
  //  (*outTxtFile) <<"returning : " << dist2<<endl;
  return dist2;
}


///this is too naive..., assumes non-rotated quads
Short_t StFgtStraightTrackMaker::getQuadFromCoo(Double_t x, Double_t y)
{
  cout <<"do not use this function!!!" <<endl;
  if(x>0 && y>0)
    return 0;
  if(x>0 && y<0)
    return 1;
  if(x<0 && y<0)
    return 2;
  if(x<0 && y>0)
    return 3;

  return -9999;
}


//print the strips around the place where we expect hit
pair<Double_t,Double_t> StFgtStraightTrackMaker::getChargeRatio(Float_t r, Float_t phi, Int_t iD, Int_t iq)
{
  //first r: 
  Double_t maxRCharge=-9999;
  Double_t maxPhiCharge=-9999;
  Int_t maxRInd=-1;
  Int_t maxPInd=-1;
  for(unsigned int i=0;i<  pStrips[iD*4+iq].size();i++)
    {
      Int_t geoId=pStrips[iD*4+iq][i].geoId;
      generalStrip& pStrip=pStrips[iD*4+iq][i];
      Short_t disc, quadrant,strip;
      Char_t layer;
      Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
      StFgtGeom::getPhysicalCoordinate(geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      StFgtGeom::decodeGeoId(geoId,disc, quadrant, layer, strip);
      //      if(layer=='P' && disc==iD && iq==quadrant)
      //	cout <<"looking for " << phi << " have: " << ordinate <<" diff: " << fabs(ordinate-phi) <<endl;
      if(disc==iD && iq==quadrant && ((layer =='R' && fabs(ordinate-r)<0.7) || (layer=='P' && fabs(ordinate-phi)<0.03) || (layer=='P' && fabs(ordinate-phi+2*MY_PI)<0.03 ) || (layer=='P' && fabs(ordinate-phi-2*MY_PI)<0.03)|| (layer=='P' && fabs(ordinate-phi+MY_PI)<0.03 ) || (layer=='P' && fabs(ordinate-phi-MY_PI)<0.03)))
	{
	  if(layer=='P')
	    {
	      if(pStrip.charge>maxPhiCharge)
		{
		  maxPhiCharge=pStrip.charge;
		  maxPInd=i;
		}
	    }
	  else
	    {
	      if(pStrip.charge>maxRCharge)
		{
		  maxRCharge=pStrip.charge;
		  maxRInd=i;
		}
	    }
	}
    }
  if(maxRInd>=0 && maxPInd>=0)
    {
      if(maxRCharge>1000 && maxPhiCharge>1000)
	{
	  ////might pick up phi strips... oh wlll
	  if(maxRInd>0)
	    maxRCharge+=pStrips[iD*4+iq][maxRInd-1].charge;
	  if(maxRInd< (int)(pStrips[iD*4+iq].size()-1))
	    maxRCharge+=pStrips[iD*4+iq][maxRInd+1].charge;
	  if(maxPInd>0)
	    maxPhiCharge+=pStrips[iD*4+iq][maxPInd-1].charge;
	  if(maxPInd< (int)(pStrips[iD*4+iq].size()-1))
	    maxPhiCharge+=pStrips[iD*4+iq][maxPInd+1].charge;

	  return pair<Double_t,Double_t>(maxRCharge,maxPhiCharge);
	}

    }
  return pair<Double_t,Double_t>(-9999,-9999);
}



Bool_t StFgtStraightTrackMaker::getTrack(vector<AVPoint>& points, Double_t ipZ)
{
  //  (*outTxtFile) <<"getTrack" <<endl;
  //      cout <<"get track" <<endl;
  ipZ=-9999; //get ourselves
  StFgtGeneralBase *fgtGenMkr = static_cast<StFgtGeneralBase * >( GetMaker("fgtGenBase"));

  ipZ=fgtGenMkr->vtxZ;
  Int_t vtxRank=fgtGenMkr->vtxRank;
  vector<AVPoint>::iterator iter=points.begin();
  Double_t A = 0, B = 0, Cx = 0, Cy = 0, D = 0, Ex = 0, Ey = 0;
  Double_t dist = 0;

  //LOG_INFO << " --------------------------> calling makeLine with " << points.size() << " 0x" << std::hex << discBitArray << std::dec << endm;
  //  cout <<"get track2" <<endl;
  //do the alignment		   
  if(isCosmic)
    {

      float tmpX, tmpY,tmpZ,tmpP,tmpR;
      for( ; iter != points.end(); ++iter ){
	getAlign(iter->dID,iter->phi,iter->r,tmpX,tmpY,tmpZ,tmpP,tmpR);
	//    cout <<"before: " << iter->phi << ", " << iter->r <<" " << iter->x <<" " << iter->y << ", " << iter->z <<endl;
	iter->phi=tmpP;
	iter->r=tmpR;
	iter->x=tmpX;
	iter->y=tmpY;
	iter->z=tmpZ;
	//    cout <<"after: " << iter->phi << ", " << iter->r <<" " << iter->x <<" " << iter->y << ", " << iter->z <<endl;
      }
    }
  else
    {
      //      cout <<"no cosmic" <<endl;
    }
  
  //  cout <<" we have " << points.size() << " points " << endl;
  for( iter=points.begin(); iter != points.end(); ++iter ){

    Double_t x = iter->x;
    Double_t y = iter->y;
    Double_t z = iter->z;
    //    cout <<"x: " << x << " y: " << y << " z: " << z <<endl;
    A += z*z;
    B += z;
    Cx += x*z;
    Cy += y*z;
    Ex += x;
    Ey += y;

    //        cout << "*** Point located at " << x << ' ' << y << ' ' << z << " Disk: " << iter->dID <<endl;
  }
  //  cout <<"ipZ: " << ipZ <<endl;
  //  cout <<"get track3" <<endl;
  D = points.size();
  //invalid is -999

  if(doFitWithVertex)
    {

      if(isMuDst && vtxRank>0&&ipZ>-100 && ipZ<100)
        {
      cout <<"fit with vertex!!" <<endl;
          A+=ipZ*ipZ;
          B+=ipZ;
          D++;
        }
    }
  //cout << "*** Consts " << A << ' ' << B << ' ' << Cx << ' ' << Cy << ' ' << D << ' ' << Ex << ' ' << Ey << endl;
  Double_t denom = D*A - B*B;
  if( denom )
    {
      Double_t bx = (-B*Cx + A*Ex)/denom;
      Double_t by = (-B*Cy + A*Ey)/denom;
      Double_t mx = ( D*Cx - B*Ex)/denom;
      Double_t my = ( D*Cy - B*Ey)/denom;
      //    cout <<"bx: " << bx <<" by: " << by <<" mx: " << mx << " my: " << my <<endl;    
      dist = 0;
      //  cout <<"get track4" <<endl;
      vector<AVPoint> redPoints;
      // find closest points...
      for(int iDx=0;iDx<6;iDx++)
	{
	  Double_t minDistance=9999;
	  Int_t pointIdx=-1;
	  Int_t cnt=0;
	  for( iter = points.begin(); iter != points.end(); ++iter ){
	    Int_t dId=iter->dID;
	    Double_t distX=fabs((iter->z*mx+bx)-(iter->x));
	    Double_t distY=fabs((iter->z*my+by)-(iter->y));
	    //	  cout <<"distX: " << distX <<" distY: " << distY <<endl;
	    Double_t distance=distX*distX+distY*distY;
	    //	  cout << " got distance " << distance << endl;
	    if((iDx==dId)&&(distance<minDistance))
	      {
		minDistance=distance;
		pointIdx=cnt;
		//	      cout <<"min dist now:" << minDistance<<" for this dik " << iDx <<" pointIdx: " << pointIdx <<endl;
	      }
	    cnt++;
	  }
	  if(pointIdx>=0)
	    {
	      //	    	    cout <<"pushing back " << pointIdx <<endl;
	      redPoints.push_back(points[pointIdx]);
	    }
	}//end of looping over discs

      //// reduced points
      //////have to do a refit now..
      //    cout <<"doing refit... " <<endl;
      //  cout <<"get track5" <<endl;
      A=0.0;
      B=0.0;
      Cx=0.0;
      Cy=0.0;
      Ex=0.0;
      Ey=0.0;

      for(vector<AVPoint>::iterator iterR=redPoints.begin() ; iterR != redPoints.end(); ++iterR )
	{
	  Double_t x = iterR->x;
	  Double_t y = iterR->y;
	  Double_t z = iterR->z;
	
	  A += z*z;
	  B += z;
	  Cx += x*z;
	  Cy += y*z;
	  Ex += x;
	  Ey += y;
	}
      D = redPoints.size();
      if(doRefitWithVertex)
	{
          if(isMuDst && vtxRank>0&&ipZ>-100 && ipZ<100)
            {
	      cout <<"refit with vertex! " <<endl;
              A+=ipZ*ipZ;
              B+=ipZ;
              D++;
            }
	}

      Double_t denom = D*A - B*B;
      //      cout <<"get track6" <<endl;
      if( denom )
	{
	  bx = (-B*Cx + A*Ex)/denom;
	  by = (-B*Cy + A*Ey)/denom;
	  mx = ( D*Cx - B*Ex)/denom;
	  my = ( D*Cy - B*Ey)/denom;
	}
      //   cout <<"after refit: bx: " << bx <<" by: " << by <<" mx: " << mx << " my: " << my <<endl;    
      //          cout <<"we have refit line: "<< bx << " by: " << by <<" mx: " << mx << " my: " << my <<endl;
      ///end of refit
      for(vector<AVPoint>::iterator iterR = redPoints.begin(); iterR != redPoints.end(); ++iterR )
	{
	  Double_t distX, distY;
	  distX=fabs((iterR->z*mx+bx)-(iterR->x));
	  distY=fabs((iterR->z*my+by)-(iterR->y));
	  //		cout <<"distX: " << distX <<" distY: " << distY <<endl;
	  dist += (distX*distX+distY*distY);
	  //		cout <<"adding " << (distX*distX+distY*distY) <<" to chi2: " << endl;
	  //       cout << "*** DistSq " << dist << endl;
	}
      //          cout <<"get track8" <<endl;
      dist/=D;
      //    cout <<" end chi2: " <<dist << ", ip: " << ipZ <<endl;
      //this ipZ is the TPC vertex! Is reset below...
      m_tracks.push_back(AVTrack(mx,my,bx,by,ipZ,dist));
      //    cout <<" we have " <<m_tracks.size() <<" track now " <<endl;
      (m_tracks.back()).vtxRank=vtxRank;
      points.clear();
      for(vector<AVPoint>::iterator it=redPoints.begin();it!=redPoints.end();it++)
	{
	  points.push_back(*it);
	}

      for(vector<AVPoint>::iterator iter = points.begin(); iter != points.end(); ++iter ){
	//                 cout << "--- Location at each disc at z: " << iter->z << " "
	//                   << "X: " << mx*iter->z+bx << " vs " << iter->x << ' '
	//                 	   << "Y: " << my*iter->z+by << " vs " << iter->y << " "
	//      		<< " charge phi: " << iter->phiCharge <<" rcharge: "<< iter->rCharge <<endl;

      }
      //    cout <<endl<<endl;
      //    cout <<"dist again:  " <<dist <<endl;
      vector<AVTrack>::iterator it_lastTrack=m_tracks.end();
      it_lastTrack--;
      pair<double,double> dca=getDca(it_lastTrack);
      Double_t vertZ = (  -( it_lastTrack->mx*it_lastTrack->ax + it_lastTrack->my*it_lastTrack->ay )/(it_lastTrack->mx*it_lastTrack->mx+it_lastTrack->my*it_lastTrack->my));
      (it_lastTrack)->trkZ=vertZ;
      //    cout <<"reset vertex to: " << vertZ <<endl;
      it_lastTrack->dca=dca.second;
      it_lastTrack->ipZ=dca.first;

      //  cout <<"get track10" <<endl;

      //    cout <<" returning true " <<endl;
      return true;
    }
  //  cout <<" false... " <<endl;
  return false;
};

Double_t StFgtStraightTrackMaker::getRPhiRatio(vector<generalCluster>::iterator hitIterBegin, vector<generalCluster>::iterator hitIterEnd)
{
  Short_t quad;
  Char_t layer; 
  Int_t numR=0;
  Int_t numPhi=0;
  vector<generalCluster>::iterator hitIter=hitIterBegin;
  for(;hitIter!=hitIterEnd;hitIter++)
    {
      quad=hitIter->quad;
      layer=hitIter->layer;

      if(layer=='R')
	numR++;
      else
	numPhi++;
    }

  if(numR+numPhi>0)
    return (numR-numPhi)/((Double_t)(numR+numPhi));
  else
    return -1;
};

/** grab the cluster container, fill histograms

*/
Int_t StFgtStraightTrackMaker::Make()
{
  //  cout <<"tracker! had " << m_tracks.size() << " old tracks" <<endl;
  for(vector<AVTrack>::iterator it=m_tracks.begin();it!=m_tracks.end();it++)
    {
      if(it->points)
	delete it->points;
    }
  m_tracks.clear();

  //  cout <<"ave make " <<endl;
  Int_t ierr = kStOk;
  StFgtGeneralBase *fgtGenMkr = static_cast<StFgtGeneralBase * >( GetMaker("fgtGenBase"));


  pClusters=fgtGenMkr->getClusters();
  for(int i=0;i<6;i++)
    {
      //     cout <<"there are " << pClusters[i]->size() << " clusters in disk " << i <<endl;
      for(int j=0;j<pClusters[i]->size();j++)
	{

	  /*	  if((*(pClusters[i]))[j].layer=='R')
	    cout <<"R layer, ";
	    else
	    cout <<"Phi layer, ";*/

	  Double_t posPhi=(*(pClusters[i]))[j].posPhi;
	  Double_t posR=(*(pClusters[i]))[j].posR;
	  Int_t clusSize=(*(pClusters[i]))[j].clusterSize;
	  Double_t charge=(*(pClusters[i]))[j].clusterCharge;
	  Int_t cntGeoId=(*(pClusters[i]))[j].centralStripGeoId;
	  Int_t seedType=(*(pClusters[i]))[j].seedType;
	  //	  cout <<"cluster pos phi: " << posPhi <<" posR: " << posR <<" clusSize: " << clusSize << " charge: "<< charge <<" geoId: "<< cntGeoId <<" seedT : " << seedType<<endl;
	}
    }
  pStrips=fgtGenMkr->getStrips();

  //  cout <<" mtracks size: " << m_tracks.size() << " oldnum: "<< oldNumTracks <<endl;
  
  Float_t x;
  Float_t y;
  //  if(vtxRank<=1)
  //    return kStOk;

  //look at the r phi ratio for each disk
  //  for(int i=0;i<6;i++)
  //    {
  //      Double_t ratio=getRPhiRatio(pClusters[i]->begin(),pClusters[i]->end());
  //      rPhiRatioPlots[i]->Fill(ratio);
  //      cout << "ratio for disk: " << i << " is " << ratio <<" disk has: " << tmpClusterCol->getHitVec().size() << "hits" <<endl;
  //    }

  //vector<generalCluster> &hitVecD1=*(pClusters[0]);
  //vector<generalCluster> &hitVecD6=*(pClusters[5]);
  set<Int_t> usedPoints;//saves the points that have been used for tracks (and shouldn't be reused)
  Double_t D1Pos=StFgtGeneralBase::getLocDiscZ(0);
  Double_t D6Pos=StFgtGeneralBase::getLocDiscZ(5);
  Double_t zArm=D6Pos-D1Pos;
  vector<generalCluster>::reverse_iterator hitIterD1,hitIterD6;
  vector<generalCluster>::iterator  hitIterD1R, hitIterD6R, hitIter, hitIter2;
  StFgtHit* fgtHitD1Phi;
  StFgtHit* fgtHitD1R;
  StFgtHit* fgtHitD6Phi;
  StFgtHit* fgtHitD6R;


  for(int locMinNumFitPoints=6;locMinNumFitPoints>=minNumFitPoints;locMinNumFitPoints--)
    {
  //  cout <<"eff disk : " << m_effDisk <<endl;
  for(int iSeed1=0;iSeed1<5;iSeed1++)
    {
      for(int iSeed2=iSeed1+1;iSeed2<6;iSeed2++)
	{
	  if((iSeed2-iSeed1)<1)//to have large enough lever arm. Also, since three points are required shouldn't matter?
	    continue;
	  if(iSeed1==m_effDisk || iSeed2==m_effDisk)
	    continue;
	  if(pClusters[iSeed1]->size() > maxClusters || pClusters[iSeed2]->size() > maxClusters)
	    {
	      //	           cout <<"too many clusters in the disk!!!"<<endl<<endl;
	      continue;
	    }
	  //track where we have hits in disks
	  //	  cout <<"seed 1: " << iSeed1 <<" seed2: "<< iSeed2 <<endl;
	  vector<generalCluster> &hitVecSeed1=*(pClusters[iSeed1]);
	  vector<generalCluster> &hitVecSeed2=*(pClusters[iSeed2]);

	  D1Pos=StFgtGeneralBase::getLocDiscZ(iSeed1);
	  D6Pos=StFgtGeneralBase::getLocDiscZ(iSeed2);
	  zArm=D6Pos-D1Pos;

	  //	  for(hitIterD1=hitVecSeed1.begin();hitIterD1 != hitVecSeed1.end();hitIterD1++)
	  for(hitIterD1=hitVecSeed1.rbegin();hitIterD1 != hitVecSeed1.rend();hitIterD1++)
	    {
	      //this is from the loose clustering and the cluster doesn't have energy match
	      if(useChargeMatch && !hitIterD1->hasMatch)
		continue;
	      Short_t quadP=hitIterD1->quad;
	      Char_t layer=hitIterD1->layer;

	      Double_t seed1ChargePhi=hitIterD1->CHARGE_MEASURE;
	      Double_t seed1SizePhi=hitIterD1->clusterSize;
	      //	      if(quadP>2)
	      //		continue;

	      //do 1D 'fit' with r strips and the (x,y) thing
	      Int_t geoIdSeed1=hitIterD1->centralStripGeoId;
	      if(usedPoints.find(geoIdSeed1)!=usedPoints.end())
		continue;
	      if(layer!='P')
		continue;
	      //    cout <<"ave make1 " <<endl;
	      Float_t phiD1=hitIterD1->posPhi;
	      fgtHitD1Phi=hitIterD1->fgtHit;
	      for(hitIterD6=hitVecSeed2.rbegin();hitIterD6 != hitVecSeed2.rend();hitIterD6++)
		{
		  if(useChargeMatch &&  !hitIterD6->hasMatch)
		    continue;

		  Int_t geoIdSeed2=hitIterD6->centralStripGeoId;
		  Short_t quadP_2=hitIterD6->quad;
		  //Short_t disc=hitIterD6->disc;
		  //Short_t strip=hitIterD6->strip;
		  Char_t layer=hitIterD6->layer;
		  Double_t seed2ChargePhi=hitIterD6->CHARGE_MEASURE;
		  Double_t seed2SizePhi=hitIterD6->clusterSize;
		  if(layer!='P')
		    continue;
		  if(usedPoints.find(geoIdSeed2)!=usedPoints.end())
		    continue;
		  Float_t phiD6=hitIterD6->posPhi;
		  fgtHitD6Phi=hitIterD6->fgtHit;
		  if(fabs(phiD6-phiD1)>maxPhiDiff)
		    continue;

		  for(hitIterD1R=hitVecSeed1.begin();hitIterD1R != hitVecSeed1.end();hitIterD1R++)
		    {
		      if(useChargeMatch && !hitIterD1R->hasMatch)
			continue;
		      Int_t geoIdSeed1R=hitIterD1R->centralStripGeoId;
		      Short_t quadR=hitIterD1R->quad;
		      //Short_t disc=hitIterD1R->disc;
		      //Short_t strip=hitIterD1R->strip;
		      Char_t layer=hitIterD1R->layer;
		      Double_t seed1ChargeR=hitIterD1R->CHARGE_MEASURE;
		      Double_t seed1SizeR=hitIterD1R->clusterSize;
		      Int_t quadSeed1=-1;
		      fgtHitD1R=hitIterD1R->fgtHit;
		      if(layer!='R')
			continue;
		      if(quadR!=quadP)
			continue;
		      quadSeed1=quadR;
		      if(usedPoints.find(geoIdSeed1R)!=usedPoints.end())
			continue;
		  
		      Float_t rD1=hitIterD1R->posR;
		      Float_t xD1=rD1*cos(phiD1);
		      Float_t yD1=rD1*sin(phiD1);

		      //		      cout <<"disk: " << iSeed1<<", phiD1: " << phiD1 <<" xD1: " << xD1 <<" yD1: " << yD1 <<" rD1: " << rD1 <<endl;
		      //    cout <<"ave make3 " <<endl;

		      for(hitIterD6R=hitVecSeed2.begin();hitIterD6R != hitVecSeed2.end();hitIterD6R++)
			{
			  if(useChargeMatch && !hitIterD6R->hasMatch)
			    continue;
			  Int_t geoIdSeed2R=hitIterD6R->centralStripGeoId;
			  Short_t quadR_2=hitIterD6R->quad;
			  //Short_t disc=hitIterD6R->disc;
			  //Short_t strip=hitIterD6R->strip;
			  Char_t layer=hitIterD6R->layer;
			  Double_t seed2ChargeR=hitIterD6R->CHARGE_MEASURE;

			  Double_t seed2SizeR=hitIterD6R->clusterSize;
			  Int_t quadSeed2=-1;
			  fgtHitD6R=hitIterD6R->fgtHit;
			  if(quadP_2!=quadR_2)
			    continue;
			  quadSeed2=quadP_2;
			  if(layer!='R')
			    continue;


			  if(usedPoints.find(geoIdSeed2R)!=usedPoints.end())
			    continue;
			  Float_t rD6=hitIterD6R->posR;
			  //track goes towards smaller radii
			  if(!isCosmic)
			    {
			      if(rD1>rD6)
				continue;		  
			    }
			  vector<AVPoint>* v_points=new vector<AVPoint>;
			  vvPoints.push_back(v_points);

			  //add the seed points to the points
			  Double_t xD6=rD6*cos(phiD6);
			  Double_t yD6=rD6*sin(phiD6);
			  //			  cout <<"Disk " << iSeed2 <<", phiD6: " << phiD6 <<" xD6: " << xD6 <<" yD6: " << yD6 <<" rD6: " << rD6 <<endl;
			  AVPoint avp1(xD1,yD1,D1Pos,rD1,phiD1,iSeed1,quadSeed1,seed1ChargeR, seed1ChargePhi, seed1SizeR, seed1SizePhi);
			  avp1.fgtHitR=fgtHitD1R;
			  avp1.fgtHitPhi=fgtHitD1Phi;
			  v_points->push_back(avp1);
			  AVPoint avp2(xD6,yD6,D6Pos,rD6,phiD6,iSeed2,quadSeed2,seed2ChargeR, seed2ChargePhi, seed2SizeR, seed2SizePhi);
			  avp2.fgtHitR=fgtHitD6R;
			  avp2.fgtHitPhi=fgtHitD6Phi;
			  v_points->push_back(avp2);
			  ///for each combination in d1,d6

			  int iFound=0;
			  int iFoundR=0;
			  //    cout <<"ave make4 " <<endl;
			  //zarm is d6 position - D1
			  //Double_t xIpExp=xD1+(xD6-xD1)*(-D1Pos)/zArm;
			  //Double_t yIpExp=yD1+(yD6-yD1)*(-D1Pos)/zArm;
			  ////at x = 0
			  //Double_t zIpExpX0=(D6Pos-(xD6/xD1)*D1Pos)*1/(1-xD6/xD1);
			  ///at y = 0
			  //Double_t zIpExpY0=(D6Pos-yD6/yD1*D1Pos)*1/(1-yD6/yD1);
			  //		  cout <<" 
			  //now find other points
			  vector<generalCluster>::iterator iterClosestPhi;
			  vector<generalCluster>::iterator iterClosestR;

			  Double_t closestDist=999999;
			  Int_t closestQuad;
			  Int_t quadTestR=-1;
			  //			  cout <<"looking for more hits..." <<endl;
			  for(int iD=0;iD<6;iD++)
			    {
			      //			      cout <<"testting disk: " << iD <<endl;
			      if(iD==iSeed1 || iD==iSeed2 || iD==m_effDisk)
				continue;

			      //			      cout <<"looking at disk: " << iD <<" seed1: " << iSeed1 << " seed2: " << iSeed2 <<endl;
			      //			      cout <<"not seed " << endl;
			      Bool_t found=false;
			      Bool_t foundR=false;
			      //check for hit
			      Double_t diskZ=StFgtGeneralBase::getLocDiscZ(iD);
			      //expected

			      Double_t xPosExp=xD1+(xD6-xD1)*(diskZ-D1Pos)/zArm;
			      Double_t yPosExp=yD1+(yD6-yD1)*(diskZ-D1Pos)/zArm;
			      //			      cout <<"hope to see something x: " << xPosExp <<" y: " << yPosExp <<endl;
			      Double_t rPosExp=rD1+(rD6-rD1)*(diskZ-D1Pos)/zArm;
			      vector<generalCluster> &hitVec=*(pClusters[iD]);
			      for(hitIter=hitVec.begin();hitIter!=hitVec.end();hitIter++)
				{
				  if(useChargeMatch &&  !hitIter->hasMatch)
				    continue;
				  //do 1D 'fit' with r strips and the (x,y) thing
				  Int_t geoIdPhi=hitIter->centralStripGeoId;
				  Short_t quad=hitIter->quad;
				  Int_t quadTestPhi=quad;
				  Short_t disc=hitIter->disc;
				  Short_t strip=hitIter->strip;
				  Char_t layer=hitIter->layer;

				  if(usedPoints.find(geoIdPhi)!=usedPoints.end())
				    continue;
				  if(layer!='P')
				    continue;
				  Float_t phi=hitIter->posPhi;
				  //				  cout <<"phi : " << phi << " phiD1: " << phiD1 <<" phiD6: " << phiD6 <<endl;
				  StFgtHit* fgtHitPhi=hitIter->fgtHit;
				  if(fabs(phi-phiD1)>maxPhiDiff)
				    continue;
				  if(fabs(phi-phiD6)>maxPhiDiff)
				    continue;

				  //				  cout <<" survived max_phi_diff cuts " <<endl;
				  //Double_t phiCharge=hitIter->CHARGE_MEASURE;

				  //				  								  Double_t phiCharge=hitIter->maxAdc;
				  //			  Int_t clusterSizePhi=hitIter->clusterSize;
				  //				  if(clusterSizePhi<=1)
				  //				    continue;
				  //    cout <<"ave make5 " <<endl;
				  for(hitIter2=hitVec.begin();hitIter2!=hitVec.end();hitIter2++)
				    {
				      if(useChargeMatch && !hitIter2->hasMatch)
					continue;
				      Int_t geoIdR=hitIter2->centralStripGeoId;
				      StFgtGeom::decodeGeoId(geoIdR,disc, quad, layer, strip);//ok
				      //				      cout <<" r? " <<endl;
				      if(usedPoints.find(geoIdR)!=usedPoints.end())
					continue;
				      //				      cout <<"not used yet " <<endl;
				      if(layer!='R')
					continue;
				      quadTestR=quad;
				      if(quadTestR!=quadTestPhi)
					continue;
				      Float_t r=hitIter2->posR;
				      StFgtHit* fgtHitR=hitIter->fgtHit;
				      //make sure that the radius makes sense for a track that goes from inner to outer radious
				      if(!isCosmic)
					{
					  if(r>rD6 || r<rD1)
					    continue;
					}
				      x=r*cos(phi);
				      y=r*sin(phi);
				      //				      		      cout <<"checking with x: " << x << " y: " << y << " phi: " << phi <<" r: " << r <<endl;
				      //				      				      cout <<" x, y: " << x <<", " << y << " exp: " << xPosExp << " , " << yPosExp <<endl;
				      Double_t dist2=(x-xPosExp)*(x-xPosExp)+(y-yPosExp)*(y-yPosExp);
				      //				      				      cout <<" dist2: " << dist2 <<endl;

				      if(doAddMultiplePoints)
					{
					  if(dist2<maxDist2)
					    {
					      Double_t rCharge=hitIter2->CHARGE_MEASURE;
					      Double_t phiCharge=hitIter->CHARGE_MEASURE;
					      Int_t clusterSizeR=hitIter2->clusterSize;
					      Int_t clusterSizePhi=hitIter->clusterSize;
					      AVPoint avp(x,y,diskZ,r,phi,iD,quadTestR, rCharge,phiCharge, clusterSizeR,clusterSizePhi);
					      avp.fgtHitR=fgtHitR;
					      avp.fgtHitPhi=fgtHitPhi;
					      v_points->push_back(avp);
					    }
					}

				      if(dist2<closestDist)
					{
					  closestDist=dist2;
					  closestQuad=quadTestR;
					  iterClosestPhi=hitIter;
					  iterClosestR=hitIter2;
					}
				    }
				}
			      //			      cout <<"closest dist for disk: "<< iD <<" is : " << closestDist <<endl;
			      //    cout <<"ave make6 " <<endl;
			      if(closestDist<1000 && closestDist<maxDist2)
				{
				  found=true;
				  //				  cout <<"accepted "  <<endl;
				  double r=iterClosestR->posR;
				  double phi=iterClosestPhi->posPhi;
				  StFgtHit* fgtHitR=iterClosestR->fgtHit;
				  StFgtHit* fgtHitPhi=iterClosestPhi->fgtHit;

				  Int_t geoIdR=iterClosestR->centralStripGeoId;
				  Int_t geoIdPhi=iterClosestPhi->centralStripGeoId;

				  double x=r*cos(phi);
				  double y=r*sin(phi);
				  //				  cout<<" adding point with r: "<< r <<" phi: " << phi <<" x: " << x <<" y: " << y <<endl;
				  Double_t rCharge=iterClosestR->CHARGE_MEASURE;
				  Double_t phiCharge=iterClosestPhi->CHARGE_MEASURE;
				  Int_t clusterSizeR=iterClosestR->clusterSize;
				  Int_t clusterSizePhi=iterClosestPhi->clusterSize;
				  //				  cout <<"charge R of middle disk: "<<  iD<<": "<< rCharge <<" phicharge: " << phiCharge<<endl;

// already added before if this flag is set
				  if(!doAddMultiplePoints)
				    {
				      AVPoint avp(x,y,diskZ,r,phi,iD,closestQuad, rCharge,phiCharge, clusterSizeR,clusterSizePhi);
				      //				  cout<<" adding point with r: "<< r <<" phi: " << phi <<" x: " << x <<" y: " << y <<endl;
				      avp.fgtHitR=fgtHitR;
				      avp.fgtHitPhi=fgtHitPhi;
				      v_points->push_back(avp);
				    }

				}
			      //    cout <<"ave make8 " <<endl;
			      //only one per disk
			      if(found)
				iFound++;
			      else
				{

				}
			      if(foundR)
				iFoundR++;
			      else
				{
				  //			  cout <<"failed to find r " << rPosExp<<endl;
				  //				  v_rFail.push_back(pair<Int_t, Double_t>(iD,rPosExp));
				}
			      closestDist=999999;

			    }

			  //    cout <<"ave make9 " <<endl;
			  //		  if(iFound>=2 && fabs(xIpExp)<20 && fabs(yIpExp)<20 && fabs(zIpExpX0)<40 && fabs(zIpExpY0)<40) //at least one hit plus seed and pointing roughly to the interaction region
			  //with hard cuts on the track we can get the whole vertex distribution
			  //			  cout << " we found " << iFound <<" points " << endl;
			  if(iFound>=(locMinNumFitPoints-2)) //at least one hit plus seed and pointing roughly to the interaction region
			    {
			      //			           cout <<"found " <<endl;
			      Bool_t validTrack=false;
			      Bool_t passQCuts=true;
			      //			      if(v_x.size()>iFound)
			      {
				Double_t ipZ;
				//				cout <<"about to get track" <<endl;
				//				cout <<"check for valid track " << endl;
				//this also manipulates the points vector to only put points in there that fit
				//get Track gets the ipZ, so it is ok to put in a random value here, but it is not a reference! So you don't get ipZ back
				validTrack=getTrack(*v_points, ipZ);


			      if(m_tracks.size()>0)
				{
				  passQCuts=trackQCuts(m_tracks.back());
				}

			      if(validTrack && passQCuts)
				  {
				    ///do something...
				    (m_tracks.back()).points=v_points;
				  //at least don't duplicate seeds..., the other ones might belong to multiple tracks
				    usedPoints.insert(geoIdSeed1);
				    usedPoints.insert(geoIdSeed1R);
				    usedPoints.insert(geoIdSeed2);
				    usedPoints.insert(geoIdSeed2R);
				    //put the rest of it in as well, doesn't matter that we duplicate the seeds, this is a set<int>
				    for(vector<AVPoint>::iterator it=v_points->begin();it!=v_points->end();it++)
				      {
					usedPoints.insert(it->geoIdR);
					usedPoints.insert(it->geoIdPhi);
				      }


				  }
				else
				  {
				    v_points->clear();
				    m_tracks.pop_back();
				    delete v_points;
				  }
			      }


			      hitCounter++;
			    }
			  //start over
			  iFound=0;
			  iFoundR=0;

			}

		    }

		}

	    }

	}

    }
    }
  //  cout <<"we have " << m_tracks.size()-oldNumTracks<< " tracks in this event " <<endl;
  //  cout <<"oldNumtracsk: " << oldNumTracks << " new size: " << m_tracks.size() <<endl;
  //  numTracks->Fill(m_tracks.size()-oldNumTracks);

  return ierr;

    };

bool StFgtStraightTrackMaker::trackQCuts(AVTrack& trk)
{

      if(trk.chi2>maxChi2 || trk.trkZ> vertexCutPos || trk.trkZ< vertexCutNeg|| trk.dca> dcaCut )
	{
	  return false;
	}
      return true;
};

StFgtStraightTrackMaker::StFgtStraightTrackMaker( const Char_t* name): StMaker( name ),useChargeMatch(false),runningEvtNr(0),hitCounter(0),hitCounterR(0),maxChi2(2),dcaCut(1),vertexCutPos(70),vertexCutNeg(-120)
{
  //  cout <<"AVE constructor!!" <<endl;
  int numTb=7;
  m_effDisk=10;//
  isMuDst=true; //might want to change this...
  maxPhiDiff=0.1;
  maxClusters=10;

  doFitWithVertex=false;
  doRefitWithVertex=false;
  doAddMultiplePoints=false;
  maxDist2=1.0;
  minNumFitPoints=3;

};

StFgtStraightTrackMaker::~StFgtStraightTrackMaker()
{

  //delete histogram arrays
};

Int_t StFgtStraightTrackMaker::Finish(){
  //  cout<<" straight tracker finish" <<endl;
  //  cout <<" closing txt file " << endl;
  //  gStyle->SetPalette(1);
  //  cout <<"AVE finish function " <<endl;
  Int_t ierr = kStOk;

  ///////////////////////////track collection
  //  vector<AVTrack>::iterator it=m_tracks.begin();
  //  cout <<"we found " << m_tracks.size() <<" tracks" <<endl;
  int counter=0;

  ///  cout <<"canvases etc.. " << endl;
  //////////////////////////////////////////////////
  //  cout <<"st done " <<endl;
  return ierr;
};

// construct histograms
Int_t StFgtStraightTrackMaker::Init(){
  Int_t ierr = kStOk;
  return ierr;
};
ClassImp(StFgtStraightTrackMaker);

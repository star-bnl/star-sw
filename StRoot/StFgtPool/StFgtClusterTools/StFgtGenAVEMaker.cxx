///////estimate efficiencies of disks using straight line
#include "StFgtGenAVEMaker.h"
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

#define CHARGE_MEASURE clusterCharge

#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"
//disk for which I want to calculate efficieny

#define DISK_EFF 2
#define QUAD_EFF 1
#define MY_PI 3.14159
//#define  REFIT_WITH_VERTEX
//#define FIT_WITH_VERTEX


#define MAX_DIST_CHI 1.0
#define MAX_DIST2_EFF 1.0
#define MAX_DIST2 1.0

#define MIN_NUM_POINTS 3


pair<double,double> StFgtGenAVEMaker::getDca(  vector<AVTrack>::iterator it)
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



Double_t StFgtGenAVEMaker::findClosestPoint(double xE, double yE, Int_t iD)
{
  if(iD<0 || iD >5)
    {
      return -99999;
    }
  vector<generalCluster> &hitVec=*(pClusters[iD]);
  Double_t dist2=99999;
  for(vector<generalCluster>::iterator it=hitVec.begin();it!=hitVec.end();it++)
    {
      for(vector<generalCluster>::iterator it2=hitVec.begin();it2!=hitVec.end();it2++)
	{
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
	  Double_t mDist=(x-xE)*(x-xE)+(y-yE)*(y-yE);
	  if(mDist<dist2)
	    dist2=mDist;
	}
    }
  return dist2;
}

Short_t StFgtGenAVEMaker::getQuadFromCoo(Double_t x, Double_t y)
{
  if(x>0 && y>0)
    return 0;
  if(x>0 && y<0)
    return 1;
  if(x<0 && y<0)
    return 2;
  if(x<0 && y>0)
    return 3;
}
//print the strips around the place where we expect hit
Bool_t StFgtGenAVEMaker::printArea(Float_t r, Float_t phi, Int_t iD, Int_t iq)
{
  //first r: 

  for(int i=0;i<  pStrips[iD*4+iq].size();i++)
    {
      Int_t geoId=pStrips[iD*4+iq][i].geoId;
      generalStrip& pStrip=pStrips[iD*4+iq][i];
      Short_t disc, quadrant,strip;
      Char_t layer;
      Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
      StFgtGeom::getPhysicalCoordinate(geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      StFgtGeom::decodeGeoId(geoId,disc, quadrant, layer, strip);
      char buffer[100];
      switch(pStrip.seedType)
	{
	case kFgtSeedTypeNo:
	  sprintf(buffer,"No Seed");
	  break;
	case kFgtSeedType1:
	  sprintf(buffer,"Seed1");
	  break;
	case kFgtSeedType2:
	  sprintf(buffer,"Seed2");
	  break;
	case kFgtSeedType3:
	  sprintf(buffer,"Seed3");
	  break;
	case kFgtClusterPart:
	  sprintf(buffer,"PartOfCluster");
	  break;
	case kFgtClusterEndUp:
	  sprintf(buffer,"EoC");
	  break;
	case kFgtClusterEndDown:
	  sprintf(buffer,"BoC");
	  break;
	case kFgtDeadStrip:
	  sprintf(buffer,"DeadStrip");
	  break;
	case kFgtClusterTooBig:
	  sprintf(buffer,"cluster too big");
	  break;
	case kFgtClusterSeedInSeaOfNoise:
	  sprintf(buffer,"seed in noise");
	  break;
	default:
	  sprintf(buffer,"somethingWrong: %d", pStrip.seedType);
	}

      if(layer=='P' && disc==iD && iq==quadrant)
	cout <<"looking for " << phi << " have: " << ordinate <<" diff: " << fabs(ordinate-phi) <<endl;
      if(disc==iD && iq==quadrant && ((layer =='R' && fabs(ordinate-r)<0.7) || (layer=='P' && fabs(ordinate-phi)<0.03) || (layer=='P' && fabs(ordinate-phi+2*MY_PI)<0.03 ) || (layer=='P' && fabs(ordinate-phi-2*MY_PI)<0.03)|| (layer=='P' && fabs(ordinate-phi+MY_PI)<0.03 ) || (layer=='P' && fabs(ordinate-phi-MY_PI)<0.03)))
	{
	  cout <<" found!!!" << endl;
	  (*outTxtFile) <<StFgtGeom::encodeGeoName(iD,iq,layer,strip)<<" ord: " << ordinate <<" layer: " <<layer<<" ped: " << pStrip.ped <<" pedErr: " << pStrip.pedErr <<" seedType: " <<buffer<<" ";
	  for(int i=0;i<7;i++)
	    {
	      if(pStrip.adc[i]<pStrip.pedErr)
		(*outTxtFile) << setw(4) << " .  "<< " ";
	      else
		(*outTxtFile) <<  setw(4) <<pStrip.adc[i] <<" ";
	    }
	  (*outTxtFile) <<endl;
	}
    }
  return kStOk;
}



//print the strips around the place where we expect hit
pair<Double_t,Double_t> StFgtGenAVEMaker::getChargeRatio(Float_t r, Float_t phi, Int_t iD, Int_t iq)
{
  //first r: 
      Double_t maxRCharge=-9999;
      Double_t maxPhiCharge=-9999;
      Int_t maxRInd=-1;
      Int_t maxPInd=-1;
  for(int i=0;i<  pStrips[iD*4+iq].size();i++)
    {
      Int_t geoId=pStrips[iD*4+iq][i].geoId;
      generalStrip& pStrip=pStrips[iD*4+iq][i];
      Short_t disc, quadrant,strip;
      Char_t layer;
      Double_t ordinate, lowerSpan, upperSpan;//, prvOrdinate;
      StFgtGeom::getPhysicalCoordinate(geoId,disc,quadrant,layer,ordinate,lowerSpan,upperSpan);
      StFgtGeom::decodeGeoId(geoId,disc, quadrant, layer, strip);
      char buffer[100];
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
      if(maxRCharge>200 && maxPhiCharge>200)
	{
	  ////might pick up phi strips... oh wlll
	  if(maxRInd>0)
	    maxRCharge+=pStrips[iD*4+iq][maxRInd-1].charge;
	  if(maxRInd< (pStrips[iD*4+iq].size()-1))
	    maxRCharge+=pStrips[iD*4+iq][maxRInd+1].charge;
	  if(maxPInd>0)
	    maxPhiCharge+=pStrips[iD*4+iq][maxPInd-1].charge;
	  if(maxPInd< (pStrips[iD*4+iq].size()-1))
	    maxPhiCharge+=pStrips[iD*4+iq][maxPInd+1].charge;

	  return pair<Double_t,Double_t>(maxRCharge,maxPhiCharge);
	}

    }
  return pair<Double_t,Double_t>(-9999,-9999);
}

































Bool_t StFgtGenAVEMaker::getTrack(vector<AVPoint>& points, Double_t ipZ)
{
  //  (*outTxtFile) <<"getTrack" <<endl;
  //  cout <<"get track" <<endl;
  ipZ=-9999; //get ourselves
  ipZ=vtxZ;
  vector<AVPoint>::iterator iter=points.begin();
  Double_t A = 0, B = 0, Cx = 0, Cy = 0, D = 0, Ex = 0, Ey = 0;
  Double_t dist = 0;

  //LOG_INFO << " --------------------------> calling makeLine with " << points.size() << " 0x" << std::hex << discBitArray << std::dec << endm;
  //  cout <<"get track2" <<endl;
  for( ; iter != points.end(); ++iter ){

    Double_t x = iter->x;
    Double_t y = iter->y;
    Double_t z = iter->z;

    A += z*z;
    B += z;
    Cx += x*z;
    Cy += y*z;
    Ex += x;
    Ey += y;

    //    cout << "*** Point located at " << x << ' ' << y << ' ' << z << " Disk: " << iter->dID <<endl;
  }
  //  cout <<"ipZ: " << ipZ <<endl;
  //  cout <<"get track3" <<endl;
  D = points.size();
  //invalid is -999
#ifdef FIT_WITH_VERTEX
      if(muDst && ipZ>-100 && ipZ<100)
        {
          A+=ipZ*ipZ;
          B+=ipZ;
          D++;
        }
#endif
    //cout << "*** Consts " << A << ' ' << B << ' ' << Cx << ' ' << Cy << ' ' << D << ' ' << Ex << ' ' << Ey << endl;
  Double_t denom = D*A - B*B;
  if( denom ){
    Double_t bx = (-B*Cx + A*Ex)/denom;
    Double_t by = (-B*Cy + A*Ey)/denom;
    Double_t mx = ( D*Cx - B*Ex)/denom;
    Double_t my = ( D*Cy - B*Ey)/denom;
    //    cout <<"bx: " << bx <<" by: " << by <<" mx: " << mx << " my: " << my <<endl;    
    for( iter = points.begin(); iter != points.end(); ++iter ){
      (*outTxtFile) << "--- Location at each disc, " << iter->dID <<" "
              << "X: " << mx*iter->z+bx << " vs " << iter->x << ' '
		    << "Y: " << my*iter->z+by << " vs " << iter->y << " " <<" charge r: " << iter->rCharge <<" phi: " << iter->phiCharge <<
	" r: " << iter->r <<" phi: " << iter->phi <<endl;
    };
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
	    ///	    cout <<"pushing back " << pointIdx <<endl;
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
#ifdef REFIT_WITH_VERTEX
          if(muDst && ipZ>-100 && ipZ<100)
            {
              A+=ipZ*ipZ;
              B+=ipZ;
              D++;
            }
#endif
    Double_t denom = D*A - B*B;
    //      cout <<"get track6" <<endl;
    if( denom ){
      bx = (-B*Cx + A*Ex)/denom;
      by = (-B*Cy + A*Ey)/denom;
     mx = ( D*Cx - B*Ex)/denom;
     my = ( D*Cy - B*Ey)/denom;
    }
  //    cout <<"after refit: bx: " << bx <<" by: " << by <<" mx: " << mx << " my: " << my <<endl;    
  //    cout <<"we have refit line: "<< bx << " by: " << by <<" mx: " << mx << " my: " << my <<endl;
    ///end of refit

    for(vector<AVPoint>::iterator iterR = redPoints.begin(); iterR != redPoints.end(); ++iterR )
      {
	Double_t distX, distY;
	distX=fabs((iterR->z*mx+bx)-(iterR->x));
	distY=fabs((iterR->z*my+by)-(iterR->y));
	//	cout <<"distX: " << distX <<" distY: " << distY <<endl;
	dist += (distX*distX+distY*distY);
	//	cout <<"adding " << (distX*distX+distY*distY) <<" to chi2: " << endl;
	//cout << "*** DistSq " << dist << endl;
      }
          cout <<"get track8" <<endl;
    dist/=D;
       cout <<" end chi2: " <<dist <<endl;
    m_tracks.push_back(AVTrack(mx,my,bx,by,ipZ,dist));
    cout <<" we have " <<m_tracks.size() <<" track now " <<endl;
    
    points.clear();
    for(vector<AVPoint>::iterator it=redPoints.begin();it!=redPoints.end();it++)
      {
	points.push_back(*it);
      }
    // copy to pointer, if one provided
    ///      if( linePtr )
    ////         *linePtr = line;

    // #ifdef DEBUG
    //       cout << "*** Event " << GetEventNumber() << " final distSQ " << dist << endl;
    // #endif

    // add to vector, if thes OK
    ///      if( line.res > mFitThres )
    ///         mLineVec.pop_back();

    //  cout <<"get track9" <<endl;
    Double_t zIpExpX0=0;//x(D6Pos-(xD6/xD1)*D1Pos)*1/(1-xD6/xD1);
			  ///at y = 0
    Double_t zIpExpY0=0;//(D6Pos-yD6/yD1*D1Pos)*1/(1-yD6/yD1);

    for(vector<AVPoint>::iterator iter = points.begin(); iter != points.end(); ++iter ){
      //           cout << "--- Location at each disc at z: " << iter->z << " "
      //             << "X: " << mx*iter->z+bx << " vs " << iter->x << ' '
      //           	   << "Y: " << my*iter->z+by << " vs " << iter->y << " "
      //		<< " charge phi: " << iter->phiCharge <<" rcharge: "<< iter->rCharge <<endl;

    };
    //    cout <<endl<<endl;
    //    cout <<"dist again:  " <<dist <<endl;
    vector<AVTrack>::iterator it_lastTrack=m_tracks.end();
    it_lastTrack--;
    pair<double,double> dca=getDca(it_lastTrack);
    Double_t vertZ = (  -( it_lastTrack->mx*it_lastTrack->ax + it_lastTrack->my*it_lastTrack->ay )/(it_lastTrack->mx*it_lastTrack->mx+it_lastTrack->my*it_lastTrack->my));
    (it_lastTrack)->trkZ=vertZ;
    it_lastTrack->dca=dca.second;
    it_lastTrack->ipZ=dca.first;

    //  cout <<"get track10" <<endl;
    if(dist< MAX_DIST_CHI && fabs(vertZ)<50)// && fabs(bx)<40 && fabs(by)<40)
      {
	//	cout <<" track accepted " <<endl;
	//  cout <<"get track10-10" <<endl;
	//		cout <<"track cuts passed " <<endl;
	set<Short_t> disksHit;
	//	cout <<"track has " << points.size() <<" points " <<endl;
	for(vector<AVPoint>::iterator iterP = points.begin(); iterP != points.end(); ++iterP ){
	  //	  cout <<"testing" << endl;
	  //	  cout <<"get track10-3, " << iterP->dID<<" quad: " << iterP->quadID << endl;
	  cout <<" filling disk " << iterP->dID <<" quad " << iterP->quadID <<" with r charge: " << iterP->rCharge <<" phic " << iterP->phiCharge<<endl;
	  chargeCorr[iterP->dID*4+iterP->quadID]->Fill(iterP->rCharge,iterP->phiCharge);
	  //	  cout <<"get track10-31" <<endl;
	  h_clusterChargeR[iterP->dID]->Fill(iterP->rCharge);
	  //	  cout <<"get track10-4" <<endl;
	  h_clusterChargePhi[iterP->dID]->Fill(iterP->phiCharge);
	  h_clusterSizeR[iterP->dID]->Fill(iterP->rSize);
	  //  cout <<"get track10-5" <<endl;
	  h_clusterSizePhi[iterP->dID]->Fill(iterP->phiSize);
	  disksHit.insert(iterP->dID);
	  //  cout <<"get track10-6" <<endl;
	}
	//  cout <<"get track11" <<endl;
	for(int i=0;i<6;i++)
	  {
	    Double_t xExp=mx*StFgtGeom::getDiscZ(i)+bx;
	    Double_t yExp=my*StFgtGeom::getDiscZ(i)+by;
	    Int_t quad=-1;
	    //x=r*cos(phi)
	    //y=r*sin(phi)
	    Double_t r=sqrt(xExp*xExp+yExp*yExp);
	    Double_t phi=atan(yExp/xExp);
	    if(phi<0)
	      phi+=MY_PI;
	    if(phi>MY_PI)
	      phi-=2*MY_PI;
	    if(phi<-MY_PI)
	      phi+=2*MY_PI;

	    quad=getQuadFromCoo(xExp,yExp);
	    //convert to phi in quad.., so we have to subtract that axis...
	    phi-=StFgtGeom::phiQuadXaxis(quad);

	   if(phi>TMath::Pi())
	     phi-=(2*TMath::Pi());
	   if(phi<((-1)*TMath::Pi()))
	     phi+=(2*TMath::Pi());

	   if(phi<0)
	     phi+=MY_PI;


	    (*outTxtFile) << " looking at Track with chi2/ndf *[cm}: " << it_lastTrack->chi2 << " z vertex: " << it_lastTrack->ipZ << endl;
	    if(disksHit.find(i)!=disksHit.end())
	      {
		(*outTxtFile) <<"***** found hit in disk " <<i << " at " << xExp<<", " << yExp<<" r: " << r <<" phi: " <<phi << endl;
		printArea(r,phi,i,quad);
		radioPlotsEff[i]->Fill(xExp,yExp);
	      }
	    else//not efficient
	      {
		radioPlotsNonEff[i]->Fill(xExp,yExp);
		(*outTxtFile) <<"expected (but haven't found)  point on disk " << i <<", x: " << xExp <<" y: " << yExp << " r: " << r  <<" phi: " << phi << " quad:: " << quad << endl;
		////		cout <<" expect hit at disc " << iD <<" quad: " << iq  << " r: " <<  r <<" phi: "<< phi <<endl;
		printArea(r,phi,i,quad);
	      }
	    ///for disk for which we want to compute effi:
	    if(i==DISK_EFF && quad==QUAD_EFF)
	      {

		Double_t closestPoint=findClosestPoint(xExp,yExp,i);
		cout <<"cloest point t " << xExp <<" , " << yExp << " is : " << closestPoint << " away " << endl;
		if(findClosestPoint(xExp,yExp,i)<MAX_DIST2_EFF)
		  {
		    cout <<"found point on eff disk, x: " << xExp <<" y: " << yExp <<endl;
		    radioPlotsEff[i]->Fill(xExp,yExp);
		    hResidua->Fill(sqrt(closestPoint));
		  }
		else
		  {
		    cout <<"non eff disk, x: " << xExp <<" y: " << yExp <<endl;
		    radioPlotsNonEff[i]->Fill(xExp,yExp);
		  }
		pair<Double_t,Double_t> rPhiRatio=getChargeRatio(r,phi,i,quad);
		chargeCorrInEffDisk->Fill(rPhiRatio.first,rPhiRatio.second);
		if(rPhiRatio.first>0 && rPhiRatio.second>0)
		  {
		    double asym=fabs((Double_t)1-rPhiRatio.first/rPhiRatio.second);
		    double ratio=rPhiRatio.first/rPhiRatio.second;

		      cout <<" filling with : r charge: " << rPhiRatio.first <<" , " << rPhiRatio.second <<" ratio: " << ratio <<" asym: " << asym<<endl;
		    if(asym<2 && ratio <2)
		      {
			hChargeAsym->Fill(asym);
			hChargeRatio->Fill(ratio);
			chargeRatioInEffDisk->Fill(xExp,yExp,ratio);
			chargeAsymInEffDisk->Fill(xExp,yExp,asym);
		      }
		  }
	      }
	  }
      }
    //    cout <<" returning true " <<endl;
    return true;
  }
  //  cout <<" false... " <<endl;
  return false;
};

Double_t StFgtGenAVEMaker::getRPhiRatio(vector<generalCluster>::iterator hitIterBegin, vector<generalCluster>::iterator hitIterEnd)
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
Int_t StFgtGenAVEMaker::Make()
{
  //    cout <<"ave make " <<endl;
  Int_t ierr = kStOk;
  (*outTxtFile) <<"----------------------------- Event Nr: " << evtNr<<" -----------------" <<endl;
  StFgtGeneralBase::Make();
  Float_t x;
  Float_t y;
  //  Int_t prvGeoId=-1;
  if(vtxRank<=1)
    return kStOk;

  //look at the r phi ratio for each disk
  for(int i=0;i<6;i++)
    {
      Double_t ratio=getRPhiRatio(pClusters[i]->begin(),pClusters[i]->end());
      rPhiRatioPlots[i]->Fill(ratio);
      //      cout << "ratio for disk: " << i << " is " << ratio <<" disk has: " << tmpClusterCol->getHitVec().size() << "hits" <<endl;
    }

  vector<generalCluster> &hitVecD1=*(pClusters[0]);
  vector<generalCluster> &hitVecD6=*(pClusters[5]);


  set<Int_t> usedPoints;//saves the points that have been used for tracks (and shouldn't be reused)
  Double_t D1Pos=StFgtGeom::getDiscZ(0);
  Double_t D6Pos=StFgtGeom::getDiscZ(5);
  Double_t zArm=D6Pos-D1Pos;
  vector<generalCluster>::iterator hitIterD1,hitIterD6, hitIterD1R, hitIterD6R, hitIter, hitIter2;
  //  cout <<" there are " << hitVecD1.size() << " hits in d1 "<<endl;
  for(hitIterD1=hitVecD1.begin();hitIterD1 != hitVecD1.end();hitIterD1++)
    {
       Short_t quad=hitIterD1->quad;
           Short_t disc=hitIterD1->disc;
            Short_t strip=hitIterD1->strip;
            Char_t layer=hitIterD1->layer;
      //      if(layer=='P')
      //	cout <<"found phi d1 " <<endl;
      //      else
      //	cout <<"found r d1 " <<endl;
    }
  //  cout <<" there are " << hitVecD6.size() << " hits in d6 "<<endl;
  for(hitIterD1=hitVecD6.begin();hitIterD1 != hitVecD6.end();hitIterD1++)
    {
            Short_t quad=hitIterD1->quad;
            Short_t disc=hitIterD1->disc;
            Short_t strip=hitIterD1->strip;
            Char_t layer=hitIterD1->layer;

      //      if(layer=='P')
      //	cout <<"found phi d6 " <<endl;
      //      else
      //	cout <<"found r d6 " <<endl;
    }

  ///all seed combinations
  for(int iSeed1=0;iSeed1<5;iSeed1++)
    {
      for(int iSeed2=iSeed1+1;iSeed2<6;iSeed2++)
	{
	  //	  (*outTxtFile) << " using " << iSeed1 <<" and " << iSeed2 << " as seeds " << endl;
	  //	  if((iSeed2-iSeed1)<3)//to have large enough lever arm..
	  //	    continue;
	  if(iSeed1==DISK_EFF || iSeed2==DISK_EFF)
	    continue;

	  //	  cout <<"using " << iSeed1 << " and " << iSeed2 << " as seed " <<endl;
	  vector<generalCluster> &hitVecSeed1=*(pClusters[iSeed1]);
	  vector<generalCluster> &hitVecSeed2=*(pClusters[iSeed2]);

	  D1Pos=StFgtGeom::getDiscZ(iSeed1);
	  D6Pos=StFgtGeom::getDiscZ(iSeed2);
	  zArm=D6Pos-D1Pos;

	  for(hitIterD1=hitVecSeed1.begin();hitIterD1 != hitVecSeed1.end();hitIterD1++)
	    {

	      Short_t quadP=hitIterD1->quad;
	      	      Short_t disc=hitIterD1->disc;
	      	      Short_t strip=hitIterD1->strip;
	      Char_t layer=hitIterD1->layer;
	      
	      Double_t seed1ChargePhi=hitIterD1->CHARGE_MEASURE;
	      //	      cout <<"seed1ChargeP: " << seed1ChargePhi <<endl;
	      //	      	      Double_t seed1ChargePhi=hitIterD1->maxAdcInt;
	      //	      Double_t seed1ChargePhi=hitIterD1->maxAdc;

	      //	      cout <<"seed charge: " << hitIterD1->maxAdcInt <<endl;
	      Double_t seed1SizePhi=hitIterD1->clusterSize;

	      if(quadP>2)
		continue;

	      //do 1D 'fit' with r strips and the (x,y) thing
	      Int_t geoIdSeed1=hitIterD1->centralStripGeoId;
	      if(usedPoints.find(geoIdSeed1)!=usedPoints.end())
		continue;
	      if(layer!='P')
		continue;
	      //    cout <<"ave make1 " <<endl;
	      Float_t phiD1=hitIterD1->posPhi;
	      for(hitIterD1R=hitVecSeed1.begin();hitIterD1R != hitVecSeed1.end();hitIterD1R++)
		{
		  Int_t geoIdSeed1R=hitIterD1R->centralStripGeoId;
		  Short_t quadR=hitIterD1R->quad;
		  Short_t disc=hitIterD1R->disc;
		  Short_t strip=hitIterD1R->strip;
		  Char_t layer=hitIterD1R->layer;
		  Double_t seed1ChargeR=hitIterD1R->CHARGE_MEASURE;
				  //	      cout <<"seed1ChargeR: " << seed1ChargeR <<endl;
				  //				  			  Double_t seed1ChargeR=hitIterD1R->maxAdcInt;
				  //			  Double_t seed1ChargeR=hitIterD1R->maxAdc;
		  //		  cout <<"seed chargeR: " << hitIterD1R->maxAdcInt <<endl;
		  Double_t seed1SizeR=hitIterD1R->clusterSize;
		  //		  cout <<"strip idx fro first seed;" << hitIterD1R->centerStripIdx<<endl;
		  //		  cout <<"strip adc2 fro first seed;" << (pStrips[disc*2+quadR])[hitIterD1R->centerStripIdx].adc[2]<<endl;
		  Int_t quadSeed1=-1;

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
		  for(hitIterD6=hitVecSeed2.begin();hitIterD6 != hitVecSeed2.end();hitIterD6++)
		    {


		      Int_t geoIdSeed2=hitIterD6->centralStripGeoId;
		      Short_t quadP_2=hitIterD6->quad;
		      		      Short_t disc=hitIterD6->disc;
		      		      Short_t strip=hitIterD6->strip;
		      		      Char_t layer=hitIterD6->layer;

				      		      		      Double_t seed2ChargePhi=hitIterD6->CHARGE_MEASURE;
								      //	      cout <<"seed2ChargeP: " << seed2ChargePhi <<endl;
				      //				      Double_t seed2ChargePhi=hitIterD6->maxAdcInt;
				      //				      				      Double_t seed2ChargePhi=hitIterD6->maxAdc;
		      Double_t seed2SizePhi=hitIterD6->clusterSize;
		      if(layer!='P')
			continue;
		      if(usedPoints.find(geoIdSeed2)!=usedPoints.end())
			continue;
		      Float_t phiD6=hitIterD6->posPhi;

		      //    cout <<"ave make3 " <<endl;

		      for(hitIterD6R=hitVecSeed2.begin();hitIterD6R != hitVecSeed2.end();hitIterD6R++)
			{
			  Int_t geoIdSeed2R=hitIterD6R->centralStripGeoId;
			  Short_t quadR_2=hitIterD6R->quad;
			  Short_t disc=hitIterD6R->disc;
			  Short_t strip=hitIterD6R->strip;
			  Char_t layer=hitIterD6R->layer;
			  Double_t seed2ChargeR=hitIterD6R->CHARGE_MEASURE;
						  //						  cout <<"seed2ChargeR: " << seed2ChargeR <<endl;

						  //						  			   			  Double_t seed2ChargeR=hitIterD6R->maxAdcInt;
												  //  Double_t seed2ChargeR=hitIterD6R->maxAdc;
			  Double_t seed2SizeR=hitIterD6R->clusterSize;
			  Int_t quadSeed2=-1;

			  if(quadP_2!=quadR_2)
			    continue;
			  quadSeed2=quadP_2;
			  if(layer!='R')
			    continue;
			  if(usedPoints.find(geoIdSeed2R)!=usedPoints.end())
			    continue;

			  vector<AVPoint> v_points;
			  //add the seed points to the points
			  Float_t rD6=hitIterD6R->posR;
			  Double_t xD6=rD6*cos(phiD6);
			  Double_t yD6=rD6*sin(phiD6);
			  v_points.push_back(AVPoint(xD1,yD1,D1Pos,rD1,phiD1,iSeed1,quadSeed1,seed1ChargeR, seed1ChargePhi, seed1SizeR, seed1SizePhi));
			  v_points.push_back(AVPoint(xD6,yD6,D6Pos,rD6,phiD6,iSeed2,quadSeed2,seed2ChargeR, seed2ChargePhi, seed2SizeR, seed2SizePhi));
			  ///for each combination in d1,d6
			  vector< pair<Int_t,Double_t> > v_x;
			  vector< pair<Int_t,Double_t> > v_y;
			  vector< pair<Int_t,Double_t> > v_r;

			  vector< pair<Int_t,Double_t> > v_xFail;
			  vector< pair<Int_t,Double_t> > v_yFail;
			  vector< pair<Int_t,Double_t> > v_rFail;

			  vector< pair< Int_t, Double_t> > v_rCharge;
			  vector< pair< Int_t, Double_t> > v_phiCharge;

			  vector<Int_t> v_clusterSizeR;
			  vector<Int_t> v_clusterSizePhi;

			  vector<Int_t> v_geoIDsR;
			  vector<Int_t> v_geoIDsPhi;

			  int iFound=0;
			  int iFoundR=0;
			  //    cout <<"ave make4 " <<endl;
			  //zarm is d6 position - D1
			  Double_t xIpExp=xD1+(xD6-xD1)*(-D1Pos)/zArm;
			  Double_t yIpExp=yD1+(yD6-yD1)*(-D1Pos)/zArm;
			  ////at x = 0
			  Double_t zIpExpX0=(D6Pos-(xD6/xD1)*D1Pos)*1/(1-xD6/xD1);
			  ///at y = 0
			  Double_t zIpExpY0=(D6Pos-yD6/yD1*D1Pos)*1/(1-yD6/yD1);
			  //		  cout <<" 
			  //now find other points
			  vector<generalCluster>::iterator iterClosestPhi;
			  vector<generalCluster>::iterator iterClosestR;

			  Double_t closestDist=999999;
			  Int_t quadTestR=-1;
			  for(int iD=0;iD<6;iD++)
			    {
			      //			      cout <<"testting disk: " << iD <<endl;
			      if(iD==iSeed1 || iD==iSeed2 || iD==DISK_EFF)
				continue;
			      //			      cout <<"not seed " << endl;
			      Bool_t found=false;
			      Bool_t foundR=false;
			      //check for hit
			      Double_t diskZ=StFgtGeom::getDiscZ(iD);
			      //expected

			      Double_t xPosExp=xD1+(xD6-xD1)*(diskZ-D1Pos)/zArm;
			      Double_t yPosExp=yD1+(yD6-yD1)*(diskZ-D1Pos)/zArm;
			      Double_t rPosExp=rD1+(rD6-rD1)*(diskZ-D1Pos)/zArm;
			      vector<generalCluster> &hitVec=*(pClusters[iD]);
			      for(hitIter=hitVec.begin();hitIter!=hitVec.end();hitIter++)
				{
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
				  //				  cout <<" got phi: " << phi <<endl;
				  				  Double_t phiCharge=hitIter->CHARGE_MEASURE;

				  //				  Double_t phiCharge=hitIter->maxAdcInt;
				  //				  								  Double_t phiCharge=hitIter->maxAdc;
								  //			  Int_t clusterSizePhi=hitIter->clusterSize;
				  //				  if(clusterSizePhi<=1)
				  //				    continue;
				  //    cout <<"ave make5 " <<endl;
				  for(hitIter2=hitVec.begin();hitIter2!=hitVec.end();hitIter2++)
				    {
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
				      x=r*cos(phi);
				      y=r*sin(phi);
				      //				      cout <<"checking with x: " << x << " y: " << y <<endl;
				      //				      cout <<" x, y: " << x <<", " << y << " exp: " << xPosExp << " , " << yPosExp <<endl;
				      Double_t dist2=(x-xPosExp)*(x-xPosExp)+(y-yPosExp)*(y-yPosExp);
				      //				      cout <<" dist2: " << dist2 <<endl;

#ifdef ADD_MULTIPLE
				      if(dist2<MAX_DIST2)
					{
				  Double_t rCharge=hitIter2->CHARGE_MEASURE;
				  Double_t phiCharge=hitIter->CHARGE_MEASURE;
				  Int_t clusterSizeR=hitIter2->clusterSize;
				  Int_t clusterSizePhi=hitIter->clusterSize;
					  v_points.push_back(AVPoint(x,y,diskZ,r,phi,iD,quadTestR, rCharge,phiCharge, clusterSizeR,clusterSizePhi));
					}

#endif
				      if(dist2<closestDist)
					{
					  //					  cout <<"found point with distance : "<< dist2 <<endl;
					  closestDist=dist2;
					  iterClosestPhi=hitIter;
					  iterClosestR=hitIter2;
					}
				    }
				}
			      //			      cout <<"closest dist for disk: "<< iD <<" is : " << closestDist <<endl;
			      //    cout <<"ave make6 " <<endl;
			      if(closestDist<1000 && closestDist<MAX_DIST2)
				{
				  found=true;
				  //				  cout <<"accepted "  <<endl;
				  double r=iterClosestR->posR;
				  double phi=iterClosestPhi->posPhi;
				  Int_t geoIdR=iterClosestR->centralStripGeoId;
				  Int_t geoIdPhi=iterClosestPhi->centralStripGeoId;

				  double x=r*cos(phi);
				  double y=r*sin(phi);
				 				 // cout <<" point is : " << x <<"," <<y <<endl;



				  //				  Double_t rCharge=iterClosestR->maxAdcInt;
				  //		  Double_t rCharge=iterClosestR->maxAdc;
				  //				  Double_t phiCharge=iterClosestPhi->maxAdcInt;

				  Double_t rCharge=iterClosestR->CHARGE_MEASURE;
				  Double_t phiCharge=iterClosestPhi->CHARGE_MEASURE;
				  Int_t clusterSizeR=iterClosestR->clusterSize;
				  Int_t clusterSizePhi=iterClosestPhi->clusterSize;
				  //				  cout <<"charge R of middle disk: "<<  iD<<": "<< rCharge <<" phicharge: " << phiCharge<<endl;

				  v_x.push_back(pair<Int_t,Double_t>(iD,x));
				  v_y.push_back(pair<Int_t,Double_t>(iD,y));
				  v_rCharge.push_back(pair<Int_t, Double_t>(iD,rCharge));
				  v_phiCharge.push_back(pair<Int_t, Double_t>(iD,phiCharge));
				  //to avoid double counting, t
				  v_geoIDsPhi.push_back(geoIdPhi);
				  v_geoIDsR.push_back(geoIdR);
				  v_clusterSizeR.push_back(clusterSizeR);
				  v_clusterSizePhi.push_back(clusterSizePhi);
				  v_points.push_back(AVPoint(x,y,diskZ,r,phi,iD,quadTestR, rCharge,phiCharge, clusterSizeR,clusterSizePhi));
				}
			      //    cout <<"ave make8 " <<endl;
			      //only one per disk
			      if(found)
				iFound++;
			      else
				{
				  v_xFail.push_back(pair<Int_t,Double_t>(iD,xPosExp));
				  v_yFail.push_back(pair<Int_t,Double_t>(iD,yPosExp));
				}
			      if(foundR)
				iFoundR++;
			      else
				{
				  //			  cout <<"failed to find r " << rPosExp<<endl;
				  v_rFail.push_back(pair<Int_t, Double_t>(iD,rPosExp));
				}
			      closestDist=999999;

			    }

			  //    cout <<"ave make9 " <<endl;
			  //		  if(iFound>=2 && fabs(xIpExp)<20 && fabs(yIpExp)<20 && fabs(zIpExpX0)<40 && fabs(zIpExpY0)<40) //at least one hit plus seed and pointing roughly to the interaction region
			  //with hard cuts on the track we can get the whole vertex distribution
			  //			  cout << " we found " << iFound <<" points " << endl;
			  if(iFound>=(MIN_NUM_POINTS-2)) //at least one hit plus seed and pointing roughly to the interaction region
			    {
			      //			      cout <<"found " <<endl;
			      Bool_t validTrack=false;
			      //			      if(v_x.size()>iFound)
			      {
				Double_t ipZ;
				//				cout <<"about to get track" <<endl;
				//				cout <<"check for valid track " << endl;
				validTrack=getTrack(v_points, ipZ);
				//			      cout <<"found 2" <<endl;
				if(validTrack)
				  {
				    //				    cout <<"was valid " << endl;
				  }
			      }
			      //			      cout <<"found4 " <<endl;
			      //			      else
			      {
				//    cout <<"ave make8]10 " <<endl;
				//			  		  cout <<"filled hip with " << xIpExp << " / " << yIpExp <<endl;
				for(unsigned int i=0;i<v_x.size();i++)
				  {
				    if(validTrack)
				      {
					//at least don't duplicate seeds...
										usedPoints.insert(geoIdSeed1);
										usedPoints.insert(geoIdSeed1R);
										usedPoints.insert(geoIdSeed2);
										usedPoints.insert(geoIdSeed2R);
										//					usedPoints.insert(v_geoIDsPhi[i]);
					//					usedPoints.insert(v_geoIDsR[i]);
					//					Int_t disk=v_x[i].first;
					//					Double_t x=v_x[i].second;
					//					Double_t y=v_y[i].second;
					//					Double_t rCharge=v_rCharge[i].second;
					//					Double_t phiCharge=v_phiCharge[i].second;
					//				      cout <<"filling disk: " << disk <<" with: " << x <<" / " <<y <<endl;
					//				    				    radioPlotsEff[disk]->Fill(x,y);
					//				    chargeCorr[disk]->Fill(rCharge,phiCharge);
					//				    h_clusterSizeR[disk]->Fill(v_clusterSizeR[i]);
					//				    h_clusterSizePhi[disk]->Fill(v_clusterSizePhi[i]);
					//				    h_clusterChargeR[disk]->Fill(rCharge);
					//				    h_clusterChargePhi[disk]->Fill(phiCharge);
				      }
				  }
				hitCounter++;
			      }
			    }

			  //    cout <<"ave make8]11 " <<endl;

			  //start over
			  iFound=0;
			  iFoundR=0;
			  v_x.clear();
			  v_points.clear();
			  v_y.clear();
			  v_r.clear();
			  v_xFail.clear();
			  v_yFail.clear();
			  v_rFail.clear();
	 
			}
		    }
		}
	    }

	}
    }

  return ierr;

};
 
StFgtGenAVEMaker::StFgtGenAVEMaker( const Char_t* name): StFgtGeneralBase( name ),runningEvtNr(0),hitCounter(0),hitCounterR(0)
{
  cout <<"AVE constructor!!" <<endl;

};

StFgtGenAVEMaker::~StFgtGenAVEMaker()
{

  //delete histogram arrays
};

Int_t StFgtGenAVEMaker::Finish(){
  StFgtGeneralBase::Finish();
  cout <<" closing txt file " << endl;
  outTxtFile->close();
  cout <<" 2 " << endl;
  gStyle->SetPalette(1);
  cout <<"cluster plotter finish function " <<endl;
  Int_t ierr = kStOk;
  cout <<" 3 " << endl;

  ///////////////////////////track collection
  vector<AVTrack>::iterator it=m_tracks.begin();
    cout <<"we found " << m_tracks.size() <<" tracks" <<endl;
  int counter=0;
  for(;it!=m_tracks.end();it++)
    {
      cout <<" looking at track " << counter <<endl;
counter++;
      //      cout <<"This track has parameters: ";
      cout <<" mx: " << it->mx <<" my: " << it->my <<" bx: " << it->ax << " by: " << it->ay << " chi2: " << it->chi2 <<endl;
      Double_t vertZ = (  -( it->mx*it->ax + it->my*it->ay )/(it->mx*it->mx+it->my*it->my));

      pair<double,double> dca=getDca(it);

      if(it->chi2<MAX_DIST_CHI && fabs(vertZ)< 50 )
	{
	  hIpZ->Fill(dca.first);
	  hIp->Fill(dca.first,dca.second);
	  hTrkZ->Fill(vertZ);
	  hMx->Fill(it->mx);	  
	  hMy->Fill(it->my);
	  hBx->Fill(it->ax);	  
	  hBy->Fill(it->ay);
	  hChi2->Fill(it->chi2);
	  tpcFgtZVertexCorr->Fill(dca.first,it->ipZEv);
	  tpcFgtZVertexCorr2->Fill(vertZ,it->ipZEv);
	  tpcFgtZVertexCorr3->Fill(vertZ,dca.first);
	  hIpDca->Fill(dca.second);
	}
    }

  //////////////////////////////////////////////////
  TCanvas* cRadio=new TCanvas("radioPlots","radioPlot",1000,1500);
  TCanvas* cRadioHits=new TCanvas("radioPlotsHits","radioPlotHits",1000,1500);
  TCanvas* cRadioNonHits=new TCanvas("radioPlotsNonHits","radioPlotNonHits",1000,1500);
  cRadio->Divide(2,3); //6 discs
  cRadioHits->Divide(2,3); //6 discs
  cRadioNonHits->Divide(2,3); //6 discs
  TCanvas* cRPRatio=new TCanvas("rPhiRatio","rPhiRatios",1000,1500);
  cRPRatio->Divide(2,3); //6 discs

  TCanvas* cREff=new TCanvas("crEff","crEff",1000,1500);

  cREff->Divide(2,3); //6 discs

  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      //      cRadio->cd(iD+1)->SetLogz();
      cRadioHits->cd(iD+1);
      radioPlotsEff[iD]->Draw("colz");
      cRadioNonHits->cd(iD+1);
      radioPlotsNonEff[iD]->Draw("colz");

    }
  cRadioHits->SaveAs("radioPlotsHits.png");
  cRadioNonHits->SaveAs("radioPlotsNonHits.png");
  cout <<" 5 " << endl;

  TCanvas* cClusterSizeR=new TCanvas("clusterSizeR","clusterSizeR",1000,1500);
  cClusterSizeR->Divide(2,3);
  TCanvas* cClusterSizePhi=new TCanvas("clusterSizePhi","clusterSizePhi",1000,1500);
  cClusterSizePhi->Divide(2,3);
  TCanvas* cChargeCorr=new TCanvas("chargeCorr","chargeCorr",1000,1500);
  cChargeCorr->Divide(3,4);

  TCanvas* cClusterChargePhi=new TCanvas("clusterChargePhi","clusterChargePhi",1000,1500);
  cClusterChargePhi->Divide(2,3);
  TCanvas* cClusterChargeR=new TCanvas("clusterChargeR","clusterChargeR",1000,1500);
  cClusterChargeR->Divide(2,3);

  TCanvas cIPProj;
  hIp->Draw("colz");
  cIPProj.SaveAs("ipProj.png");

  hBx->Draw();
  cIPProj.SaveAs("hBx.png");
  hBy->Draw();
  cIPProj.SaveAs("hBy.png");
  hMx->Draw();
  cIPProj.SaveAs("hMx.png");
  hMy->Draw();
  cIPProj.SaveAs("hMy.png");

  
  hIpZ->Draw();
  cIPProj.SaveAs("ipZ.png");


  hIpDca->Draw();
  cIPProj.SaveAs("ipDca.png");

  hTrkZ->Draw();
  cIPProj.SaveAs("hTrkZ.png");

  hResidua->Draw();
  cIPProj.SaveAs("hResidua.png");

  hChi2->Draw();
  cIPProj.SaveAs("chi2Dist.png");

  tpcFgtZVertexCorr->Draw("colz");
  cIPProj.SaveAs("tpcFgtCorr.png");
  tpcFgtZVertexCorr2->Draw("colz");
  cIPProj.SaveAs("tpcFgtCorr2.png");
  tpcFgtZVertexCorr3->Draw("colz");
  cIPProj.SaveAs("trackTrackZCorr.png");


  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      cClusterSizeR->cd(iD+1);
      h_clusterSizeR[iD]->Draw();
      cClusterSizePhi->cd(iD+1);
      h_clusterSizePhi[iD]->Draw();
      for(int iq=0;iq<4;iq++)
	{
	  cChargeCorr->cd(iD*4+iq+1);
	  chargeCorr[iD*4+iq]->Draw("colz");
	}
      cClusterChargeR->cd(iD+1);
      h_clusterChargeR[iD]->Draw();
      cClusterChargePhi->cd(iD+1);
      h_clusterChargePhi[iD]->Draw();

    }
  cClusterSizeR->SaveAs("clusterSizeR.png");
  cClusterSizePhi->SaveAs("clusterSizePhi.png");
  cChargeCorr->SaveAs("chargeCorrelation.png");

  cClusterChargeR->SaveAs("clusterChargeR.png");
  cClusterChargePhi->SaveAs("clusterChargePhi.png");

  //  cout <<"saving .." <<endl;

  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      cRadio->cd(iD+1);
      TH2D* tmpAllCounts=(TH2D*)radioPlotsEff[iD]->Clone("tmp");
      radioPlotsEff[iD]->Add(radioPlotsNonEff[iD]);//all counts
      //      radioPlotsEff[iD]->Add(radioPlotsNonEff[iD],-1); //subtract non eff
      cout <<" disk: " << iD <<endl;

      ////argh, overflow bins!!
      for(int nx=1;nx<radioPlotsEff[iD]->GetNbinsX()+1;nx++)
	{
	  for(int ny=1;ny<radioPlotsEff[iD]->GetNbinsY()+1;ny++)
	    {
	      Double_t denom=radioPlotsEff[iD]->GetBinContent(nx,ny);
	      if(denom>0 && (tmpAllCounts->GetBinContent(nx,ny)/denom)<=1.0)
		{
		  cout <<" efficiency bin nx: " << nx << " ny: " << ny << ", num counts " << tmpAllCounts->GetBinContent(nx,ny) << " / " << denom << " : " << tmpAllCounts->GetBinContent(nx,ny)/denom <<endl;
		  radioPlotsEff[iD]->SetBinContent(nx,ny,tmpAllCounts->GetBinContent(nx,ny)/denom);
		  if(iD==DISK_EFF)
		    {
		      cout <<"chargeRatio is: " << chargeRatioInEffDisk->GetBinContent(nx,ny)<<endl;
		      chargeRatioInEffDisk->SetBinContent(nx,ny,chargeRatioInEffDisk->GetBinContent(nx,ny)/denom);
		      chargeAsymInEffDisk->SetBinContent(nx,ny,chargeAsymInEffDisk->GetBinContent(nx,ny)/denom);
		    }
		}
	      else
		{
		  cout <<" efficiency bin nx: " << nx << " ny: " << ny << ", num counts " << tmpAllCounts->GetBinContent(nx,ny) << " / " << denom << " : 0.0" <<endl;
		  radioPlotsEff[iD]->SetBinContent(nx,ny,0.0);
		}
	    }
	}
      //      radioPlotsEff[iD]->Divide(tmpAllCounts);
      radioPlotsEff[iD]->SetMaximum(1.0);
      radioPlotsEff[iD]->Draw("colz");
      TArc *innerArc = new TArc(0,0,103.5+11.5,0.0,90.0);
      TArc *outerArc = new TArc(0,0,394.0-11.5,0.0,90.0);
      TLine *left = new TLine( 11.5, 103.5+11.5, 11.5, 394.0-11.5 );
      TLine *right = new TLine( 103.5+11.5, 11.5, 394.0-11.5, 11.5 );
      TLine *center = new TLine(
				(103.5+11.5)*cos( 0.785398163 ),
				(103.5+11.5)*sin( 0.785398163 ),
				(394.0-11.5)*cos( 0.785398163 ),
				(394.0-11.5)*sin( 0.785398163 )
				);
      TLine *weird = new TLine(
			       (394.0-11.5)*cos( 0.190240888 ),
			       (394.0-11.5)*sin( 0.190240888 ),
			       (394.0-11.5)*cos( 0.891863248 ),
			       (394.0-11.5)*sin( 0.891863248 )
			       );
      innerArc->SetFillStyle(0);
      outerArc->SetFillStyle(0);
      innerArc->SetLineWidth(2);
      outerArc->SetLineWidth(2);
      left->SetLineWidth(2);
      right->SetLineWidth(2);
      center->SetLineWidth(2);
      weird->SetLineWidth(2);
      outerArc->Draw("only");
      innerArc->Draw("only");
      left->Draw();
      right->Draw();
      center->Draw();
      weird->Draw();





      cRPRatio->cd(iD+1);
      rPhiRatioPlots[iD]->Draw();
      cREff->cd(iD+1);

      TH1D* tmpR=(TH1D*)rEff[iD]->Clone("tmpR");
      rEff[iD]->Add(rNonEff[iD]);
      for(int nx=0;nx<rEff[iD]->GetNbinsX();nx++)
	{
	  Double_t denom=rEff[iD]->GetBinContent(nx);
	  if(denom>0)
	    rEff[iD]->SetBinContent(nx,tmpR->GetBinContent(nx)/denom);
	  else
	    rEff[iD]->SetBinContent(nx,0.0);
	}
      rEff[iD]->Draw();
    }
  cRadio->SaveAs("radioPlotsEff.png");
  cRadio->SaveAs("radioPlotsEff.pdf");
  cRadio->cd(0);
  chargeRatioInEffDisk->Draw("colz");
  cRadio->SaveAs("chargeRatioInEffDisk.png");
  chargeAsymInEffDisk->Draw("colz");
  cRadio->SaveAs("chargeAsymInEffDisk.png");
  chargeCorrInEffDisk->Draw("colz");
  cRadio->SaveAs("chargeCorrInEffDisk.png");
  hChargeAsym->Draw();
  cRadio->SaveAs("chargeAsym.png");
  hChargeRatio->Draw();
  cRadio->SaveAs("chargeRatio.png");


  cREff->SaveAs("rEff.png");
  cREff->SaveAs("rEff.pdf");

  cRPRatio->SaveAs("rpRatio.png");
  cRPRatio->SaveAs("rpRatio.pdf");
  ////this has to be the last thing!!!! Otherwise the histos become invalid and the code seg faults...
  myRootFile->Write();
  myRootFile->Close();
  cout <<"returning after finish" <<endl;
  return ierr;
};


/**
   construct histograms

*/
Int_t StFgtGenAVEMaker::Init(){
  outTxtFile=new ofstream;
  outTxtFile->open("clusExpectations.txt");
  cout <<"AVE!!" <<endl;
  myRootFile=new TFile("clusterEff.root","RECREATE");
  //  outTxtFile=new ofstream;
  //  outTxtFile->open("clusters.txt");


  Int_t ierr = kStOk;

  char buffer[100];


  chargeRatioInEffDisk=new TH2D("chargeRatioInEffDisk","chargeRatioInEffDisk",10,-40,40,10,-40,40);
  chargeRatioInEffDisk->SetMaximum(2.0);
  chargeAsymInEffDisk=new TH2D("chargeAsymInEffDisk","chargeAsymInEffDisk",10,-40,40,10,-40,40);
  chargeAsymInEffDisk->SetMaximum(1.0);
  chargeCorrInEffDisk=new TH2D("chargeCorrInEffDisk","chargeCorrInEffDisk",50,0,50000,50,0,50000);
  hChargeAsym=new TH1D("chargeAsym","chargeAsym",100,0,50);
  hChargeRatio=new TH1D("chargeAsym","chargeAsym",100,0,50);


  radioPlotsEff=new TH2D*[kFgtNumDiscs];
  radioPlotsNonEff=new TH2D*[kFgtNumDiscs];
  rPhiRatioPlots=new TH1D*[kFgtNumDiscs];
  rEff=new TH1D*[kFgtNumDiscs];
  rNonEff=new TH1D*[kFgtNumDiscs];


  chargeCorr=new TH2D*[kFgtNumDiscs*4];
  h_clusterSizeR=new TH1D*[kFgtNumDiscs];
  h_clusterSizePhi=new TH1D*[kFgtNumDiscs];

  h_clusterChargeR=new TH1D*[kFgtNumDiscs];
  h_clusterChargePhi=new TH1D*[kFgtNumDiscs];


  hIp=new TH2D("Proj_to_IP","Proj_to_Ip",50,-100,100,50,-100,100);
  hBx=new TH1D("hBx","hBx",50,-100,100);
  hBy=new TH1D("hBy","hBy",50,-100,100);
  hMx=new TH1D("hMx","hMx",50,-100,100);
  hMy=new TH1D("hMy","My",50,-0.1,0.1);
  hIpZ=new TH1D("IP_Z","IP_Z",50,-100,100);

  hIpDca=new TH1D("ipDCA","ipDCA",50,-100,100);
  hTrkZ=new TH1D("z_Vtx_From_trk_fit","z_Vtx_From_trk_fit",50,-100,100);
  hResidua=new TH1D("residua","residua",100,0,50);
  hChi2=new TH1D("chi2","chi2",50,0,2);
  tpcFgtZVertexCorr=new TH2D("tpc_fgt_corr","tpc_fgt_corr",100,-120,120,100,-120,120);
  tpcFgtZVertexCorr2=new TH2D("tpc_fgt_corr2","tpc_fgt_corr2",100,-120,120,100,-120,120);
  tpcFgtZVertexCorr3=new TH2D("fgt_fgt_corr","fgt_fgt_corr",50,-50,50,50,-50,50);

  for(int iD=0;iD<kFgtNumDiscs;iD++)
    {
      //      cout <<"id: " << iD <<endl;
      sprintf(buffer,"radioDiskEff_%d",iD);
      radioPlotsEff[iD]=new TH2D(buffer,buffer,10,-40,40,10,-40,40);
      //      cout <<"1" <<endl;
      sprintf(buffer,"rEff_%d",iD);
      rEff[iD]=new TH1D(buffer,buffer,100,0,40);
      sprintf(buffer,"rNonEff_%d",iD);
      rNonEff[iD]=new TH1D(buffer,buffer,100,0,40);
      sprintf(buffer,"clusterSizeR_Disk_%d",iD);
      h_clusterSizeR[iD]=new TH1D(buffer,buffer,20,0,20);
      h_clusterSizeR[iD]->SetFillColor(kYellow);
      sprintf(buffer,"clusterSizePhi_Disk_%d",iD);
      h_clusterSizePhi[iD]=new TH1D(buffer,buffer,20,0,20);
      h_clusterSizePhi[iD]->SetFillColor(kYellow);
      sprintf(buffer,"clusterChargeR_Disk_%d",iD);
      h_clusterChargeR[iD]=new TH1D(buffer,buffer,100,0,5000);
      h_clusterChargeR[iD]->SetFillColor(kYellow);

      sprintf(buffer,"clusterChargePhi_Disk_%d",iD);
      h_clusterChargePhi[iD]=new TH1D(buffer,buffer,100,0,5000);
      h_clusterChargePhi[iD]->SetFillColor(kYellow);
      //      cout <<"2" <<endl;
      for(int nx=0;nx<radioPlotsEff[iD]->GetNbinsX();nx++)
	{
	  for(int ny=0;ny<radioPlotsEff[iD]->GetNbinsY();ny++)
	    {
	      //	       radioPlotsEff[iD]->SetBinContent(nx,ny,0.1);//so that there is no divide by zero
	    }
	}
      //      cout <<"3" <<endl;
      for(int iq=0;iq<4;iq++)
	{
	  sprintf(buffer,"r_phi_ChargeCorrelationInDisk_%d_quad_%d",iD,iq);
	  chargeCorr[iD*4+iq]=new TH2D(buffer,buffer,200,0,70000,200,0,70000);
	}
      sprintf(buffer,"radioDiskNonEff_%d",iD);
      radioPlotsNonEff[iD]=new TH2D(buffer,buffer,10,-40,40,10,-40,40);
      for(int nx=0;nx<rEff[iD]->GetNbinsX();nx++)
	{
	  //	   rEff[iD]->SetBinContent(nx,0.1);
	}
      //      cout <<"4" <<endl;
      sprintf(buffer,"rPhiRatio_%d",iD);
      rPhiRatioPlots[iD]=new TH1D(buffer,buffer,100,-2,10);
    }

  return ierr;
};
ClassImp(StFgtGenAVEMaker);

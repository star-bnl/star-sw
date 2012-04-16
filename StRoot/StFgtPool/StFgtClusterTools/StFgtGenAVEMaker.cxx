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


#include "StRoot/StFgtUtil/geometry/StFgtGeom.h"

#include "StRoot/StEvent/StEvent.h"
#include "StRoot/StEvent/StFgtCollection.h"


#define MAX_DIST 2.0
#define MIN_NUM_POINTS 3


Bool_t StFgtGenAVEMaker::getTrack(vector<AVPoint>& points, Double_t ipZ)
{
  vector<AVPoint>::iterator iter=points.begin();
  Double_t A = 0, B = 0, Cx = 0, Cy = 0, D = 0, Ex = 0, Ey = 0;
  Double_t dist = 0;
  const StMuDst* muDst = (const StMuDst*)GetInputDS("MuDst");
  if( muDst )
    {
      cout <<"found muDst " <<endl;
      StMuEvent *event = static_cast<StMuEvent*>(muDst->event());
      if( event ){
	const StThreeVectorF& v = event->primaryVertexPosition();

	ipZ=v.z();
	cout <<" got ipZ: " << ipZ <<endl;
      }}

  //LOG_INFO << " --------------------------> calling makeLine with " << points.size() << " 0x" << std::hex << discBitArray << std::dec << endm;

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

    cout << "*** Point located at " << x << ' ' << y << ' ' << z << " Disk: " << iter->dID <<endl;
  };
  D = points.size();
  if(muDst && ipZ!=0.0)
    {
      A+=ipZ*ipZ;
      B+=ipZ;
      D++;
    }


  //cout << "*** Consts " << A << ' ' << B << ' ' << Cx << ' ' << Cy << ' ' << D << ' ' << Ex << ' ' << Ey << endl;

  Double_t denom = D*A - B*B;
  if( denom ){
    Double_t bx = (-B*Cx + A*Ex)/denom;
    Double_t by = (-B*Cy + A*Ey)/denom;
    Double_t mx = ( D*Cx - B*Ex)/denom;
    Double_t my = ( D*Cy - B*Ey)/denom;



    ////      mLineVec.push_back( StFgtLHLine( discBitArray, mx, my, bx, by ) );
    ////      StFgtLHLine& line = mLineVec.back();

    //cout << "*** Line params " << mx << ' ' << bx << ' ' << my << ' ' << by << endl;

    for( iter = points.begin(); iter != points.end(); ++iter ){
      cout << "--- Location at each disc, "
	   << "X: " << mx*iter->z+bx << " vs " << iter->x << ' '
	   << "Y: " << my*iter->z+by << " vs " << iter->y << endl;
    };

    // push back points and compute dist
    dist = 0;
    for( iter = points.begin(); iter != points.end(); ++iter ){
      Double_t distX, distY;
      distX=fabs((iter->z*mx+bx)-(iter->x));
      distY=fabs((iter->z*my+by)-(iter->y));
      //	cout <<"distX: " << distX <<" distY: " << distY <<endl;
      dist += (distX*distX+distY*distY);
      //	cout <<"adding " << (distX*distX+distY*distY) <<" to chi2: " << endl;
      //cout << "*** DistSq " << dist << endl;
    };
    dist/=D;
    cout <<" end chi2: " <<dist <<endl;


    m_tracks.push_back(AVTrack(mx,my,bx,by,ipZ,dist));

    // copy to pointer, if one provided
    ///      if( linePtr )
    ////         *linePtr = line;

    // #ifdef DEBUG
    //       cout << "*** Event " << GetEventNumber() << " final distSQ " << dist << endl;
    // #endif

    // add to vector, if thes OK
    ///      if( line.res > mFitThres )
    ///         mLineVec.pop_back();



    if(dist< MAX_DIST)
      {
	set<Short_t> disksHit;
	for( iter = points.begin(); iter != points.end(); ++iter ){
	  chargeCorr[iter->dID*2+iter->quadID]->Fill(iter->rCharge,iter->phiCharge);
	  h_clusterChargeR[iter->dID]->Fill(iter->rCharge);
	  h_clusterChargePhi[iter->dID]->Fill(iter->phiCharge);
	  h_clusterSizeR[iter->dID]->Fill(iter->rSize);
	  h_clusterSizePhi[iter->dID]->Fill(iter->phiSize);
	  disksHit.insert(iter->dID);
	}
	for(int i=0;i<6;i++)
	  {
	    Double_t xExp=mx*StFgtGeom::getDiscZ(i)+bx;
	    Double_t yExp=my*StFgtGeom::getDiscZ(i)+by;
	    if(disksHit.find(i)!=disksHit.end())
	      {
		radioPlotsEff[i]->Fill(xExp,yExp);
	      }
	    else//not efficient
	      {
		radioPlotsNonEff[i]->Fill(xExp,yExp);
	      }
	  }
      }
  }
  if(dist< MAX_DIST)
    return true;


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

  cout <<"ave make " <<endl;
  Int_t ierr = kStOk;
  StFgtGeneralBase::Make();
  Float_t x;
  Float_t y;
  //  Int_t prvGeoId=-1;


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
      //    Short_t quad=hitIterD1->quad;
      //      Short_t disc=hitIterD1->disc;
      //      Short_t strip=hitIterD1->strip;
      //      Char_t layer=hitIterD1->layer;
      //      if(layer=='P')
      //	cout <<"found phi d1 " <<endl;
      //      else
      //	cout <<"found r d1 " <<endl;
    }
  //  cout <<" there are " << hitVecD6.size() << " hits in d6 "<<endl;
  for(hitIterD1=hitVecD6.begin();hitIterD1 != hitVecD6.end();hitIterD1++)
    {
      //      Short_t quad=hitIterD1->quad;
      //      Short_t disc=hitIterD1->disc;
      //      Short_t strip=hitIterD1->strip;
      //      Char_t layer=hitIterD1->layer;

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
	  //	  if((iSeed2-iSeed1)<3)//to have large enough lever arm..
	  //	    continue;


	  cout <<"using " << iSeed1 << " and " << iSeed2 << " as seed " <<endl;
	  vector<generalCluster> &hitVecSeed1=*(pClusters[iSeed1]);
	  vector<generalCluster> &hitVecSeed2=*(pClusters[iSeed2]);

	  D1Pos=StFgtGeom::getDiscZ(iSeed1);
	  D6Pos=StFgtGeom::getDiscZ(iSeed2);
	  zArm=D6Pos-D1Pos;

	  for(hitIterD1=hitVecSeed1.begin();hitIterD1 != hitVecSeed1.end();hitIterD1++)
	    {

	      Short_t quadP=hitIterD1->quad;
	      //	      Short_t disc=hitIterD1->disc;
	      //	      Short_t strip=hitIterD1->strip;
	      Char_t layer=hitIterD1->layer;
	      Double_t seed1ChargePhi=hitIterD1->clusterCharge;
	      Double_t seed1SizePhi=hitIterD1->clusterSize;

	      if(quadP>2)
		continue;

	      //do 1D 'fit' with r strips and the (x,y) thing
	      Int_t geoIdSeed1=hitIterD1->centralStripGeoId;
	      if(usedPoints.find(geoIdSeed1)!=usedPoints.end())
		continue;
	      if(layer!='P')
		continue;

	      Float_t phiD1=hitIterD1->posPhi;
	      for(hitIterD1R=hitVecSeed1.begin();hitIterD1R != hitVecSeed1.end();hitIterD1R++)
		{
		  Int_t geoIdSeed1R=hitIterD1R->centralStripGeoId;
		  Short_t quadR=hitIterD1R->quad;
		  //		  Short_t disc=hitIterD1R->disc;
		  //		  Short_t strip=hitIterD1R->strip;
		  //		  Char_t layer=hitIterD1R->layer;
		  Double_t seed1ChargeR=hitIterD1R->clusterCharge;
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
		      //		      Short_t disc=hitIterD6->disc;
		      //		      Short_t strip=hitIterD6->strip;
		      //		      Char_t layer=hitIterD6->layer;
		      Double_t seed2ChargePhi=hitIterD6->clusterCharge;
		      Double_t seed2SizePhi=hitIterD6->clusterSize;
		      if(layer!='P')
			continue;
		      if(usedPoints.find(geoIdSeed2)!=usedPoints.end())
			continue;
		      Float_t phiD6=hitIterD6->posPhi;



		      for(hitIterD6R=hitVecSeed2.begin();hitIterD6R != hitVecSeed2.end();hitIterD6R++)
			{
			  Int_t geoIdSeed2R=hitIterD6R->centralStripGeoId;
			  Short_t quadR_2=hitIterD6R->quad;
			  //			  Short_t disc=hitIterD6R->disc;
			  //			  Short_t strip=hitIterD6R->strip;
			  //			  Char_t layer=hitIterD6R->layer;
			  Double_t seed2ChargeR=hitIterD6R->clusterCharge;
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

			  //zarm is d6 position - D1
			  Double_t xIpExp=xD1+(xD6-xD1)*(-D1Pos)/zArm;
			  Double_t yIpExp=yD1+(yD6-yD1)*(-D1Pos)/zArm;
			  ////at x = 0
			  Double_t zIpExpX0=(D6Pos-(xD6/xD1)*D1Pos)*1/(1-xD6/xD1);
			  ///at y = 0
			  Double_t zIpExpY0=(D6Pos-yD6/yD1*D1Pos)*1/(1-yD6/yD1);
			  //		  cout <<" 

			  for(int iD=0;iD<6;iD++)
			    {
			      if(iD==iSeed1 || iD==iSeed2)
				continue;

			      Bool_t found=false;
			      Bool_t foundR=false;
			      //check for hit
			      Double_t diskZ=StFgtGeom::getDiscZ(iD);
			      //expected

			      Double_t xPosExp=xD1+(xD6-xD1)*(diskZ-D1Pos)/zArm;
			      Double_t yPosExp=yD1+(yD6-yD1)*(diskZ-D1Pos)/zArm;
			      Double_t rPosExp=rD1+(rD6-rD1)*(diskZ-D1Pos)/zArm;

			      //at z=0;


			      //			      cout <<"x1: " << xD1 << " y1: " << yD1 <<" x6: " << xD6 <<" y6: " << yD6 << " zpos: " << diskZ <<" arm: " << zArm<<endl;
			      //			      cout <<"expect hit at : " << xPosExp <<" / " << yPosExp <<" r: " << rPosExp <<endl;
			      //			      StFgtHitCollection* clusterCol=mFgtCollectionPtr->getHitCollection(iD);
			      vector<generalCluster> &hitVec=*(pClusters[iD]);
			      //			      cout <<" there are " << hitVec.size() << " hits in d" << iD <<endl;
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
				  Double_t phiCharge=hitIter->clusterCharge;
				  Int_t clusterSizePhi=hitIter->clusterSize;
				  //				  if(clusterSizePhi<=1)
				  //				    continue;
				  for(hitIter2=hitVec.begin();hitIter2!=hitVec.end();hitIter2++)
				    {
				      Int_t geoIdR=hitIter2->centralStripGeoId;
				      StFgtGeom::decodeGeoId(geoIdR,disc, quad, layer, strip);//ok
				      if(usedPoints.find(geoIdR)!=usedPoints.end())
					continue;
				      if(layer!='R')
					continue;

				      Int_t quadTestR=quad;
				      if(quadTestR!=quadTestPhi)
					continue;

				      Float_t r=hitIter2->posR;
				      Double_t rCharge=hitIter2->clusterCharge;
				      Int_t clusterSizeR=hitIter2->clusterSize;

				      //				      if((clusterSizeR/clusterSizePhi)>2.0 || (clusterSizePhi/clusterSizeR)>2.0)
				      //				      					continue;

				      //				      if(clusterSizeR<=1)
				      //					continue;
				      //				      cout <<"checking with r:" << r <<endl;
				      if(fabs(r-rPosExp)<MAX_DIST)
					{
					  foundR=true;
					  //					  cout <<"found r: " << r  <<endl;
					  v_r.push_back(pair<Int_t,Double_t> (iD,r));
					}

				      x=r*cos(phi);
				      y=r*sin(phi);
				      //				      cout <<"checking with x: " << x << " y: " << y <<endl;
				      if(fabs(x-xPosExp) < MAX_DIST && fabs(y-yPosExp)<MAX_DIST) //found hit
					{
					  found=true;
					  v_points.push_back(AVPoint(x,y,diskZ,r,phi,iD,quadTestR, rCharge,phiCharge, clusterSizeR,clusterSizePhi));
					  //					  					  cout <<"found! " <<" pushing back: iD: " << iD << "x: " << x << " y "<< y  <<endl;
					  v_x.push_back(pair<Int_t,Double_t>(iD,x));
					  v_y.push_back(pair<Int_t,Double_t>(iD,y));
					  v_rCharge.push_back(pair<Int_t, Double_t>(iD,rCharge));
					  v_phiCharge.push_back(pair<Int_t, Double_t>(iD,phiCharge));
					  //to avoid double counting
					  v_geoIDsPhi.push_back(geoIdPhi);
					  v_geoIDsR.push_back(geoIdR);
					  //					  cout <<" we have rcharge: " << rCharge<<" phi: " << phiCharge <<endl;
					  v_clusterSizeR.push_back(clusterSizeR);
					  v_clusterSizePhi.push_back(clusterSizePhi);
					}
				    }
				}
			      //only one per disk
			      if(found)
				iFound++;
			      else
				{
				  //			  cout <<"failed to find, pushing back " << xPosExp << " y: " << yPosExp <<endl;
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

			    }

			  //		  cout << " Ifound: " << iFound <<endl;
			  //subtract the seed
			  if(iFound>=MIN_NUM_POINTS-2)
			    {
			      hIp->Fill(xIpExp,yIpExp);
			      hIpZAtX0->Fill(zIpExpX0);
			      hIpZAtY0->Fill(zIpExpY0);
			      //			      cout <<" we have zIPX0: " << zIpExpX0 <<" and y: " << zIpExpY0 <<endl;
			    }


			  //		  if(iFound>=2 && fabs(xIpExp)<20 && fabs(yIpExp)<20 && fabs(zIpExpX0)<40 && fabs(zIpExpY0)<40) //at least one hit plus seed and pointing roughly to the interaction region
			  //with hard cuts on the track we can get the whole vertex distribution
			  if(iFound>=(MIN_NUM_POINTS-2)  && fabs(zIpExpX0)<80) //at least one hit plus seed and pointing roughly to the interaction region
			    {
			      Bool_t validTrack=false;
			      //			      if(v_x.size()>iFound)
			      {
				Double_t ipZ;
				cout <<"about to get track" <<endl;
				validTrack=getTrack(v_points, ipZ);

				cout<<"more hits than disks hit!!!" <<endl;
				cout <<" vx_size: " << v_x.size() <<" ifound: " << iFound <<endl;
			      }
			      //			      else
			      {

				//			  		  cout <<"filled hip with " << xIpExp << " / " << yIpExp <<endl;
				for(unsigned int i=0;i<v_x.size();i++)
				  {
				    if(validTrack)
				      {
					usedPoints.insert(geoIdSeed1);
					usedPoints.insert(geoIdSeed1R);
					usedPoints.insert(geoIdSeed2);
					usedPoints.insert(geoIdSeed2R);
					usedPoints.insert(v_geoIDsPhi[i]);
					usedPoints.insert(v_geoIDsR[i]);
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
				for(unsigned int i=0;i<v_xFail.size();i++)
				  {
				    //				    Int_t disk=v_xFail[i].first;
				    //				    Double_t x=v_xFail[i].second;
				    //				    Double_t y=v_yFail[i].second;
				    //			      cout <<"filling disk fail: " << disk <<" with: " << x <<" / " <<y <<endl;
				    //				    radioPlotsNonEff[disk]->Fill(x,y);
				  }
				hitCounter++;
			      }
			    }

			  if(iFoundR>=1) //at least one hit plus seed
			    {
			      if(v_r.size()>(unsigned int)iFound)
				{
				  //			  cout<<"more r hits than disks hit!!!" <<endl;
				}
			      else
				{
				  for(unsigned int i=0;i<v_r.size();i++)
				    {
				      Int_t disk=v_r[i].first;
				      Double_t r=v_r[i].second;
				      //			      cout <<"filling  r disk: " << disk <<" with: " << r <<endl;
				      rEff[disk]->Fill(r);
				    }
				  for(unsigned int i=0;i<v_rFail.size();i++)
				    {
				      Int_t disk=v_rFail[i].first;
				      Double_t r=v_rFail[i].second;
				      //			      cout <<"filling r disk fail: " << disk <<" with: " << r <<endl;
				      rNonEff[disk]->Fill(r);
				    }
				  hitCounterR++;
				}
			    }


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
  gStyle->SetPalette(1);
  cout <<"cluster plotter finish funciton " <<endl;
  Int_t ierr = kStOk;
  ///////////////////////////track collection
  vector<AVTrack>::iterator it=m_tracks.begin();
  cout <<"we found " << m_tracks.size() <<" tracks" <<endl;
  for(;it!=m_tracks.end();it++)
    {
      //      cout <<"This track has parameters: ";
      cout <<" mx: " << it->mx <<" my: " << it->my <<" bx: " << it->ax << " by: " << it->ay << " chi2: " << it->chi2 <<endl;
      Double_t vertZ = (  -( it->mx*it->ax + it->my*it->ay )/(it->mx*it->mx+it->my*it->my));

      if(it->chi2<MAX_DIST)
	hTrkZ->Fill(vertZ);
      hChi2->Fill(it->chi2);
      cout <<"vertZ: " << vertZ <<" from tpc: " << it->ipZ <<endl;
      tpcFgtZVertexCorr->Fill(vertZ,it->ipZ);
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


  TCanvas* cClusterSizeR=new TCanvas("clusterSizeR","clusterSizeR",1000,1500);
  cClusterSizeR->Divide(2,3);
  TCanvas* cClusterSizePhi=new TCanvas("clusterSizePhi","clusterSizePhi",1000,1500);
  cClusterSizePhi->Divide(2,3);
  TCanvas* cChargeCorr=new TCanvas("chargeCorr","chargeCorr",1000,1500);
  cChargeCorr->Divide(4,3);

  TCanvas* cClusterChargePhi=new TCanvas("clusterChargePhi","clusterChargePhi",1000,1500);
  cClusterChargePhi->Divide(2,3);
  TCanvas* cClusterChargeR=new TCanvas("clusterChargeR","clusterChargeR",1000,1500);
  cClusterChargeR->Divide(2,3);

  TCanvas cIPProj;
  hIp->Draw("colz");
  cIPProj.SaveAs("ipProj.png");

  hIpZAtX0->Draw();
  cIPProj.SaveAs("ipProjAtX0.png");

  hIpZAtY0->Draw();
  cIPProj.SaveAs("ipProjAtY0.png");

  hTrkZ->Draw();
  cIPProj.SaveAs("hTrkZ.png");

  hChi2->Draw();
  cIPProj.SaveAs("chi2Dist.png");

  tpcFgtZVertexCorr->Draw("colz");
  cIPProj.SaveAs("tpcFgtCorr.png");


  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      cClusterSizeR->cd(iD+1);
      h_clusterSizeR[iD]->Draw();
      cClusterSizePhi->cd(iD+1);
      h_clusterSizePhi[iD]->Draw();
      for(int iq=0;iq<2;iq++)
	{
	  cChargeCorr->cd(iD*2+iq+1);
	  chargeCorr[iD*2+iq]->Draw("colz");
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

  cout <<"saving .." <<endl;

  for(Int_t iD=0;iD<kFgtNumDiscs;iD++)
    {
      cRadio->cd(iD+1);
      TH2D* tmpAllCounts=(TH2D*)radioPlotsEff[iD]->Clone("tmp");
      radioPlotsEff[iD]->Add(radioPlotsNonEff[iD]);//all counts
      //      radioPlotsEff[iD]->Add(radioPlotsNonEff[iD],-1); //subtract non eff
      for(int nx=0;nx<radioPlotsEff[iD]->GetNbinsX();nx++)
	{
	  for(int ny=0;ny<radioPlotsEff[iD]->GetNbinsY();ny++)
	    {
	      Double_t denom=radioPlotsEff[iD]->GetBinContent(nx,ny);
	      if(denom>0 && (tmpAllCounts->GetBinContent(nx,ny)/denom)<=1.0)
		radioPlotsEff[iD]->SetBinContent(nx,ny,tmpAllCounts->GetBinContent(nx,ny)/denom);
	      else
		radioPlotsEff[iD]->SetBinContent(nx,ny,0.0);
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


  cREff->SaveAs("rEff.png");
  cREff->SaveAs("rEff.pdf");

  cRPRatio->SaveAs("rpRatio.png");
  cRPRatio->SaveAs("rpRatio.pdf");

  cout <<"returning after finish" <<endl;
  return ierr;
};


/**
   construct histograms

*/
Int_t StFgtGenAVEMaker::Init(){
  cout <<"AVE!!" <<endl;
  myRootFile=new TFile("clusterEff.root","RECREATE");
  //  outTxtFile=new ofstream;
  //  outTxtFile->open("clusters.txt");


  Int_t ierr = kStOk;

  char buffer[100];

  radioPlotsEff=new TH2D*[kFgtNumDiscs];
  radioPlotsNonEff=new TH2D*[kFgtNumDiscs];
  rPhiRatioPlots=new TH1D*[kFgtNumDiscs];
  rEff=new TH1D*[kFgtNumDiscs];
  rNonEff=new TH1D*[kFgtNumDiscs];


  chargeCorr=new TH2D*[kFgtNumDiscs*2];
  h_clusterSizeR=new TH1D*[kFgtNumDiscs];
  h_clusterSizePhi=new TH1D*[kFgtNumDiscs];

  h_clusterChargeR=new TH1D*[kFgtNumDiscs];
  h_clusterChargePhi=new TH1D*[kFgtNumDiscs];


  hIp=new TH2D("Proj_to_IP","Proj_to_Ip",50,-100,100,50,-100,100);
  hIpZAtX0=new TH1D("Proj_to_IPAtX0","Proj_toIPAtX0",50,-100,100);
  hIpZAtY0=new TH1D("Proj_to_IPAtY0","Proj_toIPAtY0",50,-100,100);
  hTrkZ=new TH1D("z_Vtx_From_trk_fit","z_Vtx_From_trk_fit",50,-100,100);
  hChi2=new TH1D("chi2","chi2",50,0,30);
  tpcFgtZVertexCorr=new TH2D("tpc_fgt_corr","tpc_fgt_corr",50,-100,100,50,-100,100);

  for(int iD=0;iD<kFgtNumDiscs;iD++)
    {
      //      cout <<"id: " << iD <<endl;

      sprintf(buffer,"radioDiskEff_%d",iD);
      radioPlotsEff[iD]=new TH2D(buffer,buffer,20,-40,40,20,-40,40);

      //      cout <<"1" <<endl;
      sprintf(buffer,"rEff_%d",iD);
      rEff[iD]=new TH1D(buffer,buffer,100,0,40);
      sprintf(buffer,"rNonEff_%d",iD);
      rNonEff[iD]=new TH1D(buffer,buffer,100,0,40);
      sprintf(buffer,"clusterSizeR_Disk_%d",iD);
      h_clusterSizeR[iD]=new TH1D(buffer,buffer,20,0,20);
      sprintf(buffer,"clusterSizePhi_Disk_%d",iD);
      h_clusterSizePhi[iD]=new TH1D(buffer,buffer,20,0,20);
      sprintf(buffer,"clusterChargeR_Disk_%d",iD);
      h_clusterChargeR[iD]=new TH1D(buffer,buffer,100,0,50000);
      sprintf(buffer,"clusterChargePhi_Disk_%d",iD);
      h_clusterChargePhi[iD]=new TH1D(buffer,buffer,100,0,50000);
      //      cout <<"2" <<endl;
      for(int nx=0;nx<radioPlotsEff[iD]->GetNbinsX();nx++)
	{
	  for(int ny=0;ny<radioPlotsEff[iD]->GetNbinsY();ny++)
	    {
	      //	       radioPlotsEff[iD]->SetBinContent(nx,ny,0.1);//so that there is no divide by zero
	    }
	}
      //      cout <<"3" <<endl;
      for(int iq=0;iq<2;iq++)
	{
	  sprintf(buffer,"r_phi_ChargeCorrelationInDisk_%d_quad_%d",iD,iq);
	  chargeCorr[iD*2+iq]=new TH2D(buffer,buffer,50,0,15000,50,0,15000);
	}
      sprintf(buffer,"radioDiskNonEff_%d",iD);
      radioPlotsNonEff[iD]=new TH2D(buffer,buffer,20,-50,50,20,-50,50);
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

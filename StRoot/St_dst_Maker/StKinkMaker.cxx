// $Id: StKinkMaker.cxx,v 1.5 1999/07/07 15:47:53 wdeng Exp $
// $Log: StKinkMaker.cxx,v $
// Revision 1.5  1999/07/07 15:47:53  wdeng
// add Id and Log at the first two lines for the purpose of version maintainance
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StKinkMaker class for Makers                                        //
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include <stdlib.h>
#include <string.h>
#include <TMath.h>

#include "StKinkMaker.h"

#include "StChain.h"
#include "St_DataSetIter.h"

#include "pams/global/inc/StDetectorId.h"
#include "pams/global/inc/StVertexId.h"
#include "PhysicalConstants.h"
#include "StThreeVector.hh"
#include "TObjArray.h"
#include "SystemOfUnits.h"

ClassImp(StKinkMaker)

#define MAXNUMOFTRACKS 10000

//_____________________________________________________________________________
StKinkMaker::StKinkMaker(const char *name):
  StMaker(name),
  m_tkfpar(0),
  mKinkLocalTrack(0)
{
  m_kinkEvalOn = kTRUE;
}
//_____________________________________________________________________________
StKinkMaker::~StKinkMaker(){
}
//_____________________________________________________________________________
Int_t StKinkMaker::Init(){
  St_DataSet *globalParams = GetInputDB("params/global");
  assert (globalParams);
  St_DataSetIter params(globalParams);
  
  m_tkfpar = (St_tkf_tkfpar *)  params("tkfpars/tkf_tkfpar");
  if (!m_tkfpar) {
    m_tkfpar = new St_tkf_tkfpar("tkfpar",1);
    AddConst(m_tkfpar);
    m_tkfpar->SetNRows(1);
  }
  tkf_tkfpar_st parRow;  

  parRow.dcaParentDaughterMax      =  0.5;
  parRow.parentPtMin               =  0.2;   
  parRow.vertexRMax2D              =  179.;  
  parRow.vertexRMin2D              =  133.;  
  parRow.thetaMin                  =  1.; 
  parRow.numOfPadRows              =  40;  	
  parRow.parentDipAngleMax         =  0.79;	
  parRow.impactCut                 =  2.;
  parRow.parentLastDaughterStart2D =  14.;
  parRow.parentLastDaughterStartZ  =  20.;
  parRow.projectPointZDiff         =  2.;
  parRow.distanceKinkParent2D      =  14.;
  parRow.distanceKinkDaughter2D    =  14.;
  parRow.distanceKinkParentZ       =  20.;
  parRow.distanceKinkDaughterZ     =  20.;
  
  m_tkfpar->AddAt(&parRow, 0);
  
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StKinkMaker::Make(){
  //PrintInfo();
  St_DataSet *match = GetDataSet("match"); 
  St_DataSetIter matchI(match);         
    
  St_dst_track  *globtrk = (St_dst_track *) matchI("globtrk");

  St_DataSet     *primary = GetDataSet("primary"); 
  St_DataSetIter primaryI(primary);         
  St_dst_vertex  *vertex   = (St_dst_vertex *) primaryI("vertex");
  
  St_dst_tkf_vertex *kinkVertex  = (St_dst_tkf_vertex *) matchI("kinkVertex");

  Int_t numOfGlbtrk = globtrk->GetNRows();
  Int_t tkf_limit = numOfGlbtrk/10;

  if(!kinkVertex) {
    kinkVertex = new St_dst_tkf_vertex("kinkVertex", tkf_limit);
    AddData(kinkVertex);
  }

  vertex->ReAllocate( vertex->GetNRows()+ tkf_limit); 

  tkf_tkfpar_st *tkfpar = m_tkfpar->GetTable();
  dst_vertex_st *dstVertexStart = vertex->GetTable(); 
  dst_track_st  *dstTrackStart  = globtrk->GetTable();
  
  dst_tkf_vertex_st kinkVtxRow;
  dst_vertex_st     dstVtxRow;

  Int_t  dstVtxIndex  = vertex->GetNRows();
  Int_t  kinkVtxIndex = 0;

  StKinkLocalTrack* tempTrack;
  TObjArray* trackArray = new TObjArray(MAXNUMOFTRACKS);

  const Float_t B = 0.5*tesla;

  Int_t i, j;
  
  for (i=0; i<numOfGlbtrk ; i++) {
    
    dst_track_st *dstTrackPtr = dstTrackStart + i;;
    if( dstTrackPtr->n_point > tkfpar->numOfPadRows ) continue;
    if( dstTrackPtr->iflag > 0 ) 
      {
	Float_t dip   = atan(dstTrackPtr->tanl);
	Int_t    h     = (B*dstTrackPtr->icharge > 0 ? -1 : 1);
	Float_t phase = dstTrackPtr->psi*degree-h*pi/2;
	Float_t pt    = (1./dstTrackPtr->invpt)*GeV;
	Float_t curvature = fabs(c_light*nanosecond/meter*dstTrackPtr->icharge*B/tesla)/(pt/GeV);
	
	StThreeVectorD origin(dstTrackPtr->x0, dstTrackPtr->y0, dstTrackPtr->z0);  
	
	tempTrack = new StKinkLocalTrack(dstTrackPtr,
					 curvature/meter,
					 dip*radian, 
					 phase*radian,
					 origin*centimeter,
					 h);
      }
    
    if(((*tempTrack).getStartRadius2D()<tkfpar->vertexRMin2D ) &&
       ((*tempTrack).getEndRadius2D()>tkfpar->vertexRMax2D )) 
      continue; 
    
    if(((*tempTrack).getStartRadius2D()<tkfpar->vertexRMax2D ) ||
       ((*tempTrack).getEndRadius2D()>tkfpar->vertexRMin2D ))	
      {
	trackArray->Add(tempTrack);
      }
  }
  
  trackArray->Sort();

  StThreeVectorD parentMom, daughterMom; 
  StThreeVectorD mKinkVertex; 
    
  Int_t   kinkCandidate=0;
  Float_t dca;
  Float_t parentImpact, daughterImpact;
  Float_t decayAngle;
  Float_t xCoordinates[2], yCoordinates[2];
  Float_t xtarget, ytarget;

  for( i = 0; i < trackArray->GetLast(); i++)
    {
      StKinkLocalTrack* myTrack1 = (StKinkLocalTrack*)trackArray->At(i);
      if(fabs( myTrack1->helix().dipAngle()) > tkfpar->parentDipAngleMax ) continue;  
      if( myTrack1->getPt() < tkfpar->parentPtMin ) continue;     
      
      dst_vertex_st* dstVertexPtr = dstVertexStart;         
      while(1) {                                    
        if( (dstVertexPtr->id==1) && (dstVertexPtr->vtx_id==1) )
          {
            StThreeVectorD eventVertex(dstVertexPtr->x, dstVertexPtr->y, dstVertexPtr->z);
            parentImpact = myTrack1->helix().distance(eventVertex);
            break;
          }
        dstVertexPtr++;
      }
      if( parentImpact > tkfpar->impactCut ) continue;
      
      for( j = i+1; j <= trackArray->GetLast(); j++)
	{
	  StKinkLocalTrack* myTrack2 = (StKinkLocalTrack*)trackArray->At(j);
	  if( myTrack1->getCharge() != myTrack2->getCharge() ) continue;
	  if( myTrack2->getStartRadius2D() < tkfpar->vertexRMin2D ) continue;
	  
	  dst_vertex_st* dstVertexPtr1 = dstVertexStart;         
	  while(1) {                                       
	    if( (dstVertexPtr1->id==1) && (dstVertexPtr1->vtx_id==1) )
	      {
		StThreeVectorD eventVertex(dstVertexPtr1->x, dstVertexPtr1->y, dstVertexPtr1->z);
		daughterImpact = myTrack2->helix().distance(eventVertex);
		break;
	      }
	    dstVertexPtr1++;
	  }
	  if( daughterImpact < tkfpar->impactCut ) continue;
	  
	  if( fabs(myTrack2->getStartRadius2D()-myTrack1->getEndRadius2D()) > tkfpar->parentLastDaughterStart2D ) continue; 
	  
	  if( fabs(myTrack2->getStartPoint(2)-myTrack1->getLastPoint(2)) > tkfpar->parentLastDaughterStartZ ) continue;	  
	  
	  Int_t numOfSolution = meetTwoHelices2D(tkfpar->dcaParentDaughterMax, myTrack1->helix(), 
		           		         myTrack2->helix(), xCoordinates, yCoordinates); 
	  
 	  if( numOfSolution == 0 ) continue;
	  if( numOfSolution == 1 ) 
	    {
	      Float_t radiusIntersect2D = sqrt(xCoordinates[0]*xCoordinates[0] +
 				 	       yCoordinates[0]*yCoordinates[0]);
	      if( (radiusIntersect2D < tkfpar->vertexRMin2D) || 	  
		  (radiusIntersect2D > tkfpar->vertexRMax2D) )  continue;
	      xtarget = xCoordinates[0];	    
	      ytarget = yCoordinates[0];	    
	    }
	  if ( numOfSolution == 2 )
	    {
	      Float_t radiusOneIntersect2D = sqrt(xCoordinates[0]*xCoordinates[0] +
						  yCoordinates[0]*yCoordinates[0]);
	      Float_t radiusTwoIntersect2D = sqrt(xCoordinates[1]*xCoordinates[1] +
						  yCoordinates[1]*yCoordinates[1]);
	      if( (radiusOneIntersect2D > tkfpar->vertexRMin2D) && 	  
		  (radiusOneIntersect2D < tkfpar->vertexRMax2D) &&
		  (radiusTwoIntersect2D > tkfpar->vertexRMin2D) && 	  
		  (radiusTwoIntersect2D < tkfpar->vertexRMax2D) ) 
		{
		  Float_t distanceInterOneStart = sqrt(TMath::Power((xCoordinates[0]-myTrack2->getStartPoint(0)), 2) + 
						       TMath::Power((yCoordinates[0]-myTrack2->getStartPoint(1)), 2));
		  Float_t distanceInterTwoStart = sqrt(TMath::Power((xCoordinates[1]-myTrack2->getStartPoint(0)), 2) + 
						       TMath::Power((yCoordinates[1]-myTrack2->getStartPoint(1)), 2));	
		  if ( distanceInterOneStart < distanceInterTwoStart ) 
		    {
		      xtarget = xCoordinates[0];	    
		      ytarget = yCoordinates[0];	    
		    } 
		  else
		    {
		      xtarget = xCoordinates[1];	    
		      ytarget = yCoordinates[1];
		    }
		}
	      else if( (radiusOneIntersect2D > tkfpar->vertexRMin2D) && 	  
		       (radiusOneIntersect2D < tkfpar->vertexRMax2D) )
		{
		  xtarget = xCoordinates[0];	    
		  ytarget = yCoordinates[0];	    
		} 
	      else if( (radiusTwoIntersect2D > tkfpar->vertexRMin2D) && 	  
		       (radiusTwoIntersect2D < tkfpar->vertexRMax2D) )
		{
		  xtarget = xCoordinates[1];	    
		  ytarget = yCoordinates[1];
		} 
	      else continue;
	    }
	  
	  Float_t p1PathLength = myTrack1->helix().pathLength(xtarget, ytarget);
	  Float_t p2PathLength = myTrack2->helix().pathLength(xtarget, ytarget);
	  StThreeVectorD p1Project = myTrack1->helix().at(p1PathLength);
	  StThreeVectorD p2Project = myTrack2->helix().at(p2PathLength);
	  
	  parentMom   = myTrack1->helix().momentumAt(p1PathLength, B);
	  daughterMom = myTrack2->helix().momentumAt(p2PathLength, B);
	  
	  decayAngle = 57.3*parentMom.angle(daughterMom);
	  if(decayAngle<tkfpar->thetaMin) continue;
	  
	  Float_t xn1[3], xn2[3], sxz1, syz1, sxz2, syz2, point1AtDca[3], point2AtDca[3];
	  
	  xn1[0] = p1Project.x();
	  xn1[1] = p1Project.y();
	  xn1[2] = p1Project.z();
	  xn2[0] = p2Project.x();
	  xn2[1] = p2Project.y();
	  xn2[2] = p2Project.z();
	  
	  if( fabs(xn2[2]-xn1[2]) > tkfpar->projectPointZDiff ) continue;
	  
	  if( (parentMom.z()==0.) || (daughterMom.z()==0.) ) continue;
	  sxz1 = parentMom.x()/parentMom.z();
	  syz1 = parentMom.y()/parentMom.z();
	  sxz2 = daughterMom.x()/daughterMom.z();
	  syz2 = daughterMom.y()/daughterMom.z();
	  
	  dca =  dcaTwoLines(xn1, xn2, sxz1, syz1, sxz2, syz2, point1AtDca, point2AtDca);
	  if ( dca>tkfpar->dcaParentDaughterMax ) continue;
	  
	  mKinkVertex.setX((point1AtDca[0]+point2AtDca[0])/2.);
	  mKinkVertex.setY((point1AtDca[1]+point2AtDca[1])/2.);
	  mKinkVertex.setZ((point1AtDca[2]+point2AtDca[2])/2.);
	  
	  Float_t distanceKinkParent2D   = sqrt( TMath::Power(mKinkVertex.x()-myTrack1->getLastPoint(0), 2) +
					         TMath::Power(mKinkVertex.y()-myTrack1->getLastPoint(1), 2) );
	  
	  Float_t distanceKinkDaughter2D = sqrt( TMath::Power(mKinkVertex.x()-myTrack2->getStartPoint(0), 2) +
						 TMath::Power(mKinkVertex.y()-myTrack2->getStartPoint(1), 2) );
	  
	  Float_t distanceKinkParentZ    = sqrt( TMath::Power(mKinkVertex.z()-myTrack1->getLastPoint(2), 2) );
	  Float_t distanceKinkDaughterZ  = sqrt( TMath::Power(mKinkVertex.z()-myTrack2->getStartPoint(2), 2) );
	  
	  if( distanceKinkParent2D > tkfpar->distanceKinkParent2D ) continue; 
	  if( distanceKinkDaughter2D > tkfpar->distanceKinkDaughter2D ) continue; 
	  
	  if( distanceKinkParentZ > tkfpar->distanceKinkParentZ ) continue; 
	  if( distanceKinkDaughterZ > tkfpar->distanceKinkDaughterZ ) continue; 

	  StThreeVectorD pMomMinusDMom = parentMom - daughterMom;
	  
	  Float_t  deltaKaonMuon = fabs(sqrt(parentMom.mag2() + kaonMass*kaonMass)   -
					sqrt(daughterMom.mag2() + muonMass*muonMass) - 
					pMomMinusDMom.mag());
	  Float_t  deltaKaonPion = fabs(sqrt(parentMom.mag2() + kaonMass*kaonMass)   -
					sqrt(daughterMom.mag2() + pionMass*pionMass) - 
					sqrt(pMomMinusDMom.mag2() + pi0Mass*pi0Mass));
          Float_t  deltaPionMuon = fabs(sqrt(parentMom.mag2() + pionMass*pionMass)   -
					sqrt(daughterMom.mag2() + muonMass*muonMass) - 
					pMomMinusDMom.mag());  

	  if( (deltaKaonPion < deltaKaonMuon) && (deltaKaonPion < deltaPionMuon) )
	    {
	      kinkVtxRow.theta_cm = radToDeg*asin((daughterMom.mag()/kaonToPionQ)*sin(decayAngle*degToRad));
	      if( myTrack1->getCharge() > 0 )	  
		{
		  kinkVtxRow.pidd = 8;
		  kinkVtxRow.pidp = 11;
		} else {
		  kinkVtxRow.pidd = 9;
		  kinkVtxRow.pidp = 12;
		}
	    } else if( (deltaKaonMuon < deltaKaonPion) && (deltaKaonMuon < deltaPionMuon) )
	      {
		kinkVtxRow.theta_cm = radToDeg*asin((daughterMom.mag()/kaonToMuonQ)*sin(decayAngle*degToRad));
		if( myTrack1->getCharge() > 0 )	  
		  {
		    kinkVtxRow.pidd = 5;
		    kinkVtxRow.pidp = 11;
		  } else {
		    kinkVtxRow.pidd = 6;
		    kinkVtxRow.pidp = 12;
		  }   
	      } else {
		kinkVtxRow.theta_cm = radToDeg*asin((daughterMom.mag()/pionToMuonQ)*sin(decayAngle*degToRad));
		if( myTrack1->getCharge() > 0 )	  
		  {
		    kinkVtxRow.pidd = 5;
		    kinkVtxRow.pidp = 8;
		  } else {
		    kinkVtxRow.pidd = 6;
		    kinkVtxRow.pidp = 9;
		  }   
	      }
	
	  kinkVtxRow.id = kinkVtxIndex + 1;  
	  kinkVtxRow.id_vertex = dstVtxIndex + 1;
	  kinkVtxRow.idd  = myTrack2->getId();
	  kinkVtxRow.idp  = myTrack1->getId();
	  kinkVtxRow.dca  = dca;
	  kinkVtxRow.dcad = daughterImpact;
	  kinkVtxRow.dcap = parentImpact;
	  kinkVtxRow.dlf  = sqrt( TMath::Power(myTrack2->getStartPoint(0)-myTrack1->getLastPoint(0), 2) +
				  TMath::Power(myTrack2->getStartPoint(1)-myTrack1->getLastPoint(1), 2) + 
				  TMath::Power(myTrack2->getStartPoint(2)-myTrack1->getLastPoint(2), 2) ); 
	  
          kinkVtxRow.dlv  = sqrt( TMath::Power(mKinkVertex.x()-myTrack1->getLastPoint(0), 2) +
				  TMath::Power(mKinkVertex.y()-myTrack1->getLastPoint(1), 2) +
				  TMath::Power(mKinkVertex.z()-myTrack1->getLastPoint(1), 2) );
	  
	  kinkVtxRow.dE[0] = deltaKaonMuon;
	  kinkVtxRow.dE[1] = deltaKaonPion;
	  kinkVtxRow.dE[2] = deltaPionMuon;
	  
	  kinkVtxRow.p[0] = parentMom.x();
	  kinkVtxRow.p[1] = parentMom.y();
	  kinkVtxRow.p[2] = parentMom.z();
	  
	  kinkVtxRow.pd[0] = daughterMom.x();
	  kinkVtxRow.pd[1] = daughterMom.y();
	  kinkVtxRow.pd[2] = daughterMom.z();
	  	  
	  kinkVtxRow.theta = decayAngle;


	  dstVtxRow.id       = dstVtxIndex + 1;
	  dstVtxRow.det_id   = 100*myTrack1->getDetId() + myTrack2->getDetId();
	  dstVtxRow.x        = mKinkVertex.x();
	  dstVtxRow.y        = mKinkVertex.y();
	  dstVtxRow.z        = mKinkVertex.z();
	  dstVtxRow.sigma[0] = 0.;
	  dstVtxRow.sigma[1] = 0.;
	  dstVtxRow.sigma[2] = 0.;
	  dstVtxRow.pchi2    = 0.;
	  dstVtxRow.id_aux_ent = kinkVtxIndex + 1;

	  kinkCandidate++;		  
//==========================================================================
	  if(m_kinkEvalOn) {
	    St_DataSet *tpcTracks = GetDataSet("tpc_tracks"); 
	    St_DataSetIter tpcI(tpcTracks);  
	    
	    St_tpt_track  *tptTrack = (St_tpt_track *) tpcI["tptrack"];
	    St_tte_eval   *tteEval  = (St_tte_eval *)  tpcI["evaltrk"];
	   
	    tpt_track_st* tptPtr  = tptTrack->GetTable();
	    tte_eval_st*  tteEPtr = tteEval->GetTable(); 
	    
	    Int_t daughterMcId;
	    
	    for(Int_t m=0; m<tptTrack->GetNRows(); m++)
	      {
		if(tptPtr->id_globtrk == myTrack2->getId())
		  {
		    for(Int_t n=0; n<tteEval->GetNRows(); n++)
		      {
			if(tteEPtr->rtrk == tptPtr->id)
			  {
			    daughterMcId = tteEPtr->mtrk;
			    break;		
			  }
			tteEPtr++;
		      }
		  }
		tptPtr++;
	      }
	    
//==============================================================
	    tpt_track_st* tptPtr1  = tptTrack->GetTable();
	    tte_eval_st*  tteEPtr1 = tteEval->GetTable();
	    
	    Int_t parentMcId;
	    Int_t parentPid;
	    
	    for(Int_t x=0; x<tptTrack->GetNRows(); x++)
	      {
		if(tptPtr1->id_globtrk == myTrack1->getId())
		  {
		    for(Int_t y=0; y<tteEval->GetNRows(); y++)
		      {
			if(tteEPtr1->rtrk == tptPtr1->id)
			  {
			    parentMcId = tteEPtr1->mtrk;
			    parentPid = tteEPtr1->pid;		  
			    break;		
			  }
			tteEPtr1++;
		      }
		  }
		tptPtr1++;
	      }
	    
  //=====================================================================
	    Int_t stopIdParent;
	    Int_t startIdDaughter;	
	    Int_t vertexGeProc; 
	    
	    St_DataSet *geant = GetDataSet("geant"); 
	    St_DataSetIter geantI(geant);         
	    
	    St_g2t_track  *g2tTrack  = (St_g2t_track *)  geantI["g2t_track"];
	    St_g2t_vertex *g2tVertex = (St_g2t_vertex *) geantI["g2t_vertex"];
	    
	    g2t_track_st*   g2tTrackStart  = g2tTrack->GetTable();
	    g2t_vertex_st*  g2tVertexStart = g2tVertex->GetTable();
	    
	    g2t_track_st*  g2tTrackPtr;
	    g2t_vertex_st* g2tVertexPtr;
	    
	    if( daughterMcId>g2tTrack->GetNRows() || daughterMcId<1  ) {
	      goto WRONGFILL; 	    
	    }
	    if( parentMcId>g2tTrack->GetNRows() || parentMcId<1 ) {
	      goto WRONGFILL; 	    
	    }
	    
	    g2tTrackPtr = g2tTrackStart + (parentMcId -1);

	    stopIdParent = g2tTrackPtr->stop_vertex_p;
 //=====================================================================
	    g2tTrackPtr = g2tTrackStart + (daughterMcId -1);

	    startIdDaughter = g2tTrackPtr->start_vertex_p;
	    
	    if( stopIdParent>g2tVertex->GetNRows() || stopIdParent<1 )  {
	      goto WRONGFILL; 	    
	    }
	    if( startIdDaughter>g2tVertex->GetNRows() || startIdDaughter<1 ) {
	      goto WRONGFILL; 
	    }
	    
	    g2tVertexPtr = g2tVertexStart + (startIdDaughter -1);
	    
	    vertexGeProc = g2tVertexPtr->ge_proc;
	    
	    if( stopIdParent==startIdDaughter  && ( parentPid==11 || parentPid==12 ) && vertexGeProc==5 )
	      {
		dstVtxRow.iflag       = 1;
		dstVtxRow.vtx_id      = kKinkVtxId;
		dstVtxRow.n_daughters = 1; 
	      } else {
		dstVtxRow.iflag       = 0;
		dstVtxRow.vtx_id      = kOtherVtxId;
		dstVtxRow.n_daughters = 9999;                  //????????????
	      }
	    goto PROPERFILL;
	  }
//================================================================================ 
WRONGFILL:
	  cout << "filling dst_vertex.iflag with 2. \n" << endl; 
	  dstVtxRow.iflag       = 2;
	  dstVtxRow.vtx_id      = 0;
	  dstVtxRow.n_daughters = 0; 
PROPERFILL:	  
	  kinkVertex->AddAt(&kinkVtxRow, kinkVtxIndex);
	  vertex->AddAt(&dstVtxRow, dstVtxIndex);
	  
	  kinkVtxIndex++;	
	  dstVtxIndex++;
	}
    }
  trackArray->Delete();
  cout << " kinkCandidate is " << kinkCandidate << endl;
  
  return kStOK; 
}

//_____________________________________________________________________________
void StKinkMaker::PrintInfo(){
  printf("**************************************************************\n");
  printf("* $Id: StKinkMaker.cxx,v 1.5 1999/07/07 15:47:53 wdeng Exp $\n");
//  printf("* %s    *\n",m_VersionCVS);
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

//_____________________________________________________________________________
Int_t StKinkMaker::meetTwoHelices2D(const Float_t cut, const StPhysicalHelixD& helix1, 
				   const StPhysicalHelixD& helix2, Float_t xCoordinates[2], 
				   Float_t yCoordinates[2])
{       

  Float_t om1, om2, ph1, ph2;
  Float_t a, b, c, d, dia, dtouch;
  Float_t xc1[2], xc2[2], r1, r2;  
  Int_t    flag;
    
  xc1[0] = helix1.xcenter();
  xc1[1] = helix1.ycenter();
          
  xc2[0] = helix2.xcenter();
  xc2[1] = helix2.ycenter();
  
  r1 = 1./(helix1.curvature());
  r2 = 1./(helix2.curvature());
  
  /*    Clear variables */
  
  xCoordinates[0] = 0.;
  xCoordinates[1] = 0.;
  yCoordinates[0] = 0.;
  yCoordinates[1] = 0.;
  flag = 2;
  
  /*    Find the two intersections      */
  
  a = xc1[0]-xc2[0];
  b = xc1[1]-xc2[1];
  dia = sqrt(a*a + b*b);
  dtouch = dia - r1 - r2;
  c = (r1*r1 - r2*r2 + a*a + b*b)/2.0;
  d = (a*a + b*b)*r1*r1 - c*c;
  
  /*    Check if there is any solution, one or two      */
  
  if (d < 0.0)
    {
      if (dtouch <= cut)
        {
          flag = 1;
          xCoordinates[0] = xc1[0] + r1*(xc2[0]-xc1[0])/dia;
          yCoordinates[0] = xc1[1] + r1*(xc2[1]-xc1[1])/dia;
          xCoordinates[1] = xCoordinates[0];
          yCoordinates[1] = yCoordinates[0];
        }
      else
        {
          flag = 0;
        }
    }
  else
    {
      if (d == 0.0) 
        {
          flag = 1;
        }
      om1       = ( -b*c+fabs(a*sqrt(d)) ) / (a*a+b*b);
      om2       = ( -b*c-fabs(a*sqrt(d)) ) / (a*a+b*b);
      
      /*        Find the right pair     */
      
      if ( (r1*r1-om1*om1) >= 0.0)
        {
          ph1 = sqrt(r1*r1-om1*om1);
          ph2 = -ph1;
	  if ( fabs(TMath::Power((ph1+a), 2)+TMath::Power((om1+b), 2)-r2*r2) 
              <= fabs(TMath::Power((ph2+a), 2) + TMath::Power((om1+b), 2) - r2*r2) )
            {
	      xCoordinates[0] = ph1+xc1[0];
            }
          else
            {
              xCoordinates[0] = ph2+xc1[0];
            }   
	  yCoordinates[0] = om1+xc1[1];
        }
      
      /*        Second pair     */
      
      if ( (r1*r1-om2*om2) >= 0.0)
        {
          ph1 = sqrt(r1*r1-om2*om2);
          ph2 = -ph1;
          if ( fabs(TMath::Power((ph1+a), 2) + TMath::Power((om2+b), 2) - r2*r2)
              <= fabs(TMath::Power((ph2+a), 2) + TMath::Power((om2+b), 2) - r2*r2) )
            {
              xCoordinates[1] = ph1 + xc1[0];
            }
          else
            {
              xCoordinates[1] = ph2 + xc1[0];
            }   
	  yCoordinates[1] = om2 + xc1[1];
        }
    }
  return flag;
}

//_____________________________________________________________________________
Float_t StKinkMaker::dcaTwoLines(Float_t xn1[3], Float_t xn2[3], Float_t sxz1, Float_t syz1, 
		     Float_t sxz2, Float_t syz2, Float_t point1AtDca[3], Float_t point2AtDca[3])
{
  Float_t        x1, x2, y1, y2, z1, z2;
  Float_t        dx, dy, dz;
  Float_t        a1, a2, a3, c, k, l, m, b, v, A, Bb, Cc, D, E, F;
  Float_t        mdca;
  
  x1 = xn1[0];
  y1 = xn1[1];
  z1 = xn1[2];
  x2 = xn2[0];
  y2 = xn2[1];
  z2 = xn2[2];
  dx = x1-x2;
  dy = y1-y2;
  dz = z1-z2;
  a1 = syz1-syz2;
  a2 = sxz2-sxz1;
  a3 = sxz1*syz2-syz1*sxz2;
  
  mdca = fabs(dx*a1 + dy*a2 + dz*a3);
  mdca = mdca / sqrt(a1*a1 + a2*a2 + a3*a3);
  
  if ( (syz1 != 0) && (syz2 != 0) )
    {
      c = sxz1/syz1;
      k = (sxz1*y1)/syz1 - x1;
      l = y1/syz1 - z1;
      b = sxz2/syz2;
      m = (sxz2*y2)/syz2 - x2;
      v = y2/syz2 - z2;
      A = c*a2-a1;
      Bb = b*a2-a1;
      Cc = a2*(k-m);
      D = a2/syz1 - a3;
      E = a2/syz2 - a3;
      F = a2*(l-v);
      
      point2AtDca[1] = (A*F - Cc*D)/(D*Bb - A*E);
      point1AtDca[1] = (Bb*F - Cc*E)/(D*Bb - A*E); 
        
      point2AtDca[2] = point2AtDca[1]/syz2 - v;
      point1AtDca[2] = point1AtDca[1]/syz1 - l;
        
      point2AtDca[0] = b*point2AtDca[1] - m;
      point1AtDca[0] = c*point1AtDca[1]  - k;
  
    }
  else
    {
      mdca = 111111.;    
    }
  
  return mdca;
}
  

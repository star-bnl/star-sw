// $Id: StKinkMaker.cxx,v 1.25 1999/12/03 20:29:14 wdeng Exp $
// $Log: StKinkMaker.cxx,v $
// Revision 1.25  1999/12/03 20:29:14  wdeng
// Comment out the cut for dip angle of parent
//
// Revision 1.24  1999/11/10 20:29:38  wdeng
// Fixed problems with unit usage. Comment out the cut of point number for the moment.
//
// Revision 1.23  1999/10/25 21:46:52  wdeng
// More iflag options
//
// Revision 1.22  1999/09/30 13:34:27  wdeng
// Diminish the degree or radian bug
//
// Revision 1.21  1999/09/29 18:56:39  wdeng
// Accommodated to dst_track and dst_vertex change
//
// Revision 1.20  1999/09/24 01:23:36  fisyak
// Reduced Include Path
//
// Revision 1.19  1999/09/12 23:03:03  fisyak
// Move parameters into makers
//
// Revision 1.18  1999/08/31 21:56:37  fisyak
// Remove SetNRows
//
// Revision 1.17  1999/08/26 17:30:39  wdeng
// Fix typo. Reorganize Make() function. Use shorter names for identifiers
//
// Revision 1.16  1999/08/24 22:37:23  wdeng
// Add check of used rows against table size. Reallocate if necessary.
//
// Revision 1.15  1999/08/23 22:35:38  wdeng
// Fill vtx_id always with kKinkVtxId, n_daughters with 1. Reorganize the code a little bit.
//
// Revision 1.14  1999/08/20 16:02:45  wdeng
// Add a word Exit into the warning messages
//
// Revision 1.13  1999/08/02 18:41:05  wdeng
// Change if{ } range. Use new loop-variable name.
//
// Revision 1.12  1999/07/17 00:31:24  genevb
// Use StMessMgr
//
// Revision 1.11  1999/07/15 22:27:43  wdeng
// debug
//
// Revision 1.10  1999/07/14 14:58:33  wdeng
// Check if there is primary vertex. Uncomment PrintInfo().
//
// Revision 1.9  1999/07/13 15:34:34  wdeng
// Add protections in case that some tables are not there
//
// Revision 1.8  1999/07/12 19:06:59  wdeng
// New primary vertex scheme. Get B from gufld(). Use math__constants.h and phys_constants.h
//
// Revision 1.7  1999/07/08 19:09:51  fisyak
// Add tabs, remove St_glb_Maker
//
// Revision 1.6  1999/07/08 18:40:30  fisyak
// Wensheng Deng global chain
//
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

#include "StMessMgr.h"

#include "StDetectorId.h"
#include "StVertexId.h"
#include "math_constants.h"
#include "phys_constants.h"

#include "PhysicalConstants.h"
#include "StThreeVector.hh"
#include "TObjArray.h"
#include "SystemOfUnits.h"

#include "fortranc.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)

#define kaonMass     M_KAON_PLUS
#define pionMass     M_PION_PLUS
#define muonMass     M_MUON_PLUS
#define pi0Mass      M_PION_0

#define kaonToMuonQ  0.236
#define kaonToPionQ  0.205
#define pionToMuonQ  0.030
  
#define MAXNUMOFTRACKS 10000
#define SIZETRKIDCHECK 1000

ClassImp(StKinkMaker)
  
  //_____________________________________________________________________________
StKinkMaker::StKinkMaker(const char *name):
    StMaker(name),
    m_tkfpar(0)
{
  m_kinkEvalOn = kTRUE;
}
//_____________________________________________________________________________
StKinkMaker::~StKinkMaker(){
}
//_____________________________________________________________________________
Int_t StKinkMaker::Init(){
  m_tkfpar =  new St_tkf_tkfpar("tkf_tkfpar",1);
  {
    tkf_tkfpar_st parRow;  
    memset(&parRow,0,m_tkfpar->GetRowSize());
    parRow.dcaParentDaughterMax      =  0.5;
    parRow.parentPtMin               =  0.2;   
    parRow.vertexRMax2D              =  179.;  
    parRow.vertexRMin2D              =  133.;  
    parRow.thetaMin                  =  1.; 
    parRow.numOfPadRows              =  40;  	// This value is for year1 only
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
  }
  AddRunCont(m_tkfpar);
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StKinkMaker::Make(){
  PrintInfo();

  St_DataSet *match = GetDataSet("match"); 
  if (!match) {
    gMessMgr->Warning() << " match is missing. Exit from StKinkMaker." << endm;
    return kStWarn;
  }  
  St_DataSetIter matchI(match);         
  St_dst_track  *globtrk = (St_dst_track *) matchI("globtrk");
  if (!globtrk) {
    gMessMgr->Warning() << " globtrk is missing. Exit from StKinkMaker." << endm;
    return kStWarn;
  }

  St_DataSet     *primary = GetDataSet("primary"); 
  if (!primary) {
    gMessMgr->Warning() << " primary is missing. Exit from StKinkMaker." << endm;
    return kStWarn;
  }
  St_DataSetIter primaryI(primary);         
  St_dst_vertex  *vertex   = (St_dst_vertex *) primaryI("vertex");
  if (!vertex) {
    gMessMgr->Warning() << " vertex is missing. Exit from StKinkMaker." << endm;
    return kStWarn;
  }
  
  StThreeVectorD eventVertex;
  Int_t whichRow = 0;  
  dst_vertex_st *vrtx = vertex->GetTable();
  for( whichRow=0; whichRow<vertex->GetNRows(); whichRow++,vrtx++) {
    if( vrtx->vtx_id == kEventVtxId && vrtx->iflag == 1 ) {
      eventVertex.setX(vrtx->x);
      eventVertex.setY(vrtx->y);
      eventVertex.setZ(vrtx->z);
      break;
    }
  }
  if( whichRow == vertex->GetNRows() ) {
    gMessMgr->Warning() << " No primary vertex. Exit from StKinkMaker." << endm;
    return kStWarn;
  }

  Int_t numOfGlbtrk = globtrk->GetNRows();
  Int_t tkf_limit = numOfGlbtrk/10 + 100;
  
  St_dst_tkf_vertex *kinkVertex  = (St_dst_tkf_vertex *) primaryI("kinkVertex");
  if(!kinkVertex) {
    kinkVertex = new St_dst_tkf_vertex("kinkVertex", tkf_limit);
    AddData(kinkVertex);
  }

  Int_t NoVertex = vertex->GetNRows()+ tkf_limit;
  vertex->ReAllocate(NoVertex); 
  
  tkf_tkfpar_st *tkfpar = m_tkfpar->GetTable();
  dst_track_st  *dstTrackStart  = globtrk->GetTable();

  StKinkLocalTrack* tempTrack;
  TObjArray* trackArray = new TObjArray(MAXNUMOFTRACKS);
  
  Float_t x[3] = {0,0,0};
  Float_t b[3];
  gufld(x,b);
  double B     = b[2]*kilogauss;
  
  Int_t i, j;
 
  for (i=0; i<numOfGlbtrk ; i++) {
    
    dst_track_st *dstTrackPtr = dstTrackStart + i;;
    //    if( dstTrackPtr->n_point > tkfpar->numOfPadRows ) continue; 

    if( dstTrackPtr->iflag > 0 ) 
      {
	Float_t dip   = atan(dstTrackPtr->tanl);
	Int_t    h     = (B*dstTrackPtr->icharge > 0 ? -1 : 1);
	Float_t phase = dstTrackPtr->psi*degree-h*pi/2;
	Float_t curvature = dstTrackPtr->curvature;
	Float_t x0 = dstTrackPtr->r0 * cos(dstTrackPtr->phi0 * degree);
	Float_t y0 = dstTrackPtr->r0 * sin(dstTrackPtr->phi0 * degree);
	Float_t z0 = dstTrackPtr->z0;
	StThreeVectorD origin(x0, y0, z0);  

	tempTrack = new StKinkLocalTrack(dstTrackPtr,
					 curvature/centimeter,					 
					 dip*radian, 
					 phase*radian,
					 origin*centimeter,
					 h);
	
	if(((*tempTrack).startRadius2D()<tkfpar->vertexRMin2D ) &&
	   ((*tempTrack).endRadius2D()>tkfpar->vertexRMax2D )) 
	  continue; 
	
	if(((*tempTrack).startRadius2D()<tkfpar->vertexRMax2D ) ||
	   ((*tempTrack).endRadius2D()>tkfpar->vertexRMin2D ))	
	  {
	    trackArray->Add(tempTrack);
	  }
      }
  }

  trackArray->Sort();
  
  dstVtxIndex  = vertex->GetNRows();
  kinkVtxIndex = 0;  

  Int_t   kinkCandidate=0;
  
  for( i = 0; i < trackArray->GetLast(); i++)
    {
      myTrack1 = (StKinkLocalTrack*)trackArray->At(i);

      //if(fabs( myTrack1->helix().dipAngle()) > tkfpar->parentDipAngleMax ) continue;  
      if( myTrack1->pt() < tkfpar->parentPtMin ) continue;     

      parentImpact = myTrack1->helix().distance(eventVertex);
      if( parentImpact > tkfpar->impactCut ) continue;
      
      for( j = i+1; j <= trackArray->GetLast(); j++)
	{
	  myTrack2 = (StKinkLocalTrack*)trackArray->At(j);

	  if( myTrack1->charge() != myTrack2->charge() ) continue;
	  if( myTrack2->startRadius2D() < tkfpar->vertexRMin2D ) continue;

	  daughterImpact = myTrack2->helix().distance(eventVertex);
	  if( daughterImpact < tkfpar->impactCut ) continue;
	  
	  if( fabs(myTrack2->startRadius2D()-myTrack1->endRadius2D()) > tkfpar->parentLastDaughterStart2D ) continue; 
	  if( fabs(myTrack2->startPoint(2)-myTrack1->lastPoint(2)) > tkfpar->parentLastDaughterStartZ ) continue;	  
	  
	  Float_t xCords[2], yCords[2];
	  Float_t xtarget, ytarget;
	  Int_t numOfSolution = MeetTwoHelices2D(tkfpar->dcaParentDaughterMax, myTrack1->helix(), 
		           		         myTrack2->helix(), xCords, yCords); 
	  
 	  if( numOfSolution == 0 ) continue;
	  if( numOfSolution == 1 ) 
	    {
	      Float_t radius2D = sqrt(xCords[0]*xCords[0] + yCords[0]*yCords[0]);
	      if( (radius2D < tkfpar->vertexRMin2D) || 	  
		  (radius2D > tkfpar->vertexRMax2D) )  continue;
	      xtarget = xCords[0];	    
	      ytarget = yCords[0];	    
	    }
	  if ( numOfSolution == 2 )
	    {
	      Float_t radiusOne2D = sqrt(xCords[0]*xCords[0] + yCords[0]*yCords[0]);
	      Float_t radiusTwo2D = sqrt(xCords[1]*xCords[1] + yCords[1]*yCords[1]);
	      if( (radiusOne2D > tkfpar->vertexRMin2D) && 	  
		  (radiusOne2D < tkfpar->vertexRMax2D) &&
		  (radiusTwo2D > tkfpar->vertexRMin2D) && 	  
		  (radiusTwo2D < tkfpar->vertexRMax2D) ) 
		{
		  Float_t distanceOne = sqrt(TMath::Power((xCords[0]-myTrack2->startPoint(0)), 2) + 
					     TMath::Power((yCords[0]-myTrack2->startPoint(1)), 2));
		  Float_t distanceTwo = sqrt(TMath::Power((xCords[1]-myTrack2->startPoint(0)), 2) + 
					     TMath::Power((yCords[1]-myTrack2->startPoint(1)), 2));	
		  if ( distanceOne < distanceTwo ) 
		    {
		      xtarget = xCords[0];	    
		      ytarget = yCords[0];	    
		    } 
		  else
		    {
		      xtarget = xCords[1];	    
		      ytarget = yCords[1];
		    }
		}
	      else if( (radiusOne2D > tkfpar->vertexRMin2D) && 	  
		       (radiusOne2D < tkfpar->vertexRMax2D) )
		{
		  xtarget = xCords[0];	    
		  ytarget = yCords[0];	    
		} 
	      else if( (radiusTwo2D > tkfpar->vertexRMin2D) && 	  
		       (radiusTwo2D < tkfpar->vertexRMax2D) )
		{
		  xtarget = xCords[1];	    
		  ytarget = yCords[1];
		} 
	      else continue;
	    }
	  
	  Float_t t1PathLength = myTrack1->helix().pathLength(xtarget, ytarget);
	  Float_t t2PathLength = myTrack2->helix().pathLength(xtarget, ytarget);
	  StThreeVectorD t1Project = myTrack1->helix().at(t1PathLength);
	  StThreeVectorD t2Project = myTrack2->helix().at(t2PathLength);
	  
	  if( fabs(t2Project.z()-t1Project.z()) > tkfpar->projectPointZDiff ) continue;

	  parentMoment   = myTrack1->helix().momentumAt(t1PathLength, B);
	  daughterMoment = myTrack2->helix().momentumAt(t2PathLength, B);
	  
	  decayAngle = (1./degree) * parentMoment.angle(daughterMoment);
	  if(decayAngle<tkfpar->thetaMin) continue;
	  
	  if( (parentMoment.z()==0.) || (daughterMoment.z()==0.) ) continue;

	  Float_t point1AtDca[3], point2AtDca[3];
	  dca =  DcaTwoLines(t1Project, t2Project, parentMoment, daughterMoment, point1AtDca, point2AtDca);
	  if ( dca>tkfpar->dcaParentDaughterMax ) continue;
	  
	  mKinkVertex.setX((point1AtDca[0]+point2AtDca[0])/2.);
	  mKinkVertex.setY((point1AtDca[1]+point2AtDca[1])/2.);
	  mKinkVertex.setZ((point1AtDca[2]+point2AtDca[2])/2.);
	  
	  Float_t distanceKinkParent2D   = sqrt( TMath::Power(mKinkVertex.x()-myTrack1->lastPoint(0), 2) +
					         TMath::Power(mKinkVertex.y()-myTrack1->lastPoint(1), 2) );
	  
	  Float_t distanceKinkDaughter2D = sqrt( TMath::Power(mKinkVertex.x()-myTrack2->startPoint(0), 2) +
						 TMath::Power(mKinkVertex.y()-myTrack2->startPoint(1), 2) );
	  
	  Float_t distanceKinkParentZ    = sqrt( TMath::Power(mKinkVertex.z()-myTrack1->lastPoint(2), 2) );
	  Float_t distanceKinkDaughterZ  = sqrt( TMath::Power(mKinkVertex.z()-myTrack2->startPoint(2), 2) );
	  
	  if( distanceKinkParent2D > tkfpar->distanceKinkParent2D ) continue; 
	  if( distanceKinkDaughter2D > tkfpar->distanceKinkDaughter2D ) continue; 
	  
	  if( distanceKinkParentZ > tkfpar->distanceKinkParentZ ) continue; 
	  if( distanceKinkDaughterZ > tkfpar->distanceKinkDaughterZ ) continue; 

	  kinkCandidate++;		  

	  FillTableRow();
  
	  Int_t kvTableSize = kinkVertex->GetTableSize();
	  Int_t dstvTableSize = vertex->GetTableSize();
	  
	  if( kinkVertex->GetNRows() == kvTableSize )
	    kinkVertex->ReAllocate( kvTableSize + 100 );
	  if( vertex->GetNRows() == dstvTableSize )
	    vertex->ReAllocate( dstvTableSize + 100 );
	  
	  kinkVertex->AddAt(&kinkVtxRow, kinkVtxIndex);
	  vertex->AddAt(&dstVtxRow, dstVtxIndex);	  
	  kinkVtxIndex++;	
	  dstVtxIndex++;
	}
    }
  trackArray->Delete();
  gMessMgr->Info() << " Found " << kinkCandidate << " kink candidates " << endm;

  // Check if two or more parents (daughters) have the same dst_track id. 
  // Refill iflag entry of dst_vertex as -1 (-2) if necessary. This also works for real data.

  TObjArray* trkIdChkArray = new TObjArray(SIZETRKIDCHECK);
  StKinkTrkIdCheck *trkIdChk1;
  StKinkTrkIdCheck *trkIdChk2;
 
  dst_tkf_vertex_st *kinkVtxStart = kinkVertex->GetTable();
  dst_vertex_st     *dstVtxStart = vertex->GetTable();

  for(Int_t lv1=0; lv1<kinkVertex->GetNRows()-1; lv1++) {
    dst_tkf_vertex_st *kinkVtxPtr1 = kinkVtxStart + lv1;

    for(Int_t lv2=lv1+1; lv2<kinkVertex->GetNRows(); lv2++) {
      dst_tkf_vertex_st *kinkVtxPtr2 = kinkVtxStart + lv2;
      
      if( kinkVtxPtr1->idp == kinkVtxPtr2->idp ) {
        trkIdChk1 = new StKinkTrkIdCheck();
        trkIdChk2 = new StKinkTrkIdCheck();

        trkIdChk1->setCommonIdp(1);
        trkIdChk1->setPosInKinkVtx(lv1);

        trkIdChk2->setCommonIdp(1);
        trkIdChk2->setPosInKinkVtx(lv2);

        trkIdChkArray->Add(trkIdChk1);
        trkIdChkArray->Add(trkIdChk2);
      }
      
      if( kinkVtxPtr1->idd == kinkVtxPtr2->idd ) {
        trkIdChk1 = new StKinkTrkIdCheck();
        trkIdChk2 = new StKinkTrkIdCheck();

        trkIdChk1->setCommonIdd(1);
        trkIdChk1->setPosInKinkVtx(lv1);

        trkIdChk2->setCommonIdd(1);
        trkIdChk2->setPosInKinkVtx(lv2);

        trkIdChkArray->Add(trkIdChk1);
        trkIdChkArray->Add(trkIdChk2);
      }

    }
  }

  for( i = 0; i <=trkIdChkArray->GetLast(); i++)
    {
      StKinkTrkIdCheck* trkIdChk3 = (StKinkTrkIdCheck*)trkIdChkArray->At(i);

      if( trkIdChk3->commonIdp() == 1 ) {
        Int_t posInKinkVtx = trkIdChk3->posInKinkVtx();
        dst_tkf_vertex_st *tkfVtx = kinkVtxStart + posInKinkVtx;
        Int_t dstVtxId = tkfVtx->id_vertex;
        dst_vertex_st *dstVtx = dstVtxStart + dstVtxId -1;
        dstVtx->iflag = -1;
      }
             
      if( trkIdChk3->commonIdd() == 1 ) {
        Int_t posInKinkVtx = trkIdChk3->posInKinkVtx();
        dst_tkf_vertex_st *tkfVtx = kinkVtxStart + posInKinkVtx;
        Int_t dstVtxId = tkfVtx->id_vertex;
        dst_vertex_st *dstVtx = dstVtxStart + dstVtxId -1;
        dstVtx->iflag = -2;
      }
    
    }

  trkIdChkArray->Delete();
  
  return kStOK; 
}

//_____________________________________________________________________________
void StKinkMaker::FillTableRow()
{	  
  StThreeVectorD pMomMinusDMom = parentMoment - daughterMoment;
  
  Float_t  deltaKaonMuon = fabs(sqrt(parentMoment.mag2() + kaonMass*kaonMass)   -
				sqrt(daughterMoment.mag2() + muonMass*muonMass) - 
				pMomMinusDMom.mag());
  Float_t  deltaKaonPion = fabs(sqrt(parentMoment.mag2() + kaonMass*kaonMass)   -
				sqrt(daughterMoment.mag2() + pionMass*pionMass) - 
				sqrt(pMomMinusDMom.mag2() + pi0Mass*pi0Mass));
  Float_t  deltaPionMuon = fabs(sqrt(parentMoment.mag2() + pionMass*pionMass)   -
				sqrt(daughterMoment.mag2() + muonMass*muonMass) - 
				pMomMinusDMom.mag());  
  
  if( (deltaKaonPion < deltaKaonMuon) && (deltaKaonPion < deltaPionMuon) )
    {
      kinkVtxRow.theta_cm = (1./degree) * asin((daughterMoment.mag()/kaonToPionQ)*sin(decayAngle*degree));
      if( myTrack1->charge() > 0 )	  
	{
	  kinkVtxRow.pidd = 8;
	  kinkVtxRow.pidp = 11;
	} else {
	  kinkVtxRow.pidd = 9;
	  kinkVtxRow.pidp = 12;
	}
    } else if( (deltaKaonMuon < deltaKaonPion) && (deltaKaonMuon < deltaPionMuon) )
      {
	kinkVtxRow.theta_cm = (1./degree) * asin((daughterMoment.mag()/kaonToMuonQ)*sin(decayAngle*degree));
	if( myTrack1->charge() > 0 )	  
	  {
	    kinkVtxRow.pidd = 5;
	    kinkVtxRow.pidp = 11;
	  } else {
	    kinkVtxRow.pidd = 6;
	    kinkVtxRow.pidp = 12;
	  }   
      } else {
	kinkVtxRow.theta_cm = (1./degree) * asin((daughterMoment.mag()/pionToMuonQ)*sin(decayAngle*degree));
	if( myTrack1->charge() > 0 )	  
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
  kinkVtxRow.idd  = myTrack2->Id();
  kinkVtxRow.idp  = myTrack1->Id();
  kinkVtxRow.dca  = dca;
  kinkVtxRow.dcad = daughterImpact;
  kinkVtxRow.dcap = parentImpact;
  kinkVtxRow.dlf  = sqrt( TMath::Power(myTrack2->startPoint(0)-myTrack1->lastPoint(0), 2) +
			  TMath::Power(myTrack2->startPoint(1)-myTrack1->lastPoint(1), 2) + 
			  TMath::Power(myTrack2->startPoint(2)-myTrack1->lastPoint(2), 2) ); 
  
  kinkVtxRow.dlv  = sqrt( TMath::Power(mKinkVertex.x()-myTrack1->lastPoint(0), 2) +
			  TMath::Power(mKinkVertex.y()-myTrack1->lastPoint(1), 2) +
			  TMath::Power(mKinkVertex.z()-myTrack1->lastPoint(2), 2) );
  
  kinkVtxRow.dE[0] = deltaKaonMuon;
  kinkVtxRow.dE[1] = deltaKaonPion;
  kinkVtxRow.dE[2] = deltaPionMuon;
  
  kinkVtxRow.p[0] = parentMoment.x();
  kinkVtxRow.p[1] = parentMoment.y();
  kinkVtxRow.p[2] = parentMoment.z();
	  
  kinkVtxRow.pd[0] = daughterMoment.x();
  kinkVtxRow.pd[1] = daughterMoment.y();
  kinkVtxRow.pd[2] = daughterMoment.z();
	  
  kinkVtxRow.theta = decayAngle;

  dstVtxRow.vtx_id      = kKinkVtxId;
  dstVtxRow.n_daughters = 1;    
  dstVtxRow.id          = dstVtxIndex + 1;
  FillIflag();
  dstVtxRow.det_id      = 100*myTrack1->DetId() + myTrack2->DetId();
  dstVtxRow.id_aux_ent  = kinkVtxIndex + 1;
 
  dstVtxRow.x        = mKinkVertex.x();
  dstVtxRow.y        = mKinkVertex.y();
  dstVtxRow.z        = mKinkVertex.z();
  dstVtxRow.covar[0] = 0.;
  dstVtxRow.covar[1] = 0.;
  dstVtxRow.covar[2] = 0.;
  dstVtxRow.covar[3] = 0.;
  dstVtxRow.covar[4] = 0.;
  dstVtxRow.covar[5] = 0.;
  dstVtxRow.chisq[0]    = 0.;
  dstVtxRow.chisq[1]    = 0.;

}

//_____________________________________________________________________________
void StKinkMaker::FillIflag()
{
  if(m_kinkEvalOn) {
    St_DataSet *tpcTracks = GetDataSet("tpc_tracks"); 
    if (tpcTracks) {
      St_DataSetIter tpcI(tpcTracks);  
      
      St_tpt_track  *tptTrack = (St_tpt_track *) tpcI["tptrack"];
      St_tte_eval   *tteEval  = (St_tte_eval *)  tpcI["evaltrk"];
      if (tptTrack && tteEval) {
	tpt_track_st* tptPtr  = tptTrack->GetTable();
	tte_eval_st*  tteEPtr = tteEval->GetTable(); 
	
	Int_t daughterMcId;
	
	for(Int_t m=0; m<tptTrack->GetNRows(); m++)
	  {
	    if(tptPtr->id_globtrk == myTrack2->Id())
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
	
	for(Int_t xl=0; xl<tptTrack->GetNRows(); xl++)
	  {
	    if(tptPtr1->id_globtrk == myTrack1->Id())
	      {
		for(Int_t yl=0; yl<tteEval->GetNRows(); yl++)
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
	// Int_t numDaughter;
	
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
	
	if( stopIdParent>g2tVertex->GetNRows() || stopIdParent<0 )  {
	  goto WRONGFILL; 	    
	}
	if( startIdDaughter>g2tVertex->GetNRows() || startIdDaughter<1 ) {
	  goto WRONGFILL; 
	}
	
	g2tVertexPtr = g2tVertexStart + (startIdDaughter -1);
	
	vertexGeProc = g2tVertexPtr->ge_proc;
	// numDaughter = g2tVertexPtr->n_daughter;
	
	dstVtxRow.iflag = -10;

	if( stopIdParent != startIdDaughter ) {
	  dstVtxRow.iflag = 0;
	} else {
	  if( vertexGeProc == 20 ) {
	    dstVtxRow.iflag = -3;
	  }

	  if( vertexGeProc != 20 && vertexGeProc != 5 ) {
	    dstVtxRow.iflag = -4;
	  }
	  
	  if( vertexGeProc == 5 ) {
	    if( parentPid == kinkVtxRow.pidp ) {
	      dstVtxRow.iflag = 1;
	    } else {
	      dstVtxRow.iflag = 100 * parentPid + 1;
	    }
	  }
	}                    

	if( parentMcId == daughterMcId )  dstVtxRow.iflag = -9;

	goto PROPERFILL;
      }
    }
  }
  //================================================================================ 
 WRONGFILL:
  dstVtxRow.iflag = 2;
 PROPERFILL:	  
  assert(1);

}

//_____________________________________________________________________________
Int_t StKinkMaker::MeetTwoHelices2D(const Float_t cut, const StPhysicalHelixD& helix1, 
				    const StPhysicalHelixD& helix2, Float_t xCords[2], 
				    Float_t yCords[2])
{       
  
  Float_t  om1, om2, ph1, ph2;
  Float_t  a, b, c, d, dia, dtouch;
  Float_t  xc1[2], xc2[2], r1, r2;  
  Int_t    flag;
  
  xc1[0] = helix1.xcenter();
  xc1[1] = helix1.ycenter();
  
  xc2[0] = helix2.xcenter();
  xc2[1] = helix2.ycenter();
  
  r1 = 1./(helix1.curvature());
  r2 = 1./(helix2.curvature());
  
  /*    Clear variables */
  
  xCords[0] = 0.;
  xCords[1] = 0.;
  yCords[0] = 0.;
  yCords[1] = 0.;
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
          xCords[0] = xc1[0] + r1*(xc2[0]-xc1[0])/dia;
          yCords[0] = xc1[1] + r1*(xc2[1]-xc1[1])/dia;
          xCords[1] = xCords[0];
          yCords[1] = yCords[0];
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
	      xCords[0] = ph1+xc1[0];
            }
          else
            {
              xCords[0] = ph2+xc1[0];
            }   
	  yCords[0] = om1+xc1[1];
        }
      
      /*        Second pair     */
      
      if ( (r1*r1-om2*om2) >= 0.0)
        {
          ph1 = sqrt(r1*r1-om2*om2);
          ph2 = -ph1;
          if ( fabs(TMath::Power((ph1+a), 2) + TMath::Power((om2+b), 2) - r2*r2)
	       <= fabs(TMath::Power((ph2+a), 2) + TMath::Power((om2+b), 2) - r2*r2) )
            {
              xCords[1] = ph1 + xc1[0];
            }
          else
            {
              xCords[1] = ph2 + xc1[0];
            }   
	  yCords[1] = om2 + xc1[1];
        }
    }
  return flag;
}

//_____________________________________________________________________________
Float_t  StKinkMaker::DcaTwoLines(const StThreeVectorD& t1Project, const StThreeVectorD& t2Project, 
				  const StThreeVectorD& parentMom, const StThreeVectorD& daughterMom, 
				  Float_t point1AtDca[3], Float_t point2AtDca[3])
{
  Float_t        x1, x2, y1, y2, z1, z2;
  Float_t        sxz1, syz1, sxz2, syz2; 
  Float_t        dx, dy, dz;
  Float_t        a1, a2, a3, c, k, l, m, b, v, A, Bb, Cc, D, E, F;
  Float_t        mdca;
	  
  x1 = t1Project.x();
  y1 = t1Project.y();
  z1 = t1Project.z();
  x2 = t2Project.x();
  y2 = t2Project.y();
  z2 = t2Project.z();
  sxz1 = parentMom.x()/parentMom.z();
  syz1 = parentMom.y()/parentMom.z();
  sxz2 = daughterMom.x()/daughterMom.z();
  syz2 = daughterMom.y()/daughterMom.z();

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


/***************************************************************************
 *
 * $Id: StSsdAlign.cc,v 1.5 2004/01/26 23:05:44 perev Exp $
 *
 * Author: Ludovic Gaudichet
 ***************************************************************************
 *
 * Description : SVT & SSD alignment code
 *
 ***************************************************************************
 *
 * $Log: StSsdAlign.cc,v $
 * Revision 1.5  2004/01/26 23:05:44  perev
 * Cleanup
 *
 * Revision 1.4  2003/09/02 17:59:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2001/06/16 00:14:42  jeromel
 * REmoved unused var
 *
 * Revision 1.2  2001/05/09 16:33:02  gaudiche
 * bug on Solaris fixed - cleanup
 *
 *
 ***************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "StSsdAlign.hh"

#include "tables/St_svg_geom_Table.h"
#include "StSvtClassLibrary/StSvtConfig.hh"
#include "StDbUtilities/StSvtCoordinateTransform.hh"
#include "StDbUtilities/StSvtLocalCoordinate.hh"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StMessMgr.h"


StSsdLadder::StSsdLadder(const int nWaf) {
  mWafer = nWaf;
}


StSsdLadder::~StSsdLadder() {
};


StSsdLayer::StSsdLayer(const int nWafer, const int nLadder) {
  mWafer = nWafer;
  mLadder = nLadder;
  for ( int i = 0; i < nLadder; i++ )
    ladders[i] = new StSsdLadder(nWafer);
}


StSsdLayer::~StSsdLayer() {
  for ( int i = 0; i < mLadder; i++ )
    delete ladders[i];
};


StSsdAlign::StSsdAlign() {
  mLayer = 7;
  layers[0] = new StSsdLayer(4,8);
  layers[1] = new StSsdLayer(4,8);

  layers[2] = new StSsdLayer(6,12);
  layers[3] = new StSsdLayer(6,12);

  layers[4] = new StSsdLayer(7,16);
  layers[5] = new StSsdLayer(7,16);

  layers[6] = new StSsdLayer(16,20);
  // (wafers, ladders)
  mNumberOfEvents = 0;
}


StSsdAlign::~StSsdAlign() {
  for ( int i = 0; i < mLayer; i++ )
    delete layers[i];
  
  for ( int ev = 0; ev < mNumberOfEvents; ev++ )
    delete mEvents[ev];
};


void StSsdAlign::init(svg_geom_st* geom, StSvtConfig* config, int NumberOfEvents)
{ 
  mNumberOfEvents = NumberOfEvents;
  mgeom = geom;
  
  double x2[3], d2[3], t2[3], n2[3];

  // SVT :
  for ( int i=0 ; i<216 ; i++ )
    {
      for (int j =0; j<3; j++)
	{
	  x2[j] = (double)geom[i].x[j];
	  d2[j] = (double)geom[i].d[j];
	  t2[j] = (double)geom[i].t[j];
	  n2[j] = (double)geom[i].n[j];
	};
     
      int barrelId = (geom[i].layer+1)/2;
      int WaferId = config->getHybridIndex(barrelId,geom[i].ladder, geom[i].num_chip,1)/2;

      layers[ ( geom[i].layer-1) ]->ladders[ (geom[i].ladder-1) ]->wafers[ (geom[i].num_chip-1) ] =
	&mWafer[WaferId];
      mWafer[WaferId].init( x2, d2, t2, n2, geom[i].layer-1, geom[i].ladder-1, geom[i].num_chip-1, 3., 3. );
    };

  //SSD :
 for ( int i=216 ; i<536 ; i++ )
    {
      for (int j =0; j<3; j++)
	{
	  x2[j] = (double)geom[i].x[j];
	  d2[j] = (double)geom[i].d[j];
	  t2[j] = (double)geom[i].t[j];
	  n2[j] = (double)geom[i].n[j];
	};

      layers[ ( geom[i].layer-1) ]->ladders[ (geom[i].ladder-1) ]->wafers[ (geom[i].num_chip-1) ] =
	&mWafer[i];
      mWafer[i].init( x2, d2, t2, n2, geom[i].layer-1, geom[i].ladder-1, geom[i].num_chip-1, 3.75, 2.1 );
    };
};


void StSsdAlign::updateGeom(svg_geom_st* geom, StSvtConfig* config)
{ 
  // SVT :
  for ( int i=0 ; i<216 ; i++ )
    {
      int barrelId = (geom[i].layer+1)/2;
      int WaferId = config->getHybridIndex(barrelId,geom[i].ladder, geom[i].num_chip,1)/2;
      
      for (int j =0; j<3; j++)
	{
	  geom[i].x[j] = mWafer[WaferId].x(j);
	  geom[i].d[j] = mWafer[WaferId].d(j);
	  geom[i].t[j] = mWafer[WaferId].t(j);
	  geom[i].n[j] = mWafer[WaferId].n(j);
	};
    };
  
  //SSD :
  for ( int i=216 ; i<536 ; i++ )
    {
      for (int j =0; j<3; j++)
	{
	  geom[i].x[j] = mWafer[i].x(j);
	  geom[i].d[j] = mWafer[i].d(j);
	  geom[i].t[j] = mWafer[i].t(j);
	  geom[i].n[j] = mWafer[i].n(j);
	};
    };
};


int StSsdAlign::addNewHit( const double x, const double y, const double z )
{
  globalPoint global;
  global.x = x;
  global.y = y;
  global.z = z;
  localPoint local;
  
  int badOne = 0;
  
  for (int i=0; i<536; i++)
    {
      mWafer[i].global2Local(&global, &local);
      
      badOne = mWafer[i].addNewHit( &global );
     
      if(badOne)
	gMessMgr->Warning("trying to add a hit to a wafer far away or off !");
	//printf("trying to add a hit to a wafer far away or off !");
      
       if (badOne) break;
    };

  return badOne;
};


int StSsdAlign::recordEventHits()
{
  int bip = 1;
  for (int i=0; i<536; i++ )
    {
      bip = mWafer[i].recordEvent();
      if (!(bip)) break;
    };
  return bip;
};


double StSsdAlign::takeDerivatives( double step)
{
  double shifts[6]={0.0001*step, 0.0001*step, 0.0001*step, 0.0005*step, 0.0005*step, 0.0005*step};
 
  double derivativeMax = 0.;
  globalPoint p;

  for (int iwafer=0; iwafer<536; iwafer++ )
    if ( (mWafer[iwafer].isOn()) && (mWafer[iwafer].numberOfHits()) )
      for (int iparam=0; iparam<6; iparam++)
	{
	  mDerivatives[iwafer][iparam] = 0.;
	  mWafer[iwafer].shiftParam(iparam, shifts[iparam] );
	  
	  for (int iev=0; iev < mNumberOfEvents; iev++ )
	    for (int itrack=0; itrack<mEvents[iev]->numberOfTracks(); itrack++ )
	      if ( mEvents[iev]->isTrackSelected(itrack) )
		{
		  int hitNumber = mEvents[iev]->trackHitNumber(itrack,iwafer);
		  if (hitNumber!=-1)
		    {
		      mWafer[iwafer].speedHit( iev, mEvents[iev]->pointID( itrack, hitNumber), &p );
		      mEvents[iev]->setChi2( 2, itrack, hitNumber, p);
		    };
		};
	  
	  mWafer[iwafer].shiftParam(iparam, -2.*shifts[iparam] );

	  int compt=0;
	  for (int iev=0; iev < mNumberOfEvents; iev++ )
	    for (int itrack=0; itrack<mEvents[iev]->numberOfTracks(); itrack++ )
	      if ( mEvents[iev]->isTrackSelected(itrack) )
		{
		  int hitNumber = mEvents[iev]->trackHitNumber(itrack,iwafer);
		  if (hitNumber!=-1)
		    {		    
		      mWafer[iwafer].speedHit( iev, mEvents[iev]->pointID( itrack, hitNumber), &p );
		      mEvents[iev]->setChi2( 1, itrack, hitNumber, p);
		      mDerivatives[iwafer][iparam] += mEvents[iev]->chi2DiffPerHit(itrack);
		      compt++;
		    };
		};
	  if (compt) mDerivatives[iwafer][iparam] /=( compt*2.*shifts[iparam] );

	  if ( derivativeMax < fabs( mDerivatives[iwafer][iparam] ) )
	    derivativeMax = fabs(mDerivatives[iwafer][iparam]);
	  
	  mWafer[iwafer].shiftParam(iparam, shifts[iparam] );
	};

  return derivativeMax;
};


void StSsdAlign::shiftParams(double sh)
{
  double steps[6] = { 0.03*sh, 0.03*sh, 0.03*sh, 0.003*sh, 0.005*sh, 0.005*sh };
  for (int iwafer=0; iwafer<536; iwafer++)
    if (mWafer[iwafer].isOn())
      for (int jparam=0; jparam<6; jparam++)
	mWafer[iwafer].shiftParam(jparam, steps[jparam]*mDerivatives[iwafer][jparam] );
};


double StSsdAlign::totalChi2()
{
  double val=0.;
  for (int ev=0; ev<mNumberOfEvents; ev++)
    val += mEvents[ev]->processTracks();

 return ( val/mNumberOfEvents );
};


void StSsdAlign::updateGlobalPoints( int ev)
{
  globalPoint p;

  for (int nt=0; nt<mEvents[ev]->numberOfTracks(); nt++)
    {
      track *tr = mEvents[ev]->getTrack(nt);
      for ( int np=0; np<tr->numberOfHits; np++)
	{
	  mWafer[ tr->waferID[np] ].speedHit( ev, tr->pointID[np], &p );
	  tr->p[np] = p;
	};
      
    };
};


void StSsdAlign::makeAlign( int iMax )
{
  gMessMgr->Info()<<"SVT & SSD Au-Au alignment (primary vertex constraint)."<< endm; 
  gMessMgr->Info()<<"Chi2 decreasing procedure ..."<< endm; 
 
  double step =	2.; 
  double chi0 = totalChi2();
  double derm;
  int i	= 0;
  
  while((step >	0.2) &&	(i < iMax))
    {
      i++;
      gMessMgr->Info()<<"  step = "<< i << ", chi2 = "<< chi0 << endm;
      //printf("\n\nMakeAlign(): N %d, step=%.2f, chi2=%G\n", i, step, chi0);
      derm = takeDerivatives(step);
      shiftParams( -step/derm);

      for (int ev=0; ev<mNumberOfEvents; ev++)  updateGlobalPoints(ev);

      double chin = totalChi2();
      if(chin < chi0)
	{
	  chi0 = chin;
	}
      else
	{
	  shiftParams(0.5*step/derm);
	  for (int ev=0; ev<mNumberOfEvents; ev++) updateGlobalPoints(ev);

	  chin = totalChi2();
	  if(chin < chi0)
	    {
	      chi0 = chin;
	    }
	  else
	    {
	      shiftParams(0.5*step/derm);
	      for (int ev=0; ev<mNumberOfEvents; ev++) updateGlobalPoints(ev);
	      chi0 = totalChi2();
	      step /= 2.;
	    }
	}
    }
}


void StSsdAlign::makeAlign2( int iMax )
{
  double step =	1.; 
  double chi0 = totalChi2();
  double derm;
  
  gMessMgr->Info()<<"SVT & SSD cosmic alignment (primary vertex constraint)."<< endm;
  int nTotTracks = 0;
  for (int ev=0; ev<mNumberOfEvents; ev++)
    for (int tr=0; tr<mEvents[ev]->numberOfTracks(); tr++)
      if (mEvents[ev]->isTrackSelected(tr))
	nTotTracks++;
  //cout <<nTotTracks<<" tracks"<< endl;
  printf("   %d tracks\n", nTotTracks); 

  for (int i=0; i<iMax; i++)
    { 
      if ( 9./(double)i < step ) step = 9./(double)i;
      
      derm = takeDerivatives(step);
      shiftParams( -step/derm);
      for (int ev=0; ev<mNumberOfEvents; ev++) updateGlobalPoints(ev);      
      chi0 = totalChi2();
      
      gMessMgr->Info()<<"  i = "<<i<<", alignment constant = "<<step<<",     chi2 = "
		      << chi0 <<endm; 
      //printf("\nMakeAlign(): N %d, step=%.2f, chi2=%G\n", i, step, chi0);
    }
}


globalPoint StSsdAlign::findVertex(int ev)
{
  float distTreshold = 0.1;//0.07
  float chi2_max = 0.1;
  float vertex_width = 1.;
  float vertex_length = 12.1;
  float cosTheta = cos(30.*PI/180.); //research cone angle
  float maxDist01 = ((10.61-6.63)/cosTheta) * ((10.61-6.63)/cosTheta);
  
  int barrel0, barrel1, barrel2;
  int hits0, hits2;
  float lambda;
  track eventTracks[2000];
  globalPoint a,b;
  int num_track;

  StSsdEvent pEvent;
  
  for ( barrel0 = 0; barrel0 < 32; barrel0++ )    
    for ( barrel1 = 32; barrel1 < 104; barrel1++)
      
      if ( ( areWafersCloth( barrel0, barrel1, maxDist01 ) ) &&
	   ( mWafer[barrel0].isOn() ) &&
	   ( mWafer[barrel1].isOn() ) )
	{
	  num_track = 0;
	  for ( barrel2 = 104; barrel2 < 216; barrel2++ )
	    if ( ( areWafersAligned( barrel0, barrel1, barrel2 ) ) && ( mWafer[barrel2].isOn() ) )
	      {
		for ( hits0 = 0; hits0 < mWafer[barrel0].numberOfHits(ev); hits0++ )
		  for ( hits2 = 0; hits2 < mWafer[barrel2].numberOfHits(ev); hits2++ )
		    {
		      globalPoint p0, p2;
		      mWafer[ barrel0 ].speedHit( ev, hits0, &p0 );
		      mWafer[ barrel2 ].speedHit( ev, hits2, &p2 );
		      int hit_num = mWafer[barrel1].clotherHit(ev, &p0, &p2, distTreshold );

		      if ( hit_num != -1 )
			{
			  b.x = p2.x - p0.x; b.y = p2.y - p0.y; b.z = p2.z - p0.z;
			  lambda = ( p0.x*b.x + p0.y*b.y )/( b.x*b.x + b.y*b.y  );
			  a.x = p0.x - lambda*b.x;
			  a.y = p0.y - lambda*b.y;
			  a.z = p0.z - lambda*b.z;

			  if ( (::sqrt(a.z*a.z) < vertex_length) &&
			       (::sqrt(a.x*a.x + a.y*a.y) < vertex_width) )
			    {
			      eventTracks[num_track].p[0] = p0;
			      eventTracks[num_track].waferID[0] = barrel0;
			      eventTracks[num_track].pointID[0] = hits0;
			      eventTracks[num_track].p[1] = mWafer[barrel1].hit( ev, hit_num ) ;
			      eventTracks[num_track].waferID[1] = barrel1;
			      eventTracks[num_track].pointID[1] = hit_num;
			      eventTracks[num_track].p[2] = p2;
			      eventTracks[num_track].waferID[2] = barrel2;
			      eventTracks[num_track].pointID[2] = hits2;
			      eventTracks[num_track].numberOfHits = 3;
			      eventTracks[num_track].a = a;
			      eventTracks[num_track].b = b;
			      eventTracks[num_track].chi2_1 = 
				( pEvent.chi2ab_Np( eventTracks[num_track].a, eventTracks[num_track].b,
						     &(eventTracks[num_track].p[0]), 3,
						     eventTracks[num_track].p[0], 0) )/3.;
			      num_track++;
			    };		    
			};
		    };
	      };
	  for (int i = 0; i<num_track; i++)
	    for (int ipt=0; ipt<eventTracks[i].numberOfHits; ipt++)
	      for (int j = 0; j<num_track; ++j)
		for (int jpt=0; jpt<eventTracks[j].numberOfHits; jpt++)
		  if ( ( eventTracks[i].waferID[ipt] == eventTracks[j].waferID[jpt] )&&
		       ( eventTracks[i].pointID[ipt] == eventTracks[j].pointID[jpt] )&&
		       (i!=j) )
		    {
		      if (eventTracks[i].chi2_1 < eventTracks[j].chi2_1)
			eventTracks[j].chi2_1+=999.;
		      else eventTracks[i].chi2_1+=999.;
		    };
	  for (int i = 0; i<num_track; i++)
	    if (eventTracks[i].chi2_1<chi2_max)
	      pEvent.addTrack( eventTracks[i] );
	};
  
  globalPoint p;
  p = pEvent.getVertex();
  gMessMgr->Info()<<"SVT & SSD Au-Au alignment, primary vertex"<< endm;
  cout <<"    event "<<ev<<", vertex at ("<<p.x<<","<<p.y<<","<<p.z<<")"<< endl;
  //printf("    preliminary vertex search %d= %f, %f, %f\n", ev, p.x, p.y, p.z);

  return p;
};


int StSsdAlign::areWafersCloth( int inner, int outer, float distance2 )
{
  double x1, y1, z1, x2, y2, z2;

  x1 = *(mWafer[inner].center() );
  y1 = *(mWafer[inner].center() + 1 );
  z1 = *(mWafer[inner].center() + 2 );
  x2 = *(mWafer[outer].center() );
  y2 = *(mWafer[outer].center() + 1 );
  z2 = *(mWafer[outer].center() + 2 );

  float d = (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1);
  return ( d < distance2 );
};


int StSsdAlign::areWafersAligned( globalPoint *p1, int middle, int outer )
{
  globalPoint p3 = {*(mWafer[outer].center()), *(mWafer[outer].center()+1), *(mWafer[outer].center()+2)};
  localPoint P2 = mWafer[middle].pointOnWafer( p1, &p3 );
  double x2 = *(mWafer[middle].center());
  double y2 = *(mWafer[middle].center()+1);
  
  int result = ( (fabs(P2.X)<6) && (fabs(P2.Y)<6) && (x2*p3.x>=0.) && (y2*p3.y>=0.) );
  return result;
};


int StSsdAlign::areWafersAligned( int inner, int middle, int outer )
{
  globalPoint p1 = {*(mWafer[inner].center()), *(mWafer[inner].center()+1), *(mWafer[inner].center()+2)};
  globalPoint p3 = {*(mWafer[outer].center()), *(mWafer[outer].center()+1), *(mWafer[outer].center()+2)};
  localPoint P2 = mWafer[middle].pointOnWafer( &p1, &p3 );
  double x2 = *(mWafer[middle].center());
  double y2 = *(mWafer[middle].center()+1);
  
  int result = ( (fabs(P2.X)<6) && (fabs(P2.Y)<6) && (x2*p3.x>=0.) && (y2*p3.y>=0.) );
  return result;
};


//  Tracking (Au-Au event): at least 3 or 4 hits on one straight track
//********************************************************************
int StSsdAlign::tracking()
{
  float distTreshold = 0.1;
  float chi2_max = 0.1;
  float dca2max = 0.09*0.09;

  int barrel0, barrel1, barrel2, barrel3;
  int hits0, hits1, hits2, hits3;
  float lambda;

  double dca2;
  track eventTracks[2000];
  globalPoint a,b;
  int num_track;

  for (int ev=0; ev<mNumberOfEvents; ev++)
    {
      globalPoint vtx;

      vtx = findVertex(ev);
      //mEvents[ev] = new StSsdEvent;
      //***********************************************
      // search tracks (0,1,2), (0,1,3) and (0,1,2,3) :
      //***********************************************
      
      for ( barrel0 = 0; barrel0 < 32; barrel0++ )    
	for ( barrel1 = 32; barrel1 < 104; barrel1++)
	  
	  if ( ( areWafersAligned( &vtx, barrel0, barrel1) ) &&
	       ( mWafer[barrel0].isOn() ) &&
	       ( mWafer[barrel1].isOn() ) )
	    {
	      //************************************************************
	      num_track = 0;

	      for ( barrel2 = 104; barrel2 < 216; barrel2++ )
		if ( ( areWafersAligned( barrel0, barrel1, barrel2 ) ) && ( mWafer[barrel2].isOn() ) )
		  {
		    for ( hits0 = 0; hits0 < mWafer[barrel0].numberOfHits(ev); hits0++ )
		      for ( hits2 = 0; hits2 < mWafer[barrel2].numberOfHits(ev); hits2++ )
			{
			  globalPoint p0, p2;
			  mWafer[ barrel0 ].speedHit( ev, hits0, &p0 );
			  mWafer[ barrel2 ].speedHit( ev, hits2, &p2 );
			  int hit_num = mWafer[barrel1].clotherHit(ev, &p0, &p2, distTreshold );

			  if ( hit_num != -1 )
			    {
			      b.x = p2.x - p0.x; b.y = p2.y - p0.y; b.z = p2.z - p0.z;
			      lambda = ( p0.x*b.x + p0.y*b.y )/( b.x*b.x + b.y*b.y  );
			      a.x = p0.x - lambda*b.x - vtx.x;
			      a.y = p0.y - lambda*b.y - vtx.y;
			      a.z = p0.z - lambda*b.z - vtx.z;
			      dca2 = a.x*a.x + a.y*a.y + a.z*a.z - 
				(a.x*b.x + a.y*b.y + a.z*b.z)*(a.x*b.x + a.y*b.y + a.z*b.z)/
				(b.x*b.x + b.y*b.y + b.z*b.z);
			      if ( dca2 < dca2max )
			      {
				  eventTracks[num_track].p[0] = p0;
				  eventTracks[num_track].waferID[0] = barrel0;
				  eventTracks[num_track].pointID[0] = hits0;
				  eventTracks[num_track].p[1] = mWafer[barrel1].hit( ev, hit_num ) ;
				  eventTracks[num_track].waferID[1] = barrel1;
				  eventTracks[num_track].pointID[1] = hit_num;
				  eventTracks[num_track].p[2] = p2;
				  eventTracks[num_track].waferID[2] = barrel2;
				  eventTracks[num_track].pointID[2] = hits2;
				  eventTracks[num_track].numberOfHits = 3;
				  eventTracks[num_track].a = a;
				  eventTracks[num_track].b = b;
				  eventTracks[num_track].chi2_1 = 
				    ( mEvents[ev]->chi2ab_Np(eventTracks[num_track].a , eventTracks[num_track].b,
							     &(eventTracks[num_track].p[0]),
							     3, eventTracks[num_track].p[0], 0) )/3.;
				  num_track++;
				};		    
			    };
			};
		  };
	      for ( barrel3 = 216; barrel3 < 536; barrel3++ )
		if ( ( areWafersAligned( barrel0, barrel1, barrel3 ) ) && ( mWafer[barrel3].isOn() ) )
		  {
		    for ( hits0 = 0; hits0 < mWafer[barrel0].numberOfHits(ev); hits0++ )
		      for ( hits3 = 0; hits3 < mWafer[barrel3].numberOfHits(ev); hits3++ )
			{
			  globalPoint p0, p3;
			  mWafer[ barrel0 ].speedHit( ev, hits0, &p0 );
			  mWafer[ barrel3 ].speedHit( ev, hits3, &p3 );
			  
			  int hit_num = mWafer[barrel1].clotherHit(ev, &p0, &p3, distTreshold ); 
			  
			  if ( hit_num != -1 ) // if it is a (0,1,2,3) track
			    for ( int nt = 0; nt < num_track; nt++ ) 
			      if ( (eventTracks[nt].waferID[0]==barrel0)&&
				   (eventTracks[nt].pointID[0]==hits0)&&
				   (eventTracks[nt].waferID[1]==barrel1)&&
				   (eventTracks[nt].pointID[1]==hit_num) )
				{
				  eventTracks[nt].p[3] = p3; 
				  eventTracks[nt].waferID[3] = barrel3;
				  eventTracks[nt].pointID[3] = hits3;
				  eventTracks[nt].numberOfHits = 4;
				  eventTracks[nt].chi2_1 =
				    ( mEvents[ev]->chi2ab_Np( eventTracks[nt].a, eventTracks[nt].b,
							      &(eventTracks[num_track].p[0]),
							      4, eventTracks[num_track].p[0], 0) )/4.;
				  hit_num = -1;
				};

			  if ( hit_num != -1 )
			    {                       //cut de StSsdEvent::AddTrack
			      b.x = p3.x - p0.x;  b.y = p3.y - p0.y; b.z = p3.z - p0.z;
			      lambda = ( p0.x*b.x + p0.y*b.y )/( b.x*b.x + b.y*b.y  );
			      a.x = p0.x - lambda*b.x - vtx.x;
			      a.y = p0.y - lambda*b.y - vtx.y;
			      a.z = p0.z - lambda*b.z - vtx.z;
			      dca2 = a.x*a.x + a.y*a.y + a.z*a.z - 
				(a.x*b.x + a.y*b.y + a.z*b.z)*(a.x*b.x + a.y*b.y + a.z*b.z)/
				(b.x*b.x + b.y*b.y + b.z*b.z);
			      if ( dca2 < dca2max )
				{
				  eventTracks[num_track].p[0] = p0;
				  eventTracks[num_track].waferID[0] = barrel0;
				  eventTracks[num_track].pointID[0] =hits0;
				  eventTracks[num_track].p[1] = mWafer[barrel1].hit( ev, hit_num );
				  eventTracks[num_track].waferID[1] = barrel1;
				  eventTracks[num_track].pointID[1] = hit_num;
				  eventTracks[num_track].p[2] = p3;
				  eventTracks[num_track].waferID[2] = barrel3;
				  eventTracks[num_track].pointID[2] = hits3;
				  eventTracks[num_track].numberOfHits = 3;
				  eventTracks[num_track].a = a;
				  eventTracks[num_track].b = b;
				  eventTracks[num_track].chi2_1 = 
				    ( mEvents[ev]->chi2ab_Np( eventTracks[num_track].a, eventTracks[num_track].b,
							      &(eventTracks[num_track].p[0]),
							      3, eventTracks[num_track].p[0], 0) )/3.;

				  num_track++;
				};
			    };
			};
		  };
	      for (int i = 0; i<num_track; i++)
		for (int ipt=0; ipt<eventTracks[i].numberOfHits; ipt++)
		  for (int j = 0; j<num_track; ++j)
		    for (int jpt=0; jpt<eventTracks[j].numberOfHits; jpt++)
		      if ( ( eventTracks[i].waferID[ipt] == eventTracks[j].waferID[jpt] )&&
			   ( eventTracks[i].pointID[ipt] == eventTracks[j].pointID[jpt] )&&
			   (i!=j) )
			{
			  if (eventTracks[i].chi2_1 < eventTracks[j].chi2_1)
			    eventTracks[j].chi2_1+=999.;
			  else eventTracks[i].chi2_1+=999.;
			};
	      for (int i = 0; i<num_track; i++)
		if (eventTracks[i].chi2_1<chi2_max)
		  mEvents[ev]->addTrack( eventTracks[i] );
	    };
      

      //****************************************
      // tracks (0,2,3) and (1,2,3)
      //**************************************** 
      for ( barrel2 = 104; barrel2 < 216; barrel2++ )    
	for ( barrel3 = 216; barrel3 < 536; barrel3++)
	  
	  if ( ( areWafersAligned( &vtx, barrel2, barrel3) ) &&
	       ( mWafer[barrel2].isOn() ) &&
	       ( mWafer[barrel3].isOn() ) )
	    {
	      //************************************************************
	      num_track = 0;

	      for ( barrel0 = 0; barrel0 < 32; barrel0++ )
		if ( ( areWafersAligned( barrel0, barrel2, barrel3 ) ) &&
		     ( mWafer[barrel0].isOn() ) )
		  {
		    for ( hits0 = 0; hits0 < mWafer[barrel0].numberOfHits(ev); hits0++ )
		      for ( hits3 = 0; hits3 < mWafer[barrel3].numberOfHits(ev); hits3++ )
			{
			  globalPoint p0, p3;
			  mWafer[ barrel0 ].speedHit( ev, hits0, &p0 );
			  mWafer[ barrel3 ].speedHit( ev, hits3, &p3 );
			  int hit_num = mWafer[barrel2].clotherHit(ev, &p0, &p3, distTreshold );
			  
			  if ( hit_num != -1 ) // if it is a (0,1,2,3) track
			    for ( int nt = 0; nt < mEvents[ev]->numberOfTracks(); nt++ ) 
			      if ( (mEvents[ev]->getTrack(nt)->waferID[0] == barrel0)&&
				   (mEvents[ev]->getTrack(nt)->pointID[0] == hits0)&&
				   (mEvents[ev]->getTrack(nt)->waferID[2] == barrel2)&&
				   (mEvents[ev]->getTrack(nt)->pointID[2] == hit_num)&&
				   (mEvents[ev]->getTrack(nt)->waferID[3] == barrel3)&&
				   (mEvents[ev]->getTrack(nt)->pointID[3] == hits3) )
				hit_num = -1;
			  
			  if ( hit_num != -1 )
			    {
			      b.x = p3.x - p0.x; b.y = p3.y - p0.y; b.z = p3.z - p0.z;
			      lambda = ( p0.x*b.x + p0.y*b.y )/( b.x*b.x + b.y*b.y  );
			      a.x = p0.x - lambda*b.x - vtx.x;
			      a.y = p0.y - lambda*b.y - vtx.y;
			      a.z = p0.z - lambda*b.z - vtx.z;
			      dca2 = a.x*a.x + a.y*a.y + a.z*a.z - 
				(a.x*b.x + a.y*b.y + a.z*b.z)*(a.x*b.x + a.y*b.y + a.z*b.z)/
				(b.x*b.x + b.y*b.y + b.z*b.z);
			      if ( dca2 < dca2max )
				{
				  eventTracks[num_track].p[0] = p0;
				  eventTracks[num_track].waferID[0] = barrel0;
				  eventTracks[num_track].pointID[0] = hits0;
				  eventTracks[num_track].p[1] = mWafer[barrel2].hit( ev, hit_num ) ;
				  eventTracks[num_track].waferID[1] = barrel2;
				  eventTracks[num_track].pointID[1] = hit_num;
				  eventTracks[num_track].p[2] = p3;
				  eventTracks[num_track].waferID[2] = barrel3;
				  eventTracks[num_track].pointID[2] = hits3;
				  eventTracks[num_track].numberOfHits = 3;
				  eventTracks[num_track].a = a;
				  eventTracks[num_track].b = b;
				  eventTracks[num_track].chi2_1 =
				    ( mEvents[ev]->chi2ab_Np( eventTracks[num_track].a, eventTracks[num_track].b,
							      &(eventTracks[num_track].p[0]),
							      3, eventTracks[num_track].p[0], 0) )/3.;
				  num_track++;
				};
			    };
			};
		  };
	      
	      for ( barrel1 = 32; barrel1 < 104; barrel1++ )
		if ( ( areWafersAligned( barrel1, barrel2, barrel3 ) ) &&
		     ( mWafer[barrel1].isOn() ) )
		  {
		    for ( hits1 = 0; hits1 < mWafer[barrel1].numberOfHits(ev); hits1++ )
		      for ( hits3 = 0; hits3 < mWafer[barrel3].numberOfHits(ev); hits3++ )
			{
			  globalPoint p1, p3;
			  mWafer[ barrel1 ].speedHit( ev, hits1, &p1 );
			  mWafer[ barrel3 ].speedHit( ev, hits3, &p3 );
			  int hit_num = mWafer[barrel2].clotherHit(ev, &p1, &p3, distTreshold );
			  
			  if ( hit_num != -1 ) // if it is a (0,1,2,3) track
			    for ( int nt = 0; nt < mEvents[ev]->numberOfTracks(); nt++ ) 
			      if ( (mEvents[ev]->getTrack(nt)->waferID[1] == barrel1)&&
				   (mEvents[ev]->getTrack(nt)->pointID[1] == hits1)&&
				   (mEvents[ev]->getTrack(nt)->waferID[2] == barrel2)&&
				   (mEvents[ev]->getTrack(nt)->pointID[2] == hit_num)&&
				   (mEvents[ev]->getTrack(nt)->waferID[3] == barrel3)&&
				   (mEvents[ev]->getTrack(nt)->pointID[3] == hits3) )
				hit_num = -1;
			  
			  if ( hit_num != -1 )
			    {
			      b.x = p3.x - p1.x; b.y = p3.y - p1.y; b.z = p3.z - p1.z;
			      lambda = ( p1.x*b.x + p1.y*b.y )/( b.x*b.x + b.y*b.y  ); 
			      a.x = p1.x - lambda*b.x - vtx.x;
			      a.y = p1.y - lambda*b.y - vtx.y;
			      a.z = p1.z - lambda*b.z - vtx.z;
			      dca2 = a.x*a.x + a.y*a.y + a.z*a.z - 
				(a.x*b.x + a.y*b.y + a.z*b.z)*(a.x*b.x + a.y*b.y + a.z*b.z)/
				(b.x*b.x + b.y*b.y + b.z*b.z);
			      if ( dca2 < dca2max )
				{
				  eventTracks[num_track].p[0] = p1;
				  eventTracks[num_track].waferID[0] = barrel1;
				  eventTracks[num_track].pointID[0] = hits1;
				  
				  eventTracks[num_track].p[1] = mWafer[barrel2].hit( ev, hit_num ) ;
				  eventTracks[num_track].waferID[1] = barrel2;
				  eventTracks[num_track].pointID[1] = hit_num;
				  
				  eventTracks[num_track].p[2] = p3;
				  eventTracks[num_track].waferID[2] = barrel3;
				  eventTracks[num_track].pointID[2] = hits3;
				  eventTracks[num_track].numberOfHits = 3;
				  eventTracks[num_track].a = a;
				  eventTracks[num_track].b = b;
				  eventTracks[num_track].chi2_1 =
				    ( mEvents[ev]->chi2ab_Np( eventTracks[num_track].a, eventTracks[num_track].b,
							      &(eventTracks[num_track].p[0]),
							      3, eventTracks[num_track].p[0], 0) )/3.;
				  num_track++;
				};
			    };
			};
		  };
	      //printf("numtrack=%d\n",num_track);
	      for (int i = 0; i<num_track; i++)
		for (int ipt=0; ipt<eventTracks[i].numberOfHits; ipt++)
		  for (int j = 0; j<num_track; ++j)
		    for (int jpt=0; jpt<eventTracks[j].numberOfHits; jpt++)
		      if ( ( eventTracks[i].waferID[ipt] == eventTracks[j].waferID[jpt] )&&
			   ( eventTracks[i].pointID[ipt] == eventTracks[j].pointID[jpt] )&&
			   (i!=j) )
			{
			  if (eventTracks[i].chi2_1 < eventTracks[j].chi2_1)
			    eventTracks[j].chi2_1+=999.;
			  else eventTracks[i].chi2_1+=999.;
			};

	      for (int i = 0; i<num_track; i++)
		if (eventTracks[i].chi2_1<chi2_max)
		  mEvents[ev]->addTrack( eventTracks[i] );
	    };
    }; // end event
  
  return 1;
};


void StSsdAlign::saveTrackedEvents()
{
 FILE *tracked;
 tracked = fopen("trackedEvents.dat","wb");

 fwrite( &mNumberOfEvents , sizeof(int), 1, tracked);
 for (int ev=0; ev<mNumberOfEvents; ev++)
   {
     int nboftr = mEvents[ev]->numberOfTracks();     
     fwrite( &nboftr  , sizeof(int), 1, tracked);
     for (int i=0; i < mEvents[ev]->numberOfTracks() ; i++)
       fwrite( mEvents[ev]->getTrack(i) , sizeof(track), 1, tracked);
   };
 
 fclose(tracked);
};


void StSsdAlign::loadTrackedEvents(char * fichier)
{
 FILE *tracked;
 tracked = fopen(fichier,"rb");

 int numEvt;
 fread( &numEvt , sizeof(int), 1, tracked);
 for (int ev=0; ev < numEvt; ev++)
   {
     mEvents[ev] = new StSsdEvent;
     int numTr;
     track tmpTrack;
     fread( &numTr , sizeof(int), 1, tracked);
     for (int i=0; i  <numTr ; i++)
       {
	 fread( &tmpTrack , sizeof(track), 1, tracked);
	 mEvents[ev]->addTrack( tmpTrack );	 
       };
   };
 fclose(tracked);
};


// simulations ********************************************************************

// extern "C" void ranlux_(float &RVEC, const int &LEN); //better random generator
// inline float ranlux()
// {
//   float val;
//   ranlux_( val, 1);
//   return val;
// };
// extern "C" void rluxgo_(const int &LUX,const int &INT,const int &K1,const int &K2);
// inline void setRanlux(const int &lux, const int &inte, const int &k1, const int &k2)
// {
//   rluxgo_(lux, inte, k1, k2);
// };


double monrandom(double n)
{
double val;
 val = (double(rand())/(double)RAND_MAX)*n;
 //val = ranlux()*n;
return val;
};

double rgg()
{
  double  u=0.;
  for (int i=0; i<10 ; i++)
    u += monrandom(1.);
  return (u-5.)/0.911790;
};

int  hitProba(double efficacity)
{
  double val = monrandom( 100. );
  return (val < efficacity);
};


int StSsdAlign::simulUnAlignment(double trans, double rot)
{
  //setRanlux( 4, 13, 0, 0);
  for (int i=0; i<536; i++)
    {
      mWafer[i].setDx( monrandom(trans*2.)-trans );
      mWafer[i].setDy( monrandom(trans*2.)-trans );
      mWafer[i].setDz( monrandom(trans*2.)-trans );
      mWafer[i].setAlpha(monrandom(rot*2.)-rot );
      mWafer[i].setBeta(monrandom(rot*2.)-rot );
      mWafer[i].setGamma(monrandom(rot*2.)-rot );
    };
  return 1;
};


int StSsdAlign::simulUnAlignment(double transW, double rotW,double transL, double rotL )
{
  //setRanlux( 4, 13, 0, 0);
  for (int l=0; l<7; l++)
    for (int lad=0; lad < layers[l]->numberOfLadders(); lad++)
      {
	double laddz = rgg()*transL;
	double laddy = rgg()*transL;
	double ladGamma = rgg()*rotL;

	for (int waf=0; waf < layers[l]->ladders[lad]->numberOfWafers() ; waf++)
	  {
	    layers[l]->ladders[lad]->wafers[waf]->setDx( rgg()*transW );
	    layers[l]->ladders[lad]->wafers[waf]->setDy( rgg()*transW + laddy );
	    layers[l]->ladders[lad]->wafers[waf]->setDz( rgg()*transW + laddz );
	    layers[l]->ladders[lad]->wafers[waf]->setAlpha( rgg()*rotW );
	    layers[l]->ladders[lad]->wafers[waf]->setBeta(  rgg()*rotW );
	    layers[l]->ladders[lad]->wafers[waf]->setGamma( rgg()*rotW + ladGamma );
	  };
      };
  return 1;
};


int  StSsdAlign::simulEvents(int ev, int numberOfTracks ,int level)
{
  //setRanlux( 4, 0, 0, 0);
  gMessMgr->Info()<<"SVT & SSD Au-Au alignment : event "<<ev<<" simulation :"<< endm;
  gMessMgr->Info()<<numberOfTracks<<" generated tracks."<< endm;

  int ntracks=0, nTrackHit;
  CreateEvent(ev);
    
  //int nbofhits[536] = {0};
  globalPoint vertex;
  vertex.x = monrandom(2.)- 1.;
  vertex.y = monrandom(2.)- 1.;
  vertex.z = monrandom(20.) - 10.;
  gMessMgr->Info()<<"  vertex at ("<<vertex.x<<","<<vertex.y<<","<<vertex.z<<")." << endm;
  //printf("    vertex %d at : (%f, %f, %f)\n", ev+1, vertex.x, vertex.y, vertex.z );
  do
    { //***
      double teta, n, phi;
      phi = monrandom(2.*PI);
      n = monrandom(2.);
      teta = acos(1. - n);
      globalPoint p; //, pOnWaf;
      p.x = sin(teta)*cos(phi) +  vertex.x;
      p.y = sin(teta)*sin(phi) + vertex.y;
      p.z = cos(teta) + vertex.z;
      localPoint P;
      localPoint lP[4];
      int ilP[4];
      
      nTrackHit = 0;
      for (int i=0; i<536; i++)
	if ( (mWafer[i].isTouched( &vertex, &p, &P ))&&(hitProba(99.)) ) // isTouched inclue isOn()
	  {		
	    if (level>=1) // detectors position resolution :
	      {
		if (i>215)
		  {
		    P.X += 0.002*rgg();
		    P.Y += 0.08*rgg();
		    //P.Y += 0.002*rgg();
		      } else
			{
			  P.X += 0.002*rgg();
			  P.Y += 0.002*rgg();
			};
	      };
	    
	    if (nTrackHit<4)
	      {
		lP[nTrackHit] = P;
		ilP[nTrackHit] = i;
	      };		
	    nTrackHit++;
	  };
      //printf("simu :nTrackHit=%d\n",nTrackHit);
      if ( (nTrackHit<5)&&(nTrackHit>2) ) 
	{
	  for (int i = 0; i<nTrackHit; i++)
	    {
	      mWafer[ ilP[i] ].addNewHit( &lP[i] );
	      nbofhits[ ilP[i] ]++;	
	      //printf("simu wafer %d point %d\n",ilP[i], nbofhits[ ilP[i] ] - 1);
	    };
	  ntracks++;
	};
      //printf("nombre de hits dans la trace =%d\n",nTrackHit);
    } while (ntracks < numberOfTracks);
//   if (level>=2) // background
//     {
//       localPoint P;
//       P.Z = 0.;
//       int j;
      
//       for (int i=0; i<(int) (numberOfTracks*20./100.); i++)
// 	{
// 	  j = (int)monrandom(32.);
// 	  P.X = (monrandom(2.)-1.)*mWafer[j].localXsize();
// 	  P.Y = (monrandom(2.)-1.)*mWafer[j].localYsize();
// 	  mWafer[j].addNewHit( &P );
// 	  j = 32 + (int)monrandom(72.);
// 	  P.X = (monrandom(2.)-1.)*mWafer[j].localXsize();
// 	  P.Y = (monrandom(2.)-1.)*mWafer[j].localYsize();
// 	  mWafer[j].addNewHit( &P );
// 	  j = 104 + (int)monrandom(112.);
// 	  P.X = (monrandom(2.)-1.)*mWafer[j].localXsize();
// 	  P.Y = (monrandom(2.)-1.)*mWafer[j].localYsize();
// 	  mWafer[j].addNewHit( &P );
// 	};
//       for (int i=0; i<(int) (numberOfTracks*1.39/100.); i++)//1.39
// 	{
// 	  j = 216 + (int)monrandom(320.);
// 	  P.X = (monrandom(2.)-1.)*mWafer[j].localXsize();
// 	  P.Y = (monrandom(2.)-1.)*mWafer[j].localYsize();
// 	  mWafer[j].addNewHit( &P );
// 	};
//     };
  recordEventHits();
  return 1;
};


//-------------------------------------------------------------------------
//-------------------------------------------------------------------------
// 'cosmic alignment' :


void StSsdAlign::simulCosmics(int ev, int numberOfTracks ,int level)
{ 
  gMessMgr->Info()<<"SVT & SSD cosmic alignment : event "<<ev<<" simulation :"<< endm;
  gMessMgr->Info()<<numberOfTracks<<" generated cosmic rays."<< endm;

  int ntracks=0, nTrackHit;
  CreateEvent(ev);

  do {
    globalPoint p1;
    p1.x = monrandom(80) - 40;
    p1.y = monrandom(80) - 40;
    p1.z = monrandom(100) - 50;
    
    double teta, n, phi;
    phi = monrandom(2.*PI);
    do { 
	    n = monrandom(1.);
	    teta = monrandom(PI/2);
    } while (cos(teta)*cos(teta)<n); // theoritical zenith angle distribution is (cos teta)^2
    globalPoint p2;
    p2.x = sin(teta)*cos(phi) + p1.x;
    p2.y = cos(teta) + p1.y;
    p2.z = sin(teta)*sin(phi) + p1.z;
      
      localPoint P;
      localPoint lP[16];
      int ilP[16];
      
      nTrackHit = 0;
      for (int i=0; i<536; i++)
	{
	  if ( (mWafer[i].isTouched( &p1, &p2, &P ))&&(hitProba(99.)) )
	    {		
	      if (level>=1) // detectors position resolution :
		if (i>215)
		  {
		    P.X+=0.002*rgg();
		    P.Y+=0.08*rgg();
		  } else
		    {
		      P.X+=0.002*rgg();
		      P.Y+=0.002*rgg();
		    };
	      lP[nTrackHit] = P;
	      ilP[nTrackHit] = i;		
	      nTrackHit++;
	    };
	  if ( (mWafer[i].isTouched( &p2, &p1, &P ))&&(hitProba(99.)) )
	    {		
	      if (level>=1) // detectors position resolution :
		if (i>215)
		  {
		    P.X+=0.002*rgg();
		    P.Y+=0.08*rgg();
		  } else
		    {
		      P.X+=0.002*rgg();
		      P.Y+=0.002*rgg();
		    };
	      lP[nTrackHit] = P;
	      ilP[nTrackHit] = i;
	      nTrackHit++;
	    }; 
	};
      globalPoint gP[16];
      for(int i=0; i<nTrackHit; i++)     
	mWafer[ilP[i]].local2Global(&lP[i],&gP[i]);
      
      if( nTrackHit > 0)
	FillTrack(ev, ntracks, nTrackHit, gP, ilP);
      
      if ( (nTrackHit<7)&&(nTrackHit>2) )ntracks++;
      
  } while (ntracks < numberOfTracks);

  recordEventHits();
};


//_____________________________________________________________
void StSsdAlign::cosmicAlign( int iMax )
{ 
  gMessMgr->Info()<<"SVT & SSD cosmic alignment (no primary vertex constraint)."<< endm; 
  gMessMgr->Info()<<"Chi2 decreasing procedure ..."<< endm; 
  
  double step =	1.; 
  double chi0 = totalCosmicChi2();
  double derm;
  int i	= 0;
  
  while((step >	0.1) &&	(i < iMax))
    {
      i++;
      gMessMgr->Info()<<"  step = "<< i << ", chi2 = "<< chi0 << endm;
      derm = takeCosmicDerivatives(step);
      shiftParams( -step/derm);
      for (int ev=0; ev<mNumberOfEvents; ev++) updateGlobalPoints(ev);
      double chin = totalCosmicChi2();

      if(chin < chi0)
 	{
 	  chi0 = chin;
 	}
      else
 	{
	  shiftParams(0.5*step/derm);
	  for (int ev=0; ev<mNumberOfEvents; ev++) updateGlobalPoints(ev);
	  
	  chin = totalCosmicChi2();
	  if(chin < chi0)
	    {
	      chi0 = chin;
	    }
	  else
	    {
	      shiftParams(0.5*step/derm);
	      for (int ev=0; ev<mNumberOfEvents; ev++) updateGlobalPoints(ev);
	      chi0 = totalCosmicChi2();
	      step /= 2.;
	    }
	}
    }
}

void StSsdAlign::cosmicAlign2( int iMax )
{  
  gMessMgr->Info()<<"SVT & SSD cosmic alignment (no primary vertex constraint)."<< endm;
  int nTotTracks = 0;
  for (int ev=0; ev<mNumberOfEvents; ev++)
    nTotTracks+=mEvents[ev]->numberOfTracks();
  cout <<nTotTracks<<" tracks"<< endl;
  //printf("   %d tracks\n", nTotTracks);

  double step =	1.; 
  double chi0 = totalCosmicChi2();
  double derm;
 
  for (int i=0; i<iMax; i++)
    { 
      if ( 9./(double)i < step ) step = 9./(double)i;
      
      derm = takeCosmicDerivatives(step);
      shiftParams( -step/derm);
      for (int ev=0; ev<mNumberOfEvents; ev++) updateGlobalPoints(ev);      
      chi0 = totalCosmicChi2();
      //cout <<"  i = "<<i<<", alignment constant = "<<step<<",     chi2 = "<< chi0 <<endl; 
      printf("  i = %d, alignment constant = %f, chi2 = %f\n",i,step,chi0);
    }
}


double StSsdAlign::totalCosmicChi2()
{
  double val=0.;
  for (int ev=0; ev<mNumberOfEvents; ev++)
    val += mEvents[ev]->processCosmics();
 
 return ( val/mNumberOfEvents );
};


double StSsdAlign::takeCosmicDerivatives( double step)
{
  double shifts[6]={0.0001*step, 0.0001*step, 0.0001*step, 0.0005*step, 0.0005*step, 0.0005*step};

  double derivativeMax = 0.;
  globalPoint p;

  for (int iwafer=0; iwafer<536; iwafer++ )
    if ( (mWafer[iwafer].isOn()) && (mWafer[iwafer].numberOfHits()) )
      for (int iparam=0; iparam<6; iparam++)
	{
	  mDerivatives[iwafer][iparam] = 0.;
	  mWafer[iwafer].shiftParam(iparam, shifts[iparam] );
	  
	  for (int iev=0; iev < mNumberOfEvents; iev++ )
	    for (int itrack=0; itrack<mEvents[iev]->numberOfTracks(); itrack++ )
	      if ( mEvents[iev]->isTrackSelected(itrack) )
		{
		  int hitNumber = mEvents[iev]->trackHitNumber(itrack,iwafer);
		  if (hitNumber!=-1)
		    {
		      mWafer[iwafer].speedHit( iev, mEvents[iev]->pointID( itrack, hitNumber), &p );
		      mEvents[iev]->setCosmicChi2( 2, itrack, hitNumber, p);
		    };
		};
	  
	  mWafer[iwafer].shiftParam(iparam, -2.*shifts[iparam] );

	  int compt=0;
	  for (int iev=0; iev < mNumberOfEvents; iev++ )
	    for (int itrack=0; itrack<mEvents[iev]->numberOfTracks(); itrack++ )
	      if ( mEvents[iev]->isTrackSelected(itrack) )
		{
		  int hitNumber = mEvents[iev]->trackHitNumber(itrack,iwafer);
		  if (hitNumber!=-1)
		    {		    
		      mWafer[iwafer].speedHit( iev, mEvents[iev]->pointID( itrack, hitNumber), &p );
		      mEvents[iev]->setCosmicChi2( 1, itrack, hitNumber, p);
		      mDerivatives[iwafer][iparam] += mEvents[iev]->chi2DiffPerHit(itrack);
		      compt++;
		    };
		};
	  if (compt) mDerivatives[iwafer][iparam] /=( compt*2.*shifts[iparam] );
	  //printf ("nb traces ds wafer %d =%d\n",iwafer,compt); 

	  if ( derivativeMax < fabs( mDerivatives[iwafer][iparam] ) )
	    derivativeMax = fabs(mDerivatives[iwafer][iparam]);
	  
	  mWafer[iwafer].shiftParam(iparam, shifts[iparam] );
	};

  return derivativeMax;
};


void StSsdAlign::FillTrack(int ev, int TrackNumber, int nTrackHit,
			   globalPoint* gP, int *ilP)
{
  // ilP Records which wafer hit is on
  //StSvtLocalCoordinate Local;
  //StGlobalCoordinate Global;
  localPoint lP;
    
  if ( (nTrackHit<17)&&(nTrackHit>2) ) 
    {
      track goodTrack;
      
      for (int i = 0; i<nTrackHit; i++)
  	{
	  mWafer[ ilP[i] ].global2Local( &gP[i],&lP); 
	  mWafer[ ilP[i] ].addNewHit( &lP );
  	  nbofhits[ ilP[i] ]++;
	  goodTrack.p[i] = gP[i];
	  goodTrack.waferID[i] = ilP[i] ;
	  goodTrack.pointID[i] = nbofhits[ ilP[i] ] - 1;
	 
	};
      goodTrack.numberOfHits = nTrackHit;
      mEvents[ev]->addTrack( goodTrack );
      
    };
}

//_________________________________________________________________
void StSsdAlign::CreateEvent(int ev){
  mEvents[ev] = new StSsdEvent;
  for(int i=0; i<536; i++) nbofhits[i]=0;
}


void StSsdAlign::tetaDistri(int &nval, double *teta)
{
  int compt = 0, itrack = 0;
  int ev = 0;

  while ((compt<nval)&&(ev<mNumberOfEvents))
    {
      if (mEvents[ev]->numberOfTracks() == itrack)
	{ ev++; itrack=0; }
      else
	{
	  teta[compt] =
	    atan( ::sqrt( mEvents[ev]->getTrack(itrack)->b.x*mEvents[ev]->getTrack(itrack)->b.x +
			mEvents[ev]->getTrack(itrack)->b.z*mEvents[ev]->getTrack(itrack)->b.z )/
		  fabs(mEvents[ev]->getTrack(itrack)->b.y) );
	  itrack++;
	  compt++;
	};
    };
  nval = compt-1;
};


void StSsdAlign::nHitsPerTrackDistri(int &nval, int *number)
{
  int compt = 0, itrack = 0;
  int ev = 0;

  while ((compt<nval)&&(ev<mNumberOfEvents))
    {
      if (mEvents[ev]->numberOfTracks() == itrack)
	{ ev++; itrack=0; }
      else
	{
	  number[compt] = mEvents[ev]->getTrack(itrack)->numberOfHits;
	  itrack++;
	  compt++;
	};
    };
  nval = compt-1;
};


void StSsdAlign::chi2Distri(int &nval, double *chi2)
{
  int compt = 0, itrack = 0;
  int ev = 0;

  for (int i = 0; i<mNumberOfEvents; i++)
    mEvents[ev]->processCosmics();

  while ((compt<nval)&&(ev<mNumberOfEvents))
    {
      if (mEvents[ev]->numberOfTracks() == itrack)
	{ ev++; itrack=0; }
      else
	{
	  chi2[compt] = mEvents[ev]->getTrack(itrack)->chi2_1;
	  // dans processCosmics, remplacer val par mTracks[i]->chi2_1
	  // et voir si ca marche bien
	  itrack++;
	  compt++;
	};
    };
  nval = compt-1;
};


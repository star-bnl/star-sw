//*-- Author : Jan Balewski
//  
// JB 3/30/01 - divorce with MC. Only StEvent is used. No evaluation
//
// $Id: StppLPfindMaker.cxx,v 1.8 2001/11/28 23:03:41 balewski Exp $
// $Log: StppLPfindMaker.cxx,v $
// Revision 1.8  2001/11/28 23:03:41  balewski
// ppLMV uses only tracks matched to CTB slats, runs with DAQ & MC data
//
// Revision 1.7  2001/06/07 17:02:52  balewski
// *** empty log message ***
//
// Revision 1.6  2001/05/03 23:38:10  balewski
// *** empty log message ***
//
// Revision 1.5  2001/04/27 20:50:45  balewski
// *** empty log message ***
//
// Revision 1.4  2001/04/26 20:04:52  balewski
// *** empty log message ***
//
// Revision 1.3  2001/04/23 19:44:26  balewski
// *** empty log message ***
//
// Revision 1.2  2001/04/23 15:02:10  balewski
// *** empty log message ***
//
// Revision 1.6  2001/04/19 21:30:36  balewski
// add I/O to ppDst
//
// Revision 1.5  2001/04/13 20:15:13  balewski
// *** empty log message ***
//
// Revision 1.4  2001/04/12 21:05:46  balewski
// *** empty log message ***
//
// Revision 1.3  2001/04/12 15:19:09  balewski
// *** empty log message ***
//
// Revision 1.2  2001/02/28 19:06:12  balewski
// some reorganizations
//
// Revision 1.1.1.1  2001/01/31 14:00:07  balewski
// First release
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
//   Search for the leading charge particle in the event                //
//   use DST as output
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <strings.h>
#include <math.h>

#include "StppLPfindMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StEventTypes.h"
#include "TH2.h"

#include "StppDst.h"  

#include "StTreeMaker/StTreeMaker.h"


#include "tables/St_dst_track_Table.h"
#include "tables/St_tpt_track_Table.h" 
#include "tables/St_tcl_tphit_Table.h" //tmp for CL vs. nPrim

#include "tables/St_g2t_vertex_Table.h" // tmp for Dz(vertex)


// for Helix model
#include "StarCallf77.h"
extern "C" {void type_of_call F77_NAME(gufld,GUFLD)(float *x, float *b);}
#define gufld F77_NAME(gufld,GUFLD)
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif


ClassImp(StppLPfindMaker)
//void StppLPfindMaker::Streamer(TBuffer &b){};  // do NOT change it J.B.

//_____________________________________________________________________________
StppLPfindMaker::StppLPfindMaker(const char *name):StMaker(name)
{
  //cout <<" Cccccccccccccccccccccccccccccccccccc construct::"<<GetName() <<endl;
}
//_____________________________________________________________________________
StppLPfindMaker::~StppLPfindMaker()
{
}

//_____________________________________________________________________________
Int_t StppLPfindMaker::Init()
{
  cout <<" Iiiiiiiiiiiiiiiiiiiiiiiiiiiii init ::"<<GetName() <<endl;
  init_histo();
  nEVtot=nEVfound=0;
  // setup params
  EtaCut=1.4; // tracks with larger eta are not considered

  //StTreeMaker *treeMk = (StTreeMaker *) GetMaker("outputStream"); 
  //treeMk->IntoBranch("ppDstBranch","dst");//<< ppDst.root file will be produced
  //printf("JB:  %s-maker ppDstBranch created\n",GetName());

  return StMaker::Init();
}

//_____________________________________________________________________________
Int_t StppLPfindMaker::Finish()
{
  cout <<" Finish fffffffffffffffff ::"<<GetName() <<endl;
  printf(" nEve tot=%d \n", nEVtot);
  //printStat();
  return  kStOK;
}


//_____________________________________________________________________________
Int_t StppLPfindMaker::Make()
{
  int i;
  nEVtot++;
  primV=NULL;
  stEvent=NULL;
  hv[6]->Fill(0);
  cout <<" Mmmmmmmmmmmmmmmmmmmmmm   start maker ::"<<GetName() <<" mode="<<m_Mode<<endl;
  stEvent= (StEvent *) GetInputDS("StEvent");  assert(stEvent);
  int EventId=stEvent->id();

  printf("  EventID=%d \n",EventId);

  //   G E T   D A T A
  St_DataSet    *ds1=GetDataSet("tpc_tracks"); assert(ds1);
  St_tpt_track  *tptr=(St_tpt_track   *) ds1->Find("tptrack");

  if(tptr==NULL)printf(" NULL pointer to St_tpt_track table\n"); 

  //  G E T     P R I M     V E R T E X 
  primV=stEvent->primaryVertex();
  if(!primV)
    printf("primaryVertex()=NULL\n");
  else {
    printf("primaryVertex()= %f, %f %f, nTracks=%d\n",primV->position().x(),primV->position().y(),primV->position().z(),primV->numberOfDaughters());  
  }

  //  G E T     P R I M     T R A C K S 
  St_DataSet    *dst=GetInputDS("dst"); assert(dst); 
  St_DataSetIter dstIter(dst);
  St_dst_track     *dstPrimTr=(St_dst_track *) dstIter("primtrk");
  if(dstPrimTr==NULL) printf(" NULL pointer to primtrk St_Table \n");
  //if(dstPrimTr)dstPrimTr->Print(0,10);

 //   G E T   TCL HITS 
 St_DataSet *ds=GetDataSet("tpc_hits"); assert(ds);
 St_tcl_tphit  *tpcl=(St_tcl_tphit  *) ds->Find( "tphit");
 if(tpcl==0) printf("NULL pointer to St_tcl_tphit table\n");
 int nCL=tpcl->GetNRows();
 
 // tmp for test only
 {
   int i1;
   tcl_tphit_st *TCL=tpcl->GetTable();
   for(i1=0; i1<nCL; i1++, TCL++) {
     float phi=atan2(TCL->y, TCL->x) /3.1416*180.;
     if (phi<0) phi=phi+360.;
     hv[10]->Fill(phi);
     ((TH2F *)hv[11])->Fill(phi,TCL->z);
   }
 }
 // test end
 cout <<" Mmmmmmmmmmmmmmmmmmmmmm   all tables OK ::"<<GetName() <<endl;
 
  //------------------------------------------------
  //   A C T I O N :    find reconstructed Leading Particle
  //------------------------------------------------
 
  hv[6]->Fill(1);

  if(!primV) return kStOK; // no primary vertex
  hv[6]->Fill(2);
  if(!dstPrimTr) return kStOK; // no primary tracks
  hv[6]->Fill(3);

  // search for reconstructed LP  ......................
  dst_track_st *DSTT=dstPrimTr->GetTable(); 
  dst_track_st *rLP=NULL;
  for(i=1;i<dstPrimTr->GetNRows();i++,DSTT++) {
    if(DSTT->iflag<=0) continue; // not a valid track
    float reta=asinh(DSTT->tanl);// rec eta
    //printf("rID=%d rPt=%f, reta=%f \n",DSTT->id,1./DSTT->invpt,reta);
    if(fabs(reta)>EtaCut) continue; // those tracks were also not considered for M-C
    hv[8]->Fill(DSTT->psi);
    hv[9]->Fill(DSTT->psi,reta);
    if(rLP==NULL) { rLP=DSTT; continue;}
    if(rLP->invpt<DSTT->invpt) continue;
    rLP=DSTT;
    }
  if(!rLP) {
    printf("No rLP found among %d rPrim tracks\n",(int)dstPrimTr->GetNRows());
    return kStOK; 
  }
  
  hv[6]->Fill(4);

  float  rPt=1./rLP->invpt;  
  printf(" rLP pT=%f , nPts=%d found among  %d rPrim tracks\n",rPt,(int)rLP->n_point,(int)dstPrimTr->GetNRows());


  //  if(rPt<1. )   return kStOK;  // disqualify events with too low rLP PT

  // find this primary rLP among TPT-tracks
  // count all valid TPT tracks for histo only
  int nTPTtr=0;
  tpt_track_st *rTPT=NULL, *TPTR=tptr->GetTable();
  for(i=0;i<tptr->GetNRows(); i++,TPTR++) {
    if(TPTR->flag>0) nTPTtr++; 
    if(TPTR->id_globtrk != rLP->id) continue;
    printf(" FOUND ID: prim=%d, tpt=%d, tpt chi2[0]=%f/f, [1]=%f/f, pT=%f \n",(int)rLP->id,(int)TPTR->id,TPTR->chisq[0]/(TPTR->nrec-2),TPTR->chisq[1]/(TPTR->nrec-2),1./TPTR->invp);
    rTPT=TPTR;
    break;
  } 
  assert(rTPT);
  float delZ, lpRxy, delRxy;
  DcaTract2Vert(rTPT,delZ, lpRxy, delRxy);

  nEVfound++;
  
  printf("nEve tot=%d, nFound=%d delRxy=%f\n", nEVtot, nEVfound, delRxy);
  //if(nEVtot%20==0) printStat();
  
  // =========================================================
  printf(" ppMiniDst   U p d a t e     . . .\n");
  // =========================================================

 ppDst_t row;
 printf("new ppMiniDst row\n");

  row.vertX=primV->position().x();
  row.vertY=primV->position().y();
  row.vertZ=primV->position().z();
  row.nPrim=primV->numberOfDaughters();

  row.pt=rPt;
  row.eta=asinh(rLP->tanl);
  row.psi=rLP->psi;
  row.nTclHit=rTPT->nrec;
  row.chi2f=TPTR->chisq[0]/(TPTR->nrec-2);
  row.PrimId=rLP->id; // for evaluation only

  row.Dz=delZ; // LP - vert
  row.DRxy=delRxy;// LP - vert
  row.Rxy=lpRxy;// LP - vert

  //tmp2 -search for phi-error
  {
    float reta=asinh(rLP->tanl);// rec eta
    hv[12]->Fill(rLP->psi);
    hv[13]->Fill(rLP->psi,reta);
  }

  
  if(0){ // for M-C
    //    G E A N T
    St_DataSet *gds=GetDataSet("geant"); assert(gds);
    St_g2t_vertex  *gver=(St_g2t_vertex  *) gds->Find("g2t_vertex");
    g2t_vertex_st *GVER=gver->GetTable(); 
    row.Dz=GVER->ge_x[2]-primV->position().z();
    printf("WARN, ppDST.DZ=%f is temporary changed !!!\n",row.Dz);
  }



  ppDst *my=new ppDst("rec_lp",1); // name of the table in ppDst
  my->AddAt(&row);

  St_DataSet *dst1 = GetDataSet("dst");
  assert(dst1);
  dst1->Add(my);


  // =========================================================
  //printf(" ppTag   U p d a t e     . . . NOT implemented\n");
  // =========================================================

  //-----------------------------
  //    H I S T O
  //-----------------------------
  hv[0]->Fill(row.vertZ);
  hv[1]->Fill(row.Dz);
  hv[2]->Fill(row.DRxy);
  hv[3]->Fill(row.nPrim);
  hv[4]->Fill(row.pt);
  hv[5]->Fill(nTPTtr);
  
  
 ((TH2F *)hv[7])->Fill(row.nPrim,nCL/1000.);
 
 int i1;
 tcl_tphit_st *TCL=tpcl->GetTable();
 for(i1=0; i1<nCL; i1++, TCL++) {
   float phi=atan2(TCL->y, TCL->x) /3.1416*180.;
   if (phi<0) phi=phi+360.;
   hv[10]->Fill(phi);
   ((TH2F *)hv[11])->Fill(phi,TCL->z);
 }
return kStOK;
}


//_____________________________________________________________________________
void  StppLPfindMaker::DcaTract2Vert(tpt_track_st *TPT,float &delZ, float &lpRxy, float &delRxy)
{
  delRxy=-1;
  
  // get DCA of TPT-track vs, rVertex
  double spath,h;
  double x0,y0,z0;
  double ptinv,psi,tanl;
  double px,py,pz;
  
  // Get BField from gufld(,) 
  //  cout<<"Trying to Get the BField the old way..."<<endl;
  float x[3] = {0,0,0};
  float b[3];
  gufld(x,b);
  double bfield = 0.1*b[2]; //This is now Tesla.
  
  // First point on Helix
  x0 = TPT->r0*cos(TPT->phi0* degree);
  y0 = TPT->r0*sin(TPT->phi0* degree);
  z0 = TPT->z0;
  
  StThreeVectorD origin(x0*centimeter, y0*centimeter, z0*centimeter);
  
  // Helicity / Sense of Curvatutre
  h  = 1.0;  if( bfield*TPT->q > 0.0 )h=-1.0;
  double qtrk = 1.0; if( h*bfield > 0.0)qtrk=-1.0;
  
  // Track direction at first point
  ptinv  = TPT->invp;
  tanl   = TPT->tanl;
  psi    = TPT->psi* degree; 
  if(psi<0.0){psi=psi+2.*M_PI;}
  
  px   = (1./ptinv)*cos(psi);
  py   = (1./ptinv)*sin(psi);
  pz   = (1./ptinv)*tanl;
  StThreeVectorD MomFstPt(px*GeV, py*GeV, pz*GeV);
  StPhysicalHelixD TrkHlx(MomFstPt, origin, bfield*tesla, qtrk);
  
  double xorigin = primV->position().x();
  double yorigin = primV->position().y();
  spath = TrkHlx.pathLength(xorigin, yorigin);
  StThreeVectorD XMinVec = TrkHlx.at(spath);
  cout<<"XYZ of rLP_TPT at DCA to vertex= "<<XMinVec<<endl;
  delZ=primV->position().z()-XMinVec.z();
  float dx=primV->position().x()-XMinVec.x();
  float dy=primV->position().y()-XMinVec.y();
  lpRxy=sqrt(XMinVec.x()*XMinVec.x() +XMinVec.y()*XMinVec.y() );
  delRxy=sqrt(dx*dx+dy*dy);
  printf("zVert - ZrLP_TPT =%f cm, delRxy/cm=%f, lpRxy/cm=%f\n",delZ,delRxy,lpRxy);
  
}


//------------  N O T   U S E D   -------------------------------


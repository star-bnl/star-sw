//*-- Author : Jan Balewski
//  
// $Id: StppLPevalMaker.cxx,v 1.1.1.1 2001/04/21 00:43:13 fisyak Exp $
// $Log: StppLPevalMaker.cxx,v $
// Revision 1.1.1.1  2001/04/21 00:43:13  fisyak
// *** empty log message ***
//
// Revision 1.2  2001/04/19 21:30:36  balewski
// add I/O to ppDst
//
// Revision 1.1  2001/04/12 15:19:08  balewski
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
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include <strings.h>
#include <math.h>

#include "StppLPevalMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "StEventTypes.h"
#include "TH2.h"

#include "tables/St_jdata1_Table.h"
#include "StppDst.h"  

#include "tables/St_dst_track_Table.h"
#include "tables/St_tcl_tphit_Table.h" 

#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_tpc_hit_Table.h"

#include "tables/St_g2t_ctf_hit_Table.h" 
#include "tables/St_g2t_emc_hit_Table.h" 

#include "tpc/St_tpt_residuals_Module.h"

//tmp
#include "tables/St_tpt_track_Table.h" 
#include "tables/St_g2t_vertex_Table.h"

ClassImp(StppLPevalMaker)

//_____________________________________________________________________________
StppLPevalMaker::StppLPevalMaker(const char *name):StMaker(name)
{
  cout <<" Cccccccccccccccccccccccccccccccccccc construct::"<<GetName() <<endl;
}
//_____________________________________________________________________________
StppLPevalMaker::~StppLPevalMaker()
{
}

//_____________________________________________________________________________
Int_t StppLPevalMaker::Init()
{
  cout <<" Iiiiiiiiiiiiiiiiiiiiiiiiiiiii init ::"<<GetName() <<endl;
  Clear(" ");
  init_histo();
  nEVtot=nTrigOK=nAcc=0;

  // setup params
  EtaCut=1.4; // tracks with larger eta are not considered
  //MinTclPts=999; // Not USED

  g2tTpcNhitCut=20; // tracks with lower # of hits are considered to be not reconstructable in the TPC
  h2hMatchRmin=1.;//(cm) maximal allowed distance
  t2tMatchEff=0.51; // ration of (used hits matched to GLP) to (all matched u-hits)  


  return StMaker::Init();
}

//_____________________________________________________________________________
void StppLPevalMaker::Clear(const char *opt) 
{
  stEvent=NULL; // clear old pointer
  cout <<" clear rrrrrrrrrrrrrrrr  ::"<<GetName() <<endl;
  return;
}

//_____________________________________________________________________________
Int_t StppLPevalMaker::Finish()
{
  cout <<" Finish fffffffffffffffff ::"<<GetName() <<endl;
  printf(" nEve tot=%d, nTrigOK=%d, nAcc=%d \n", nEVtot, nTrigOK, nAcc);
  printf(" %d input events total in this JOB-JB \n",nEVtot);
  printStat();
  return  kStOK;
}


//_____________________________________________________________________________
Int_t StppLPevalMaker::Make()
{
  int ilp,i;
  nEVtot++;
  LeadPartAnal *lpa=&lpaS;
  cout <<" Mmmmmmmmmmmmmmmmmmmmmm   start maker ::"<<GetName() <<" mode="<<m_Mode<<endl;
  stEvent= (StEvent *) GetInputDS("StEvent");  assert(stEvent);
  int EventId=stEvent->id();
  if(EventId%20==0) printStat();

  printf("  EventID=%d\n",EventId);

  //------------------------------------------
  //   S T E P    O N E : 
  //------------------------------------------
  // =========================================================
  printf(" ppMiniDst   Access     . . .\n");
  // =========================================================

  St_DataSet* ds1=GetDataSet("dst/rec_lp");  //assert(ds1); 
  if(ds1==NULL) {
    printf("%s-maker, no dst/rec_lp, ignore eve\n",GetName());
    return kStOk;
  }

  ppDst *myR=(ppDst *)ds1->Find("rec_lp"); //assert(myR);
  if(myR==NULL) {
    printf("%s-maker, no rec_lp, ignore eve\n",GetName());
    return kStOk;
  }

  assert(myR->GetNRows()==1);
  ppDst_t *my=(ppDst_t *)myR->GetArray();

  printf("ppDst found: rec pT=%f \n",my->pt);
  if(my->pt<0) return kStOK; //not valid event
 
  float rPt=my->pt;
  lpa->Rec.n++;
  lpa->Rec.h->Fill(rPt);

  //------------------------------------------
  //   S T E P    T W O :     get 1st  generated LP
  //------------------------------------------

  //    G E A N T
  St_DataSet *gds=GetDataSet("geant"); assert(gds);
  St_g2t_track  *gtra=(St_g2t_track  *) gds->Find("g2t_track");
  if(gtra==NULL) {
    printf(" NULL pointer to St_g2t_track table\n");
    return kStOk;
  }

  g2t_track_st *gLP=NULL;
  getGeneratedLP(gtra,gLP);
  g2t_track_st dum_g2t_track;
  if(gLP==NULL) { // assume a pT of 100KeV/c for pathological events
    gLP=&dum_g2t_track;
    dum_g2t_track.pt=0.1;
  }

  float gPt=gLP->pt;

  //------------------------------------------------
  //   S T E P   T H R E E :   evaluate rLP vs. gLP
  //------------------------------------------------
  float DelPsiMax=45.; // (deg) for evaluation
  float DelPtMax=0.5; // (GeV/c) for gPT<6GeV/c, -->1.GeV/c for larger gLP_PT

  // check delta phi
  float gPsi=atan2(gLP->p[1],gLP->p[0])*180/3.1416; // ==atan2(y,x) in deg
  if(gPsi<0) gPsi=360+gPsi;
  float delPsi=fabs(my->psi - gPsi);
  if(delPsi>180) delPsi=360-delPsi; // need only opening angle

  printf(" check PT g=%f, r=%f, g-r=%f \n",gPt, rPt,gPt-rPt); 
  float errMax= DelPtMax;
  if(gPt>6.) errMax= 0.5 + 0.125*(gPt-6.); // accounts for the TPC resolution
  if( fabs(rPt-gPt)>errMax)  return kStOk;  //<<============ cut
  lpa->dPT.n++;
  lpa->dPT.h->Fill(rPt);

  printf("check  psi g=%f, r=%f ,del=%f\n",gPsi,my->psi,  delPsi);
  if(delPsi>DelPsiMax) return kStOk;  //<<============ cut

  lpa->dPsi.n++;
  lpa->dPsi.h->Fill(rPt);

  return kStOk;
}

//------------  N O T   U S E D   -------------------------------
#if 0 // old stuff

  St_g2t_tpc_hit  *gtph=(St_g2t_tpc_hit  *) gds->Find("g2t_tpc_hit");
  if(gtph==NULL)printf("NULL pointer to St_g2t_tpc_hit table\n");

 St_g2t_vertex  *gver=(St_g2t_vertex  *) gds->Find("g2t_vertex");
 if(gver==NULL){printf(" NULL pointer to St_g2t_vertext table\n");assert(gver);}
  if(gver->GetNRows()<=0){printf(" empty St_g2t_vertext table\n"); assert(0);}
  g2t_vertex_st *GVER=gver->GetTable(); assert(GVER);
  float z_true=GVER->ge_x[2];
  //float rxy_true=sqrt(GVER->ge_x[0]*GVER->ge_x[0] + GVER->ge_x[1]*GVER->ge_x[1]);
  printf("Geant vertex at   %f %f %f\n",GVER->ge_x[0],GVER->ge_x[1],GVER->ge_x[2]);  

  //   G E T   D A T A
  St_DataSet    *ds1=GetDataSet("tpc_tracks"); assert(ds1);
  St_tpt_track  *tptr=(St_tpt_track   *) ds1->Find("tptrack");

  if(tptr==NULL)printf(" NULL pointer to St_tpt_track table\n"); 
  assert(tptr);

  St_DataSet *ds2=GetDataSet("tpc_hits"); assert(ds2);
  St_tcl_tphit  *tpcl=(St_tcl_tphit  *) ds2->Find( "tphit");
 if(tpcl==0) printf("NULL pointer to St_tcl_tphit table\n");
 assert(tpcl);

  St_DataSet    *dst=GetInputDS("dst"); assert(dst);
  St_DataSetIter dstIter(dst);
  St_dst_track     *dstPrimTr=(St_dst_track *) dstIter("primtrk");
  if(dstPrimTr==NULL) printf(" NULL pointer to primtrk St_Table \n");
  assert(dstPrimTr);

  cout <<" Mmmmmmmmmmmmmmmmmmmmmm   all tables OK ::"<<GetName() <<endl;

  float DelPsiMax=45.; // (deg) for evaluation
  float DelPtMax=0.5; // (GeV/c) for gPT<6GeV/c, -->1.GeV/c for larger gLP_PT
  float gPsi, delPsi, rPt=-1;
  int good=0; // 1=acceped&bad, 2=accepted&good
  int match=0; // LP: Gen 2 Rec

  lpa->Inp.n++;
  lpa->Inp.h->Fill(gPt);

  hv[4]->Fill(gPt); // all Pythia events
 
  //------------------------------------------------
  //   S T E P   T W O :   access rLP from DST
  //------------------------------------------------
  
  good=1;
  rPt=my->rLP.pt;
  hv[0]->Fill(z_true);  
  {
    float dz=z_true-my->rLP.vert.z;
    hv[1]->Fill(dz);  
    hv[2]->Fill(dz);  
  }
 endGood:
  
  printf("LARGE_rPT =%f  gPT=%f  r-g=%f, good=%d eve=%d \n",rPt,gPt,rPt-gPt,good,nEVtot);
  

  // D E T A I L E D   E V A L U A T I O N
  assert(good);  // may not be zero !
  match=0;
  
  //  M A T C H    G E N   L P   T O   R E C   L P
  printf("gLP->n_tpc_hit =%d\n",(int)gLP->n_tpc_hit);
  // ......  No. of TPC hits
  if(gLP->n_tpc_hit<g2tTpcNhitCut) goto endMatch;
  match=1; //<<============ cut

  // ......  Is gLP among TPT tracks ?
  {
    St_jdata1 *gh=pickHit(gLP, gtph); AddData(gh);
    if(gh->GetNRows()!=gLP->n_tpc_hit) {
      printf("\007 WARN, No. of g2t hist differ my=%d n_tpc_hit=%d\n",(int)gh->GetNRows(),(int)gLP->n_tpc_hit);      assert(0);    }
    int nMatchOne=-1;
    St_jdata1 *uh=NULL;
    int rtr_id=-3;
    int nUsed=-1;
    float eff=-.05;
    
    //SUBSTITUTE TPC-TRACK-->DST
    St_dst_track tptTr("tpc2dst_tr", tptr->GetNRows());// create new storage
    copyTpcTr2Dst(tptr,&tptTr);
    
    uh=pickHit(&tptTr, tpcl,1); AddData(uh);
    rtr_id=match1Gt2Rt(gh,uh,&nMatchOne); // find matched hits
    printf("rtr_id=%d\n",rtr_id);
    if(rtr_id>0){// some hits were matched
      dst_track_st *DSTT=tptTr.GetTable(); 
      for(i=0;i<tptTr.GetNRows();i++,DSTT++) {// find how many hits was used
	if(DSTT->iflag<=0) continue; // not a valid track
	if(DSTT->id!=rtr_id) continue;
	nUsed=DSTT->n_point;
	break;
      }
      if(nUsed>0) eff=(float)nMatchOne/nUsed;
      printf("  eff=%f =%d/%d\n",eff,nMatchOne,nUsed);
    }

    if(eff<t2tMatchEff) goto endMatch; // t2t matching failed
    match=2; //<<============ cut

    // verify if the gLP  was reconstructed as   P R I M A R Y 
    uh=NULL;
    rtr_id=-13;
    nUsed=-11;
    eff=-.015;
    uh=pickHit(dstPrimTr, tpcl,2); AddData(uh);
    rtr_id=match1Gt2Rt(gh,uh,&nMatchOne); // find matched hits
    if(rtr_id>0){ // some track was matched
      dst_track_st *DSTT=dstPrimTr->GetTable(); 
      for(i=0;i<dstPrimTr->GetNRows();i++,DSTT++){//find how many hits was used
	if(DSTT->iflag<=0) continue; // not a valid track
	if(DSTT->id!=rtr_id) continue;
	nUsed=DSTT->n_point;
	break;
      }
      if(nUsed>0) eff=(float)nMatchOne/nUsed;
      printf("  eff2=%f =%d/%d\n",eff,nMatchOne,nUsed);
    }

    if(eff<t2tMatchEff) goto endMatch; //2nd t2t matching failed
    match=3; //<<============ cut

    if(my->rLP.PrimId!=rtr_id) goto endMatch;  // gLP .ne. rLP
    match=4; //<<============ cut
    printf("step: rLP=gLP   passed \n");

    if(good==1)printf("BAD =%f  gPT=%f  r-g=%f, good=%d eve=%d\n",rPt,gPt,rPt-gPt,good,nEVtot);
      
    } // end gLP in TPT or Prim track check
    
 endMatch:

  cout <<" Mmmmmmmmmmmmmmmmmmmmmm  end maker ::"<<GetName() <<endl;
  printf("this eve good=%d, match=%d\n",good,match);

  printf("%d nEve tot=%d, nTrigOK=%d, nAcc=%d \n",good, nEVtot, nTrigOK, nAcc);
  if(nEVtot%20==0) printStat();

  return kStOK;
#endif










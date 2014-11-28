// $Id: trsNtuple.C,v 1.2 1999/06/24 20:24:57 kathy Exp $
// $Log: trsNtuple.C,v $
// Revision 1.2  1999/06/24 20:24:57  kathy
// updated comment lines at top
//
//======================================================================
// owner:  Herb Ward
// what it does: see below
//======================================================================
//////////////////////////////////////////////////////////////////////////
// For creating and filling an ntuple for testing Trs.
// Herb Ward, April 10 1999.
// To use this macro:
// 1. Using an editor, modify your macro bfc.C:
//    a.  At the top of the file, near where "chain" is declared, 
//        declare "trsNtuple" by inserting this line:
//          class TNtuple;  TNtuple *trsNtuple=0;
//    b.  Before the event loop (after chain->Init())insert this line:
//           trsNtuple = MakeTrsNtuple();
//    c. In the event loop, immediately after chain->Make(),
//       insert this line
//           FillTrsNtuple(chain,trsNtuple);
// 2. Start root, load this file, run the macro from step 1, and then
//    draw histograms.
//           $ echo $DISPLAY
//           $ echo $STAR
//           $ rootPstar
//           root [0] .L trsNtuple.C
//           root [1] .x bfc.C     
//           root [2] trsNtuple->Draw("xgeant:xtphit","","lego");
//////////////////////////////////////////////////////////////////////////
class StChain;
TNtuple *MakeTrsNtuple() {
  TNtuple *trstest;
  trstest = new TNtuple("trs","trs",
    "xgeant:ygeant:zgeant:crossAngle:tanl:xtphit:ytphit:ztphit:q");
  return trstest;
}
void FillTrsNtuple(StChain *theChain,TNtuple *trstest) {

  int tphitIdx,indexIdx,geantIdx,indexIdx,geantNrow,tphitNrow,indexNrow;
  int geantKey,tphitKey;
  Float_t p0,p1,p2;

  St_DataSetIter *iter0=new St_DataSetIter(theChain); if(!iter0) return;

  St_g2t_tpc_hit *geantObj=
      (St_g2t_tpc_hit*)iter0(".make/geant/.data/g2t_tpc_hit");
  if(!geantObj) { printf("Error 96.\n"); return; }
  g2t_tpc_hit_st *geantTbl=(g2t_tpc_hit_st*) geantObj->GetTable();
  if(!geantTbl) { printf("Error 396.\n"); return; }
  geantNrow=geantObj->GetNRows();

  St_tcl_tpc_index *indexObj=iter0(".make/tpc_hits/.data/index");
  if(!indexObj) { printf("Error 66.\n"); return; }
  tcl_tpc_index_st *indexTbl=(tcl_tpc_index_st*) indexObj->GetTable();
  if(!indexTbl) { printf("Error 396.\n"); return; }
  indexNrow=indexObj->GetNRows();

  St_tcl_tphit *tphitObj=iter0(".make/tpc_hits/.data/tphit");
  if(!tphitObj) { printf("Error 76.\n"); return; }
  tcl_tphit_st *tphitTbl=(tcl_tphit_st*) tphitObj->GetTable();
  if(!tphitTbl) { printf("Error 396.\n"); return; }
  tphitNrow=tphitObj->GetNRows();

  for(indexIdx=0;indexIdx<indexNrow;indexIdx++) {

    geantKey=indexTbl[indexIdx].key1;
    for(geantIdx=geantNrow-1;geantIdx>=0;geantIdx--) {
      if(geantTbl[geantIdx].id==geantKey) break;
    }
    if(geantIdx<0) { printf("no match key1 (%d).\n",geantKey); continue; }

    tphitKey=indexTbl[indexIdx].key2;
    for(tphitIdx=tphitNrow-1;tphitIdx>=0;tphitIdx--) {
      if(tphitTbl[tphitIdx].id==tphitKey) break;
    }
    if(tphitIdx<0) { printf("no match key2 (%d).\n",tphitKey); continue; }

    p0=geantTbl[geantIdx].p[0];
    p1=geantTbl[geantIdx].p[1];
    p2=geantTbl[geantIdx].p[2];

    trstest->Fill(
    (Float_t)(geantTbl[geantIdx].x[0]),
    (Float_t)(geantTbl[geantIdx].x[1]),
    (Float_t)(geantTbl[geantIdx].x[2]),
    (Float_t)(atan2(p0,p1)),
    (Float_t)(atan(p2/sqrt(p0*p0+p1*p1))),
    (Float_t)(tphitTbl[tphitIdx].x),
    (Float_t)(tphitTbl[tphitIdx].y),
    (Float_t)(tphitTbl[tphitIdx].z),
    (Float_t)(tphitTbl[tphitIdx].q)
    );
  }
}

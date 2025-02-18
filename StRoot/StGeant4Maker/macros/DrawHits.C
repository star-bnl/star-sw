#ifndef __CLING__
#include <TH2F.h> 
#include <TCanvas.h>
#include <TTable.h>
#include   <g2t_emc_hit.h>
#include   <g2t_tpc_hit.h>
#include   <g2t_vertex.h>
#include <StBFChain.h>
#include <StChain.h>
#endif

TH2F* hYX = 0;
TH2F* vYX = 0;
TCanvas* hcanvas = 0;
TCanvas* vcanvas = 0;
void DrawHits() {

if ( 0==hYX ) hYX = new TH2F("hYX","Hits    ;x [cm]; y [cm]", 601,-300.5,300.6,601,-300.5,300.5 ); 
if ( 0==vYX ) vYX = new TH2F("vYX","Vertices;x [cm]; y [cm]", 601,-300.5,300.6,601,-300.5,300.5 ); 

hYX->Reset();
vYX->Reset(); 

TTable* vertices = (TTable*)chain->FindByName("g2t_vertex");
TTable* tpchits  = (TTable*)chain->FindByName("g2t_tpc_hit");
TTable* emchits  = (TTable*)chain->FindByName("g2t_emc_hit"); 

int nvtx=vertices->GetNRows();
int ntpc=tpchits->GetNRows();
int nemc=emchits->GetNRows();

for ( int i=0;i<nvtx;i++ ) {
   g2t_vertex_st* v = (g2t_vertex_st*)vertices->At(i);
   double x = v->ge_x[0];
   double y = v->ge_x[1];
   vYX->Fill(x,y); 
}

for ( int i=0;i<ntpc;i++ ) {
   g2t_tpc_hit_st* v = (g2t_tpc_hit_st*)tpchits->At(i);
   double x = v->x[0];
   double y = v->x[1];
   hYX->Fill(x,y); 
}

for ( int i=0;i<nemc;i++ ) {
   g2t_emc_hit_st* v = (g2t_emc_hit_st*)emchits->At(i);
   double x = v->x;
   double y = v->y;
   hYX->Fill(x,y); 
}

hcanvas=new TCanvas("hcanvas","TPC and BEMC hits",500,500); hYX->Draw("colz"); 
vcanvas=new TCanvas("vcanvas","Track vertices   ",500,500); vYX->Draw("colz"); 

};

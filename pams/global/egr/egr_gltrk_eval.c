/*:>--------------------------------------------------------------------
  
  GLOBAL TRACK EVALUATOR MODULE

  written by:    Ricardo C. Mastroleo, UT Austin
  ported to STAF by : Spiros Margetis, June 1996
  
-----------------------------------------------------------------------
**: ROUTINE:    egr_gltrk_eval_
**: DESCRIPTION: Global tracking evaluation module
**: ARGUMENTS:
**:       IN:
**:          g2t_track    - PLEASE FILL IN DESCRIPTION HERE
**:         g2t_vertex    - PLEASE FILL IN DESCRIPTION HERE
**:        g2t_svt_hit    - PLEASE FILL IN DESCRIPTION HERE
**:        g2t_tpc_hit    - PLEASE FILL IN DESCRIPTION HERE
**:         svt_ctrack    - PLEASE FILL IN DESCRIPTION HERE
**:             tpeval    - PLEASE FILL IN DESCRIPTION HERE
**:              glbvl    - PLEASE FILL IN DESCRIPTION HERE
**:    INOUT:
**:            svt_spt    - PLEASE FILL IN DESCRIPTION HERE
**:          svt_track    - PLEASE FILL IN DESCRIPTION HERE
**:            tptrack    - PLEASE FILL IN DESCRIPTION HERE
**:            globtrk    - PLEASE FILL IN DESCRIPTION HERE
**:      OUT:
**: RETURNS:    STAF Condition Value
**:<------------------------------------------------------------------ */

#include <stdio.h>
#include <math.h>

/* ----------------- TAS generated includes ------------- */
#include "egr_gltrk_eval.h"
/* ----------------------- Macros -----------------------*/

#define SQR(a)   ((a) * (a))
#define MAG(x,y) sqrt(SQR(x) + SQR(y))
#define QFAC     0.7
#define B        0.5     /*  Tesla  */


/* ----------------- Function  prototypes--------------- */

static int search_row_sv ( STK_TRACK_ST  *svt_track, 
                           int               nok,
                           int               gl_val );

static int search_row_tp ( TTE_EVAL_ST     *tpeval,
                           int               nok,
                           int               tp_val );

static int search_tp_val ( TPT_TRACK_ST    *tptrack,
                           int               nok,
                           int               gl_val );

static int find_row_mk_svt ( STK_CTRACK_ST   *svt_ctrack,
                             SCS_SPT_ST      *svt_sp,
                             int                 row_sv );

static int find_row_mk_tpc ( TABLE_HEAD_ST       *tptrack_h,
                             TPT_TRACK_ST      *tptrack,
                             TABLE_HEAD_ST       *tpeval_h,
                             TTE_EVAL_ST       *tpeval,
                             int                 tp_val,
                             int                 gl_val );

static void mom_diff ( G2T_TRACK_ST          *g2t_track,
                       G2T_SVT_HIT_ST      *g2t_svt_hit,
                       G2T_TPC_HIT_ST      *g2t_tpc_hit,
                       EGR_GLOBTRK_ST        *globtrk,
                       EGR_GLOBTRK_EVAL_ST   *glbvl,
                       int                   row_gl,
                       int                   row_mh_svt,
                       int                   row_mh_tpc,
                       int                   row_mk );

static void min_dist_vrt ( G2T_VERTEX_ST          *g2t_vertex,
                           EGR_GLOBTRK_ST        *globtrk,
                           EGR_GLOBTRK_EVAL_ST   *glbvl,
                           int                   row_gl,
                           int                   row_mv );

static void appr_min_dist ( float *xv,
                            float *xh,
                            float *p,
                            float *xh_min );

static void bad_track ( EGR_GLOBTRK_EVAL_ST   *glbvl,
                        int                   row_gl );


/* ----------------- Begin main function ----------------- */

long type_of_call egr_gltrk_eval_(
  TABLE_HEAD_ST      *g2t_track_h,      G2T_TRACK_ST        *g2t_track ,
  TABLE_HEAD_ST     *g2t_vertex_h,     G2T_VERTEX_ST       *g2t_vertex ,
  TABLE_HEAD_ST    *g2t_svt_hit_h,    G2T_SVT_HIT_ST      *g2t_svt_hit ,
  TABLE_HEAD_ST    *g2t_tpc_hit_h,    G2T_TPC_HIT_ST      *g2t_tpc_hit ,
  TABLE_HEAD_ST        *svt_spt_h,        SCS_SPT_ST          *svt_spt ,
  TABLE_HEAD_ST     *svt_ctrack_h,     STK_CTRACK_ST       *svt_ctrack ,
  TABLE_HEAD_ST      *svt_track_h,      STK_TRACK_ST        *svt_track ,
  TABLE_HEAD_ST        *tptrack_h,      TPT_TRACK_ST          *tptrack ,
  TABLE_HEAD_ST         *tpeval_h,       TTE_EVAL_ST           *tpeval ,
  TABLE_HEAD_ST        *globtrk_h,    EGR_GLOBTRK_ST          *globtrk ,
  TABLE_HEAD_ST          *glbvl_h, EGR_GLOBTRK_EVAL_ST            *glbvl )
{

 int row_gl , ok_line = 0;

 for(row_gl = 0; row_gl < globtrk_h->nok; row_gl++) {

  int gl_val ,tp_val , row_sv , row_mk_svt , row_mk_tpc , row_mh_svt , 
      row_mh_tpc , row_mv;

  gl_val = globtrk[row_gl].id;
  glbvl[row_gl].id = ++ok_line;

  row_sv = search_row_sv (svt_track , svt_track_h->nok , gl_val);
  row_mk_svt = find_row_mk_svt (svt_ctrack , svt_spt , row_sv);

  tp_val = search_tp_val (tptrack, tptrack_h->nok, gl_val);
  row_mk_tpc = find_row_mk_tpc (tptrack_h, tptrack, tpeval_h, tpeval, 
                                tp_val, gl_val);

  if(row_mk_svt <= 0 && row_mk_tpc <= 0) {     /*    no or bad SVT track;   */
       glbvl[row_gl].svt_seg = row_mk_svt;     /*    no or bad TPC track    */
       glbvl[row_gl].tpc_seg = row_mk_tpc;
       bad_track (glbvl, row_gl);
  }  
  else if(row_mk_svt >0 && row_mk_tpc == 0) {  /*   SVT track; no TPC track */
       glbvl[row_gl].svt_seg = 1;
       glbvl[row_gl].tpc_seg = 0;
       glbvl[row_gl].mc_pid = g2t_track[row_mk_svt - 1].ge_pid;
       row_mh_svt = svt_ctrack[row_sv - 1].spt[0];        /* 1st SVT hit */
       mom_diff (g2t_track, g2t_svt_hit, g2t_tpc_hit, globtrk, glbvl, 
                      row_gl, row_mh_svt, 0, row_mk_svt);
       row_mv = g2t_track[row_mk_svt - 1].start_vertex_p;
       min_dist_vrt (g2t_vertex, globtrk, glbvl, row_gl, row_mv);
  }
  else if(row_mk_svt >0 && row_mk_tpc == -1) {  /*  SVT track; bad TPC track */
       glbvl[row_gl].svt_seg = 1;
       glbvl[row_gl].tpc_seg = -1;
       bad_track (glbvl, row_gl);
  }
  else if(row_mk_svt == 0 && row_mk_tpc > 0) {  /*  no SVT track; TPC track  */
       glbvl[row_gl].svt_seg = 0;
       glbvl[row_gl].tpc_seg = 1;
       glbvl[row_gl].mc_pid = g2t_track[row_mk_tpc - 1].ge_pid;
       row_mh_tpc = 0; /* tpeval[row_tp - 1].inhit;       1st TPC hit; wrong */
       mom_diff (g2t_track, g2t_svt_hit, g2t_tpc_hit, globtrk, glbvl, 
                       row_gl, 0, row_mh_tpc, row_mk_tpc);
       row_mv = g2t_track[row_mk_tpc - 1].start_vertex_p;
       min_dist_vrt (g2t_vertex, globtrk, glbvl, row_gl, row_mv);
  }
  else if(row_mk_svt == -1 && row_mk_tpc > 0) {  /*  bad SVT track; TPC track */
       glbvl[row_gl].svt_seg = -1;
       glbvl[row_gl].tpc_seg = 1;
       bad_track (glbvl, row_gl);
  }  
  else if(row_mk_svt > 0 && row_mk_tpc > 0) {    /*  SVT track; TPC track   */
       if(row_mk_svt == row_mk_tpc) {            /*  matched SVT-TPC tracks */
            glbvl[row_gl].svt_seg = 1;
            glbvl[row_gl].tpc_seg = 1;
            glbvl[row_gl].mc_pid = g2t_track[row_mk_svt - 1].ge_pid;
            row_mh_svt = svt_ctrack[row_sv - 1].spt[0];        /* 1st SVT hit */
            row_mh_tpc = 0; /* tpeval[row_tp - 1].inhit;   1st TPC hit; wrong */
            mom_diff (g2t_track, g2t_svt_hit, g2t_tpc_hit, globtrk, glbvl, 
                      row_gl, row_mh_svt, row_mh_tpc, row_mk_svt);
            row_mv = g2t_track[row_mk_svt - 1].start_vertex_p;
            min_dist_vrt (g2t_vertex, globtrk, glbvl, row_gl, row_mv);
       }
       else {                                  /*  unmatched SVT-TPC tracks  */
            glbvl[row_gl].svt_seg = 1;
            glbvl[row_gl].tpc_seg = 1;
            bad_track (glbvl, row_gl);
       }
  }   /* end if-else ladder */

 };   /* end loop */

 glbvl_h->nok = ok_line;

 return STAFCV_OK;
}

/* ------------------------------------------------------------- */

int search_row_sv ( STK_TRACK_ST  *svt_trck,
                    int               nok, 
                    int               gl_val )
{
  int t;

  for(t = 0; t < nok; t++)
    if(gl_val == svt_trck[t].id_globtrk) return t+1;
  return -1;
}
 
/* ------------------------------------------------------------- */

int search_row_tp ( TTE_EVAL_ST     *tpeval,
                    int               nok,
                    int               tp_val )
{
  int t;

  for(t = 0; t < nok; t++)
    if(tp_val == tpeval[t].rtrk) return t+1;
  return -1;
}

/* ------------------------------------------------------------- */

int search_tp_val ( TPT_TRACK_ST    *tptrack,
                    int               nok,
                    int               gl_val )
{
  int t;

  for(t = 0; t < nok; t++)
    if(gl_val == tptrack[t].id_globtrk) return t+1;
  return -1;
}

/* ------------------------------------------------------------- */

int find_row_mk_svt ( STK_CTRACK_ST   *svt_ctrack,
                      SCS_SPT_ST      *svt_spt,
                      int                 row_sv )
{
  int row , row_sp , row_mk;
  
  if(row_sv == -1) return 0;

  row_mk = svt_ctrack[row_sv - 1].id_mctrack;
  for(row = 0; row < svt_ctrack[row_sv - 1].nspt; row++) {
     row_sp = svt_ctrack[row_sv - 1].spt[row];
     if(svt_spt[row_sp - 1].id_mctrack != row_mk) return -1;
  };
  return row_mk;
}

/* ------------------------------------------------------------- */

int find_row_mk_tpc ( TABLE_HEAD_ST       *tptrack_h,
                      TPT_TRACK_ST      *tptrack,
                      TABLE_HEAD_ST       *tpeval_h,
                      TTE_EVAL_ST       *tpeval,
                      int                 tp_val,
                      int                 gl_val )
{
  int row_mk, row_tp;

  if(tp_val == -1) return 0;

  row_tp = search_row_tp (tpeval , tpeval_h->nok , tp_val);
  if(row_tp == -1) return -1;
  if(tpeval[row_tp -1].qfact <= QFAC) return -1;

  row_mk = tpeval[row_tp - 1].mtrk;
  return row_mk;
}

/* ------------------------------------------------------------- */

void mom_diff ( G2T_TRACK_ST          *g2t_track,
                G2T_SVT_HIT_ST      *g2t_svt_hit,
                G2T_TPC_HIT_ST      *g2t_tpc_hit,
                EGR_GLOBTRK_ST        *globtrk,
                EGR_GLOBTRK_EVAL_ST   *glbvl,
                int                   row_gl,
                int                   row_mh_svt,
                int                   row_mh_tpc,  /* wrong value */
                int                   row_mk )
{
  float pt_gl , pz_gl , ptot_gl , eta_gl;
  float pt_mk , pz_mk , ptot_mk , eta_mk;

  glbvl[row_gl].dpt_svt[0]  = 0.0;
  glbvl[row_gl].dpt_svt[1]  = 0.0;
  glbvl[row_gl].dpz_svt[0]  = 0.0;
  glbvl[row_gl].dpz_svt[1]  = 0.0;
  glbvl[row_gl].dpto_svt[0] = 0.0;
  glbvl[row_gl].dpto_svt[1] = 0.0;
  glbvl[row_gl].deta_svt[0] = 0.0;
  glbvl[row_gl].deta_svt[1] = 0.0;
  glbvl[row_gl].dpt_tpc[0]  = 0.0;
  glbvl[row_gl].dpt_tpc[1]  = 0.0;
  glbvl[row_gl].dpz_tpc[0]  = 0.0;
  glbvl[row_gl].dpz_tpc[1]  = 0.0;
  glbvl[row_gl].dpto_tpc[0] = 0.0;
  glbvl[row_gl].dpto_tpc[1] = 0.0;
  glbvl[row_gl].deta_tpc[0] = 0.0;
  glbvl[row_gl].deta_tpc[1] = 0.0;

  pt_mk = MAG( g2t_track[row_mk - 1].p[0] , g2t_track[row_mk - 1].p[1] );
  pt_gl = 1.0 / globtrk[row_gl].invpt;
  glbvl[row_gl].dpt_vrt[0] = pt_gl - pt_mk;
  glbvl[row_gl].dpt_vrt[1] = 100 * glbvl[row_gl].dpt_vrt[0] / pt_mk;

  pz_mk = g2t_track[row_mk - 1].p[2];
  pz_gl = pt_gl * globtrk[row_gl].tanl;
  glbvl[row_gl].dpz_vrt[0] = fabs(pz_gl) - fabs(pz_mk);
  glbvl[row_gl].dpz_vrt[1] = 100 * glbvl[row_gl].dpz_vrt[0] / pz_mk;

  ptot_mk = MAG( pt_mk , pz_mk );
  ptot_gl = MAG( pt_gl , pz_gl );
  glbvl[row_gl].dpto_vrt[0] = ptot_gl - ptot_mk;
  glbvl[row_gl].dpto_vrt[1] = 100 * glbvl[row_gl].dpto_vrt[0] / ptot_mk;

  eta_mk = 0.5 * log( (ptot_mk + pz_mk) / (ptot_mk - pz_mk) );
  eta_gl = 0.5 * log( (ptot_gl + pz_gl) / (ptot_gl - pz_gl) );
  glbvl[row_gl].deta_vrt[0] = eta_gl - eta_mk;
  glbvl[row_gl].deta_vrt[1] = 100 * glbvl[row_gl].deta_vrt[0] / eta_mk;

  if(row_mh_svt > 0) {

     float pt_mh , pz_mh , ptot_mh , eta_mh;

     pt_mh = MAG( g2t_svt_hit[row_mh_svt - 1].p[0] , 
                  g2t_svt_hit[row_mh_svt - 1].p[1] );
     glbvl[row_gl].dpt_svt[0] = pt_gl - pt_mh;
     glbvl[row_gl].dpt_svt[1] = 100 * glbvl[row_gl].dpt_svt[0] / pt_mh;

     pz_mh = g2t_svt_hit[row_mh_svt - 1].p[2];
     if(fabs(pz_mh) < 1.0e-07) {
         printf("\n svt %d %d %f", row_gl+1, row_mh_svt, pz_mh); 
         pz_mh = 1.0e-07;
     };
     glbvl[row_gl].dpz_svt[0] = fabs(pz_gl) - fabs(pz_mh);
     glbvl[row_gl].dpz_svt[1] = 100 * glbvl[row_gl].dpz_svt[0] / pz_mh;
        
     ptot_mh = MAG( pt_mh , pz_mh );
     glbvl[row_gl].dpto_svt[0] = ptot_gl - ptot_mh;
     glbvl[row_gl].dpto_svt[1] = 100 * glbvl[row_gl].dpto_svt[0] / ptot_mh;

     eta_mh = 0.5 * log( (ptot_mh + pz_mh) / (ptot_mh - pz_mh) );
     glbvl[row_gl].deta_svt[0] = eta_gl - eta_mh;
     glbvl[row_gl].deta_svt[1] = 100 * glbvl[row_gl].deta_svt[0] / eta_mh;
  };

  if(row_mh_tpc > 0) {  /* this whole block gives wrong results */

     float pt_mh , pz_mh , ptot_mh , eta_mh;

     pt_mh = MAG( g2t_tpc_hit[row_mh_tpc - 1].p[0] , 
                  g2t_tpc_hit[row_mh_tpc - 1].p[1] );
     glbvl[row_gl].dpt_tpc[0] = pt_gl - pt_mh;
     glbvl[row_gl].dpt_tpc[1] = 100 * glbvl[row_gl].dpt_tpc[0] / pt_mh;

     pz_mh = g2t_tpc_hit[row_mh_tpc - 1].p[2];
 /*     if(fabs(pz_mh) < 1.0e-07) 
         printf("\n tpc %d %d %f", row_gl+1, row_mh_tpc, pz_mh); */ 
     glbvl[row_gl].dpz_tpc[0] = fabs(pz_gl) - fabs(pz_mh);
     glbvl[row_gl].dpz_tpc[1] = 100 * glbvl[row_gl].dpz_tpc[0] / pz_mh;
        
     ptot_mh = MAG( pt_mh , pz_mh );
     glbvl[row_gl].dpto_tpc[0] = ptot_gl - ptot_mh;
     glbvl[row_gl].dpto_tpc[1] = 100 * glbvl[row_gl].dpto_tpc[0] / ptot_mh;

     eta_mh = 0.5 * log( (ptot_mh + pz_mh) / (ptot_mh - pz_mh) );
     glbvl[row_gl].deta_tpc[0] = eta_gl - eta_mh;
     glbvl[row_gl].deta_tpc[1] = 100 * glbvl[row_gl].deta_tpc[0] / eta_mh;
  };

  return;
}

/* ------------------------------------------------------------- */

void min_dist_vrt ( G2T_VERTEX_ST          *g2t_vertex,
                    EGR_GLOBTRK_ST        *globtrk,
                    EGR_GLOBTRK_EVAL_ST   *glbvl,
                    int                   row_gl,
                    int                   row_mv )

{
  extern svt_project_track_();           /*  These functions are in    */
  extern svt_update_track_param_();      /* the file egr_gltrk_eval.F  */

  int q , f = 1;
  float rh , phi , rad , h , xc[2] , xv[3] , x[2] , trk[6] , trk2[6] , 
        xh[3] , p[3] , xh_min[3] , c = 0.002998 , pi = 3.14159265; 
  
  rad = pi / 180.0;
  q = globtrk[row_gl].icharge;

  if(row_mv != 1) f = -1;

  rh = 1 / ( fabs(q * B) * c * globtrk[row_gl].invpt );
  h = - q * B / fabs(q * B);

  trk[0] = globtrk[row_gl].r0;
  trk[1] = globtrk[row_gl].r0 * globtrk[row_gl].phi0 * rad;
  trk[2] = globtrk[row_gl].z0;
  trk[3] = globtrk[row_gl].psi * rad;
  trk[4] = globtrk[row_gl].tanl;
  trk[5] = h / rh;

  phi = trk[3] - h * pi / 2.0;

  xc[0] = trk[0] * cos(trk[1] / trk[0]) - rh * cos(phi);
  xc[1] = trk[0] * sin(trk[1] / trk[0]) - rh * sin(phi);

  xv[0] = g2t_vertex[row_mv - 1].ge_x[0];
  xv[1] = g2t_vertex[row_mv - 1].ge_x[1];
  xv[2] = g2t_vertex[row_mv - 1].ge_x[2];

  svt_project_track_(xc, &rh, xv, x);
  svt_update_track_param_(xc, &rh, x, trk, trk2);
  
  xh[0] = x[0];
  xh[1] = x[1];
  xh[2] = trk2[2];
  
  p[0] = cos(trk2[3]) / globtrk[row_gl].invpt;
  p[1] = sin(trk2[3]) / globtrk[row_gl].invpt;
  p[2] = trk2[4] / globtrk[row_gl].invpt;

  appr_min_dist(xv, xh, p, xh_min);

  glbvl[row_gl].dmin_vrt[0] = f * 
                              MAG( (xh_min[0] - xv[0]) , (xh_min[1] - xv[1]) ); 
  glbvl[row_gl].dmin_vrt[1] = f * 
                        MAG( glbvl[row_gl].dmin_vrt[0] , (xh_min[2] - xv[2]) ); 

  return;
}
 
/* ------------------------------------------------------------- */

void appr_min_dist ( float *xv,
                     float *xh,
                     float *p,
                     float *xh_min )

{
  float fc1 , fc2 , fc3;

  fc1 = (SQR(p[1]) + SQR(p[2])) * xh[0] - (xh[1] * p[1] + xh[2] * p[2]) * p[0] +
        (xv[0] * p[0] + xv[1] * p[1] + xv[2] * p[2]) * p[0];

  fc2 = (xv[0] - xh[0]) * p[0] + (xv[1] - xh[1]) * p[1] + 
        (xv[2] - xh[2]) * p[2];

  fc3 = SQR(p[0]) + SQR(p[1]) + SQR(p[2]);

  xh_min[0] = fc1 / fc3;
  xh_min[1] = xh[1] + p[1] * fc2 / fc3;
  xh_min[2] = xh[2] + p[2] * fc2 / fc3;

  return;
} 

/* ------------------------------------------------------------- */

void bad_track ( EGR_GLOBTRK_EVAL_ST   *glbvl,
                 int                   row_gl )

{
  glbvl[row_gl].mc_pid      =   0;  
  glbvl[row_gl].dpt_vrt[0]  = 0.0;
  glbvl[row_gl].dpt_vrt[1]  = 0.0;
  glbvl[row_gl].dpz_vrt[0]  = 0.0;
  glbvl[row_gl].dpz_vrt[1]  = 0.0;
  glbvl[row_gl].dpto_vrt[0] = 0.0;
  glbvl[row_gl].dpto_vrt[1] = 0.0;
  glbvl[row_gl].deta_vrt[0] = 0.0;
  glbvl[row_gl].deta_vrt[1] = 0.0;
  glbvl[row_gl].dpt_svt[0]  = 0.0;
  glbvl[row_gl].dpt_svt[1]  = 0.0;
  glbvl[row_gl].dpz_svt[0]  = 0.0;
  glbvl[row_gl].dpz_svt[1]  = 0.0;
  glbvl[row_gl].dpto_svt[0] = 0.0;
  glbvl[row_gl].dpto_svt[1] = 0.0;
  glbvl[row_gl].deta_svt[0] = 0.0;
  glbvl[row_gl].deta_svt[1] = 0.0;
  glbvl[row_gl].dpt_tpc[0]  = 0.0;
  glbvl[row_gl].dpt_tpc[1]  = 0.0;
  glbvl[row_gl].dpz_tpc[0]  = 0.0;
  glbvl[row_gl].dpz_tpc[1]  = 0.0;
  glbvl[row_gl].dpto_tpc[0] = 0.0;
  glbvl[row_gl].dpto_tpc[1] = 0.0;
  glbvl[row_gl].deta_tpc[0] = 0.0;
  glbvl[row_gl].deta_tpc[1] = 0.0;
  glbvl[row_gl].dmin_vrt[0] = 0.0;
  glbvl[row_gl].dmin_vrt[1] = 0.0;

  return;
}

//**********************************************************************
// File :        sal_getlocal.c
// Description : Fill  xl,yl, wafer_id  in a sal_spt table  
//   Input :     svg_config, svg_geom ,g2t_svt_hit
//   Output :    sal_spt
//**********************************************************************
# include "sal_getlocal.h"
# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include "svt.h"

long type_of_call sal_getlocal_( TABLE_HEAD_ST *config_h, SVG_CONFIG_ST *config,                     
                    TABLE_HEAD_ST *svt_geom_h, SVG_GEOM_ST  *svt_geom ,
                    TABLE_HEAD_ST *g2t_svt_hit_h, G2T_SVT_HIT_ST  *g2t_svt_hit ,
                    TABLE_HEAD_ST *sal_spt_h, SAL_SPT_ST  *sal_spt,
                    TABLE_HEAD_ST *sal_vrtx_h, SAL_VRTX_ST *sal_vrtx)
{

float r1, r2, r3, r4, r5, r6;
int i, j, k, l, n, p, pf[NEV+1], iev = 0;
Point P, P1, P2;
float CUT_ALIGN_dE = 0.0;
int  waf_id;

r1 = config[0].layer_radius[0]*10;
r2 = config[0].layer_radius[1]*10;
r3 = config[0].layer_radius[2]*10;
r4 = config[0].layer_radius[3]*10;
r5 = config[0].layer_radius[4]*10;
r6 = config[0].layer_radius[5]*10;

work work(r1, r2, r3, r4, r5, r6); 

//***********************************************************************
//           Creating of event hits map
//***********************************************************************
p = 1; pf[0] = 0; 
for(iev = 1; iev <= NEV; iev++)
 {
  while(g2t_svt_hit[p].id > 0)  p++;
  pf[iev] = p+1;
  p++;
 }

P2.X = 0; P2.Y = 0; P2.Z = 0;  //think about vertex!!!
for(iev = 0; iev < NEV; iev++)
 {
//**********************************************************************
//          Filling structure by hits and vertex coordinates
//**********************************************************************
  p = pf[iev];

// pf[0] = 10;

  while(p < pf[iev+1])
   if(g2t_svt_hit[p].de > CUT_ALIGN_dE)
    {
     P.Y = g2t_svt_hit[p].x[0]*10;
     P.Z = g2t_svt_hit[p].x[1]*10;
     P.X = g2t_svt_hit[p].x[2]*10;
     waf_id  = g2t_svt_hit[p].volume_id ;

    
     i = (int)((waf_id/1000-1)/2);
 
    P1.nl = work.Events[iev].Bars[i].WhichLadder(P, P2);
    P1 = work.Events[iev].Bars[i].WhichWaffer(P, P2, P1.nl);
    work.Events[iev].Bars[i].PutPoint(P1.nl, P1.nw, P1.X, P1.Y);
   
    
    p++;
  
  }
 sal_vrtx[iev].id = iev;
 sal_vrtx[iev].x[0] = 0.0;
 sal_vrtx[iev].x[1] = 0.0;
 sal_vrtx[iev].x[2] = 0.0;


//*************************************************************************
//          filling of  new odering  hits table  
//*************************************************************************
 p = pf[iev];
 for( i = 0; i < 3; i++)
  for( n = 0; n < 2; n++)  
   for( j = 0; j < work.Events[iev].Bars[i].num_lad/2; j++)
     for( k = work.Events[iev].Bars[i].ladders[j*2+n].num_waf - 1; k >= 0 ; k--)
        for( l = 0; l <  work.Events[iev].Bars[i].ladders[j*2+n].waffers[k].num;l++)
	 {
           sal_spt[p].xl = work.Events[iev].Bars[i].ladders[j*2+n].waffers[k].points[2*l]*0.1 ;
           sal_spt[p].yl = work.Events[iev].Bars[i].ladders[j*2+n].waffers[k].points[2*l+1]*0.1;
           sal_spt[p].id = p;
           sal_spt[p].wafer_id = 1000*(n+i*2+1) + j + 1 +
                           100*(work.Events[iev].Bars[i].ladders[j*2+n].num_waf - k);
           p++;
         }

sal_spt[p-1].id = - sal_spt[p-1].id;


}

sal_vrtx_h->nok = iev;
sal_spt_h->nok = p;
return STAFCV_OK;

}


//              Old method   
//     n = (int)(waf_id/1000);
//     k = (int)(waf_id/100)- 10*((int)(waf_id/1000));
//     l = ((int)(n/2) + 1 - (int)((n+1)/2));
//     j = 2*(waf_id - 100*((int)waf_id/100) - 1) + l;  
//     k = work.Events[iev].Bars[i].ladders[j].num_waf - k;
//     P1.X = P.X;
//     P1.Y = P.Y*work.Events[iev].Bars[i].SinTheta[2*j] - 
//            P.Z*work.Events[iev].Bars[i].CosTheta[2*j];
//     P1.Z = P.Y*work.Events[iev].Bars[i].CosTheta[2*j] + 
//            P.Z*work.Events[iev].Bars[i].SinTheta[2*j];
//     P = work.Events[iev].Bars[i].ladders[j].waffers[k].WhichPoint(P1,P2);              
//     work.Events[iev].Bars[i].PutPoint(j, k, P.X, P.Y);
   

//**********************************************************************
//File sal_change_geom.cc
//Description recalculation of local hits coordinates from ideal SVT
//configuration to missalignment one

//input : svg_config, sal_spt, sal_vrtx, sal_geom_real, svg_geom

//output: sal_real_hits
//*********************************************************************/


# include "sal_change_geom.h"

# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include "svt.h"


long type_of_call sal_change_geom_(TABLE_HEAD_ST *config_h, SVG_CONFIG_ST *config,
                      TABLE_HEAD_ST *sal_spt_h, SAL_SPT_ST  *sal_spt,
                      TABLE_HEAD_ST  *sal_vrtx_h, SAL_VRTX_ST *sal_vrtx,
                      TABLE_HEAD_ST *sal_geom_real_h, SAL_GEOM_ST *sal_geom_real, 
                      TABLE_HEAD_ST *geom_h, SVG_GEOM_ST *geom, 
                      TABLE_HEAD_ST  *sal_real_hits_h, SAL_SPT_ST *sal_real_hits )
{

 long waf_id;
 int iev = 0, p, pf[NEV+1], i, j, k, l, n;
 float r1, r2, r3, r4, r5, r6;
//***********************************************************************
//        Initialization of classes
//***********************************************************************
 r1 = config[0].layer_radius[0]*10;
 r2 = config[0].layer_radius[1]*10;
 r3 = config[0].layer_radius[2]*10;
 r4 = config[0].layer_radius[3]*10;
 r5 = config[0].layer_radius[4]*10;
 r6 = config[0].layer_radius[5]*10;

 work work(r1, r2, r3, r4, r5, r6); 
//**************************************************************************
//              Obtaining misalignment parameters
//************************************************************************** 
 Point P, P1, P2, P3, P4;
 p = 0;
 for(i = 0; i < 3; i++)
  for(l = 0; l < 2; l++)  
   for(j = 0; j < work.Events[0].Bars[i].num_lad/2; j++)
    for(k = work.Events[0].Bars[i].ladders[j*2+l].num_waf-1; k >=0; k--)
     {
       P1.Y = geom[p].x[0]*10;  
       P1.Z = geom[p].x[1]*10;  
       P1.X = geom[p].x[2]*10;

       P2.Y = sal_geom_real[p].x[0]*10;   
       P2.Z = sal_geom_real[p].x[1]*10;  
       P2.X = sal_geom_real[p].x[2]*10;
       work.par[i][j*2+l][k][3] = sal_geom_real[p].par[3];  
       work.par[i][j*2+l][k][4] = sal_geom_real[p].par[4]; 
       work.par[i][j*2+l][k][5] = sal_geom_real[p].par[5]; 
          
       p++;

       work.par[i][j*2+l][k][0] = P2.X - P1.X;
       work.par[i][j*2+l][k][2] = sqrt(P2.Z*P2.Z + P2.Y*P2.Y)
                                - sqrt(P1.Z*P1.Z + P1.Y*P1.Y); 
       P3.X = P2.X;
       P3.Y = P2.Y*work.Events[0].Bars[i].SinTheta[2*(j*2+l)] - 
                 P2.Z*work.Events[0].Bars[i].CosTheta[2*(j*2+l)];
       P3.Z = P2.Y*work.Events[0].Bars[i].CosTheta[2*(j*2+l)] + 
                 P2.Z*work.Events[0].Bars[i].SinTheta[2*(j*2+l)];
       P4.X = 0; P4.Y = 0; P4.Z = 0;
        
       P = work.Events[0].Bars[i].ladders[j*2+l].waffers[k].WhichPoint(P4,P3);              
       work.par[i][j*2+l][k][1] = P.Y; 
     }

//***********************************************************************
//                    Creating of event hits map 
//***********************************************************************
 p = 1; pf[0] = 0; 
 for(iev = 1; iev <= NEV; iev++)
  {
   while(sal_spt[p].id > 0)  p++;
   pf[iev] = p+1;
   p++;
  }

 for(iev = 0; iev < NEV; iev++)
  {
//*************************************************************************
//             Fill structure by hits         
//*************************************************************************
   p = pf[iev];
   while(p < pf[iev+1])
    {
     waf_id  = sal_spt[p].wafer_id ;
     n = (int)(waf_id/1000);
     i = (int)((waf_id/1000-1)/2);
     k = (int)(waf_id/100)- 10*((int)(waf_id/1000));
     l = ((int)(n/2) + 1 - (int)((n+1)/2));
     j = 2*(waf_id - 100*((int)waf_id/100) - 1) + l;  
     k = work.Events[iev].Bars[i].ladders[j].num_waf - k;
     work.Events[iev].Bars[i].PutPoint(j, k, sal_spt[p].xl*10,
                                             sal_spt[p].yl*10);
     p++;
    }

//*************************************************************************
//            ... by vertex
//*************************************************************************
   work.Events[iev].Vertex.Y = sal_vrtx[iev].x[0]*10; 
   work.Events[iev].Vertex.Z = sal_vrtx[iev].x[1]*10;
   work.Events[iev].Vertex.X = sal_vrtx[iev].x[2]*10;

//**************************************************************************
//      Changing of geometry
//*************************************************************************
 
   work.ChangeGeom(iev); 

//*************************************************************************
//          filling of  new hits table  
//*************************************************************************
   p = pf[iev];
   for( i = 0; i < 3; i++)
    for( n = 0; n < 2; n++)  
     for( j = 0; j < work.Events[iev].Bars[i].num_lad/2; j++)
      for( k = work.Events[iev].Bars[i].ladders[j*2+n].num_waf - 1; k >= 0 ; k--)
        for( l = 0; l <  work.Events[iev].Bars[i].ladders[j*2+n].waffers[k].num;l++)
	 {
           sal_real_hits[p].xl = work.Events[iev].Bars[i].ladders[j*2+n].waffers[k].points[2*l]*0.1 ;
           sal_real_hits[p].yl = work.Events[iev].Bars[i].ladders[j*2+n].waffers[k].points[2*l+1]*0.1;
           sal_real_hits[p].id = p;
           sal_real_hits[p++].wafer_id = 1000*(n+i*2+1) + j + 1 +
                           100*(work.Events[iev].Bars[i].ladders[j*2+n].num_waf - k);
         }

    sal_real_hits[p-1].id = - sal_real_hits[p-1].id;
  }

sal_real_hits_h->nok = p;
return STAFCV_OK;
}


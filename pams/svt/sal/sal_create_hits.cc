//**********************************************************************
//File: sal_create_hits.c
//Description: Create ideal points 
//
//Input:  svg_config
//Output: sal_spt, sal_vrtx
//********************************************************************* 

# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include <time.h>
# include "svt.h"
# include "sal_create_hits.h"


long type_of_call sal_create_hits_(TABLE_HEAD_ST *config_h, SVG_CONFIG_ST *config,
                      TABLE_HEAD_ST *sal_spt_h, SAL_SPT_ST  *sal_spt,
                      TABLE_HEAD_ST  *sal_vrtx_h, SAL_VRTX_ST *sal_vrtx ) 
{
 int p = 0, i, j, k, l, n, iev;
 float r1, r2, r3, r4, r5, r6;


r1 = config[0].layer_radius[0]*10;
r2 = config[0].layer_radius[1]*10;
r3 = config[0].layer_radius[2]*10;
r4 = config[0].layer_radius[3]*10;
r5 = config[0].layer_radius[4]*10;
r6 = config[0].layer_radius[5]*10;

work work(r1, r2, r3, r4, r5, r6); 

 

 srand(time(0));
 for (iev = 0; iev < NEV; iev++)
 {
  work.GenerateEvent(iev);
  
  sal_vrtx[iev].id = iev; 
  sal_vrtx[iev].x[0] = work.Events[iev].Vertex.Y*0.1;
  sal_vrtx[iev].x[1] = work.Events[iev].Vertex.Z*0.1;
  sal_vrtx[iev].x[2] = work.Events[iev].Vertex.X*0.1;

 for( i = 0; i < 3; i++)
  for( n = 0; n < 2; n++)  
   for( j = 0; j < work.Events[iev].Bars[i].num_lad/2; j++)
     for( k = work.Events[iev].Bars[i].ladders[j*2+n].num_waf - 1; k >= 0 ; k--)
       if(work.Events[iev].Bars[i].ladders[j*2+n].waffers[k].num > 0)
        for( l = 0; l <  work.Events[iev].Bars[i].ladders[j*2+n].waffers[k].num;l++)
	 {
           sal_spt[p].xl = work.Events[iev].Bars[i].ladders[j*2+n].waffers[k].points[2*l]*0.1 ;
           sal_spt[p].yl = work.Events[iev].Bars[i].ladders[j*2+n].waffers[k].points[2*l+1]*0.1;
           sal_spt[p].id = p;
           sal_spt[p++].wafer_id=1000*(n+i*2+1)+ j+1+
                    (work.Events[iev].Bars[i].ladders[j*2+n].num_waf - k)*100;
         }
 sal_spt[p-1].id = - sal_spt[p-1].id;

  work.Events[iev].Clear();
  work.Events[iev].FindAllTracks();
  printf("total %d\n",work.Events[iev].num);
  work.Events[iev].OptimizeTracks();
  work.Events[iev].SelectTracks();
  j = 0;
  for(i = 0; i < work.Events[iev].num; i++)
  if(work.Events[iev].tracks[i].flag == 1) j++;
  printf("select %d\n",j);
  work.Events[iev].FindVertex();
  printf("%.3f %.3f %.3f",work.Events[iev].Vertex.X,
         work.Events[iev].Vertex.Y, work.Events[iev].Vertex.Z);

  
 }   
sal_vrtx_h->nok = iev;
sal_spt_h->nok = p;
return STAFCV_OK;
}


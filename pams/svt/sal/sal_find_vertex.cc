//*********************************************************************** 
//File : sal_find_vertex.c
//Description : find tracks parameters and defined the vertex coordinates
//Input: svg_config, sal_spt, sal_vrtx 
//Output: sal_vrtx_find, and must be sal_tracks !!!
//***********************************************************************

# include <stdio.h>
# include <stdlib.h>
# include <math.h>

# include "svt.h"
# include "sal_find_vertex.h"

long type_of_call sal_find_vertex_(TABLE_HEAD_ST *config_h, SVG_CONFIG_ST *config,
                      TABLE_HEAD_ST *sal_spt_h, SAL_SPT_ST  *sal_spt,
                      TABLE_HEAD_ST  *sal_vrtx_h, SAL_VRTX_ST *sal_vrtx,
                      TABLE_HEAD_ST  *sal_vrtx_find_h, SAL_VRTX_ST *sal_vrtx_find )
{




long waf_id;
float r1, r2, r3, r4, r5, r6;
int iev, pf[NEV+1], p, i, j, k, l, n;


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
  while(sal_spt[p].id > 0)  p++;
  pf[iev] = p+1;
  printf("%d \n",pf[iev]);
  p++;
 }

for(iev = 0; iev < NEV; iev++)
 {
//**********************************************************************
//          Filling structure by hits and vertex coordinates
//**********************************************************************
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

printf("events %d reading data ok",iev); 

  work.Events[iev].Clear();
  work.Events[iev].FindAllTracks();
  printf("\n Total tracks found : %d\n  ", work.Events[iev].num);
  
  work.Events[iev].OptimizeTracks();
  work.Events[iev].SelectTracks();
  
  int nt = 0;
  for(int q = 0; q < work.Events[iev].num; q++) 
  if (work.Events[iev].tracks[q].flag == 1) nt++;
  printf("Selected tracks  :  %d\n", nt);
  
  work.Events[iev].FindVertex();
  printf("\n %.4f %.4f %.4f\n",work.Events[iev].Vertex.Y*0.1,
       work.Events[iev].Vertex.Z*0.1,work.Events[iev].Vertex.X*0.1);  
 
  printf("%.3f %.3f %.3f\n",sal_vrtx[iev].x[0],
       sal_vrtx[iev].x[1], sal_vrtx[iev].x[2]);


  sal_vrtx_find[iev].x[0] = work.Events[iev].Vertex.Y*0.1;
  sal_vrtx_find[iev].x[1] = work.Events[iev].Vertex.Z*0.1;
  sal_vrtx_find[iev].x[2] = work.Events[iev].Vertex.X*0.1;
 }

sal_vrtx_find_h->nok = iev;
return STAFCV_OK;
}

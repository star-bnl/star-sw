//*********************************************************************** 
//File : sal_get_chi.cc
//Description : find tracks chi^2
//Input: svg_config, sal_spt
//Output: file "chi.dat"
//***********************************************************************

# include <stdio.h>
# include <stdlib.h>
# include <math.h>

# include "svt.h"
# include "sal_get_chi.h"

long type_of_call sal_get_chi_(TABLE_HEAD_ST *config_h, SVG_CONFIG_ST *config,
                  TABLE_HEAD_ST *sal_spt_h, SAL_SPT_ST  *sal_spt)
{


FILE *f;

int iev;
long waf_id;
int p, i, j, k, l, n, pf[NEV+1];
float r1, r2, r3, r4, r5, r6;

r1 = config[0].layer_radius[0]*10;
r2 = config[0].layer_radius[1]*10;
r3 = config[0].layer_radius[2]*10;
r4 = config[0].layer_radius[3]*10;
r5 = config[0].layer_radius[4]*10;
r6 = config[0].layer_radius[5]*10;

work work(r1, r2, r3, r4, r5, r6); 

work.ClearParam();
work.InstallParam();

//***********************************************************************
//           Creating of event hits map
//***********************************************************************
p = 1; pf[0] = 0; 
for(iev = 1; iev <= NEV; iev++)
 {
  while(sal_spt[p].id > 0)  p++;
  pf[iev] = p+1;
  p++;
 }
printf("\n Creating of event hits map Ok");
//**********************************************************************
//          Filling structure by hits and vertex coordinates
//**********************************************************************
for(iev = 0; iev < NEV; iev++)
{
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
 

//***********************************************************************
//   Calculation of tracks chi^2
//***********************************************************************
 work.Events[iev].Clear();
 work.Events[iev].FindAllTracks();
 work.Events[iev].OptimizeTracks();
// work.Events[iev].SelectTracks();		
// work.Events[iev].FindVertex();
}
//***********************************************************************
//  Writing output
//***********************************************************************
f = fopen("chi.dat","w");
for(iev = 0; iev < NEV; iev++)
 {
  printf("\n n_tracks : %d",work.Events[iev].num);
  for(j = 0; j < work.Events[iev].num; j++)
//   if (work.Events[iev].tracks[j].flag == 1)
     fprintf(f,"%.6f\n",work.Events[iev].tracks[j].chi2);
     
  }
fclose(f);
return STAFCV_OK;
}










//**************************************************************************
//  File:        sal_get_sigma.c
//  Description: Fill  xl,yl, wafer_id and  in a sal_spt table  
//  Input:       svg_config,  g2t_svt_hit, g2t_svt_track
//  Output:      file "sigma.dat"
//**************************************************************************

//ami/call sal_get_sigma config  g2t_svtall Event/g2t_track

# include "sal_get_sigma.h"
# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include "svt.h"

long type_of_call sal_get_sigma_( TABLE_HEAD_ST *config_h, SVG_CONFIG_ST *config,                     
                    TABLE_HEAD_ST *g2t_svt_hit_h, G2T_SVT_HIT_ST  *g2t_svt_hit ,
                    TABLE_HEAD_ST *g2t_track_h, G2T_TRACK_ST *g2t_track)
{
int nb = 0, p, i, j, k, l, n, waf_id, track_id;
FILE *f;
FILE *fx;
FILE *fy;
float r1, r2, r3, r4, r5, r6;
Point P1, P2, P3, P4, Pv, P, Pr;

r1 = config[0].layer_radius[0]*10; r2 = config[0].layer_radius[1]*10;
r3 = config[0].layer_radius[2]*10; r4 = config[0].layer_radius[3]*10;
r5 = config[0].layer_radius[4]*10; r6 = config[0].layer_radius[5]*10;

event Event(r1, r2, r3, r4, r5, r6); 

f = fopen("sigma1.dat","w");
fx = fopen("sigma1x.dat","w");
fy = fopen("sigma1y.dat","w");
p = 0;
Pv.X = 0; Pv.Y = 0; Pv.Z = 0;
while(p < g2t_svt_hit_h->nok/2)
 {
   P1.Y = g2t_svt_hit[p].x[0]*10;
   P1.Z = g2t_svt_hit[p].x[1]*10;
   P1.X = g2t_svt_hit[p].x[2]*10;
   waf_id  = g2t_svt_hit[p].volume_id;
   track_id = g2t_svt_hit[p].track_p; 
   p++;
   
   n = (int)(waf_id/1000);
   i = (int)((waf_id/1000-1)/2);
   k = (int)(waf_id/100)- 10*((int)(waf_id/1000));
   l = ((int)(n/2) + 1 - (int)((n+1)/2));
   j = 2*(waf_id - 100*((int)waf_id/100) - 1) + l;  
   k = Event.Bars[i].ladders[j].num_waf - k;

//printf("%d %d %d %d ",waf_id, i,j,k );
  if (i == nb)
    {
     for(l = 0; l < g2t_track_h->nok; l++)
      if( g2t_track[l].id == track_id)
        {
         P2.Y = g2t_track[l].p[0]*100;
         P2.Z = g2t_track[l].p[1]*100;
         P2.X = g2t_track[l].p[2]*100;
       
  // printf("%d ",track_id);        
         continue;
        }
     //  printf("%.3f %.3f %.3f %d\n",P2.X,P2.Y,P2.Z,i);
       P3.nl = Event.Bars[i].WhichLadder(Pv, P1);
       P3 = Event.Bars[i].WhichWaffer(Pv, P1, P3.nl);
       
       P4.nl = Event.Bars[i].WhichLadder(Pv, P2);
       P4 = Event.Bars[i].WhichWaffer(Pv, P2, P4.nl);
      
  if((fabs(P4.X) > 30)||(P3.nl != P4.nl))
      {
       Pr.Z = 10000; 
       printf("%d %d      ",P3.nl,P4.nl);
      }
   else
      {
       Pr.Z = (P4.X-P3.X)*(P4.X-P3.X)+(P4.Y-P3.Y)*(P4.Y-P3.Y); 
       fprintf(f,"%.6f\n",sqrt(Pr.Z));
       fprintf(fx,"%.6f\n",P4.X-P3.X);
       fprintf(fy,"%.6f\n",P4.Y-P3.Y);
      }
 //     printf("%.6f %d %d\n",sqrt(Pr.Z),P3.nl,P4.nl);
  
     }
}
fclose(f);
fclose(fx);
fclose(fy);
return STAFCV_OK;
}

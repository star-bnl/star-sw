//**************************************************************************
//  File:        sal_get_sigma.c
//  Description: Fill  xl,yl, wafer_id and  in a sal_spt table  
//  Input:       svg_config,  g2t_svt_hit, g2t_svt_track
//  Output:      file "sigma.dat"
//**************************************************************************

//ami/call sal_get config  g2t_svtall Event/g2t_track

# include "sal_get.h"
# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include "svt.h"

long type_of_call sal_get_( TABLE_HEAD_ST *config_h, SVG_CONFIG_ST *config,                     
                    TABLE_HEAD_ST *g2t_svt_hit_h, G2T_SVT_HIT_ST  *g2t_svt_hit ,
                    TABLE_HEAD_ST *g2t_track_h, G2T_TRACK_ST *g2t_track)
{
int nb[3] = {-1, -1, -1}, p, i, j, k, l, n, h, waf_id;
FILE *f;
float r1, r2, r3, r4, r5, r6;
float e[3],imp;
Point Pv, Pp, P[3], Pn[3];

r1 = config[0].layer_radius[0]*10; r2 = config[0].layer_radius[1]*10;
r3 = config[0].layer_radius[2]*10; r4 = config[0].layer_radius[3]*10;
r5 = config[0].layer_radius[4]*10; r6 = config[0].layer_radius[5]*10;

event Event(r1, r2, r3, r4, r5, r6); 

f = fopen("en.dat","w");

p = 0;
Pv.X = 0; Pv.Y = 0; Pv.Z = 0;
while(p < 200)//g2t_track_h->nok)
 {
   Pp.Y = g2t_track[p].p[0];
   Pp.Z = g2t_track[p].p[1];
   Pp.X = g2t_track[p].p[2];
//   printf("%d\n",g2t_track[p].id);

   for(h = 0; h < g2t_svt_hit_h->nok; h++)  
     {
     if(g2t_track[p].id == g2t_svt_hit[h].track_p)
       {
         waf_id  = g2t_svt_hit[h].volume_id;
       
         n = (int)(waf_id/1000);
         i = (int)((waf_id/1000-1)/2);
         k = (int)(waf_id/100)- 10*((int)(waf_id/1000));
         l = ((int)(n/2) + 1 - (int)((n+1)/2));
         j = 2*(waf_id - 100*((int)waf_id/100) - 1) + l;  
         k = Event.Bars[i].ladders[j].num_waf - k;
         
         e[i] =  g2t_svt_hit[h].de;  
         P[i].Y = g2t_svt_hit[h].x[0]*10;
         P[i].Z = g2t_svt_hit[h].x[1]*10;
         P[i].X = g2t_svt_hit[h].x[2]*10;
         P[i].nl = j;
         P[i].nw = k;
         P[i].np = i;
         nb[0] = i;
         h = g2t_svt_hit_h->nok;
       }
      }
   for(h = 0; h < g2t_svt_hit_h->nok; h++)  
    if(g2t_track[p].id == g2t_svt_hit[h].track_p)
       {
         waf_id  = g2t_svt_hit[h].volume_id;
       
         n = (int)(waf_id/1000);
         i = (int)((waf_id/1000-1)/2);
         k = (int)(waf_id/100)- 10*((int)(waf_id/1000));
         l = ((int)(n/2) + 1 - (int)((n+1)/2));
         j = 2*(waf_id - 100*((int)waf_id/100) - 1) + l;  
         k = Event.Bars[i].ladders[j].num_waf - k;
         
         if(i == nb[0]) continue;
         
         e[i] =  g2t_svt_hit[h].de; 
         P[i].Y = g2t_svt_hit[h].x[0]*10;
         P[i].Z = g2t_svt_hit[h].x[1]*10;
         P[i].X = g2t_svt_hit[h].x[2]*10;
         P[i].nl = j;
         P[i].nw = k;
         P[i].np = i;
         nb[1] = i;
         h = g2t_svt_hit_h->nok;
       }

 
   for(h = 0; h < g2t_svt_hit_h->nok; h++)  
     if(g2t_track[p].id == g2t_svt_hit[h].track_p)
       {
         waf_id  = g2t_svt_hit[h].volume_id;
       
         n = (int)(waf_id/1000);
         i = (int)((waf_id/1000-1)/2);
         k = (int)(waf_id/100)- 10*((int)(waf_id/1000));
         l = ((int)(n/2) + 1 - (int)((n+1)/2));
         j = 2*(waf_id - 100*((int)waf_id/100) - 1) + l;  
         k = Event.Bars[i].ladders[j].num_waf - k;
         
         if((i == nb[0])||(i == nb[1])) continue;
         e[i] =  g2t_svt_hit[h].de;
         P[i].Y = g2t_svt_hit[h].x[0]*10;
         P[i].Z = g2t_svt_hit[h].x[1]*10;
         P[i].X = g2t_svt_hit[h].x[2]*10;
         P[i].nl = j;
         P[i].nw = k; 
         P[i].np = i;
         nb[2] = i;
         h = g2t_svt_hit_h->nok;
       }

 p++;   
// printf("\n%d %d %d\n",nb[0], nb[1], nb[2]);
 if((nb[0]==-1)||(nb[1]==-1)||(nb[2]==-1)) continue; 

  Pn[0].nl = Event.Bars[P[0].np].WhichLadder(Pv, P[0]);
  Pn[0] = Event.Bars[P[0].np].WhichWaffer(Pv, P[0], Pn[0].nl);
      
  Pn[1].nl = Event.Bars[P[1].np].WhichLadder(Pv, P[1]);
  Pn[1] = Event.Bars[P[1].np].WhichWaffer(Pv, P[1], Pn[1].nl);
 
  Pn[2].nl = Event.Bars[P[2].np].WhichLadder(Pv, P[2]);
  Pn[2] = Event.Bars[P[2].np].WhichWaffer(Pv, P[2], Pn[2].nl);
    
//    printf("1-st %d %d   %d %d\n", P[0].nl, Pn[0].nl, P[0].nw, Pn[0].nw);
//    printf("2-nd %d %d   %d %d\n", P[1].nl, Pn[1].nl, P[1].nw, Pn[1].nw);
//    printf("3-rd %d %d   %d %d\n", P[2].nl, Pn[2].nl, P[2].nw, Pn[2].nw);

if((Pn[0].nl == P[0].nl)&&(Pn[1].nl == P[1].nl)&&(Pn[2].nl == P[2].nl)&&
     (Pn[0].nl == P[0].nl)&&(Pn[1].nl == P[1].nl)&&(Pn[2].nl == P[2].nl))
   {
    e[0]+=e[1]+e[2];
    imp = sqrt(Pp.X*Pp.X + Pp.Y*Pp.Y + Pp.Z*Pp.Z);
    printf("%.4f %.4f\n",e[0],imp);
    fprintf(f,"%.5f  %.5f\n",e[0],imp);
   }
printf("\n");

  }
 fclose(f);      
return STAFCV_OK;
}

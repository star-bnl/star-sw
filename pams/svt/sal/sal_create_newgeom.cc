//**************************************************************************
//File: sal_create_newgeom.cc
//Description: Create new positions of the wafers rotated and translated 
//from the ideal GEANT position. 
//
//Input: svg_geom, sal_rotran, must be svg_geom
//
//Output: sal_geom_real
//************************************************************************

# include "sal_create_newgeom.h"
# include <math.h>
# include <stdlib.h>
# include <stdio.h> 
# include <time.h>
# include "svt.h"

long type_of_call sal_create_newgeom_(TABLE_HEAD_ST *config_h, SVG_CONFIG_ST *config,
                         TABLE_HEAD_ST *sal_rotran_h, SAL_ROTRAN_ST *sal_rotran,
                         TABLE_HEAD_ST    *sal_geom_real_h, SAL_GEOM_ST  *sal_geom_real )
{
 
 int i, j, k, l, s;
 float par[6], r1, r2, r3, r4, r5, r6;
 Point P2;
  
r1 = config[0].layer_radius[0]*10;
r2 = config[0].layer_radius[1]*10;
r3 = config[0].layer_radius[2]*10;
r4 = config[0].layer_radius[3]*10;
r5 = config[0].layer_radius[4]*10;
r6 = config[0].layer_radius[5]*10;

 event Event(r1, r2, r3, r4, r5, r6);
 //srand(time(0));
 
 s = 0;
 for(i = 0; i < 3; i++)
  for(l = 0; l < 2; l++)  
   for(j = 0; j < Event.Bars[i].num_lad/2; j++)
   for(k = Event.Bars[i].ladders[j*2+l].num_waf - 1; k >= 0; k--)
   {
    Event.Bars[i].PutPoint(j*2+l, k, 0.0, 0.0); 

    // [0,1,2] is in milimeters HERE and [i][3,4,5] in Rad 
    sal_geom_real[s].par[0] = rand()/(RAND_MAX/(2*sal_rotran[0].dx)) 
                                                - sal_rotran[0].dx;
    sal_geom_real[s].par[1] = rand()/(RAND_MAX/(2*sal_rotran[0].dy))  
                                                - sal_rotran[0].dy;
    sal_geom_real[s].par[2] = rand()/(RAND_MAX/(2*sal_rotran[0].dz))   
                                                - sal_rotran[0].dz;
    sal_geom_real[s].par[3] = rand()/(RAND_MAX/(2*sal_rotran[0].dthx)) 
                                                - sal_rotran[0].dthx;
    sal_geom_real[s].par[4] = rand()/(RAND_MAX/(2*sal_rotran[0].dthy)) 
                                                - sal_rotran[0].dthy;
    sal_geom_real[s].par[5] = rand()/(RAND_MAX/(2*sal_rotran[0].dthz)) 
                                                - sal_rotran[0].dthz ;

    par[0] = sal_geom_real[s].par[0];
    par[1] = sal_geom_real[s].par[1];
    par[2] = sal_geom_real[s].par[2];
    par[3] = par[4] = par[5] = 0;
    Event.Bars[i].SetParam(j*2+l, k, par);
    P2 = Event.Bars[i].GetPoint(j*2+l, k, 0);

    sal_geom_real[s].x[0] = P2.Y*0.1;
    sal_geom_real[s].x[1] = P2.Z*0.1;
    sal_geom_real[s].x[2] = P2.X*0.1;
    sal_geom_real[s].id = s;
    s ++;
   }
printf("\n ok\n");
sal_geom_real_h->nok = 216;
return STAFCV_OK;

}

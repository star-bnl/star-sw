//*********************************************************************** 
//File : sal_make_align.cc
//Description : find tracks parameters and defined the vertex coordinates
//Input: svg_config, sal_spt, sal_vrtx
//Output: sal_vrtx_find
//***********************************************************************

# include <stdio.h>
# include <stdlib.h>
# include <math.h>

# include "svt.h"
# include "sal_main.h"

long type_of_call sal_main_(TABLE_HEAD_ST *config_h, SVG_CONFIG_ST *config,
               TABLE_HEAD_ST *svt_geom_h, SVG_GEOM_ST  *svt_geom ,
               TABLE_HEAD_ST *sal_geom_real_h,  SAL_GEOM_ST *sal_geom_real,
               TABLE_HEAD_ST *g2t_svt_hit_h, G2T_SVT_HIT_ST  *g2t_svt_hit ,
               TABLE_HEAD_ST *vertex_h, SAL_VRTX_ST *vertex,
               TABLE_HEAD_ST *sal_vrtx_find_h, SAL_VRTX_ST *sal_vrtx_find,
               TABLE_HEAD_ST *sal_geom_h,  SAL_GEOM_ST *sal_geom )
{




Point P, P1, P2, P3, P4;
int iev;
long waf_id;
float r1, r2, r3, r4, r5, r6;
int p, i, j, k, l, n, pf[NEV+1];

r1 = config[0].layer_radius[0]*10;
r2 = config[0].layer_radius[1]*10;
r3 = config[0].layer_radius[2]*10;
r4 = config[0].layer_radius[3]*10;
r5 = config[0].layer_radius[4]*10;
r6 = config[0].layer_radius[5]*10;

work work(r1, r2, r3, r4, r5, r6); 
event Event(r1, r2, r3, r4, r5, r6);
//work.ClearParam();
/*
//*********************************************************************
//  Parameters installation for misalignment simulation 
//*********************************************************************
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
*/
p = 0;
for(i = 0; i < 3; i++)
 for(l = 0; l < 2; l++)  
  for(j = 0; j < work.Events[0].Bars[i].num_lad/2; j++)
   for(k = work.Events[0].Bars[i].ladders[j*2+l].num_waf-1; k >=0; k--)
     {
      work.par[i][j*2+l][k][0] = sal_geom_real[p].par[0];
      work.par[i][j*2+l][k][1] = sal_geom_real[p].par[1];
      work.par[i][j*2+l][k][2] = sal_geom_real[p].par[2];
   
      work.par[i][j*2+l][k][3] = sal_geom_real[p].par[3];
      work.par[i][j*2+l][k][4] = sal_geom_real[p].par[4];
      work.par[i][j*2+l][k][5] = sal_geom_real[p].par[5];
     p++;
     }     
work.InstallParam();


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

//**********************************************************************
//          Filling structure by hits and vertex coordinates
//**********************************************************************
//FILE *f;
//f = fopen("point.dat","w");

for(iev = 0; iev < NEV; iev++)
{
 p = pf[iev];

 P2.Y = vertex[iev].x[0]*10; 
 P2.Z = vertex[iev].x[1]*10; 
 P2.X = vertex[iev].x[2]*10;  //think about vertex!!!
 
 //fprintf(f,"%d\n",pf[iev+1]);
 
 while(p < pf[iev+1])
  {
     P.Y = g2t_svt_hit[p].x[0]*10;
     P.Z = g2t_svt_hit[p].x[1]*10;
     P.X = g2t_svt_hit[p].x[2]*10;
     waf_id  = g2t_svt_hit[p].volume_id ;
     i = (int)((waf_id/1000-1)/2);
     
 //    fprintf(f,"%d %.3f %.3f %.3f\n",i,P.X,P.Y,P.Z);     

    P1.nl = work.Events[iev].Bars[i].WhichLadder(P, P2);
    P1 = work.Events[iev].Bars[i].WhichWaffer(P, P2, P1.nl);
    work.Events[iev].Bars[i].PutPoint(P1.nl, P1.nw, P1.X, P1.Y);
    p++;
  }

  //printf("%d\n",p);
  work.ry[iev] = P2.Y;
  work.rz[iev] = P2.Z;
  work.rx[iev] = P2.X;

}
//fclose(f);
//***************************************************************
//   Installation of "measured" geometry
//***************************************************************
work.ClearParam();  
work.InstallParam();

//***********************************************************************
//   Calling of alignment procedure
//***********************************************************************
int nt = 0;
work.chi0 = 0;
//FILE *f;
//f = fopen("chi.dat","w");
 for(iev = 0; iev < NEV; iev++) 
  {
   work.Events[iev].Clear();
   work.Events[iev].FindAllTracks();
   printf("\n Total tracks found : %d\n  ", work.Events[iev].num);

   work.Events[iev].OptimizeTracks(); 
   work.Events[iev].SelectTracks();
   work.Events[iev].FindVertex();
   printf(" %.3f %.3f %.3f\n",work.Events[iev].Vertex.X,
              work.Events[iev].Vertex.Y, work.Events[iev].Vertex.Z);
   for(int q = 0; q < work.Events[iev].num; q++) 
    if ((work.Events[iev].tracks[q].w != 0)&&(work.Events[iev].tracks[q].flag==1)) 
 // if(work.Events[iev].tracks[q].flag==1)
     {
       work.chi0 += work.Events[iev].tracks[q].chi2;
       nt++;
//       fprintf(f,"%.3f\n",work.Events[iev].tracks[q].chi2);		
     }
 
 }   
//fclose(f);
work.chi0 /=nt;
printf("Initial chi = %.1f, Selected tracks  :  %d",work.chi0, nt/NEV);
 work.chi0 = 200 + fabs(work.chi0 - 200)*2.5; 
 printf("  Start %.1f\n", work.chi0);
 work.MakeAlign();

//***********************************************************************
//  Writing output
//***********************************************************************
/*for(iev = 0; iev < NEV; iev++)
{
 printf("\n Find : %.3f %.3f %.3f",work.Events[iev].Vertex.X,
              work.Events[iev].Vertex.Y,work.Events[iev].Vertex.Z);  
// printf("\n Real : %.4f %.4f %.4f\n",work.ry[iev],
//                    work.rz[iev],work.rx[iev] );
 sal_vrtx_find[iev].x[0] = work.Events[iev].Vertex.Y*0.1;
 sal_vrtx_find[iev].x[1] = work.Events[iev].Vertex.Z*0.1;
 sal_vrtx_find[iev].x[2] = work.Events[iev].Vertex.X*0.1;
}
p = 0;
for(i = 0; i < 3; i++)
 for(l = 0; l < 2; l++)  
  for(j = 0; j < Event.Bars[i].num_lad/2; j++)
   for(k = Event.Bars[i].ladders[j*2+l].num_waf - 1; k >= 0; k--)
    {
      Event.Bars[i].PutPoint(j*2+l, k, 0.0, 0.0); 
                        
      Event.Bars[i].SetParam(j*2+l, k, work.par[i][j*2+l][k]);
      P = Event.Bars[i].GetPoint(j*2+l, k, 0);
      
      sal_geom[p].x[0] = P.Y*0.1;
      sal_geom[p].x[1] = P.Z*0.1;
      sal_geom[p].x[2] = P.X*0.1;
      sal_geom[p].par[0] = work.par[i][j*2+l][k][0];  
      sal_geom[p].par[1] = work.par[i][j*2+l][k][1];
      sal_geom[p].par[2] = work.par[i][j*2+l][k][2];
      sal_geom[p].par[3] = work.par[i][j*2+l][k][3];
      sal_geom[p].par[4] = work.par[i][j*2+l][k][4];
      sal_geom[p].par[5] = work.par[i][j*2+l][k][5];
     // printf("%5.3f %5.3f %5.3f %5.3f %5.3f %5.3f\n",
     //    sal_geom[p].par[0]-sal_geom_real[p].par[0],
     //    sal_geom[p].par[1]-sal_geom_real[p].par[1],
     //    sal_geom[p].par[2]-sal_geom_real[p].par[2],
     //    sal_geom[p].par[3]-sal_geom_real[p].par[3],
     //    sal_geom[p].par[4]-sal_geom_real[p].par[4],
     //    sal_geom[p].par[5]-sal_geom_real[p].par[5]);
     // 
      sal_geom[p].id = p;
      p++; 
    }
*/
sal_geom_h->nok = p;
//sal_vrtx_find_h->nok = iev;
return STAFCV_OK;
}


#include "StFgtCosmicAlignment.h"
#include <math.h>
const int MaxDisc=3;
//initial z offsets for disks
float z0[MaxDisc]={0.0, 16.51, 2.0*16.51};
//center of rotaion
float x00=2.0, y00=-26.0, z00=0.0;
//Alignment xyz offsets and Euler angles for each disc
float par[MaxDisc][6]= { {0.0, 0.0, 0.0, 0.0, 0.0, 0.0},
                         {-0.0314,-0.1427, 0.1298, -0.0746,  0.0247, -0.0311},
                         {0.0228, 0.3543, 0.0815, -0.1006, -0.0110, -0.0372}}; 


void rot(float x, float y, float z,
         float x0,float y0,float z0,
         float o, float p, float q,
         float &xx,float &yy,float &zz){
  //printf("inp = %8.3f %8.3f %8.3f\n",x,y,z);
  //printf("off = %8.3f %8.3f %8.3f\n",x0,y0,z0);
  float co=cos(o), so=sin(o);
  float cp=cos(p), sp=sin(p);
  float cq=cos(q), sq=sin(q);
  float x1=x+x0-x00;
  float y1=y+y0-y00;
  float z1=z+z0-z00;
  //printf("opq  = %8.3f %8.3f %8.3f\n",o,p,q);
  //printf("out1 = %8.3f %8.3f %8.3f\n",xx,yy,zz);
  //printf("cos  = %8.3f %8.3f %8.3f\n",co,cp,cq);
  //printf("sin  = %8.3f %8.3f %8.3f\n",so,sp,sq);
  xx=x1*co*cp + y1*(-cq*sp+sq*so*cp) + z1*( sq*sp+cq*so*cp) + x00;
  yy=x1*co*sp + y1*( cq*cp+sq*so*sp) + z1*(-sq*cp+cq*so*sp) + y00;
  zz=x1*(-so) + y1*(       sq*co   ) + z1*(       cq*co   ) + z00;
  //printf("out2 = %8.3f %8.3f %8.3f\n",xx,yy,zz);
};
void getAlign(int idisc, float pl, float rl, float &x, float &y, float &z, float &p, float &r){
  float xx=rl * cos(pl);
  float yy=rl * sin(pl);      
  rot(xx,yy,0.0,par[idisc][0],par[idisc][1],par[idisc][2],par[idisc][3],par[idisc][4],par[idisc][5],x,y,z);
  z+=z0[idisc];      
  r=sqrt(x*x + y*y);
  p=atan2(y,x);    
};

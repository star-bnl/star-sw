#include <stdio.h>

void getCellPosition2015pA(int nstb, int row, int col, float &x1, float &y1, float &x2, float &y2, float &z){
  const float large_width = 58.0; // large cell width
  const float small_width = 38.0; // small cell width
  float x=0.0, y=0.0;
  switch(nstb)
    {
    case 1:
      x = -24.0 - 25.0 - 58.6 * col;
      y = 965.5 - 58.5 * row;
      z = 7208;
      break;
    case 2:
      x = -24.0 - 8.0 + 66.6 + 58.4 * col; //new position May 5 2015
      y = 965.1 - 58.6 * row;
      z = 7188;
      break;
    case 3:
      if(row>16.5)
        {
          x = -22.0 - 17.7 - 38.2 * col;
          y = 422.7 - 37.9 * row;
          z = 7208;
        }
      else
        {
          x = -22.0 - 17.7 - 38.2 * col;
          y = 4.75 + 438.9 - 39.0 * row;
          z = 7204;
        };
      break;
    case 4:
      x = -22.0 - 8.0 + 60. + 38.4 * col; // new position May 5 2015
      y = 444. - 38.7 * row;
      z = 7188;
      break;
    };
  
  if(nstb<=2) 
    {
      x1 = x - large_width/2.0;
      x2 = x + large_width/2.0;
      y1 = y - large_width/2.0;
      y2 = y + large_width/2.0;
    }
  else
    {
      x1 = x - small_width/2.0;
      x2 = x + small_width/2.0;
      y1 = y - small_width/2.0;
      y2 = y + small_width/2.0;
    };
  
  x1/=10.0;
  y1/=10.0;
  x2/=10.0;
  y2/=10.0;
  z/=10.0;
  //  printf("nstb=%d row=%3d col=%3d  xyz=%8.2f %8.2f %8.2f\n",nstb,row,col,x,y,z);
}
  

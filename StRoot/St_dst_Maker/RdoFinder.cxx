// $Id: RdoFinder.cxx,v 1.2 2002/02/19 16:07:56 wdeng Exp $
// $Log: RdoFinder.cxx,v $
// Revision 1.2  2002/02/19 16:07:56  wdeng
// cvs control strings.
//

#include <iostream.h>
#include <math.h>

#include "RdoFinder.h"

#include "tables/St_dst_track_Table.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#include "StHelixD.hh"

RdoFinder* RdoFinder::mRdoFinder=0;

RdoFinder* RdoFinder::Instance() {
  if(!mRdoFinder) mRdoFinder = new RdoFinder();
  return mRdoFinder;
}

RdoFinder::RdoFinder():avg_gap(3.8*cos(M_PI/12)){}

int RdoFinder::PossiblePoints(dst_track_st *dstTrack){
  //This function calculates the number of possible point from the track geometry 

  float dip   = atan(dstTrack->tanl);
  int    h    = ((magneticField * dstTrack->icharge) > 0 ? -1 : 1);
  float phase = dstTrack->psi*degree-h*pi/2;
  float curvature = dstTrack->curvature;
  float x0 = dstTrack->r0 * cos(dstTrack->phi0 * degree);
  float y0 = dstTrack->r0 * sin(dstTrack->phi0 * degree);
  float z0 = dstTrack->z0;
  StThreeVectorD origin(x0, y0, z0);  
  StHelixD trackHelix(curvature, dip, phase, origin, h);

  int MAX_Z = 195;
  if(MAX_Z < fabs(z0)) MAX_Z = abs(z0) + 5;

  float first_x = x0;
  float first_y = y0;
  float first_z = z0;

  float temp_x,temp_y;
  float last_x,last_y,last_z;

  float center_x = trackHelix.xcenter();
  float center_y = trackHelix.ycenter();

  float radius = 1/trackHelix.curvature();

  if(RowCrossings(center_x,center_y,radius,59.5,&first_x,&first_y,1) &&
     RowCrossings(center_x,center_y,radius,59.5,&temp_x,&temp_y,2)){
    if(((first_x-x0)*(first_x-x0)+(first_y-y0)*(first_y-y0))>
       ((temp_x-x0)*(temp_x-x0)+(temp_y-y0)*(temp_y-y0))){
      first_x = temp_x;
      first_y = temp_y;
    }
  }else{
    float center_ang = atan2(center_y,center_x);
    
    if(x0*center_y - y0*center_x > 0){
      //set minimum crossing angle is 5 degrees
      first_x = center_x + radius*cos(center_ang + M_PI + M_PI/36);
      first_y = center_y + radius*sin(center_ang + M_PI + M_PI/36);
    }else{
      first_x = center_x + radius*cos(center_ang + M_PI - M_PI/36);
      first_y = center_y + radius*sin(center_ang + M_PI - M_PI/36);
    }
  }
   
  float temp = sqrt((first_x-x0)*(first_x-x0)+
		    (first_y-y0)*(first_y-y0))/2/radius;
  float temp_ang = (temp<1) ? 2*asin(temp) : M_PI;
  int outward    = (Local_y(x0,y0) < Local_y(first_x,first_y)) ? 1:-1;
  first_z        = z0 + outward*temp_ang*radius*tan(dip); 

  if(fabs(first_z) > MAX_Z){  // flew out the end
    float ang_z = 0;
    
    if(dip>0) ang_z = (z0 + MAX_Z)/(radius*fabs(tan(dip)));
    else      ang_z = (MAX_Z - z0)/(radius*fabs(tan(dip)));
    first_z = z0 - ang_z*radius*tan(dip);

    float ang_start = atan2(y0-center_y,x0-center_x);

    if(x0*center_y - y0*center_x > 0) ang_z = -ang_z;
    first_x = center_x + radius*cos(ang_start + ang_z);
    first_y = center_y + radius*sin(ang_start + ang_z);
  }

  if(RowCrossings(center_x,center_y,radius,190.195,&last_x,&last_y,1) &&
     RowCrossings(center_x,center_y,radius,190.195,&temp_x,&temp_y,2)){
    if(((last_x-x0)*(last_x-x0)+(last_y-y0)*(last_y-y0))>
       ((temp_x-x0)*(temp_x-x0)+(temp_y-y0)*(temp_y-y0))){
      last_x = temp_x;
      last_y = temp_y;
    }
  }else{
    float center_ang = atan2(center_y,center_x);

      if(x0*center_y - y0*center_x < 0){ 
      //set minimum crossing angle is 5 degrees
      last_x = center_x + radius*cos(center_ang + M_PI/36);
      last_y = center_y + radius*sin(center_ang + M_PI/36);
    }else{
      last_x = center_x + radius*cos(center_ang - M_PI/36);
      last_y = center_y + radius*sin(center_ang - M_PI/36);
    }
  }

  temp = sqrt((last_x-x0)*(last_x-x0)+
	      (last_y-y0)*(last_y-y0))/2/radius;
  temp_ang = (temp<1) ? 2*asin(temp) : M_PI;
  outward  = (Local_y(x0,y0) < Local_y(last_x,last_y)) ? 1:-1;
  last_z   = z0 + outward*temp_ang*radius*tan(dip); 
    
  if(fabs(last_z) > MAX_Z){  // flew out the end
    float ang_z = 0;
    
    if(dip>0) ang_z = (MAX_Z - z0)/(radius*fabs(tan(dip)));
    else      ang_z = (z0 + MAX_Z)/(radius*fabs(tan(dip)));
    last_z = z0 + ang_z*radius*tan(dip);
    float ang_start = atan2(y0-center_y,x0-center_x);

    if(x0*center_y - y0*center_x < 0) ang_z = -ang_z;
    last_x = center_x + radius*cos(ang_start + ang_z);
    last_y = center_y + radius*sin(ang_start + ang_z);

  }

  int first_row = Row(first_x,first_y); 
  int last_row  = Row(last_x,last_y);
  

  int n_poss = last_row - first_row + 1 - 
    nRowsInSectorGaps(first_x,first_y,center_x,center_y,last_x,last_y);

  if(n_poss<1) return -1;
  return n_poss;  
}

int RdoFinder::BinarySubset(int a,int b,int n){
  //Is "a" a binary subset of "b" in the first right n places?
  // returns 2 in the case a==b, and 1 if "a" is a subset of "b",otherwise 0
  //masking the Geometry/Hits information bit
  a |= (1<<28);
  b |= (1<<28);

  if(a==b) return 2;
  for(int i=0;i<n;i++){if((a&(1<<i)) > (b&(1<<i))) return 0;}
  return 1;
}

void RdoFinder::PrintResults(int rdoFinderInt){
  //The function is used to display the results of rdo integers.

  //bit 24 is not used and is 0
  //
  //bits 25,26 and 27 code  000->inners 001->outers 010->rdos1or2
  //                        011->rds3   100->rdo4   101->rdo5   110->rdo6
  //bit  28 states how the information was obtained 0->track geometry
  //                                                1->hits on the track
  cout<<endl<<"+++++++++++++++++++++++++++++++++++++++++"<<endl;
  cout<<"The following infromation was obtained from ";
  if(rdoFinderInt&(1<<28)) cout<<"the hits on the track."<<endl;
  else cout<<"the track geometry."<<endl;

  if(rdoFinderInt&(1<<26)||rdoFinderInt&(1<<27)){
    int rdo = 0;
    rdo += (rdoFinderInt&(1<<25)) ? 1:0;
    rdo += (rdoFinderInt&(1<<26)) ? 2:0;
    rdo += (rdoFinderInt&(1<<27)) ? 4:0;

    cout<<"  Information for RD0";
    if(rdo==2) cout<<"s 1 or 2."<<endl;
    else       cout<<" "<<rdo<<"."<<endl;
  }else{
    if(rdoFinderInt&(1<<25)) cout<<"  Information for outer sectors."<<endl;
    else                     cout<<"  Information for inner sectors."<<endl;
  }

  cout<<"Sectors 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24"<<endl<<"        ";
  for(int i=0;i<24;i++){
    if(i>8) cout<<" ";
    cout<<(int)((rdoFinderInt&(1<<i))>0)<<" ";
  }
  
  cout<<endl<<"+++++++++++++++++++++++++++++++++++++++++"<<endl;

}

//private function



int RdoFinder::nRowsInSectorGaps(float x_first,float y_first,float x_cent,float y_cent,float x_last,float y_last){
  //This funtions calculate the number of hits that are in sector gaps

  int exclude_nrows = 0;
  if(Row(x_first,y_first)>Row(x_last,y_last)){
    float tx = x_first;
    float ty = y_first;
    x_first = x_last;   y_first = y_last;
    x_last = tx;        y_last = ty;
  }
  
  int first_row  = Row(x_first,y_first) - 1;
  int last_row   = Row(x_last,y_last);

  float radius = sqrt((x_first - x_cent)*(x_first - x_cent) +
		       (y_first - y_cent)*(y_first - y_cent)  );
  
  for(int i=0;i<12;i++){ // 0 is the 1-2 boundary
    float division_ang = M_PI/4 - i*M_PI/6;

  //-M_PI/2 for the rotation
    float x_c =  x_cent*cos(division_ang - M_PI/2) + 
                             y_cent*sin(division_ang - M_PI/2);
    float y_c = -x_cent*sin(division_ang - M_PI/2) + 
                             y_cent*cos(division_ang - M_PI/2);

    float local_first_x =  x_first*cos(division_ang - M_PI/2) + 
                           y_first*sin(division_ang - M_PI/2);
    float local_first_y = -x_first*sin(division_ang - M_PI/2) + 
                           y_first*cos(division_ang - M_PI/2);

    int first_in_gap = 0;
    if(fabs(local_first_x)<avg_gap && local_first_y>60 && local_first_y<190.195) first_in_gap = 1;

    float tempx1 = local_first_x - x_c; 
    float tempy1 = local_first_y - y_c;

    float tempx2 =  x_last*cos(division_ang - M_PI/2)  + 
      y_last*sin(division_ang - M_PI/2)  - x_c;
    float tempy2 = -x_last*sin(division_ang - M_PI/2)  + 
      y_last*cos(division_ang - M_PI/2)  - y_c;

    float x_cross[4];
    float local_y_cross[4];
    int inner_row_cross[4];

    for(int j=0;j<4;j++){
      if(j<2){ x_cross[j] = (x_c>0) ? x_c - avg_gap : x_c + avg_gap;} 
                                       // move to account for gap
      else{    x_cross[j] = (x_c>0) ? x_c + avg_gap : x_c - avg_gap;}
                                      // far gap
      local_y_cross[j] = 0;
      inner_row_cross[j] = 0;

      float y_dist = radius*radius - x_cross[j]*x_cross[j];
      if(y_dist<0) continue;
      y_dist = sqrt(y_dist);

      local_y_cross[j] = y_c;
      local_y_cross[j] += (j%2) ? y_dist : -y_dist;
      local_y_cross[j] *= cos(M_PI/12);
      if(local_y_cross[j]<60 || local_y_cross[j]>190.195) continue; 
      
      float tempx0 = -x_cross[j];
      float tempy0 = (j%2) ? y_dist : -y_dist;

     //is it between first and last point????
      //this assumes that the track travels less than 180 degrees
      //assumption validy if function called from PossiblePoints
          if((tempx1*tempy0 - tempy1*tempx0)*
	     (tempx0*tempy2 - tempy0*tempx2) < 0) continue;

	  float temp1 =sqrt((tempx1-tempx0)*(tempx1-tempx0)+ 
			    (tempy1-tempy0)*(tempy1-tempy0) )/2/radius;
	  temp1 = (temp1<1) ? 2*asin(temp1) : M_PI;
	  float temp2 =sqrt((tempx2-tempx0)*(tempx2-tempx0)+
			    (tempy2-tempy0)*(tempy2-tempy0) )/2/radius;
	  temp2 = (temp2<1) ? 2*asin(temp2) : M_PI;
	  if(temp1 + temp2 > M_PI) continue;
	  
      if(local_y_cross[j]>93.6) inner_row_cross[j] = 8;
      else{
	inner_row_cross[j] = (int)((local_y_cross[j] - 60)/4.8) + 1; 
	continue;
      }
      if(local_y_cross[j]<98.8) continue;
      if(local_y_cross[j]>119.6) inner_row_cross[j] += 5;
      else{inner_row_cross[j] += (int)((local_y_cross[j] - 98.8)/5.2)+1; continue;} 
      
      if(local_y_cross[j]<126.195) continue;
      else{
	inner_row_cross[j] += (int)((local_y_cross[j] - 126.195)/2)+1;
	continue;
      } 
    }
    
    if(!(inner_row_cross[0]||inner_row_cross[1]||
	 inner_row_cross[2]||inner_row_cross[3]) ) continue;

    if(inner_row_cross[0] && !first_in_gap){
      if(inner_row_cross[2]){ 
	exclude_nrows += inner_row_cross[2]-inner_row_cross[0];
      }
      else if(inner_row_cross[1]){
	exclude_nrows += inner_row_cross[1]-inner_row_cross[0];
	continue;
      }
      else if(inner_row_cross[3]){
	cout<<"TROUBLE LOCAL Y CROSS 0-3??? "<<endl; 
	continue;
      }
      else{
	exclude_nrows += last_row - inner_row_cross[0];
	continue;
      }
    }

    if(inner_row_cross[1]&&!inner_row_cross[0]&&
       !inner_row_cross[2]&&!inner_row_cross[3]){
      exclude_nrows += inner_row_cross[1] - first_row;
    }
    
    if(inner_row_cross[2]&&!inner_row_cross[0]){
      exclude_nrows += inner_row_cross[2] - first_row;
    }
 
    if(inner_row_cross[3]){
      if(inner_row_cross[1]){
	exclude_nrows += inner_row_cross[1] - inner_row_cross[3];
      }
      else{
	exclude_nrows += last_row - inner_row_cross[3];
      }
      continue;
    }
  }
  
  return exclude_nrows;
}


int RdoFinder::CageCrossings(float x_cent, float y_cent, float radius, float cage_radius, float* x_out,float* y_out, int first_or_second){
  //1 is returned if the cage_radius is crossed, otherwise returns 0
  //if 1 is return the first or second crossings "x" and "y" are 
  //returned through x_out and y_out respectively

  *x_out = *y_out = 0;
  
  float r_cent = sqrt(x_cent*x_cent+y_cent*y_cent);
  if(cage_radius<r_cent-radius||cage_radius>r_cent+radius) return 0;

  float x_prim = (cage_radius*cage_radius - radius*radius + r_cent*r_cent)/(2*r_cent);
  float y_prim = sqrt(cage_radius*cage_radius - x_prim*x_prim);
  if(first_or_second == 1) y_prim =- y_prim;

  (*x_out) = x_prim*x_cent/r_cent - y_prim*y_cent/r_cent;
  (*y_out) = x_prim*y_cent/r_cent + y_prim*x_cent/r_cent;
    
  return 1;
}

int RdoFinder::RowCrossings(float x_cent, float y_cent, float radius, float y_row, float* x_out,float* y_out, int first_or_second){
  //1 is returned if the row is crossed, otherwise returns 0
  //if 1 is return the first or second crossings "x" and "y" are 
  //returned through x_out and y_out respectively

  *x_out = *y_out = 0;

  if(y_row<0){return 0;}
  float yc_local = Local_y(x_cent,y_cent);
  if(fabs(yc_local-y_row)>radius) return 0;

  float dx_row = y_row*tan(M_PI/12);
  int first_found = 0;

  for(float i=1;i<13;i++){
    float sector_ang = -i*M_PI/6;
    float lx_c = x_cent*cos(sector_ang) + y_cent*sin(sector_ang);
    float ly_c = y_cent*cos(sector_ang) - x_cent*sin(sector_ang);
    float dy   = fabs(y_row-ly_c); 
    if(dy > radius) continue;

    float dx = sqrt(radius*radius - dy*dy);
    float x1 = lx_c + dx;
    float x2 = lx_c - dx;
    
    if(fabs(x1)<dx_row && fabs(x2)<dx_row){
      if(first_or_second==1){
	*x_out = x1*cos(sector_ang)    - y_row*sin(sector_ang);
	*y_out = y_row*cos(sector_ang) + x1*sin(sector_ang);
	return 1;
      }else{
	*x_out = x2*cos(sector_ang)    - y_row*sin(sector_ang);
	*y_out = y_row*cos(sector_ang) + x2*sin(sector_ang);	
        return 1;
      }
    }else if(fabs(x1)<dx_row || fabs(x2)<dx_row){
      if(first_or_second==1||first_found){
	if(fabs(x1)<dx_row){
	  *x_out = x1*cos(sector_ang)    - y_row*sin(sector_ang);
	  *y_out = y_row*cos(sector_ang) + x1*sin(sector_ang);
	  return 1;
	}else{
	  *x_out = x2*cos(sector_ang)    - y_row*sin(sector_ang);
	  *y_out = y_row*cos(sector_ang) + x2*sin(sector_ang);
	  return 1;
	}
      }
      first_found = 1;
    }
  }

  return 0;
}


int RdoFinder::FindSectorCrossings(float x_first,float y_first,float x_cent,float y_cent,float x_last,float y_last,float r_first_row,float r_last_row){
  //returns all sector crossed into between r_first_row and r_last_row

  int crossings = 0;
  if(Row(x_first,y_first)>Row(x_last,y_last)){
    float tx = x_first;
    float ty = y_first;
    x_first = x_last;   y_first = y_last;
    x_last = tx;        y_last = ty;
  }
  
  float radius = sqrt((x_first - x_cent)*(x_first - x_cent) +
		       (y_first - y_cent)*(y_first - y_cent)  );

  for(int i=0;i<12;i++){ // 0 is the 1-2 boundary
    float division_ang = M_PI/4 - i*M_PI/6;

  //-M_PI/2 for the rotation
    float x_c =  x_cent*cos(division_ang - M_PI/2) + 
                  y_cent*sin(division_ang - M_PI/2);
    if(fabs(x_c)>radius) continue;
    float delta_y = sqrt(radius*radius - x_c*x_c);
    float y_c = -x_cent*sin(division_ang - M_PI/2) + 
                  y_cent*cos(division_ang - M_PI/2);

    for(int j=0;j<4;j++){  // in center of circle frame
      float y_cross = j%2 ? delta_y:-delta_y;
      if(y_c+y_cross<r_first_row || y_c+y_cross>r_last_row) continue;

      float x_cross = -x_c + ((j<2) ? -avg_gap:avg_gap);
      float l_x1 =  x_first*cos(division_ang - M_PI/2) + 
	             y_first*sin(division_ang - M_PI/2) - x_c;
      float l_y1 = -x_first*sin(division_ang - M_PI/2) + 
                     y_first*cos(division_ang - M_PI/2) - y_c;
      float l_x2 =  x_last*cos(division_ang - M_PI/2) + 
                     y_last*sin(division_ang - M_PI/2) - x_c;
      float l_y2 = -x_last*sin(division_ang - M_PI/2) + 
                     y_last*cos(division_ang - M_PI/2) - y_c;

    //is it between first and last point????
    //this assumes that the track travels less than 180 degrees
    //assumption validy if function called from InSector(StTrack*)
      if((l_x1*y_cross-l_y1*x_cross)*(x_cross*l_y2-y_cross*l_x2) < 0) continue;
      float temp1 = sqrt((l_x1-x_cross)*(l_x1-x_cross)+ 
			 (l_y1-y_cross)*(l_y1-y_cross) )/2/radius;
      temp1 = (temp1<1) ? 2*asin(temp1) : M_PI;
      float temp2 = sqrt((l_x2-x_cross)*(l_x2-x_cross)+
			 (l_y2-y_cross)*(l_y2-y_cross) )/2/radius;
      temp2 = (temp2<1) ? 2*asin(temp2) : M_PI;
      if(temp1 + temp2 > M_PI) continue;
 
      if(i!=11) crossings |= (j<2) ? 1<<i:1<<i+1;
      else      crossings |= (j<2) ? 1<<i:1<<0;
    }
  }

  return crossings;
}

int RdoFinder::Sector(float x,float y,float z){
  // this function return the sector of x,y,z

  float ang = atan2(y,x)/M_PI*180 - 75;
  if(ang<0) ang+=360;

  int sec = 12 - ((int) ang/30);
  if(z>0) return sec;
  if(sec<12) return 24 - sec;
  return 24;
}

int RdoFinder::Row(float x, float y){
  //returns the row give global x,y

  float dist_local_y = Local_y(x,y);
  if(dist_local_y > 189 ) return 45;
  if(dist_local_y < 62.4 ) return 1;
  if(dist_local_y <  96  ) return (int)((dist_local_y - 62.4)/4.8) + 2;
  if(dist_local_y <  96.2) return 8;
  if(dist_local_y < 122.2) return (int)((dist_local_y - 96.2)/5.2) + 9;
  if(dist_local_y < 127) return 14;
  return (int)((dist_local_y - 126.195)/2) + 14;
}

float RdoFinder::Local_y(float x,float y){
  //returns local_y from global x,y
 
  float rot_ang = M_PI/3 - (Sector(x,y,10) - 1)*M_PI/6; 
  return (x*cos(rot_ang) + y*sin(rot_ang));
}

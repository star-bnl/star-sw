#include <Stiostream.h>
#include <math.h>

#include "RdoFinder.h"

#include "tables/St_dst_track_Table.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#include "StHelixD.hh"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"

#include "StEventTypes.h"
#include "StEventMaker/StEventMaker.h"

RdoFinder* RdoFinder::mRdoFinder=0;

RdoFinder* RdoFinder::Instance() {
  if(!mRdoFinder) mRdoFinder = new RdoFinder();
  return mRdoFinder;
}


RdoFinder::RdoFinder():avg_gap(1.65){
  OuterOffRdos[0]=OuterOffRdos[1]=OuterOffRdos[2]=OuterOffRdos[3]=0;
  OuterRdoRs[0]=126.195;OuterRdoRs[1]=142.195;OuterRdoRs[2]=158.195;
  OuterRdoRs[3]=174.195;OuterRdoRs[4]=190.195;

  cout<<"RdoFinder Warning: Number of possible points excludes row 13."<<endl; 
}

void RdoFinder::setMagneticField(float bField){
  magneticField=bField;

  static StDetectorDbTpcRDOMasks* mask;
  if(!mask) mask = StDetectorDbTpcRDOMasks::instance();  
  for(int rdo=0;rdo<4;rdo++){
    OuterOffRdos[rdo] = 0;
    for(int i=0;i<24;i++) if(!mask->isOn(i+1,rdo+3)) OuterOffRdos[rdo] |= (1<<i);
  }
}

int RdoFinder::PossiblePoints(dst_track_st *t){
  //This function calculates the number of possible point from
  //the track geometry 
  return nRowsCrossed(t,0,0,0) - removeOffRdos(t);
}

int RdoFinder::PossiblePoints(StTrack *t){
  //This function calculates the number of possible point from
  //the track geometry 
  return nRowsCrossed(t,0,0,0) - removeOffRdos(t);
}

int RdoFinder::removeOffRdos(dst_track_st *t){
  int sum=0;
  for(int rdo=0;rdo<4;rdo++){
    if(!OuterOffRdos[rdo]) continue;
    for(int i=0;i<24;i++){ 
      if(OuterOffRdos[rdo] & (1<<i)){
	int temp_sum=nRowsCrossed(t,OuterRdoRs[rdo]+.1,OuterRdoRs[rdo+1]-.1,i+1);
	//if(temp_sum>8||temp_sum<0){cout<<"trouble with temp sum"<<endl;}
	if(temp_sum>8)      sum+=8;
	else if(temp_sum>0) sum+=temp_sum;
      }
    }
  }
  
  return sum;
}

int RdoFinder::removeOffRdos(StTrack* t){
  int sum=0;
  for(int rdo=0;rdo<4;rdo++){
    if(!OuterOffRdos[rdo]) continue;
    for(int i=0;i<24;i++){ 
      if(OuterOffRdos[rdo] & (1<<i)){
	int temp_sum=nRowsCrossed(t,OuterRdoRs[rdo]+.1,OuterRdoRs[rdo+1]-.1,i+1);
	//if(temp_sum>8||temp_sum<0){cout<<"trouble with temp sum"<<endl;}
	if(temp_sum>8)      sum+=8;
	else if(temp_sum>0) sum+=temp_sum;
      }
    }
  }
  
  return sum;
}

int RdoFinder::nRowsCrossed(dst_track_st *t,float r_first_row, float r_last_row,int sector){
  int last_cross=0;

  float ends[11];
  if(!FindEndPoints(t,ends)) return 0;

  if(sector){
    float p[11];
    if(!FindPointsBetween(t,p,r_first_row,r_last_row,ends)) return 0;

    float z=p[2];
    if(z*p[5]<0){
      if((sector<=12&&z>0)||(sector>12&&z<0)){p[3]=p[6];p[4]=p[7];}
      else                                   {p[0]=p[6];p[1]=p[7];z=p[5];}
    }else if(sector<=12){if(z<0) return 0;
    }else               {if(z>0) return 0;}
    
    int pos_sector=sector>12?24-sector:sector;
    if(sector!=Sector(p[0],p[1],z)||InGap(p)){
      if(!GotoNextCross(p,&last_cross,pos_sector)) return 0;
    }
    //if(sector!=Sector(p[0],p[1],z)){cout<<"Wrong Sector: "<<Sector(p[0],p[1],z)<<endl;return 0;}
    
    float t_x=p[0],t_y=p[1]; 
    if(!GotoNextCross(p,&last_cross,pos_sector)){
      return Row(p[3],p[4]) - Row(p[0],p[1]) + 1;
    }
    return Row(p[0],p[1]) - Row(t_x,t_y) + 1;
  }else if(InGap(ends)){ 
    if(!GetOutOfGap(ends,&last_cross)){
      //cout<<"Never left the gap"<<endl;
      return 0;
    }
  }
  
  int add=0;
  int i=0;
  float x_temp=ends[0],y_temp=ends[1];
  while(GotoNextCross(ends,&last_cross,Sector(ends[0],ends[1],10))){

    //Hard Wire Removal of Row 13
    if(Row(ends[0],ends[1])>=13&&Row(x_temp,y_temp)<=13){ add--; }

    add += Row(ends[0],ends[1]) - Row(x_temp,y_temp) + 1;

    x_temp=ends[0],y_temp=ends[1];
    if(!GetOutOfGap(ends,&last_cross)){ return add; }

    if(Row(ends[0],ends[1])==Row(x_temp,y_temp)){ add--; }
    x_temp=ends[0],y_temp=ends[1];

    if(i++>25){
      cout<<"Hit troubled waters"<<endl;
      break;
    }
  }

  //Hard Wire Removal of Row 13
  if(Row(ends[3],ends[4])>=13&&Row(ends[0],ends[1])<=13){ add--; }
  return add + Row(ends[3],ends[4]) - Row(ends[0],ends[1]) + 1;
}

int RdoFinder::nRowsCrossed(StTrack* t,float r_first_row, float r_last_row,int sector){
  int last_cross=0;

  float ends[11];
  if(!FindEndPoints(t,ends)) return 0;

  if(sector){
    float p[11];
    if(!FindPointsBetween(t,p,r_first_row,r_last_row,ends)) return 0;

    float z=p[2];
    if(z*p[5]<0){
      if((sector<=12&&z>0)||(sector>12&&z<0)){p[3]=p[6];p[4]=p[7];}
      else                                   {p[0]=p[6];p[1]=p[7];z=p[5];}
    }else if(sector<=12){if(z<0) return 0;
    }else               {if(z>0) return 0;}
    
    int pos_sector=sector>12?24-sector:sector;
    if(sector!=Sector(p[0],p[1],z)||InGap(p)){
      if(!GotoNextCross(p,&last_cross,pos_sector)) return 0;
    }
    //if(sector!=Sector(p[0],p[1],z)){cout<<"Wrong Sector: "<<Sector(p[0],p[1],z)<<endl;return 0;}
    
    float t_x=p[0],t_y=p[1]; 
    if(!GotoNextCross(p,&last_cross,pos_sector)){
      return Row(p[3],p[4]) - Row(p[0],p[1]) + 1;
    }
    return Row(p[0],p[1]) - Row(t_x,t_y) + 1;
  }else if(InGap(ends)){ 
    if(!GetOutOfGap(ends,&last_cross)){
      //cout<<"Never left the gap"<<endl;
      return 0;
    }
  }
  
  int add=0;
  int i=0;
  float x_temp=ends[0],y_temp=ends[1];
  while(GotoNextCross(ends,&last_cross,Sector(ends[0],ends[1],10))){

    //Hard Wire Removal of Row 13
    if(Row(ends[0],ends[1])>=13&&Row(x_temp,y_temp)<=13){ add--; }

    add += Row(ends[0],ends[1]) - Row(x_temp,y_temp) + 1;

    x_temp=ends[0],y_temp=ends[1];
    if(!GetOutOfGap(ends,&last_cross)){ return add; }

    if(Row(ends[0],ends[1])==Row(x_temp,y_temp)){ add--; }
    x_temp=ends[0],y_temp=ends[1];

    if(i++>25){
      cout<<"Hit troubled waters"<<endl;
      break;
    }
  }

  //Hard Wire Removal of Row 13
  if(Row(ends[3],ends[4])>=13&&Row(ends[0],ends[1])<=13){ add--; }
  return add + Row(ends[3],ends[4]) - Row(ends[0],ends[1]) + 1;
}

int RdoFinder::InGap(float *p){
  int sec = Sector(p[0],p[1],10);
  for(int i=1;i<=2;i++){
    float division_ang = M_PI/4 - (sec-i)*M_PI/6;
    if(fabs(p[0]*cos(division_ang - M_PI/2) + p[1]*sin(division_ang - M_PI/2)) < avg_gap) return 1;
  }

  return 0;
}

int RdoFinder::GetOutOfGap(float *p,int *last_cross){
  int found = 0;
  int   local_last=*last_cross;
  float x_temp,y_temp,x_orig=p[0],y_orig=p[1];
  
  if(GotoNextCross(p,&local_last,Sector(p[0],p[1],10)-1)){
    found=local_last;x_temp=p[0];y_temp=p[1];
  }

  local_last=*last_cross;p[0]=x_orig;p[1]=y_orig;
  if(GotoNextCross(p,&local_last,Sector(p[0],p[1],10))){
    if(!found||(found&&Local_y(p[0],p[1])<Local_y(x_temp,y_temp))){
      found=local_last;x_temp=p[0];y_temp=p[1];
    }
  }
  
  local_last=*last_cross;p[0]=x_orig;p[1]=y_orig;
  if(GotoNextCross(p,&local_last,Sector(p[0],p[1],10)+1)){
    if(!found||(found&&Local_y(p[0],p[1])<Local_y(x_temp,y_temp))){
      found=local_last;x_temp=p[0];y_temp=p[1];
    }
  }
  
  if(!found) return 0;
  p[0]=x_temp;p[1]=y_temp;
  return *last_cross=found;
}

int RdoFinder::GotoNextCross(float *p,int *last_cross,int pos_side_sector){
  // positive_side_sector is necessary for 
  // y_cross<0 continue to be consistant with division angle 

  int found=0;
  float x_found=0,y_found=0,y_local=0;
  
  for(int i=1;i<=2;i++){
    float division_ang = M_PI/4 - (pos_side_sector-i)*M_PI/6;
    float x_cross;
    if(i==1)  x_cross = -avg_gap;
    else      x_cross = +avg_gap;
    
    //-M_PI/2 for the rotation
    float x_c =  p[8]*cos(division_ang - M_PI/2) + 
                 p[9]*sin(division_ang - M_PI/2);

    if(fabs(x_c-x_cross)>p[10]) continue;
    float delta_y = ::sqrt(p[10]*p[10] - (x_c-x_cross)*(x_c-x_cross));
    float y_c = -p[8]*sin(division_ang - M_PI/2) + 
                 p[9]*cos(division_ang - M_PI/2);
    for(int j=0;j<2;j++){
      int new_cross=j*2+i;
      if(new_cross==*last_cross) continue;
      float y_cross = (j==0) ? y_c+delta_y:y_c-delta_y;
      if(y_cross<0) continue;
      float x_new =  x_cross*cos(division_ang - M_PI/2)  
	            -y_cross*sin(division_ang - M_PI/2); //sin(-t) -> -sin(t)
      float y_new =  x_cross*sin(division_ang - M_PI/2) + 
	             y_cross*cos(division_ang - M_PI/2);
      if(!IsItBetween(x_new,y_new,p)) continue;

      float new_ly = Local_y(x_new,y_new);
      if(!found||new_ly<y_local){
	found=new_cross;
	y_local=new_ly;
	x_found = x_new;
	y_found = y_new;
      }
    }
  }
  
  if(!found) return 0; 
  p[0]=x_found;p[1]=y_found;
  return *last_cross=found;
}

int RdoFinder::FindEndPoints(dst_track_st *t,float *p){
  return FindPointsBetween(t,p,59,192,0);
}
int RdoFinder::FindEndPoints(StTrack *t,float *p){
  return FindPointsBetween(t,p,59,192,0);
}

int RdoFinder::FindPointsBetween(dst_track_st *t,float *p,float r_first_row, float r_last_row, float* limits){
  //returns 1 if crossing points are found between limits, otherwise 0
  //  p[0]=first_x;p[1]=first_y;p[2]=first_z;
  //  p[3]=last_x;p[4]=last_y;p[5]=last_z;
  //  p[6]=mem_x;p[7]=mem_y;
  //  p[8]=center_x;p[9]=center_y;
  //  p[10]=radius;
  
  float dip   = atan(t->tanl);
  int    h    = ((magneticField * t->icharge) > 0 ? -1 : 1);
  float phase = t->psi*degree-h*pi/2;
  float curvature = t->curvature;
  if (fabs(curvature) < 0.00001) curvature=0.00001;
  float x0 = t->r0 * cos(t->phi0 * degree);
  float y0 = t->r0 * sin(t->phi0 * degree);
  float z0 = t->z0;
  StThreeVectorD origin(x0, y0, z0);  
  StHelixD trackHelix(curvature, dip, phase, origin, h);

  float MAX_Z = 195;
  if(MAX_Z < fabs(z0)) MAX_Z = fabs(z0) + 5;
    
  p[0]  = x0; p[1] = y0; p[2] = z0;
  p[8]  = trackHelix.xcenter(); p[9] = trackHelix.ycenter();
  p[10] = 1/curvature;

  float x_in1[2];
  float x_out1[2];
  int in1  = FindRowCrossing(p,r_first_row,x_in1,1);
  int out1 = FindRowCrossing(p,r_last_row,x_out1,1);
  
  if(!in1&&!out1){  //never crosses into the section 
    float temp = Local_y(p[8],p[9]);
    if(temp>r_last_row||temp<r_first_row) return 0; //and not within the section
  }
  
  if(in1){
    float x_in2[2];
    if(!FindRowCrossing(p,r_first_row,x_in2,2)){
      //cout<<"trouble only found one inner cross???"<<endl;
      return 0;
    }
    int betw=3;
    if(limits){
      betw = IsItBetween(x_in1[0],x_in1[1],limits) + 2*IsItBetween(x_in2[0],x_in2[1],limits);
      if(!betw)   return 0;
      if(betw==1) {p[0]=x_in1[0];p[1]=x_in1[1];}
      if(betw==2) {p[0]=x_in2[0];p[1]=x_in2[1];}
    }
    if(betw==3){
      if(((x_in1[0]-x0)*(x_in1[0]-x0)+(x_in1[1]-y0)*(x_in1[1]-y0))>
	 ((x_in2[0]-x0)*(x_in2[0]-x0)+(x_in2[1]-y0)*(x_in2[1]-y0))){
	p[0]=x_in2[0];p[1]=x_in2[1];
      }else{
	p[0]=x_in1[0];p[1]=x_in1[1]; 
      }
    }
  }else{
    float center_ang = atan2(p[9],p[8]);    
    if(x0*p[9]-y0*p[8]>0){
      //set minimum crossing angle is 1 degrees
      p[0] = p[8] + p[10]*cos(center_ang + M_PI + M_PI/180);
      p[1] = p[9] + p[10]*sin(center_ang + M_PI + M_PI/180);
    }else{
      p[0] = p[8] + p[10]*cos(center_ang + M_PI - M_PI/180);
      p[1] = p[9] + p[10]*sin(center_ang + M_PI - M_PI/180);
    }
  }

  {// finding first z
    float temp = ::sqrt((p[0]-x0)*(p[0]-x0)+(p[1]-y0)*(p[1]-y0))/2/p[10];
    float temp_ang = (temp<1) ? 2*asin(temp) : M_PI;
    if((x0*x0+y0*y0)>(p[0]*p[0]+p[1]*p[1])){
      p[2] = z0 - temp_ang*p[10]*tan(dip);
    }else{
      p[2] = z0 + temp_ang*p[10]*tan(dip);    
    }
    if(fabs(p[2])>MAX_Z){ //flew out the end
      float ang_z = 0;
      if(dip>0){
	if(z0>MAX_Z) return 0; //flew out the ends
	ang_z = (z0 + MAX_Z)/(p[10]*fabs(tan(dip)));
      }
      else{
	if(z0<-MAX_Z) return 0; //both flew out the ends
	ang_z = (MAX_Z - z0)/(p[10]*fabs(tan(dip)));
      }
      p[2] = z0 - ang_z*p[10]*tan(dip);
      float ang_start = atan2(y0-p[9],x0-p[8]);      
      if(x0*p[9] - y0*p[8] > 0) ang_z = -ang_z;
      p[0] = p[8] + p[10]*cos(ang_start + ang_z);
      p[1] = p[9] + p[10]*sin(ang_start + ang_z);
    }
  }

  if(out1){
    float x_out2[2];
    if(!FindRowCrossing(p,r_last_row,x_out2,2)){
      //cout<<"trouble only found one outer cross"<<endl;
      return 0;
    }
    int betw=3;
    if(limits){
      betw = IsItBetween(x_out1[0],x_out1[1],limits) + 2*IsItBetween(x_out2[0],x_out2[1],limits);
      if(!betw)   return 0;
      if(betw==1){ p[3]=x_out1[0];p[4]=x_out1[1];}
      if(betw==2){ p[3]=x_out2[0];p[4]=x_out2[1];}
    }
    if(betw==3){
      if(((x_out1[0]-x0)*(x_out1[0]-x0)+(x_out1[1]-y0)*(x_out1[1]-y0))>
	 ((x_out2[0]-x0)*(x_out2[0]-x0)+(x_out2[1]-y0)*(x_out2[1]-y0))){
	p[3] = x_out2[0]; p[4] = x_out2[1];
      }else{
	p[3] = x_out1[0]; p[4] = x_out1[1];
      }
    }
  }else{
    float center_ang = atan2(p[9],p[8]);
    if(x0*p[9]-y0*p[8]<0){ 
      //set minimum crossing angle is 1 degrees
      p[3] = p[8] + p[10]*cos(center_ang + M_PI/180);
      p[4] = p[9] + p[10]*sin(center_ang + M_PI/180);
    }else{
      p[3] = p[8] + p[10]*cos(center_ang - M_PI/180);
      p[4] = p[9] + p[10]*sin(center_ang - M_PI/180);
    }
  }

  {// finding last z
    float temp = ::sqrt((p[3]-x0)*(p[3]-x0)+(p[4]-y0)*(p[4]-y0))/2/p[10];
    float temp_ang = (temp<1) ? 2*asin(temp) : M_PI;
    if((x0*x0+y0*y0)<(p[3]*p[3]+p[4]*p[4])){
      p[5] = z0 + temp_ang*p[10]*tan(dip); 
    }else{
      p[5] = z0 - temp_ang*p[10]*tan(dip); 
    }
    if(fabs(p[5])>MAX_Z){  // flew out the end
      float ang_z = 0;
      if(dip>0) ang_z = (MAX_Z - z0)/(p[10]*fabs(tan(dip)));
      else      ang_z = (z0 + MAX_Z)/(p[10]*fabs(tan(dip)));
      p[5] = z0 + ang_z*p[10]*tan(dip);
      float ang_start = atan2(y0-p[9],x0-p[8]);
      if(x0*p[9] - y0*p[8] < 0) ang_z = -ang_z;
      p[3] = p[8] + p[10]*cos(ang_start + ang_z);
      p[4] = p[9] + p[10]*sin(ang_start + ang_z);
    }
  }
  
  if(Local_y(p[0],p[1])>r_last_row||Local_y(p[3],p[4])<r_first_row) return 0;
  
  p[6]=p[7]=0;
  if(p[2]*p[5]<0){
    float ang_z = -z0/(p[10]*tan(dip));
    float ang_start = atan2(y0-p[9],x0-p[8]);
    if(x0*p[9] - y0*p[8] < 0) ang_z = -ang_z;
    p[6] = p[8] + p[10]*cos(ang_start + ang_z);
    p[7] = p[9] + p[10]*sin(ang_start + ang_z);
  }

  return 1;
}

int RdoFinder::FindPointsBetween(StTrack *t,float *p,float r_first_row, float r_last_row, float* limits){
  //returns 1 if crossing points are found between limits, otherwise 0
  //  p[0]=first_x;p[1]=first_y;p[2]=first_z;
  //  p[3]=last_x;p[4]=last_y;p[5]=last_z;
  //  p[6]=mem_x;p[7]=mem_y;
  //  p[8]=center_x;p[9]=center_y;
  //  p[10]=radius;
  
  float x0  = t->geometry()->origin().x();
  float y0  = t->geometry()->origin().y();
  float z0  = t->geometry()->origin().z();
  float dip = t->geometry()->dipAngle();

  float MAX_Z = 195;
  if(MAX_Z < fabs(z0)) MAX_Z = fabs(z0) + 5;

  p[0] = x0; p[1] = y0; p[2] = z0;
  p[8] = t->geometry()->helix().xcenter();p[9] = t->geometry()->helix().ycenter();
  p[10] = 100000.;
  float curva = t->geometry()->curvature();
  if (fabs(curva)>0.00001) p[10] = 1/curva;

  float x_in1[2];
  float x_out1[2];
  int in1  = FindRowCrossing(p,r_first_row,x_in1,1);
  int out1 = FindRowCrossing(p,r_last_row,x_out1,1);
  
  if(!in1&&!out1){  //never crosses into the section 
    float temp = Local_y(p[8],p[9]);
    if(temp>r_last_row||temp<r_first_row) return 0; //and not within the section
  }
  
  if(in1){
    float x_in2[2];
    if(!FindRowCrossing(p,r_first_row,x_in2,2)){
      //cout<<"trouble only found one inner cross???"<<endl;
      return 0;
    }
    int betw=3;
    if(limits){
      betw = IsItBetween(x_in1[0],x_in1[1],limits) + 2*IsItBetween(x_in2[0],x_in2[1],limits);
      if(!betw)   return 0;
      if(betw==1) {p[0]=x_in1[0];p[1]=x_in1[1];}
      if(betw==2) {p[0]=x_in2[0];p[1]=x_in2[1];}
    }
    if(betw==3){
      if(((x_in1[0]-x0)*(x_in1[0]-x0)+(x_in1[1]-y0)*(x_in1[1]-y0))>
	 ((x_in2[0]-x0)*(x_in2[0]-x0)+(x_in2[1]-y0)*(x_in2[1]-y0))){
	p[0]=x_in2[0];p[1]=x_in2[1];
      }else{
	p[0]=x_in1[0];p[1]=x_in1[1]; 
      }
    }
  }else{
    float center_ang = atan2(p[9],p[8]);    
    if(x0*p[9]-y0*p[8]>0){
      //set minimum crossing angle is 1 degrees
      p[0] = p[8] + p[10]*cos(center_ang + M_PI + M_PI/180);
      p[1] = p[9] + p[10]*sin(center_ang + M_PI + M_PI/180);
    }else{
      p[0] = p[8] + p[10]*cos(center_ang + M_PI - M_PI/180);
      p[1] = p[9] + p[10]*sin(center_ang + M_PI - M_PI/180);
    }
  }

  {// finding first z
    float temp = ::sqrt((p[0]-x0)*(p[0]-x0)+(p[1]-y0)*(p[1]-y0))/2/p[10];
    float temp_ang = (temp<1) ? 2*asin(temp) : M_PI;
    if((x0*x0+y0*y0)>(p[0]*p[0]+p[1]*p[1])){
      p[2] = z0 - temp_ang*p[10]*tan(dip);
    }else{
      p[2] = z0 + temp_ang*p[10]*tan(dip);    
    }
    if(fabs(p[2])>MAX_Z){ //flew out the end
      float ang_z = 0;
      if(dip>0){
	if(z0>MAX_Z) return 0; //flew out the ends
	ang_z = (z0 + MAX_Z)/(p[10]*fabs(tan(dip)));
      }
      else{
	if(z0<-MAX_Z) return 0; //both flew out the ends
	ang_z = (MAX_Z - z0)/(p[10]*fabs(tan(dip)));
      }
      p[2] = z0 - ang_z*p[10]*tan(dip);
      float ang_start = atan2(y0-p[9],x0-p[8]);      
      if(x0*p[9] - y0*p[8] > 0) ang_z = -ang_z;
      p[0] = p[8] + p[10]*cos(ang_start + ang_z);
      p[1] = p[9] + p[10]*sin(ang_start + ang_z);
    }
  }

  if(out1){
    float x_out2[2];
    if(!FindRowCrossing(p,r_last_row,x_out2,2)){
      //cout<<"trouble only found one outer cross"<<endl;
      return 0;
    }
    int betw=3;
    if(limits){
      betw = IsItBetween(x_out1[0],x_out1[1],limits) + 2*IsItBetween(x_out2[0],x_out2[1],limits);
      if(!betw)   return 0;
      if(betw==1){ p[3]=x_out1[0];p[4]=x_out1[1];}
      if(betw==2){ p[3]=x_out2[0];p[4]=x_out2[1];}
    }
    if(betw==3){
      if(((x_out1[0]-x0)*(x_out1[0]-x0)+(x_out1[1]-y0)*(x_out1[1]-y0))>
	 ((x_out2[0]-x0)*(x_out2[0]-x0)+(x_out2[1]-y0)*(x_out2[1]-y0))){
	p[3] = x_out2[0]; p[4] = x_out2[1];
      }else{
	p[3] = x_out1[0]; p[4] = x_out1[1];
      }
    }
  }else{
    float center_ang = atan2(p[9],p[8]);
    if(x0*p[9]-y0*p[8]<0){ 
      //set minimum crossing angle is 1 degrees
      p[3] = p[8] + p[10]*cos(center_ang + M_PI/180);
      p[4] = p[9] + p[10]*sin(center_ang + M_PI/180);
    }else{
      p[3] = p[8] + p[10]*cos(center_ang - M_PI/180);
      p[4] = p[9] + p[10]*sin(center_ang - M_PI/180);
    }
  }

  {// finding last z
    float temp = ::sqrt((p[3]-x0)*(p[3]-x0)+(p[4]-y0)*(p[4]-y0))/2/p[10];
    float temp_ang = (temp<1) ? 2*asin(temp) : M_PI;
    if((x0*x0+y0*y0)<(p[3]*p[3]+p[4]*p[4])){
      p[5] = z0 + temp_ang*p[10]*tan(dip); 
    }else{
      p[5] = z0 - temp_ang*p[10]*tan(dip); 
    }
    if(fabs(p[5])>MAX_Z){  // flew out the end
      float ang_z = 0;
      if(dip>0) ang_z = (MAX_Z - z0)/(p[10]*fabs(tan(dip)));
      else      ang_z = (z0 + MAX_Z)/(p[10]*fabs(tan(dip)));
      p[5] = z0 + ang_z*p[10]*tan(dip);
      float ang_start = atan2(y0-p[9],x0-p[8]);
      if(x0*p[9] - y0*p[8] < 0) ang_z = -ang_z;
      p[3] = p[8] + p[10]*cos(ang_start + ang_z);
      p[4] = p[9] + p[10]*sin(ang_start + ang_z);
    }
  }
  
  if(Local_y(p[0],p[1])>r_last_row||Local_y(p[3],p[4])<r_first_row) return 0;
  
  p[6]=p[7]=0;
  if(p[2]*p[5]<0){
    float ang_z = -z0/(p[10]*tan(dip));
    float ang_start = atan2(y0-p[9],x0-p[8]);
    if(x0*p[9] - y0*p[8] < 0) ang_z = -ang_z;
    p[6] = p[8] + p[10]*cos(ang_start + ang_z);
    p[7] = p[9] + p[10]*sin(ang_start + ang_z);
  }

  return 1;
}

int RdoFinder::FindRowCrossing(float* p,float y_row, float* x, int first_or_second){
  //1 is returned if the row is crossed, otherwise returns 0
  //if 1 is return the first or second crossings "x" and "y" are returned through x

  if(y_row<0) return 0;
  float yc_local = Local_y(p[8],p[9]);
  if(fabs(yc_local-y_row)>p[10]) return 0;

  float dx_row = y_row*tan(M_PI/12);
  int first_found = 0;

  for(float i=1;i<13;i++){
    float sector_ang = -i*M_PI/6;
    float lx_c = p[8]*cos(sector_ang) + p[9]*sin(sector_ang);
    float ly_c = p[9]*cos(sector_ang) - p[8]*sin(sector_ang);
    float dy   = fabs(y_row-ly_c); 
    if(dy > p[10]) continue;

    float dx = ::sqrt(p[10]*p[10] - dy*dy);
    float x1 = lx_c + dx;
    float x2 = lx_c - dx;
    
    if(fabs(x1)<dx_row && fabs(x2)<dx_row){
      if(first_or_second==1){
	x[0] = x1*cos(sector_ang)    - y_row*sin(sector_ang);
	x[1] = y_row*cos(sector_ang) + x1*sin(sector_ang);
	return 1;
      }else{
	x[0] = x2*cos(sector_ang)    - y_row*sin(sector_ang);
	x[1] = y_row*cos(sector_ang) + x2*sin(sector_ang);	
        return 1;
      }
    }else if(fabs(x1)<dx_row || fabs(x2)<dx_row){
      if(first_or_second==1||first_found){
	if(fabs(x1)<dx_row){
	  x[0] = x1*cos(sector_ang)    - y_row*sin(sector_ang);
	  x[1] = y_row*cos(sector_ang) + x1*sin(sector_ang);
	  return 1;
	}else{
	  x[0] = x2*cos(sector_ang)    - y_row*sin(sector_ang);
	  x[1] = y_row*cos(sector_ang) + x2*sin(sector_ang);
	  return 1;
	}
      }
      first_found = 1;
    }
  }

  return 0;
}

int RdoFinder::IsItBetween(float x,float y,float* p){
  //is x,y between the first and last point on the track????
  //this assumes that the track travels less than 180 degrees

  float x1 = p[0] - p[8];
  float y1 = p[1] - p[9];
  float x2 = x    - p[8];
  float y2 = y    - p[9];
  float x3 = p[3] - p[8];
  float y3 = p[4] - p[9];

  if((x1*y2-y1*x2)*(x2*y3-y2*x3)>0){
    float temp1 = ::sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))/2/p[10];
    if(temp1>=1) return 0;
    float temp2 = ::sqrt((x3-x2)*(x3-x2)+(y3-y2)*(y3-y2))/2/p[10];
    if(temp2>=1) return 0;
    if(2*asin(temp1)+2*asin(temp2) < M_PI) return 1;
  }

  return 0;
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


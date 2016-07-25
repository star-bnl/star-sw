/* 
Yuri, The five functions below should aid you with electron and positron selection. You may need
to include the standard math library, "math.h". 
                                                  Ian 
*/
//*********Function 1 *******
float Get_imass(StEvent *e,StTrack* t1, StTrack* t2, float m1=.0005, float m2=.0005){
  double r1 = 1/t1->geometry()->curvature();
  double r2 = 1/t2->geometry()->curvature();
  double xdiff =  t1->geometry()->helix().xcenter() - t2->geometry()->helix().xcenter();
  double ydiff =  t1->geometry()->helix().ycenter() - t2->geometry()->helix().ycenter();
  double dist = sqrt(xdiff*xdiff + ydiff*ydiff);

  double cos_ang = cos(t1->geometry()->dipAngle()-t2->geometry()->dipAngle());
  if(dist < (r1 + r2)){
    cos_ang *= cos(M_PI - acos((r1*r1 + r2*r2 - dist*dist)/(2*r1*r2)));
  }
  double ee1 = t1->geometry()->momentum().mag2() + m1*m1;
  double ee2 = t2->geometry()->momentum().mag2() + m2*m2;
  
  return sqrt(ee1 + ee2 + 2*sqrt(ee1)*sqrt(ee2) - 
              t1->geometry()->momentum().mag2() -
              t2->geometry()->momentum().mag2() -
              2*t1->geometry()->momentum().mag()*
              t2->geometry()->momentum().mag()*cos_ang);
}
//*********end of Function 1*******



//*********Function 2*******
float Get_rz_opening_angle(StTrack *t1,StTrack *t2){
  double diff,d1,d2;
    d1 = t1->geometry()->dipAngle();
    d2 = t2->geometry()->dipAngle();
  diff = d1 - d2;

  return (diff > 0) ? diff:-diff;
}
//*********end of Function 2*******



//*********Function 3*******
float Get_trk1_trk2_xy_dca(StTrack *t1,StTrack *t2){
  double cent_dist = sqrt(
      (t1->geometry()->helix().xcenter() - t2->geometry()->helix().xcenter())*
      (t1->geometry()->helix().xcenter() - t2->geometry()->helix().xcenter()) +
      (t1->geometry()->helix().ycenter() - t2->geometry()->helix().ycenter())*
      (t1->geometry()->helix().ycenter() - t2->geometry()->helix().ycenter()));

  double diff_rads__cent_dist = cent_dist - 
    1/t1->geometry()->curvature() - 1/t2->geometry()->curvature();
  
  return (diff_rads__cent_dist>0) ? diff_rads__cent_dist:-diff_rads__cent_dist;
}
//*********end of Function 3*******




//*********Function 4*******
float Get_sign_dca(StEvent* e,StTrack *t){
  double x = t->geometry()->helix().xcenter();
  double y = t->geometry()->helix().ycenter();

  if(e)
    if(e->primaryVertex()){
      x = x - e->primaryVertex()->position().x();
      y = y - e->primaryVertex()->position().y();
    }

  return sqrt(x*x+y*y) - 1/t->geometry()->curvature();
}
//*********end of Function 4*******




//*********Function 5*******
float Get_xy_conversion_dist(StEvent* e, StTrack *t){
  double x = t->geometry()->helix().xcenter();
  double y = t->geometry()->helix().ycenter();

  if(e->primaryVertex()){
    x = x - e->primaryVertex()->position().x();
    y = y - e->primaryVertex()->position().y();
  }
  double r = sqrt(x*x+y*y);
  double rad = 1/t->geometry()->curvature();

  if(rad > r) return 0;
  return sqrt(r*r - rad*rad);
}
//*********end of Function 5*******



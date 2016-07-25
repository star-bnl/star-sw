{
  gSystem->Load("StarRoot.so");
  int nsurf=0;
  double x[3],p[3],rho;
  double pnt[3],pdir[3],xnear[3],vbok[3];
  //               [0]+x*[1]+y*[2]+z*[3].+x*x*[4]+y*y[5]+z*z*[6]
  double surf[7] = {0.,   0., -20.,   0.,      1.,    1.,     0.};
  nsurf = 7;
//  double surf[4]= {0.,1.,-1.,0.};
//  double surf[4]= {0.,1.,0.,0.};
//  double surf[4]= {-1.,0.,1.,0.};
//  double surf[4]= {-1.,0.,0.,1.};
  memset(x,0,24);
  memset(p,0,24);
  double Rad = 10.;
  double Phi = 0.3;
  x[0] = Rad*cos(Phi);
  x[1] = Rad*sin(Phi);
  p[0] = -sin(Phi);
  p[1] =  cos(Phi);
  p[2] = 1.;

  
  rho =1/Rad;
//  rho = 0.000;
  THelixTrack *tx = new  THelixTrack(x,p,rho);
//  double ang = 3.1415926/2; 
//  tx->Step(ang,x,p);
  
//  printf("x= %f %f %f  p = %f %f %f \n",x[0],x[1],x[2],p[0],p[1],p[2]);
  
  memset(x,0,24);
  memset(p,0,24);
  
  double step = tx->Step(3.,surf,nsurf,x,p);
  printf("dist= %f x= %f %f %f  p = %f %f %f \n",step,x[0],x[1],x[2],p[0],p[1],p[2]);


  Rad = 1000; 
  rho =1/Rad;
  x[0] = 1; x[1] = 1;x[2] = 0;
  p[0] = 1; p[1] =-1;p[2] = 0;
  double ds = 3.141592*2*fabs(Rad)/10;
  THelixTrack hel(x,p,rho);
  hel.Print();
  for (int i=0;i<10; i++) {
    step = (i+1)*ds;
    hel.Step(step,pnt,pdir);
    vbok[0]=1.;vbok[1]=1.;vbok[2]=1.;
    double tmp=0;
    for (int ii=0;ii<3;ii++) {tmp+=vbok[ii]*pdir[ii];}
    for (int ii=0;ii<3;ii++) {vbok[ii]-=tmp*pdir[ii];}
    for (int ii=0;ii<3;ii++) {pnt[ii] +=vbok[ii];}
    double step2 = hel.Step(pnt);
    printf("step=%f step2=%f\n",step,step2);
  }

  double PNTS[10][3];
  ds /=2;

  for (int i=0;i<10; i++) {
    step = -(i+1)*ds;
    hel.Step(step,pnt,pdir);
    double tmp = pow(pnt[0]-x[0]+Rad*p[1],2)+pow(pnt[1]-x[1]-Rad*p[0],2);
    tmp = sqrt(tmp);
    printf("%d %f\n",i,tmp);
    memcpy(PNTS[i],pnt,3*8);
  }
  THelixTrack fit(PNTS[0],10);
  fit.Print();

}

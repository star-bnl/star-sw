{

  double qwe = 1./sqrt(3.);
  double h = 0.000000000004;
  StThreeVectorD p(1.,1.,1.);
  StThreeVectorD o(-qwe,-qwe,-qwe);
  StPhysicalHelixD hlx(p,o,h,1.);
  StThreeVectorD r(qwe,qwe,qwe);
  StThreeVectorD n(qwe,qwe,qwe);
  
  double path = hlx.pathLength(r,n);
  printf("IhPath = %f\n",path);

  double xyz[3]; 
  xyz[0]=o.x()*100;
  xyz[1]=o.y()*100;
  xyz[2]=o.z()*100;
  double dir[3]; 
  dir[0]=p.x();
  dir[1]=p.y();
  dir[2]=p.z();
  double rho = hlx.curvature();
  if (rho) printf("Rho,Rad = %f %f\n",rho,1./rho);
  rho /=100;
  double surf[4];
  surf[0] = - r*n*100;
  surf[1] = n.x();
  surf[2] = n.y();
  surf[3] = n.z();
  
  THelixTrack myHlx(xyz,dir,rho,0);
  path = myHlx.Step(1.e+10, surf, 4);
  printf("MyPath = %f\n",path/100);

  
  
}		  
		  

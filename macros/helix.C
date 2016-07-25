{
gSystem->Load("StarRoot.so");
double xbeg[3],pbeg[3],h[3],stp[100],totstp,rho;
double x[3],p[3],xx[3],pp[3];
TRandom rr;
for(int i=0; i<3;i++) {
 xbeg[i]=rr.Rndm();
 pbeg[i]=rr.Rndm();
 h[i]=rr.Rndm();
}
//pbeg[2]=0;
double tmp = sqrt(pbeg[0]*pbeg[0]+pbeg[1]*pbeg[1]+pbeg[2]*pbeg[2]);
for(int i=0; i<3;i++) {pbeg[i]/=tmp;}
rho = rr.Rndm();
for(int i=0; i<100;i++) {stp[i]=rr.Rndm();}

printf("XBEG = %f %f %f PBEG = %f %f %f \n",xbeg[0],xbeg[1],xbeg[2],pbeg[0],pbeg[1],pbeg[2]);

totstp=0;
THelixTrack hx(xbeg,pbeg,rho,h);
//THelixTrack hx(xbeg,pbeg,rho,0);
for(int i=0; i<100;i++) {
totstp += stp[i];
hx.Step(stp[i],x,p);
hx.Set(x,p,rho,h);

}
printf("XEND = %f %f %f PEND = %f %f %f \n",x[0],x[1],x[2],p[0],p[1],p[2]);

hx.Step(-totstp,x,p);

printf("XBak = %f %f %f PBak = %f %f %f \n",x[0],x[1],x[2],p[0],p[1],p[2]);


}


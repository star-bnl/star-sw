class HexLatice {
public:
  // data members
  TVector2 U,V; // _versors_ of the grid
  TVector2 bU,bV; // base vectors of the grid, contain pitch
  double uv,w0,pitch;

  HexLatice(double pitchX,double phi1) {
    const double pi=2*acos(0);
    pitch=pitchX;
    bU.SetMagPhi(pitch,phi1);
    bV=bU.Rotate(pi/3);
    U=bU.Unit();
    V=bV.Unit();
    uv=U*V, w0=(1-uv*uv)*pitch;
    printf("Hex Grid base: U(%.3f,%3f), V(%.3f,%3f)  U*V=%.3f\n",U.X(),V.Y(),U.X(),V.Y(),uv);
  }
  TVector2 snap(TVector2 &P, int &ku,int &kv){ // returns hexLatice coordinates
    // find  hex latice coordinates 
    double a=P*U, b=P*V;
    double u=(a-b*uv)/w0, v=(b-a*uv)/w0;
    double iu=floor(u), iv=floor(v);
    //    printf("P(%.3f,%3f) --> hexLatice(%.3f,%3f)*pitch -->floor(%.0f %.0f) \n", P.X(),P.Y(),u,v,iu,iv);
    int iN;
    double maxD2=999999;
   
    TVector2 Psnap=TVector2(99999,99999);
    for(iN=0;iN<4;iN++) {
      int ju=iu+iN%2;
      int jv=iv+iN/2;
      TVector2 Q=ju*bU + jv*bV;
      double dist2=(Q-P).Mod2();
      //  printf("iNode=%d  Q(%.3f,%3f) -->dist=%.2f\n",iN,Q.X(),Q.Y(),sqrt(dist2));
      if(maxD2<dist2) continue;
      maxD2=dist2;
      Psnap=Q;

      ku=ju;
      kv=jv;
    }
    
    //    printf("closest node(%d,%d) Psnap(%.3f,%3f) -->dist=%.2f\n",ku,kv,Psnap.X(),Psnap.Y(),sqrt(maxD2));

     return Psnap;
  }
};


//=================================
hexSnap(int np=1) {
  const double pi=2*acos(0);
  double phi1=pi/4;
  double pitch=10;
  double Rmax=40;
  gStyle->SetOptStat(0);
  HexLatice hexLat(pitch,phi1);
  TRandom3 *rnd=new TRandom3;
  c=new TCanvas("bb","bb",600,600);
  h=new TH2F("aa","aa",10,0,Rmax,10,0,Rmax); h->Draw();
  drawHoles(38,hexLat);

  int i;
  int j=4;
  for(j=0;j<10;j++)
  for(i=0;i<10;i++){
    //TVector2 P(rnd->Uniform(5.,Rmax*.8),rnd->Uniform(5.,Rmax*.8));
    TVector2 P(3+j*.7,3+i*.7);
    int kU,kV;
    hexLat.snap(P,kU,kV); // returns also new vector, you may use it
    kU+=1000;
    kV+=1000;
    //printf("k=%d %d\n",kU,kV);
    tm=new TMarker(P.X(),P.Y(),5);
    int off=kU%2 +2*(kV%2);
    //   printf("off=%d\n",off);
    tm->Draw(); tm->SetMarkerColor(1+off);
  }

 
  return;
}

//========================================
drawHoles(double r2,HexLatice &HL) {
  int i=1,j=1;

  for(j=-5; j<10;j++) 
  for(i=1; i<10;i++) {
    TVector2 r=i*HL.bU + j*HL.bV;
    
    if(r.X()<0 || r.Y()<0) continue;
    if(r.X()>r2 || r.Y()>r2) continue;
    el=new TEllipse(r.X(),r.Y(),0.5);
    el->Draw();
    int  kU=i+1000;
    int kV=j+1000;
    int off=kU%2 +2*(kV%2);    
    el->SetFillColor(1+off);
  }
}  


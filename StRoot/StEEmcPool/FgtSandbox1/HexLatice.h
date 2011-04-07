#ifndef HexLatice_h
#define  HexLatice_h
#include <math.h>
class HexLatice {	
 public:		
  // data members
  TVector2 U,V; // _versors_ of the grid
  TVector2 bU,bV; // base vectors of the grid, length=pitch
  double uv,w0,pitch;
  
  HexLatice(double pitchX,double phi1deg) {
    const double pi=2*acos(0);
    pitch=pitchX;
    bU.SetMagPhi(pitch,phi1deg/180*pi);
    bV=bU.Rotate(pi/3);
    U=bU.Unit();
    V=bV.Unit();
    uv=U*V; w0=(1-uv*uv)*pitch;
    printf("Hex Grid base: U(%.3f,%3f), V(%.3f,%3f)  U*V=%.3f\n",U.X(),U.Y(),V.X(),V.Y(),uv);
  }
  
  TVector2 snap(TVector2 &P, int &ku,int &kv){ // returns hexLatice coordinates
    // f	 ind  hex latice coordinates 
    double a=P*U, b=P*V;
    double u=(a-b*uv)/w0, v=(b-a*uv)/w0;
    double iu=floor(u), iv=floor(v);
    printf("P(%.3f,%3f) --> hexLatice(%.3f,%.3f)*pitch -->floor(%.0f %.0f) \n", P.X(),P.Y(),u,v,iu,iv);
    int iN;
    double maxD2=999999;
    
    TVector2 Psnap=TVector2(99999,99999);
    for(iN=0;iN<4;iN++) {
      double ju=iu+iN%2;
      double jv=iv+iN/2;
      TVector2 Q=ju*bU + jv*bV;
      double dist2=(Q-P).Mod2();
      //printf("iNode=%d  Q(%.3f,%3f) -->dist/um=%.2f\n",iN,Q.X(),Q.Y(),sqrt(dist2)*1e4);
      if(maxD2<dist2) continue;
      maxD2=dist2;
      Psnap=Q;
      
      ku=(int)ju;
      kv=(int)jv;
    }
    
    printf("closest node(%d,%d) Psnap(%.3f,%3f) -->dist/um=%.2f\n",ku,kv,Psnap.X(),Psnap.Y(),sqrt(maxD2)*1e4);
    
    return Psnap;
  }
};

#endif

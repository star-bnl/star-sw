/***********************************************************************
 *
 *  StHbtV0.hh,v 1.0 1999/09/07
 *
 * Authors: Helen Caines, Tom Humanic 07-Sep-1999
 *
 ***********************************************************************
 *
 * Description: V0 class with members copied from StV0hbtV0.hh
 *
 ***********************************************************************
 *
 * $Log: StHbtV0.hh,v $
 * Revision 1.2  1999/10/15 02:32:37  lisa
 * Helen has added method StHbtV0::UpdateV0() to fill derived information - this leads to smaller microDSTs
 *
 * Revision 1.1  1999/09/16 18:47:59  lisa
 * replace placeholder HbtV0Track stuff with Helens StHbtV0 classes
 *
 * 
 *
 ***********************************************************************/
#ifndef StHbtV0_hh
#define StHbtV0_hh
// these are included in StV0MiniDst.hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh" //same as in StHbtTrack.hh


class StHbtV0 {
public:
  StHbtV0(){/* no-op */}
  ~StHbtV0(){/* no-op */}


  float decayLengthV0() const;       // 3-d decay distance
  StHbtThreeVector decayVertexV0() const; // Coordinates of decay vertex
  float dcaV0Daughters() const;      // DCA of v0 daughters at decay vertex
  float dcaV0ToPrimVertex() const;   // DCA of v0 to primary vertex
  float dcaPosToPrimVertex() const;  // DCA of pos v0 daughter to pri vertex
  float dcaNegToPrimVertex() const;  // DCA of neg v0 daughter to pri vertex
  StHbtThreeVector momPos() const;   // Momentum components of pos. daughter
  StHbtThreeVector momNeg() const;   // Momentum components of neg. daughter

  int   tpcHitsPos() const;          // Number of TPC hits on pos. daughter
  int   tpcHitsNeg() const;          // Number of TPC hits on neg. daughter

  StHbtThreeVector momV0() const ;    // Momentum components of V0
  float alphaV0() const ;             // Armenteros-Podolanski variable
  float ptArmV0() const ;            // Armenteros-Podolanski variable
  float eLambda() const ;             // Energy assuming lambda hypothesis
  float eK0Short() const ;            // Energy assuming k-short hypothesis
  float ePosProton() const ;          // Energy of pos. daughter assuming proton
  float ePosPion() const ;            // Energy of pos. daughter assuming pion
  float eNegProton() const ;          // Energy of neg. daughter assuming antiproton
  float eNegPion() const ;            // Energy of neg. daughter assuming pion
  float massLambda() const ;          // Mass assuming lambda hypothesis
  float massAntiLambda() const ;      // Mass assuming antilambda hypothesis
  float massK0Short() const ;         // Mass assuming k-short hypothesis
  float rapLambda() const ;           // Rapidity assuming (anti) constlambda
  float rapK0Short() const ;          // Rapidity assuming k-short
  float cTauLambda() const ;          // Lifetime (ctau) const assuming (anti) constlambda
  float cTauK0Short() const ;         // Lifetime (ctau) const assuming k-short
  float ptV0() const ;                // Transverse momentum
  float ptotV0() const ;              // Total momentum
  float ptPos() const ;               // Transverse momentum of pos. daughter
  float ptotPos() const ;             // Total momentum of pos. daughter
  float ptNeg() const ;               // Transverse momentum of neg. daughter
  float ptotNeg() const ;             // Total momentum of neg. daughter
  int   idNeg() const;               // Id of negative track
  int   idPos() const;               // Id of positive track

  void UpdateV0(); // Fills derived info
  void SetdecayLengthV0(const float&);  
  void SetdecayVertexV0(const StHbtThreeVector&);  
  void SetdcaV0Daughters(const float&); 
  void SetdcaV0ToPrimVertex(const float&);  
  void SetdcaPosToPrimVertex(const float&); 
  void SetdcaNegToPrimVertex(const float&); 
  void SetmomPos(const StHbtThreeVector&);  
  void SetmomNeg(const StHbtThreeVector&);  

  void SettpcHitsPos(const int&);      
  void SettpcHitsNeg(const int&);      

  void SetmomV0( StHbtThreeVector&);
  void SetalphaV0( float&);       
  void SetptArmV0( float&);       
  void SeteLambda( float&);     
  void SeteK0Short( float&);    
  void SetePosProton( float&);  
  void SetePosPion( float&);    
  void SeteNegProton( float&);  
  void SeteNegPion( float&);    
  void SetmassLambda( float&);  
  void SetmassAntiLambda( float&);
  void SetmassK0Short( float&);  
  void SetrapLambda( float&);    
  void SetrapK0Short( float&);   
  void SetcTauLambda( float&);   
  void SetcTauK0Short( float&);  
  void SetptV0( float&);         
  void SetptotV0( float&);       
  void SetptPos( float&);        
  void SetptotPos( float&);      
  void SetptNeg( float&);        
  void SetptotNeg( float&);
  void SetidNeg(const int&);
  void SetidPos(const int&);
     
  
  friend ostream& operator<<(ostream& out, StHbtV0& v0);
  friend istream& operator>>(istream& in,  StHbtV0& v0);

protected:
 
  float mdecayLengthV0;
  StHbtThreeVector mdecayVertexV0;
  float mdcaV0Daughters;
  float mdcaV0ToPrimVertex;
  float mdcaPosToPrimVertex;
  float mdcaNegToPrimVertex;
  StHbtThreeVector mmomPos;
  StHbtThreeVector mmomNeg;

  int   mtpcHitsPos;
  int   mtpcHitsNeg;
       
  StHbtThreeVector mmomV0;                            
  float malphaV0;
  float mptArmV0;
  float meLambda;
  float meK0Short;
  float mePosProton;
  float mePosPion;
  float meNegProton;
  float meNegPion;
  float mmassLambda;
  float mmassAntiLambda;
  float mmassK0Short;
  float mrapLambda;
  float mrapK0Short;
  float mcTauLambda;
  float mcTauK0Short;
  float mptV0;
  float mptotV0;
  float mptPos;
  float mptotPos;
  float mptNeg;
  float mptotNeg;

  int   midNeg;
  int   midPos;

};



inline float StHbtV0::decayLengthV0() const 
             { return mdecayLengthV0; }
inline StHbtThreeVector StHbtV0::decayVertexV0() const
             { return mdecayVertexV0; } 
inline float StHbtV0::dcaV0Daughters() const 
             { return mdcaV0Daughters; }
inline float StHbtV0::dcaV0ToPrimVertex() const 
             { return mdcaV0ToPrimVertex; }
inline float StHbtV0::dcaPosToPrimVertex() const 
             { return mdcaPosToPrimVertex; }
inline float StHbtV0::dcaNegToPrimVertex() const 
             { return mdcaNegToPrimVertex; }
inline StHbtThreeVector StHbtV0::momPos() const
             { return mmomPos; }
inline StHbtThreeVector StHbtV0::momNeg() const
             { return mmomNeg; }
inline StHbtThreeVector StHbtV0::momV0() const 
             { return mmomV0; }
inline float StHbtV0::alphaV0() const  
             { return malphaV0; }
inline float StHbtV0::ptArmV0() const 
             {return mptArmV0;}
inline float StHbtV0::eLambda() const 
             {return meLambda;}
inline float StHbtV0::eK0Short() const 
             {return meK0Short;}
inline float StHbtV0::ePosProton() const 
             {return mePosProton;}
inline float StHbtV0::ePosPion() const 
             {return mePosPion;}
inline float StHbtV0::eNegProton() const 
             {return meNegProton;}
inline float StHbtV0::eNegPion() const 
             {return meNegPion;}
inline float StHbtV0::massLambda() const 
             {return mmassLambda;}
inline float StHbtV0::massAntiLambda() const 
             {return mmassAntiLambda;}
inline float StHbtV0::massK0Short() const 
             {return mmassK0Short;}
inline float StHbtV0::rapLambda() const 
             {return mrapLambda;}
inline float StHbtV0::rapK0Short() const 
             {return mrapK0Short;}
inline float StHbtV0::cTauLambda() const 
             {return mcTauLambda;}
inline float StHbtV0::cTauK0Short() const 
             {return mcTauK0Short;}
inline float StHbtV0::ptV0() const 
             {return mptV0;}
inline float StHbtV0::ptotV0() const 
             {return mptotV0;}
inline float StHbtV0::ptPos() const 
             {return mptPos;}
inline float StHbtV0::ptotPos() const 
             {return mptotPos;}
inline float StHbtV0::ptNeg() const 
             {return mptNeg;}
inline float StHbtV0::ptotNeg() const 
             {return mptotNeg;}

inline int   StHbtV0::tpcHitsPos() const
             { return mtpcHitsPos; }
inline int   StHbtV0::tpcHitsNeg() const
             { return mtpcHitsNeg; }
inline int   StHbtV0::idNeg() const
             { return midNeg; }
inline int   StHbtV0::idPos() const
             { return midPos; }




inline void StHbtV0::SetdecayLengthV0(const float& x){ mdecayLengthV0= x;}   
inline void StHbtV0::SetdecayVertexV0(const StHbtThreeVector& v){mdecayVertexV0 = v;} 
inline void StHbtV0::SetdcaV0Daughters(const float& x){mdcaV0Daughters= x;} 
inline void StHbtV0::SetdcaV0ToPrimVertex(const float& x){mdcaV0ToPrimVertex= x;}   
inline void StHbtV0::SetdcaPosToPrimVertex(const float& x){mdcaPosToPrimVertex = x;} 
inline void StHbtV0::SetdcaNegToPrimVertex(const float& x){mdcaNegToPrimVertex = x;} 
inline void StHbtV0::SetmomPos(const StHbtThreeVector& v){mmomPos = v;} 
inline void StHbtV0::SetmomNeg(const StHbtThreeVector& v){mmomNeg = v;} 
inline void StHbtV0::SettpcHitsPos(const int& i){mtpcHitsPos=i;} 
inline void StHbtV0::SettpcHitsNeg(const int& i){mtpcHitsNeg=i;}
inline void StHbtV0::SetmomV0(StHbtThreeVector& v){mmomV0= v;}
inline void StHbtV0::SetalphaV0( float& x){malphaV0= x;}
inline void StHbtV0::SetptArmV0( float& x){mptArmV0 = x;}
inline void StHbtV0::SeteLambda( float& x){meLambda= x;}       
inline void StHbtV0::SeteK0Short( float& x){meK0Short= x;}
inline void StHbtV0::SetePosProton( float& x){mePosProton= x;}      
inline void StHbtV0::SetePosPion( float& x){mePosPion= x;}      
inline void StHbtV0::SeteNegProton( float& x){meNegProton= x;} 
inline void StHbtV0::SeteNegPion( float& x){meNegPion= x;}       
inline void StHbtV0::SetmassLambda( float& x){mmassLambda = x;} 
inline void StHbtV0::SetmassAntiLambda( float& x){mmassAntiLambda= x;} 
inline void StHbtV0::SetmassK0Short( float& x){mmassK0Short= x;}  
inline void StHbtV0::SetrapLambda( float& x){mrapLambda= x;}
inline void StHbtV0::SetrapK0Short( float& x){mrapK0Short = x;}   
inline void StHbtV0::SetcTauLambda( float& x){mcTauLambda = x;}   
inline void StHbtV0::SetcTauK0Short( float& x){mcTauK0Short = x;}   
inline void StHbtV0::SetptV0( float& x){mptV0 = x;}          
inline void StHbtV0::SetptotV0( float& x){mptotV0 = x;}
inline void StHbtV0::SetptPos( float& x){mptPos = x;}
inline void StHbtV0::SetptotPos( float& x){mptotPos = x;}    
inline void StHbtV0::SetptNeg( float& x){ mptNeg= x;}    
inline void StHbtV0::SetptotNeg( float& x){ mptotNeg= x;}
inline void StHbtV0::SetidNeg(const int& i){ midNeg= 0;}
inline void StHbtV0::SetidPos(const int& i){ midPos= 0;}

#endif



















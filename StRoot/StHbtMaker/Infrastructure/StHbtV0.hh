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
 * Revision 1.5  2000/05/03 17:44:43  laue
 * StHbtEvent, StHbtTrack & StHbtV0 declared friend to StHbtIOBinary
 * StHbtParticle updated for V0 pos,neg track Id
 *
 * Revision 1.4  2000/02/18 21:32:24  laue
 * franksTrackCut changed. If mCharge is set to '0' there will be no cut
 * on charge. This is important for front-loaded cuts.
 *
 * copy constructor implemented for StHbtEvent, StHbtTrack and StHbtV0.
 *
 * franks1HistoD.cxx franks1HistoD.h franks2HistoD.cxx franks2HistoD.h
 * removed. We can now (CC5 on Solaris) use the versions (no D)
 *
 * Revision 1.3  2000/02/01 00:33:32  laue
 * namespaces changed to run on the new Solaris Compiler CC5
 * since we can use member templates in franks1Histo.hh we are doing it
 *
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

#include <fstream.h>
#include "StHbtMaker/Infrastructure/StHbtTypes.hh" //same as in StHbtTrack.hh

class StHbtV0 {
public:
  StHbtV0(){/* no-op */}
  StHbtV0(const StHbtV0&); // copy constructor
  ~StHbtV0(){/* no-op */}


  float decayLengthV0() const;       // 3-d decay distance
  StHbtThreeVector decayVertexV0() const; // Coordinates of decay vertex
  float decayVertexV0X() const; // Coordinates of decay vertex
  float decayVertexV0Y() const; // Coordinates of decay vertex
  float decayVertexV0Z() const; // Coordinates of decay vertex
  float dcaV0Daughters() const;      // DCA of v0 daughters at decay vertex
  float dcaV0ToPrimVertex() const;   // DCA of v0 to primary vertex
  float dcaPosToPrimVertex() const;  // DCA of pos v0 daughter to pri vertex
  float dcaNegToPrimVertex() const;  // DCA of neg v0 daughter to pri vertex
  StHbtThreeVector momPos() const;   // Momentum components of pos. daughter
  float momPosX() const;   // Momentum components of pos. daughter
  float momPosY() const;   // Momentum components of pos. daughter
  float momPosZ() const;   // Momentum components of pos. daughter
  StHbtThreeVector momNeg() const;   // Momentum components of neg. daughter
  float momNegX() const;   // Momentum components of neg. daughter
  float momNegY() const;   // Momentum components of neg. daughter
  float momNegZ() const;   // Momentum components of neg. daughter

  int   tpcHitsPos() const;          // Number of TPC hits on pos. daughter
  int   tpcHitsNeg() const;          // Number of TPC hits on neg. daughter
  unsigned long trackTopologyMapPos(unsigned int) const;
  unsigned long trackTopologyMapNeg(unsigned int) const;

  StHbtThreeVector momV0() const ;    // Momentum components of V0
  float momV0X() const ;    // Momentum components of V0
  float momV0Y() const ;    // Momentum components of V0
  float momV0Z() const ;    // Momentum components of V0
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
  unsigned short   idNeg() const;               // Id of negative track
  unsigned short   idPos() const;               // Id of positive track
  unsigned short   keyNeg() const;               // Id of negative track
  unsigned short   keyPos() const;               // Id of positive track

  void UpdateV0(); // Fills derived info
  void SetdecayLengthV0(const float);  
  void SetdecayVertexV0(const StHbtThreeVector);  
  void SetdecayVertexV0X(const float);
  void SetdecayVertexV0Y(const float);
  void SetdecayVertexV0Z(const float);
  void SetdcaV0Daughters(const float); 
  void SetdcaV0ToPrimVertex(const float);  
  void SetdcaPosToPrimVertex(const float); 
  void SetdcaNegToPrimVertex(const float); 
  void SetmomPos(const StHbtThreeVector);  
  void SetmomPosX(const float);  
  void SetmomPosY(const float);  
  void SetmomPosZ(const float);  
  void SetmomNeg(const StHbtThreeVector);  
  void SetmomNegX(const float);  
  void SetmomNegY(const float);  
  void SetmomNegZ(const float);  

  void SettpcHitsPos(const int&);      
  void SettpcHitsNeg(const int&);      

  void SetTrackTopologyMapPos(unsigned int, const unsigned long&);
  void SetTrackTopologyMapNeg(unsigned int, const unsigned long&);      

  void SetmomV0( StHbtThreeVector);
  void SetmomV0X( float);
  void SetmomV0Y( float);
  void SetmomV0Z( float);
  void SetalphaV0( float);       
  void SetptArmV0( float);       
  void SeteLambda( float);     
  void SeteK0Short( float);    
  void SetePosProton( float);  
  void SetePosPion( float);    
  void SeteNegProton( float);  
  void SeteNegPion( float);    
  void SetmassLambda( float);  
  void SetmassAntiLambda( float);
  void SetmassK0Short( float);  
  void SetrapLambda( float);    
  void SetrapK0Short( float);   
  void SetcTauLambda( float);   
  void SetcTauK0Short( float);  
  void SetptV0( float);         
  void SetptotV0( float);       
  void SetptPos( float);        
  void SetptotPos( float);      
  void SetptNeg( float);        
  void SetptotNeg( float);
  void SetidNeg(const unsigned short&);
  void SetidPos(const unsigned short&);
  void SetkeyNeg(const unsigned short&);
  void SetkeyPos(const unsigned short&);
     
  
  friend ostream& operator<<(ostream& out, StHbtV0& v0);
  friend istream& operator>>(istream& in,  StHbtV0& v0);

  friend class StHbtIOBinary;

protected:
 
  float mdecayLengthV0;
  StHbtThreeVector mdecayVertexV0;
  float mdcaV0Daughters;
  float mdcaV0ToPrimVertex;
  float mdcaPosToPrimVertex;
  float mdcaNegToPrimVertex;
  StHbtThreeVector mmomPos;
  StHbtThreeVector mmomNeg;

  unsigned long  mTrackTopologyMapPos[2];
  unsigned long  mTrackTopologyMapNeg[2];
       
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
  unsigned short   midNeg;
  unsigned short   midPos;

};



inline float StHbtV0::decayLengthV0() const { return mdecayLengthV0; }
inline StHbtThreeVector StHbtV0::decayVertexV0() const { return mdecayVertexV0; } 
inline float StHbtV0::decayVertexV0X() const { return mdecayVertexV0.x(); } 
inline float StHbtV0::decayVertexV0Y() const { return mdecayVertexV0.y(); } 
inline float StHbtV0::decayVertexV0Z() const { return mdecayVertexV0.z(); } 
inline float StHbtV0::dcaV0Daughters() const { return mdcaV0Daughters; }
inline float StHbtV0::dcaV0ToPrimVertex() const { return mdcaV0ToPrimVertex; }
inline float StHbtV0::dcaPosToPrimVertex() const { return mdcaPosToPrimVertex; }
inline float StHbtV0::dcaNegToPrimVertex() const { return mdcaNegToPrimVertex; }
inline StHbtThreeVector StHbtV0::momPos() const { return mmomPos; }
inline float StHbtV0::momPosX() const { return mmomPos.x(); }
inline float StHbtV0::momPosY() const { return mmomPos.y(); }
inline float StHbtV0::momPosZ() const { return mmomPos.z(); }
inline StHbtThreeVector StHbtV0::momNeg() const { return mmomNeg; }
inline float StHbtV0::momNegX() const { return mmomNeg.x(); }
inline float StHbtV0::momNegY() const { return mmomNeg.y(); }
inline float StHbtV0::momNegZ() const { return mmomNeg.z(); }
inline StHbtThreeVector StHbtV0::momV0() const { return mmomV0; }
inline float StHbtV0::momV0X() const { return mmomV0.x(); }
inline float StHbtV0::momV0Y() const { return mmomV0.y(); }
inline float StHbtV0::momV0Z() const { return mmomV0.z(); }
inline float StHbtV0::alphaV0() const { return malphaV0; }
inline float StHbtV0::ptArmV0() const {return mptArmV0;}
inline float StHbtV0::eLambda() const {return meLambda;}
inline float StHbtV0::eK0Short() const {return meK0Short;}
inline float StHbtV0::ePosProton() const {return mePosProton;}
inline float StHbtV0::ePosPion() const {return mePosPion;}
inline float StHbtV0::eNegProton() const {return meNegProton;}
inline float StHbtV0::eNegPion() const {return meNegPion;}
inline float StHbtV0::massLambda() const {return mmassLambda;}
inline float StHbtV0::massAntiLambda() const {return mmassAntiLambda;}
inline float StHbtV0::massK0Short() const {return mmassK0Short;}
inline float StHbtV0::rapLambda() const {return mrapLambda;}
inline float StHbtV0::rapK0Short() const {return mrapK0Short;}
inline float StHbtV0::cTauLambda() const {return mcTauLambda;}
inline float StHbtV0::cTauK0Short() const {return mcTauK0Short;}
inline float StHbtV0::ptV0() const {return mptV0;}
inline float StHbtV0::ptotV0() const {return mptotV0;}
inline float StHbtV0::ptPos() const {return mptPos;}
inline float StHbtV0::ptotPos() const {return mptotPos;}
inline float StHbtV0::ptNeg() const {return mptNeg;}
inline float StHbtV0::ptotNeg() const {return mptotNeg;}
inline int   StHbtV0::tpcHitsPos() const
             { return mtpcHitsPos; }
inline int   StHbtV0::tpcHitsNeg() const
             { return mtpcHitsNeg; }

inline unsigned long   StHbtV0::trackTopologyMapPos(unsigned int word) const { return mTrackTopologyMapPos[word]; }
inline unsigned long   StHbtV0::trackTopologyMapNeg(unsigned int word) const { return mTrackTopologyMapNeg[word]; }
inline unsigned short   StHbtV0::idNeg() const { return midNeg; }; 
inline unsigned short   StHbtV0::keyNeg() const { return midNeg; }
inline unsigned short   StHbtV0::idPos() const { return midPos; }; 
inline unsigned short   StHbtV0::keyPos() const { return midPos; }

inline void StHbtV0::SetdecayLengthV0(const float x){ mdecayLengthV0= x;}   
inline void StHbtV0::SetdecayVertexV0X(const float x){ mdecayVertexV0.setX(x);}
inline void StHbtV0::SetdecayVertexV0Y(const float x){ mdecayVertexV0.setY(x);}
inline void StHbtV0::SetdecayVertexV0Z(const float x){ mdecayVertexV0.setZ(x);}
inline void StHbtV0::SetdecayVertexV0(const StHbtThreeVector v){ mdecayVertexV0 = v; }
inline void StHbtV0::SetdcaV0Daughters(const float x){mdcaV0Daughters= x;} 
inline void StHbtV0::SetdcaV0ToPrimVertex(const float x){mdcaV0ToPrimVertex= x;}   
inline void StHbtV0::SetdcaPosToPrimVertex(const float x){mdcaPosToPrimVertex = x;} 
inline void StHbtV0::SetdcaNegToPrimVertex(const float x){mdcaNegToPrimVertex = x;} 
inline void StHbtV0::SetmomPos(const StHbtThreeVector v){mmomPos = v; }
inline void StHbtV0::SetmomPosX(const float x){mmomPos.setX(x);}
inline void StHbtV0::SetmomPosY(const float x){mmomPos.setY(x);}
inline void StHbtV0::SetmomPosZ(const float x){mmomPos.setZ(x);}
inline void StHbtV0::SetmomNeg(const StHbtThreeVector v){mmomNeg = v; }
inline void StHbtV0::SetmomNegX(const float x){mmomNeg.setX(x);}
inline void StHbtV0::SetmomNegY(const float x){mmomNeg.setY(x);}
inline void StHbtV0::SetmomNegZ(const float x){mmomNeg.setZ(x);}
inline void StHbtV0::SetTrackTopologyMapPos(unsigned int word, const unsigned long& m){mTrackTopologyMapPos[word]=m;} 
inline void StHbtV0::SetTrackTopologyMapNeg(unsigned int word, const unsigned long& m){mTrackTopologyMapNeg[word]=m;} 
inline void StHbtV0::SetmomV0(StHbtThreeVector v){mmomV0= v; }
inline void StHbtV0::SetmomV0X(const float x){mmomV0.setX(x);}
inline void StHbtV0::SetmomV0Y(const float x){mmomV0.setY(x);}
inline void StHbtV0::SetmomV0Z(const float x){mmomV0.setZ(x);}

inline void StHbtV0::SetalphaV0( float x){malphaV0= x;}
inline void StHbtV0::SetptArmV0( float x){mptArmV0 = x;}
inline void StHbtV0::SeteLambda( float x){meLambda= x;}       
inline void StHbtV0::SeteK0Short( float x){meK0Short= x;}
inline void StHbtV0::SetePosProton( float x){mePosProton= x;}      
inline void StHbtV0::SetePosPion( float x){mePosPion= x;}      
inline void StHbtV0::SeteNegProton( float x){meNegProton= x;} 
inline void StHbtV0::SeteNegPion( float x){meNegPion= x;}       
inline void StHbtV0::SetmassLambda( float x){mmassLambda = x;} 
inline void StHbtV0::SetmassAntiLambda( float x){mmassAntiLambda= x;} 
inline void StHbtV0::SetmassK0Short( float x){mmassK0Short= x;}  
inline void StHbtV0::SetrapLambda( float x){mrapLambda= x;}
inline void StHbtV0::SetrapK0Short( float x){mrapK0Short = x;}   
inline void StHbtV0::SetcTauLambda( float x){mcTauLambda = x;}   
inline void StHbtV0::SetcTauK0Short( float x){mcTauK0Short = x;}   
inline void StHbtV0::SetptV0( float x){mptV0 = x;}          
inline void StHbtV0::SetptotV0( float x){mptotV0 = x;}
inline void StHbtV0::SetptPos( float x){mptPos = x;}
inline void StHbtV0::SetptotPos( float x){mptotPos = x;}    
inline void StHbtV0::SetptNeg( float x){ mptNeg= x;}    
inline void StHbtV0::SetptotNeg( float x){ mptotNeg= x;}
inline void StHbtV0::SetidNeg(const unsigned short& s){ midNeg= s;}
inline void StHbtV0::SetidPos(const unsigned short& s){ midPos= s;}
inline void StHbtV0::SetkeyNeg(const unsigned short& s){ midNeg= s;}
inline void StHbtV0::SetkeyPos(const unsigned short& s){ midPos= s;}
inline void StHbtV0::SettpcHitsPos(const int& i){mtpcHitsPos=i;} 
inline void StHbtV0::SettpcHitsNeg(const int& i){mtpcHitsNeg=i;}

#endif



















/*!
 * \class StXiMc
 * \author Gene Van Buren
 *
 *               Monte Carlo Xi micro dst class
 *               where MC Xi is treated as a MC Kink and a MC V0
 *
 */

#ifndef  STAR_StXiMc
#define  STAR_StXiMc
#include "StKinkMc.hh"
#include "StXiI.hh"
#include "StDecayMode.hh"


class StXiMc : public StKinkMc, public virtual StXiI {
public:
  StXiMc();
  StXiMc(StMcVertex*, StMcTrack*, StStrangeEvMuDst*);
  ~StXiMc();

  Int_t V0Index();
  void SetV0Index(Int_t index);

  /// Particle charge
  Int_t   charge() const;

  Float_t decayVertexXiX() const;
  Float_t decayVertexXiY() const;      /// Coordinate of decay vertex
  Float_t decayVertexXiZ() const;

  Float_t momBachelorX() const;
  Float_t momBachelorY() const;        /// Momentum components of bachelor
  Float_t momBachelorZ() const;

  /// Momentum of Xi/Omega at decay vertex
  TVector3 momXi();
  Float_t momXiX();
  Float_t momXiY();             /// Momentum components of Xi/Omega at decay vertex
  Float_t momXiZ();

  /// Momentum of Xi/Omega at primary vertex
  TVector3 momXiAtPrimVertex();
  Float_t momXiAtPrimVertexX();
  Float_t momXiAtPrimVertexY(); /// Momentum components of Xi/Omega at primary vertex
  Float_t momXiAtPrimVertexZ();

  Float_t decayVertexV0X() const {return 999.;}
  Float_t decayVertexV0Y() const {return 999.;} /// Coordinates of decay vertex
  Float_t decayVertexV0Z() const {return 999.;}
  Float_t momPosX() const {return 999;}
  Float_t momPosY() const {return 999;} /// Momentum components of pos. daughter
  Float_t momPosZ() const {return 999;}
  Float_t momNegX() const {return 999;}
  Float_t momNegY() const {return 999;} /// Momentum components of neg. daughter
  Float_t momNegZ() const {return 999;}
  Float_t momV0X()  const {return 999;}
  Float_t momV0Y()  const {return 999;} /// Momentum components of V0
  Float_t momV0Z()  const {return 999;}

  /// Returns the decay process for the vertex,
  /// where the decay modes are enumerated in ::decayModeType
  Int_t decayMode() const;
  Int_t geantIdParent() const;

protected:
  Int_t v0;
  ClassDef(StXiMc,7)
};

inline Int_t   StXiMc::charge() const {return parentCharge();}
inline Float_t StXiMc::decayVertexXiX() const {return positionX();}
inline Float_t StXiMc::decayVertexXiY() const {return positionY();}
inline Float_t StXiMc::decayVertexXiZ() const {return positionZ();}
inline Float_t StXiMc::momBachelorX() const {return daughterMomentumX();}
inline Float_t StXiMc::momBachelorY() const {return daughterMomentumY();}
inline Float_t StXiMc::momBachelorZ() const {return daughterMomentumZ();}
inline TVector3 StXiMc::momXi()
  {return TVector3(momXiX(), momXiY(), momXiZ());}
inline Float_t StXiMc::momXiX() {return parentMomentumX();}
inline Float_t StXiMc::momXiY() {return parentMomentumY();}
inline Float_t StXiMc::momXiZ() {return parentMomentumZ();}
inline TVector3 StXiMc::momXiAtPrimVertex()
  {return TVector3(momXiAtPrimVertexX(), momXiAtPrimVertexY(), momXiAtPrimVertexZ());}
inline Float_t StXiMc::momXiAtPrimVertexX() {return parentPrimMomentumX();}
inline Float_t StXiMc::momXiAtPrimVertexY() {return parentPrimMomentumY();}
inline Float_t StXiMc::momXiAtPrimVertexZ() {return parentPrimMomentumZ();}
inline Int_t   StXiMc::V0Index() {return v0;}
inline void    StXiMc::SetV0Index(Int_t index) {v0=index;}
inline Int_t   StXiMc::geantIdParent() const {return StKinkMc::geantIdParent();}
inline Int_t   StXiMc::decayMode() const {return StKinkMc::decayMode();}


#endif


/***********************************************************************
 * $Id: StXiMc.hh,v 3.8 2005/07/21 16:48:47 perev Exp $
 * $Log: StXiMc.hh,v $
 * Revision 3.8  2005/07/21 16:48:47  perev
 * Change ClassDef(StXiMc,6)==>7
 *
 * Revision 3.7  2003/10/20 17:20:19  perev
 * Change the order of inheritance and increased version numbers
 *
 * Revision 3.6  2003/08/26 22:36:28  genevb
 * Calculate Xi momenta at/near primary vertex
 *
 * Revision 3.5  2003/06/01 04:25:19  genevb
 * Update ClassDef version for altered inheritance
 *
 * Revision 3.4  2003/05/30 21:20:20  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.3  2001/08/23 13:21:00  genevb
 * Many bug workarounds...
 *
 * Revision 3.2  2001/05/04 20:15:15  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.1  2000/07/14 21:28:34  genevb
 * Added V0Mc index for XiMc, fixed bug with entries for XiMc, cleaned up controllers
 *
 * Revision 3.0  2000/07/14 12:56:50  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.0  2000/06/05 05:19:47  genevb
 * New version of Strangeness micro DST package
 *
 ***********************************************************************/

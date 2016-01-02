#ifndef StHFCUTS_H
#define StHFCUTS_H

/* **************************************************
 *  Cut class for HF analysis
 *  - Based on PicoCuts class 
 *
 *  Initial Authors:  
 *            Xin Dong        (xdong@lbl.gov)
 *            Mustafa Mustafa (mmustafa@lbl.gov)
 *          **Jochen Thaeder  (jmthader@lbl.gov)   
 *
 *  Contributing Authors
 *            Michael Lomnitz (mrlomnitz@lbl.gov)
 *            Guannan Xie     (guannanxie@lbl.gov)
 *
 *  ** Code Maintainer
 *
 * **************************************************
 */

#include "StPicoCutsBase/StPicoCutsBase.h"


class StHFPair;
class StHFTriplet;

class StHFCuts : public StPicoCutsBase
{
 public:
  
  StHFCuts();
  StHFCuts(const Char_t *name);
  ~StHFCuts();
  
  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   

  virtual void init() { initBase(); }

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   

  bool isClosePair(StHFPair const & pair) const;

  bool isGoodSecondaryVertexPair(StHFPair const & pair) const;
  bool isGoodTertiaryVertexPair(StHFPair const & pair) const;
  bool isGoodSecondaryVertexTriplet(StHFTriplet const & triplet) const;

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   
  // -- SETTER for CUTS
  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 
  
  void setCutSecondaryPair(float dcaDaughtersMax, float decayLengthMin, float decayLengthMax, 
			   float cosThetaMin, float massMin, float massMax); 
  
  void setCutTertiaryPair(float dcaDaughtersMax, float decayLengthMin, float decayLengthMax, 
			  float cosThetaMin, float massMin, float massMax); 
  
  void setCutSecondaryTriplet(float dcaDaughters12Max, float dcaDaughters23Max, float dcaDaughters31Max, 
			      float decayLengthMin, float decayLengthMax, 
			      float cosThetaMin, float massMin, float massMax);

  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --   
  // -- GETTER for single CUTS
  // -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- 

  const float&    cutSecondaryPairDcaDaughtersMax()       const;
  const float&    cutSecondaryPairDecayLengthMin()        const;
  const float&    cutSecondaryPairDecayLengthMax()        const;
  const float&    cutSecondaryPairCosThetaMin()           const;
  const float&    cutSecondaryPairMassMin()               const;
  const float&    cutSecondaryPairMassMax()               const;

  const float&    cutTertiaryPairDcaDaughtersMax()        const;
  const float&    cutTertiaryPairDecayLengthMin()         const;
  const float&    cutTertiaryPairDecayLengthMax()         const;
  const float&    cutTertiaryPairCosThetaMin()            const;
  const float&    cutTertiaryPairMassMin()                const;
  const float&    cutTertiaryPairMassMax()                const;

  const float&    cutSecondaryTripletDcaDaughters12Max()  const;
  const float&    cutSecondaryTripletDcaDaughters23Max()  const;
  const float&    cutSecondaryTripletDcaDaughters31Max()  const;
  const float&    cutSecondaryTripletDecayLengthMin()     const;
  const float&    cutSecondaryTripletDecayLengthMax()     const;
  const float&    cutSecondaryTripletCosThetaMin()        const;
  const float&    cutSecondaryTripletMassMin()            const;
  const float&    cutSecondaryTripletMassMax()            const;

 private:
  
  StHFCuts(StHFCuts const &);       
  StHFCuts& operator=(StHFCuts const &); 

  // ------------------------------------------
  // -- Pair cuts for secondary pair
  // ------------------------------------------
  float mSecondaryPairDcaDaughtersMax;
  float mSecondaryPairDecayLengthMin; 
  float mSecondaryPairDecayLengthMax; 
  float mSecondaryPairCosThetaMin;
  float mSecondaryPairMassMin;
  float mSecondaryPairMassMax;

  // ------------------------------------------
  // -- Pair cuts tertiary pair
  // ------------------------------------------
  float mTertiaryPairDcaDaughtersMax;
  float mTertiaryPairDecayLengthMin; 
  float mTertiaryPairDecayLengthMax; 
  float mTertiaryPairCosThetaMin;
  float mTertiaryPairMassMin;
  float mTertiaryPairMassMax;

  // ------------------------------------------
  // -- Cuts of secondary triplet
  // ------------------------------------------
  float mSecondaryTripletDcaDaughters12Max;
  float mSecondaryTripletDcaDaughters23Max;
  float mSecondaryTripletDcaDaughters31Max;
  float mSecondaryTripletDecayLengthMin; 
  float mSecondaryTripletDecayLengthMax; 
  float mSecondaryTripletCosThetaMin;
  float mSecondaryTripletMassMin;
  float mSecondaryTripletMassMax;

  ClassDef(StHFCuts,1)
};

inline void StHFCuts::setCutSecondaryPair(float dcaDaughtersMax, float decayLengthMin, float decayLengthMax, 
					  float cosThetaMin, float massMin, float massMax)  {
  mSecondaryPairDcaDaughtersMax = dcaDaughtersMax;
  mSecondaryPairDecayLengthMin = decayLengthMin; mSecondaryPairDecayLengthMax = decayLengthMax;
  mSecondaryPairCosThetaMin = cosThetaMin;
  mSecondaryPairMassMin = massMin; mSecondaryPairMassMax = massMax; }

inline void StHFCuts::setCutTertiaryPair(float dcaDaughtersMax, float decayLengthMin, float decayLengthMax, 
					 float cosThetaMin, float massMin, float massMax)  {
  mTertiaryPairDcaDaughtersMax = dcaDaughtersMax;
  mTertiaryPairDecayLengthMin = decayLengthMin; mTertiaryPairDecayLengthMax = decayLengthMax;
  mTertiaryPairCosThetaMin = cosThetaMin;
  mTertiaryPairMassMin = massMin; mTertiaryPairMassMax = massMax; }
  
inline void StHFCuts::setCutSecondaryTriplet(float dcaDaughters12Max, float dcaDaughters23Max, float dcaDaughters31Max, 
					     float decayLengthMin, float decayLengthMax, 
					     float cosThetaMin, float massMin, float massMax)  {
  mSecondaryTripletDcaDaughters12Max = dcaDaughters12Max; mSecondaryTripletDcaDaughters23Max = dcaDaughters23Max; 
  mSecondaryTripletDcaDaughters31Max = dcaDaughters31Max; 
  mSecondaryTripletDecayLengthMin = decayLengthMin; mSecondaryTripletDecayLengthMax = decayLengthMax; 
  mSecondaryTripletCosThetaMin = cosThetaMin;
  mSecondaryTripletMassMin = massMin; mSecondaryTripletMassMax = massMax; }

inline const float&    StHFCuts::cutSecondaryPairDcaDaughtersMax()       const { return mSecondaryPairDcaDaughtersMax; }
inline const float&    StHFCuts::cutSecondaryPairDecayLengthMin()        const { return mSecondaryPairDecayLengthMin; }
inline const float&    StHFCuts::cutSecondaryPairDecayLengthMax()        const { return mSecondaryPairDecayLengthMax; }
inline const float&    StHFCuts::cutSecondaryPairCosThetaMin()           const { return mSecondaryPairCosThetaMin; }
inline const float&    StHFCuts::cutSecondaryPairMassMin()               const { return mSecondaryPairMassMin; }
inline const float&    StHFCuts::cutSecondaryPairMassMax()               const { return mSecondaryPairMassMax; }

inline const float&    StHFCuts::cutTertiaryPairDcaDaughtersMax()        const { return mTertiaryPairDcaDaughtersMax; }
inline const float&    StHFCuts::cutTertiaryPairDecayLengthMin()         const { return mTertiaryPairDecayLengthMin; }
inline const float&    StHFCuts::cutTertiaryPairDecayLengthMax()         const { return mTertiaryPairDecayLengthMax; }
inline const float&    StHFCuts::cutTertiaryPairCosThetaMin()            const { return mTertiaryPairCosThetaMin; }
inline const float&    StHFCuts::cutTertiaryPairMassMin()                const { return mTertiaryPairMassMin; }
inline const float&    StHFCuts::cutTertiaryPairMassMax()                const { return mTertiaryPairMassMax; }

inline const float&    StHFCuts::cutSecondaryTripletDcaDaughters12Max()  const { return mSecondaryTripletDcaDaughters12Max; }
inline const float&    StHFCuts::cutSecondaryTripletDcaDaughters23Max()  const { return mSecondaryTripletDcaDaughters23Max; }
inline const float&    StHFCuts::cutSecondaryTripletDcaDaughters31Max()  const { return mSecondaryTripletDcaDaughters31Max; }
inline const float&    StHFCuts::cutSecondaryTripletDecayLengthMin()     const { return mSecondaryTripletDecayLengthMin; }
inline const float&    StHFCuts::cutSecondaryTripletDecayLengthMax()     const { return mSecondaryTripletDecayLengthMax; }
inline const float&    StHFCuts::cutSecondaryTripletCosThetaMin()        const { return mSecondaryTripletCosThetaMin; }
inline const float&    StHFCuts::cutSecondaryTripletMassMin()            const { return mSecondaryTripletMassMin; }
inline const float&    StHFCuts::cutSecondaryTripletMassMax()            const { return mSecondaryTripletMassMax; }

#endif

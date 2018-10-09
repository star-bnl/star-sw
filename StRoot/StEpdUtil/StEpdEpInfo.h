#ifndef StEpdEpInfo_H
#define StEpdEpInfo_H

#define _EpOrderMax 3   // maximum order of EP to worry about.

#include "TVector2.h"

/// the class StEpdEpInfo has only public members.
/// No need to hide information.  Indeed, it's really only a struct,
/// with the possibility to create some methods.
class StEpdEpInfo{

  // making StEpdEpFinder a "friend class" just gives it direct access to the arrays.
  // But the general User is required to use accessors, since the numbering/index convention can be confusing
  friend class StEpdEpFinder;


 public:
  StEpdEpInfo();
  ~StEpdEpInfo(){/* no op */};

  
  // in the below, when it says "of order," then "order" begins at 1.  E.g. order=2 means second-order q vector

  //-----------------------------------------------------------------------------------------
  /// Raw (no phi-weighting) Q vector from East EPD.
  /// \parameter order     order of the Q-vector.  Begins at unity (order=1 means first-order Q)
  TVector2 EastRawQ(int order);
  /// Raw (no phi-weighting) Q vector from West EPD.
  /// \parameter order     order of the Q-vector.  Begins at unity (order=1 means first-order Q)
  TVector2 WestRawQ(int order);

  //-----------------------------------------------------------------------------------------
  /// Phi weighted Q vector from East EPD.
  /// \parameter order     order of the Q-vector.  Begins at unity (order=1 means first-order Q)
  TVector2 EastPhiWeightedQ(int order);
  /// Phi weighted Q vector from West EPD.
  /// \parameter order     order of the Q-vector.  Begins at unity (order=1 means first-order Q)
  TVector2 WestPhiWeightedQ(int order);

  //-----------------------------------------------------------------------------------------
  /// Raw (no phi-weighting, no shifting) Event plane angle from East EPD
  /// ** important note!  for first-order plane, East angle is rotated by 180 degrees,
  /// ** so that directed flow will show positive correlation between EP[1] angle for East and West 
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  double EastRawPsi(int order);
  /// Raw (no phi-weighting, no shifting) Event plane angle from West EPD
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  double WestRawPsi(int order);
  /// Raw (no phi-weighting, no shifting) Event plane angle from Full (East and West) EPDs
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  double FullRawPsi(int order);

  //-----------------------------------------------------------------------------------------
  /// Phi-weighted (but not shift flattened) Event plane angle from East EPD
  /// ** important note!  for first-order plane, East angle is rotated by 180 degrees,
  /// ** so that directed flow will show positive correlation between EP[1] angle for East and West 
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  double EastPhiWeightedPsi(int order);
  /// Phi-weighted (but not shift flattened) Event plane angle from West EPD
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  double WestPhiWeightedPsi(int order);
  /// Phi-weighted (but not shift flattened) Event plane angle from Full (East and West) EPDs
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  double FullPhiWeightedPsi(int order);

  //-----------------------------------------------------------------------------------------
  /// Phi-weighted and shift-corrected Event plane angle from East EPD
  /// ** important note!  for first-order plane, East angle is rotated by 180 degrees,
  /// ** so that directed flow will show positive correlation between EP[1] angle for East and West 
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  double EastPhiWeightedAndShiftedPsi(int order);
  /// Phi-weighted and shift-corrected Event plane angle from West EPD
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  double WestPhiWeightedAndShiftedPsi(int order);
  /// Phi-weighted and shift-corrected Event plane angle from Full (East and West) EPDs
  /// ---->  If you are saying "Just give me THE event-plane angle!!" then this is probably what you want to use.
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  double FullPhiWeightedAndShiftedPsi(int order);

  // ============================== Ring-by-ring stuff below ========================

  //-----------------------------------------------------------------------------------------
  /// Raw (no phi-weighting) Q vector from one ring of East EPD.
  /// \parameter order     order of the Q-vector.  Begins at unity (order=1 means first-order Q)
  /// \parameter ring      a.k.a. "row".  The ring number of the wheel.  ring 1 is innermost, ring 16 is outermost
  TVector2 EastRingRawQ(int order, int ring);
  /// Raw (no phi-weighting) Q vector from one ring of West EPD.
  /// \parameter order     order of the Q-vector.  Begins at unity (order=1 means first-order Q)
  /// \parameter ring      a.k.a. "row".  The ring number of the wheel.  ring 1 is innermost, ring 16 is outermost
  TVector2 WestRingRawQ(int order, int ring);

  //-----------------------------------------------------------------------------------------
  /// Phi-weighted Q vector from one ring of East EPD.
  /// \parameter order     order of the Q-vector.  Begins at unity (order=1 means first-order Q)
  /// \parameter ring      a.k.a. "row".  The ring number of the wheel.  ring 1 is innermost, ring 16 is outermost
  TVector2 EastRingPhiWeightedQ(int order, int ring);
  /// Phi-weighted Q vector from one ring of West EPD.
  /// \parameter order     order of the Q-vector.  Begins at unity (order=1 means first-order Q)
  /// \parameter ring      a.k.a. "row".  The ring number of the wheel.  ring 1 is innermost, ring 16 is outermost
  TVector2 WestRingPhiWeightedQ(int order, int ring);

  //-----------------------------------------------------------------------------------------
  /// Raw (no phi-weighting, no shifting) Event plane angle from one ring of the East EPD
  /// ** important note!  for first-order plane, East angle is rotated by 180 degrees,
  /// ** so that directed flow will show positive correlation between EP[1] angle for East and West 
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  /// \parameter ring      a.k.a. "row".  The ring number of the wheel.  ring 1 is innermost, ring 16 is outermost
  double EastRingRawPsi(int order, int ring);
  /// Raw (no phi-weighting, no shifting) Event plane angle from one ring of West EPD
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  /// \parameter ring      a.k.a. "row".  The ring number of the wheel.  ring 1 is innermost, ring 16 is outermost
  double WestRingRawPsi(int order, int ring);

  //-----------------------------------------------------------------------------------------
  /// Phi-weighted (but no shifting) Event plane angle from one ring of the East EPD
  /// ** important note!  for first-order plane, East angle is rotated by 180 degrees,
  /// ** so that directed flow will show positive correlation between EP[1] angle for East and West 
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  /// \parameter ring      a.k.a. "row".  The ring number of the wheel.  ring 1 is innermost, ring 16 is outermost
  double EastRingPhiWeightedPsi(int order, int ring);
  /// Phi-weighted (but no shifting) Event plane angle from one ring of the West EPD
  /// \parameter order     order of the EP.  Begins at unity (order=1 means first-order EP)
  /// \parameter ring      a.k.a. "row".  The ring number of the wheel.  ring 1 is innermost, ring 16 is outermost
  double WestRingPhiWeightedPsi(int order, int ring);


 private:

  bool ArgumentOutOfBounds(int order);              /// protection against user selecting "order=0" or order that has not been defined
  bool ArgumentOutOfBounds(int order, int ring);    /// additional protection that 1<=ring<=16

  TVector2 RawQ(int ew, int order);                        /// called internally
  TVector2 PhiWeightedQ(int ew, int order);                /// called internally
  TVector2 RingRawQ(int ew, int order, int ring);          /// called internally
  TVector2 RingPhiWeightedQ(int ew, int order, int ring);  /// called internally

  double RawPsi(int ewfull, int order);                    /// called internally
  double PhiWeightedPsi(int ewfull, int order);            /// called internally
  double PhiWeightedAndShiftedPsi(int ewfull, int order);  /// called internally
  double RingRawPsi(int ew, int order, int ring);          /// called internally
  double RingPhiWeightedPsi(int ew, int order, int ring);  /// called internally

  double Range(double psi, int order);                     /// puts angle psi into range (0,2pi/n)



  double QrawOneSide[2][_EpOrderMax][2];           /// indices: [east,west][order][x,y]
  double QphiWeightedOneSide[2][_EpOrderMax][2];   /// indices: [east,west][order][x,y]
  double PsiRaw[3][_EpOrderMax];                   /// indices: [east,west,full][order]
  double PsiPhiWeighted[3][_EpOrderMax];           /// indices: [east,west,full][order]
  double PsiPhiWeightedAndShifted[3][_EpOrderMax]; /// indices: [east,west,full][order]
  // ring-by-ring
  double QringRaw[2][_EpOrderMax][2][16];          /// indices: [east,west][order][x,y][ring]
  double QringPhiWeighted[2][_EpOrderMax][2][16];  /// indices: [east,west][order][x,y][ring]
  double PsiRingRaw[2][_EpOrderMax][16];           /// indices: [east,west][order][ring]
  double PsiRingPhiWeighted[2][_EpOrderMax][16];   /// indices: [east,west][order][ring]
};

#endif

/***************************************************************************
 *
 * $Id: FlowTrack.hh,v 1.1 1999/11/04 19:02:08 snelling Exp $
 *
 * Author: Raimond Snellings and Art Poskanzer
 ***************************************************************************
 *
 * Description: part of Flow Framework
 ***************************************************************************
 *
 * $Log: FlowTrack.hh,v $
 * Revision 1.1  1999/11/04 19:02:08  snelling
 * First check in of StFlowMaker. It contains the common code from
 * StFlowTagMaker and StFlowAnalysisMaker.
 *
 **************************************************************************/

#ifndef FlowTrack_hh
#define FlowTrack_hh

class FlowTrack{
public:
  FlowTrack(){/* no-op*/};
  ~FlowTrack(){/* no-op*/};

  char Pid() const;
  float Phi() const;
  float Eta() const;
  float Pt() const;

  void SetPid(const char&);
  void SetPhi(const float&);
  void SetEta(const float&);
  void SetPt(const float&);

  // For I/O of this object -- functions defined in FlowIO.cc
  //  friend ostream& operator<<(ostream& out, FlowTrack& trk);
  //  friend istream& operator>>(istream& in,  FlowTrack& trk);

private:
  char mPid;
  float mPhi;
  float mEta;
  float mPt;

};

inline void FlowTrack::SetPid(const char& pid){mPid=pid;}
inline void FlowTrack::SetPhi(const float& phi){mPhi = phi;}              
inline void FlowTrack::SetEta(const float& eta){mEta = eta;}              
inline void FlowTrack::SetPt(const float& pt){mPt = pt;}              

inline char FlowTrack::Pid() const {return mPid;}
inline float FlowTrack::Phi() const {return mPhi;}                
inline float FlowTrack::Eta() const {return mEta;}                
inline float FlowTrack::Pt() const {return mPt;}                

#endif

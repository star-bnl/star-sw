/***************************************************************************
 *
 * $Id: StarMuL3EventSummary.h,v 1.1 2002/03/05 15:41:10 jeromel Exp $
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#ifndef StarMuL3EventSummary_h
#define StarMuL3EventSummary_h

#include "TObject.h"

//* bit masks, must go in powers of 2 */
#define __VERTEX__ 1
#define __UNBIASED__ 2

#include "StarClassLibrary/StThreeVectorF.hh"

class TClonesArray;
class StL3AlgorithmInfo;
class StL3EventSummary;
class StarMuL3AlgorithmInfo;
class StEvent;

class StarMuL3EventSummary : public TObject{
 public:
  StarMuL3EventSummary();
  ~StarMuL3EventSummary();

  void clear();
  int numberOfProcessedEvents() const;
  int numberOfReconstructedEvents() const;
  unsigned int numberOfTracks() const;
  unsigned int numberOfAlgorithms() const;
  int unbiasedTriggerPreScale() const;
  bool unbiasedTrigger() const;
  bool zVertexTrigger() const;
  unsigned int l0TriggerWord() const;
  StThreeVectorF primaryVertex() const;
 protected:
  friend class StarMuEvent;
  void fill(const StEvent*);

  int mNumberOfProcessedEvents;
  int mNumberReconstructedEvents;
  int mNumberOfTracks;
  int mNumberOfAlgorithms;
  unsigned char mFlags;    //* bit mask 1=mZVertexTrigger; bit mask mUnbiasedTrigger; **/
  unsigned int mL0TriggerWord;
  int mUnbiasedPreScale;
  StThreeVectorF mPrimaryVertex;

  ClassDef(StarMuL3EventSummary,1)
};


inline int StarMuL3EventSummary::numberOfProcessedEvents() const { return mNumberOfProcessedEvents; }
inline int StarMuL3EventSummary::numberOfReconstructedEvents() const { return mNumberReconstructedEvents; }
inline unsigned int StarMuL3EventSummary::numberOfTracks() const { return mNumberOfTracks; }
inline unsigned int StarMuL3EventSummary::numberOfAlgorithms() const { return mNumberOfAlgorithms; }
inline bool StarMuL3EventSummary::zVertexTrigger() const { return (__VERTEX__ & mFlags); }
inline bool StarMuL3EventSummary::unbiasedTrigger() const { return (__UNBIASED__ & mFlags); }
inline unsigned int StarMuL3EventSummary::l0TriggerWord() const { return mL0TriggerWord; }
inline int StarMuL3EventSummary::unbiasedTriggerPreScale() const { return mUnbiasedPreScale; }
inline StThreeVectorF StarMuL3EventSummary::primaryVertex() const { return mPrimaryVertex; }

#endif

/***************************************************************************
 *
 * $Log: StarMuL3EventSummary.h,v $
 * Revision 1.1  2002/03/05 15:41:10  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/

/***************************************************************************
 *
 * $Id: StMuL3EventSummary.h,v 1.1 2002/03/08 17:04:18 laue Exp $
 * Authors: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#ifndef StMuL3EventSummary_h
#define StMuL3EventSummary_h

#include "TObject.h"

//* bit masks, must go in powers of 2 */
#define __VERTEX__ 1
#define __UNBIASED__ 2

#include "StarClassLibrary/StThreeVectorF.hh"

class TClonesArray;
class StL3AlgorithmInfo;
class StL3EventSummary;
class StMuL3AlgorithmInfo;
class StEvent;

class StMuL3EventSummary : public TObject{
 public:
  StMuL3EventSummary();
  ~StMuL3EventSummary();

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
  friend class StMuEvent;
  void fill(const StEvent*);

  int mNumberOfProcessedEvents;
  int mNumberReconstructedEvents;
  int mNumberOfTracks;
  int mNumberOfAlgorithms;
  unsigned char mFlags;    //* bit mask 1=mZVertexTrigger; bit mask mUnbiasedTrigger; **/
  unsigned int mL0TriggerWord;
  int mUnbiasedPreScale;
  StThreeVectorF mPrimaryVertex;

  ClassDef(StMuL3EventSummary,1)
};


inline int StMuL3EventSummary::numberOfProcessedEvents() const { return mNumberOfProcessedEvents; }
inline int StMuL3EventSummary::numberOfReconstructedEvents() const { return mNumberReconstructedEvents; }
inline unsigned int StMuL3EventSummary::numberOfTracks() const { return mNumberOfTracks; }
inline unsigned int StMuL3EventSummary::numberOfAlgorithms() const { return mNumberOfAlgorithms; }
inline bool StMuL3EventSummary::zVertexTrigger() const { return (__VERTEX__ & mFlags); }
inline bool StMuL3EventSummary::unbiasedTrigger() const { return (__UNBIASED__ & mFlags); }
inline unsigned int StMuL3EventSummary::l0TriggerWord() const { return mL0TriggerWord; }
inline int StMuL3EventSummary::unbiasedTriggerPreScale() const { return mUnbiasedPreScale; }
inline StThreeVectorF StMuL3EventSummary::primaryVertex() const { return mPrimaryVertex; }

#endif

/***************************************************************************
 *
 * $Log: StMuL3EventSummary.h,v $
 * Revision 1.1  2002/03/08 17:04:18  laue
 * initial revision
 *
 *
 **************************************************************************/

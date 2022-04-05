/*!
 * \class StL3AlgorithmInfo
 * \author Christof Struck, July 2001
 */
/***************************************************************************
 *
 * $Id: StL3AlgorithmInfo.h,v 2.4 2003/05/23 20:40:44 ullrich Exp $
 *
 * Author: Christof Struck, July 2001
 ***************************************************************************
 *
 * Description: L3 Algorithm Information
 *
 ***************************************************************************
 *
 * $Log: StL3AlgorithmInfo.h,v $
 * Revision 2.4  2003/05/23 20:40:44  ullrich
 * Removed dependcies on DAQ lib in header file.
 *
 * Revision 2.3  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/08/20 21:29:00  ullrich
 * Changed counter type from UInt_t to Int_t.
 *
 * Revision 2.1  2001/08/02 01:26:31  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StL3AlgorithmInfo_hh
#define StL3AlgorithmInfo_hh

#include "StObject.h"
#include "TArrayF.h"
#include "TArrayI.h"

class Algorithm_Data;

class StL3AlgorithmInfo : public StObject
{
public:
      StL3AlgorithmInfo();
      StL3AlgorithmInfo(Algorithm_Data*);
      ~StL3AlgorithmInfo();

      int          id() const;
      bool         on() const;
      bool         accept() const;
      bool         build() const;
      int          numberOfProcessedEvents() const;
      int          numberOfAcceptedEvents() const;
      int          numberOfBuildEvents() const;
      int          dataSize() const;
      float        data(int index) const;
      int          preScale() const;
      int          postScale() const;
      int          intParameterSize() const;
      int          intParameter(int) const;
      int          floatParameterSize() const;
      float        floatParameter(int) const;

      void         setCounters(int, int, int);
      void         setParameters(int*, float*);
      void         setPreScale(int);
      void         setPostScale(int);

private:
      void         initArrays();

      Int_t    mId;
      Bool_t   mOn;
      Bool_t   mAccept;
      Bool_t   mBuild;
      Int_t    mNumberOfProcessedEvents;
      Int_t    mNumberOfAcceptedEvents;
      Int_t    mNumberOfBuildEvents;
      UShort_t mDataSize;
      TArrayF  mDataArray;
      Int_t    mPreScale;
      Int_t    mPostScale;
      UShort_t mIntParameterSize;
      TArrayI  mIntParameterArray;
      UShort_t mFloatParameterSize;
      TArrayF  mFloatParameterArray;
      
      ClassDef(StL3AlgorithmInfo, 1)
};

inline int
StL3AlgorithmInfo::id() const { return mId; }

inline bool
StL3AlgorithmInfo::on() const { return mOn; }

inline bool
StL3AlgorithmInfo::accept() const { return mAccept; }

inline bool
StL3AlgorithmInfo::build() const { return mBuild; }

inline int
StL3AlgorithmInfo::numberOfProcessedEvents() const { return mNumberOfProcessedEvents; }

inline int
StL3AlgorithmInfo::numberOfAcceptedEvents() const { return mNumberOfAcceptedEvents; }

inline int
StL3AlgorithmInfo::numberOfBuildEvents() const { return mNumberOfBuildEvents; }

inline int
StL3AlgorithmInfo::dataSize() const { return mDataSize; }

inline float
StL3AlgorithmInfo::data(int i) const
{
      return i < mDataSize ? const_cast<TArrayF&>(mDataArray)[i] : 0;
}

inline int
StL3AlgorithmInfo::preScale() const { return mPreScale; }

inline int
StL3AlgorithmInfo::postScale() const { return mPostScale; }

inline int
StL3AlgorithmInfo::intParameterSize() const { return mIntParameterSize; }

inline int
StL3AlgorithmInfo::intParameter(int i) const
{ 
      return i < mIntParameterSize ? const_cast<TArrayI&>(mIntParameterArray)[i] : 0; 
}

inline int
StL3AlgorithmInfo::floatParameterSize() const { return mFloatParameterSize; }

inline float
StL3AlgorithmInfo::floatParameter(int i) const
{
      return i < mFloatParameterSize ? const_cast<TArrayF&>(mFloatParameterArray)[i] :0;
}
#endif

/***************************************************************************
 *
 * $Id: StHbtXDFReader.h,v 1.3 2000/02/26 19:06:12 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *
 * This Reader reads XDF files. 
 *
 * If the constructor is called without arguments:
 *    StHbtXDFReader* Reader = new StHbtXDFReader;
 * it is assumed that a STAR dst.xdf file is read. This means that the 
 * is looking for an St_DataSet named 'dst' and is looking for a particle
 * table in the directory 'dst/particle'.
 * The data set name and the directory to look for the particle table can 
 * be passed as arguments to the constructor: 
 *    StHbtXDFReader* Reader = new StHbtXDFReader("evgen","evgen/dst");
 * The above example will reader Hijing output.
 * 
 * This reader has three particleId lists. With the methods
 *    StHbtXDFReader::AddAcceptedParticle( int pdgId ); 
 *    StHbtXDFReader::AddAcceptedMother( int pdgId ); 
 *    StHbtXDFReader::AddAcceptedDaughter( int pdgId );
 * If a list is empty, everything will be accepted for this list.
 * If the first list is not empty, only particles which have an pdgId 
 * matching one element of the list will be accepted.
 * If the second list is not empty, only particles which have at least 
 * one mother which has a pdgID matching one element in the list will be 
 * accepted.
 * If the third list is not empty, only particles which have at least one
 * daughter which has  a pdgID matching one element in the list will be 
 * accepted.
 * A particle is accepted only if it is accepted in all three lists.
 * Example given:
 *    StHbtXDFReader* Reader = new StHbtXDFReader("evgen","evgen/particle");
 *    Reader->AddAcceptedParticle(-321);  // kaon+
 *    Reader->AddAcceptedParticle(+321);  // kaon-
 *    Reader->AddAcceptedMother(333);     // phi 
 * This example will read Hijing output. Only kaon+ and kaon- will be
 * accepted, but only if they have a phi as mother. Since no daughters
 * are specified there is no check on daughters.
 * Have fun. 
 **************************************************************************/


#ifndef StHbtXDFReader_hh
#define StHbtXDFReader_hh

#include <math.h>

#include "StChain.h"
#include "StMaker.h"
#include "St_DataSetIter.h"
//#include "St_xdfin_maker/St_xdfin_Maker.h"
#include "St_XDFFile.h"
#include "StIOInterFace.h"

#include "StHbtMaker/Base/StHbtEventReader.hh"
#include "StV0MiniDstMaker/StV0MiniDstMaker.h"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"

//class StPionPlus;
//class StKaonPlus;
//class StProton;
//class StTpcDedxPidAlgorithm;
//class StParticleDefinition;

class TOrdCollection;


typedef list<int>            pdgIdList;
typedef list<int>::iterator  pdgIdListIterator;

//  this function is defined in StHbtMcEventReader.cxx 
extern double dedxMean(double mass, double momentum);


class StHbtXDFReader : public StMaker, public StHbtEventReader {

private:

  StMaker* mTheEventMaker;      //! this is the chain where the StEventReaderMaker is
  StV0MiniDstMaker* mTheV0Maker; //! this is the chain where the StV0MiniDstMaker is
  StHbtEventCut* mEventCut;     //!
  StHbtTrackCut* mTrackCut; //!
  StHbtV0Cut* mV0Cut; //!
  long              mV0;        //! Number of v0s looked at to date
  pdgIdList* mAcceptedParticles; //!
  pdgIdList* mAcceptedMothers;   //!
  pdgIdList* mAcceptedDaughters; //!

  const char* mDataSetName; //!
  const char* mParticleTableDirectory; //!
  

 protected:
 TOrdCollection *mCollection; //!

public:
  StHbtXDFReader(const char* dataSetName="dst", const char* particleTableDirectory="dst/particle");
  ~StHbtXDFReader();

  StHbtEvent* ReturnHbtEvent();
  StHbtString Report();

  void SetTheEventMaker(StMaker*);
  StMaker* TheEventMaker();
  void SetTheV0Maker(StV0MiniDstMaker*);
  StV0MiniDstMaker* TheV0Maker();

  void AddAcceptedParticle( int pdgCode );
  void AddAcceptedMother( int pdgCode );
  void AddAcceptedDaughter( int pdgCode );
  int  CheckPdgIdList( pdgIdList* list, int pdgCode );
#ifdef __ROOT__
  ClassDef(StHbtXDFReader, 1)
#endif
};

inline void StHbtXDFReader::SetTheEventMaker(StMaker* maker){mTheEventMaker=maker;}
inline StMaker* StHbtXDFReader::TheEventMaker(){return mTheEventMaker;}
inline void StHbtXDFReader::SetTheV0Maker(StV0MiniDstMaker* maker){mTheV0Maker=maker;}
inline StV0MiniDstMaker* StHbtXDFReader::TheV0Maker(){return mTheV0Maker;}

#endif


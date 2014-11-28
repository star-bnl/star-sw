/** 
 * @file  StiToolkit.h
 * @brief Abstract interface for a STI toolkit
 * @author Claude A Pruneau, Wayne State University, 
 * @date   March 2002
 * @copyright 2002, STAR  Experiment at BNL, All rights reserved.  
 * <p> 
 * Permission to use, copy, modify and distribute this software and its
 * documentation strictly for non-commercial purposes is hereby granted 
 * without fee, provided that the above copyright notice appears in all
 * copies and that both the copyright notice and this permission notice
 * appear in the supporting documentation. The authors make no claims 
 * about the suitability of this software for any purpose. It is     
 * provided "as is" without express or implied warranty.             
 */
#ifndef StiToolkit_H
#define StiToolkit_H 1

class   StEvent;
class   StiDetector;
class   StiKalmanTrack;
class   StiKalmanTrack;
class   StiKalmanTrackNode;
class   StiHit;
template<class Factorized> class Factory;
class StiDetectorNode;


// common object containers

class 	StiDetectorContainer;
class 	StiHitContainer;
class 	StiKalmanTrackContainer;
class   StiLocalTrackSeedFinder;
// service and convenience class objects.
class   StiVertexFinder;

/** 
 * @class StiToolkit
 * @brief Definition of toolkit
 */
class StiToolkit 
{
public:
  StiToolkit(); 
  virtual Factory<StiHit>             * HitFactory()=0;
  virtual Factory<StiKalmanTrack>     * TrackFactory()=0;
  virtual Factory<StiKalmanTrackNode> * TrackNodeFactory()=0;
  virtual Factory<StiDetector>        * DetectorFactory()=0;
  virtual Factory<StiDetectorNode >   * DetectorNodeFactory()=0;
  
  // common object containers 
  virtual StiDetectorContainer        * DetectorContainer()=0;
  virtual StiHitContainer             * HitContainer()=0;
  virtual StiKalmanTrackContainer     * TrackContainer()=0;
  
  // service and convenience class objects.
  virtual StiLocalTrackSeedFinder     * TrackSeedFinder()=0;
  virtual StiVertexFinder             * VertexFinder()=0;
  
  virtual int Truth(const StiHit *hit)=0;

  static void SetToolkit(StiToolkit*toolkit);
  static StiToolkit *instance();
  static void Kill();
  
 protected:

  static StiToolkit * _instance;
};

#endif


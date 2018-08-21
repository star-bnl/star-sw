/** 
 * @file  StxToolkit.h
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
#ifndef StxToolkit_H
#define StxToolkit_H 1

class   StEvent;
class   StxDetector;
class   StxKalmanTrack;
class   StxKalmanTrack;
class   StxKalmanTrackNode;
class   StxHit;
template<class Factorized> class Factory;
class StxDetectorNode;


// common object containers

class 	StxDetectorContainer;
class 	StxHitContainer;
class 	StxKalmanTrackContainer;
class   StxLocalTrackSeedFinder;
// service and convenience class objects.
class   StxVertexFinder;

/** 
 * @class StxToolkit
 * @brief Definition of toolkit
 */
class StxToolkit 
{
public:
  StxToolkit(); 
  virtual Factory<StxHit>             * HitFactory()=0;
  virtual Factory<StxKalmanTrack>     * TrackFactory()=0;
  virtual Factory<StxKalmanTrackNode> * TrackNodeFactory()=0;
  virtual Factory<StxDetector>        * DetectorFactory()=0;
  virtual Factory<StxDetectorNode >   * DetectorNodeFactory()=0;
  
  // common object containers 
  virtual StxDetectorContainer        * DetectorContainer()=0;
  virtual StxHitContainer             * HitContainer()=0;
  virtual StxKalmanTrackContainer     * TrackContainer()=0;
  
  // service and convenience class objects.
  virtual StxLocalTrackSeedFinder     * TrackSeedFinder()=0;
  virtual StxVertexFinder             * VertexFinder()=0;
  
  virtual int Truth(const StxHit *hit)=0;

  static void SetToolkit(StxToolkit*toolkit);
  static StxToolkit *instance();
  static void Kill();
  
 protected:

  static StxToolkit * _instance;
};

#endif


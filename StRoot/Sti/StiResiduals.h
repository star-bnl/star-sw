//StiResidualMaker.h
/***************************************************************************
 *
 * $Id: StiResiduals.h,v 2.2 2003/04/29 14:53:47 andrewar Exp $
 *
 * /author Andrew Rose, Wayne State University 
 * October 2002
 *
 *   Pure virutal base class for Residual makers defining interface.
 *   Inheritor classes are actual instances.
 *
 ******************
 * $Log: StiResiduals.h,v $
 * Revision 2.2  2003/04/29 14:53:47  andrewar
 * Moved to ResidualCalculator (to conform to naming convention). Also additions
 * to support StiDetectorBuilder calls.
 *
 * Revision 2.1  2003/03/17 19:10:25  andrewar
 * Added ifdef statement
 *
 * Revision 2.0  2002/12/10 21:59:59  pruneau
 * Introducing version 2.0
 *
 * Revision 1.2  2002/11/15 17:06:31  andrewar
 * Fixed bug with virtual base class destructor.
 *
 * Revision 1.1  2002/10/16 18:42:20  andrewar
 * Base class for residual makers.
 *
 */


#ifndef StiResiduals_HH
#define StiResiduals_HH

//forward declarations
class StiTrack;
class StiTrackContainer;
class StiDetector;


class StiResiduals
{
   public:
     StiResiduals(){/*noop*/};
     virtual ~StiResiduals(){};
 

     virtual void calcResiduals(StiTrackContainer *tracks)=0;

     //write out generated hists to file
     virtual void Write(char* outfile)=0;

   private:
     //setup hists etc.
     virtual int Init()=0;
     virtual void initDetector(StiDetectorBuilder*)=0;

     virtual int trackResidue(const StiTrack*)=0;


};
  

#endif

//StiResidualMaker.h
/***************************************************************************
 *
 * $Id: StiResiduals.h,v 2.0 2002/12/10 21:59:59 pruneau Exp $
 *
 * /author Andrew Rose, Wayne State University 
 * October 2002
 *
 *   Pure virutal base class for Residual makers defining interface.
 *   Inheritor classes are actual instances.
 *
 ******************
 * $Log: StiResiduals.h,v $
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

//forward declarations
class StiTrack;
class StiTrackContainer;


class StiResiduals
{
   public:
     StiResiduals(){/*noop*/};
     virtual ~StiResiduals(){};
 
     //load track container into residual maker
     //return 0 = failed
     //return 1 = success
     virtual int calcResiduals(StiTrackContainer *tracks)=0;

     //write out generated hists to file
     virtual void Write(char* outfile)=0;

   private:
     //setup hists etc.
     virtual int Init()=0;

     virtual int trackResidue(const StiTrack*)=0;


};
  


//StiResidualMaker.h
/***************************************************************************
 *
 * $Id: StiResiduals.h,v 1.1 2002/10/16 18:42:20 andrewar Exp $
 *
 * /author Andrew Rose, Wayne State University 
 * October 2002
 *
 *   Pure virutal base class for Residual makers defining interface.
 *   Inheritor classes are actual instances.
 *
 ***************************************************************************
 * $Log: StiResiduals.h,v $
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
     StiResiduals();
     virtual ~StiResiduals();
 
     //load track container into residual maker
     //return 0 = failed
     //return 1 = success
     virtual int calcResiduals(StiTrackContainer *tracks);

     //write out generated hists to file
     virtual void Write(char* outfile);

   private:
     //setup hists etc.
     virtual int Init();

     virtual int trackResidue(const StiTrack*);


};
  


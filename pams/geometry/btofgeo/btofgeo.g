* $Id: btofgeo.g,v 1.13 2002/11/26 22:15:09 geurts Exp $
* $Log: btofgeo.g,v $
* Revision 1.13  2002/11/26 22:15:09  geurts
* allows selection of the TOFr geometry version (btofgeo3) for year 2001/2002
*
* Revision 1.12  2000/11/22 17:49:07  nevski
* tof geometry versions 1 and 2 preserved in btofgeo1, version 3 goes in btofgeo2
*

   subroutine btofgeo
   integer    ISLFLAG,Igeom

*  this simply switchs between 3 versions of btof code
 
   Igeom = ISLFLAG('BTOF','GEOM')
   if (Igeom == 1) Call BTOFGEO1
   if (Igeom == 2) Call BTOFGEO2
   if (Igeom >= 3) Call BTOFGEO3

   end

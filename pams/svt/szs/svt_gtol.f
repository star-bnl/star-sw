      integer function svt_gtol(svt_hit,svt_hit_n)
      implicit none

c     description: global to local coordinate mapping of all space points, acts on structure svt_hit
c     to produce lx,ly from x,y,z. Assume wafer #wn is hit. Also calculate the angles thn, thd of the
c     track relative to the normal and drift direction. 
c     
c     return  0 if status is OK - normal completion
c     return -1 if at least one space point was found not to belong to any wafer  

c     argument declaration
c     ====================
      include 'geant_st.f'

      record /svt_hit_st/svt_hit(svt_hit_nmax)

c     local declaration
c     =================
      integer ihit, wn
      real    xx, yy, zz, llx, lly, llz, sinth, cn, cd

c     executable code
c     ===============

      do ihit = 1, svt_hit_n

         wn = svt_hit(ihit).id
         xx = svt_hit(ihit).x - svt_geom(wn).x
         yy = svt_hit(ihit).y - svt_geom(wn).y
         zz = svt_hit(ihit).z - svt_geom(wn).z

c     lx : transverse 
c     ly : drift direction
c     lz : normal direction

         lx = xx*svt_geom(wn).wtx + yy*svt_geom(wn).wty + zz*svt_geom(wn).wtz
         ly = xx*svt_geom(wn).wdx + yy*svt_geom(wn).wdy + zz*svt_geom(wn).wdz
         lz = xx*svt_geom(wn).wnx + yy*svt_geom(wn).wny + zz*svt_geom(wn).wnz

         if (abs(lz).gt.0.001) then
            write(6,*) ' svt_gtol: point outside lz tolerance'
         end if

c     cheat to keep the signals

         if (lx.gt.svt_geom(wn).wtx) then
            lx = svt_geom(wn).wtx
         elseif (lx.lt.-svt_geom(wn).wtx) then
            lx = -svt_geom(wn).wtx
         end if

         if (ly.gt.svt_geom(wn).wty) then
            ly = svt_geom(wn).wty
         elseif (ly.gt.svt_geom(wn).wty) then
            ly = svt_geom(wn).wty
         end if

         svt_hit(ihit).lx = lx
         svt_hit(ihit).ly = ly

         sinth = sin(svt_hit(ihit).th)
         costh = cos(svt_hit(ihit).th)
         sinph = sin(svt_hit(ihit).ph)
         cosph = cos(svt_hit(ihit).ph)

         cn = svt_geom(wn).wnx*sinth*cosph + svt_geom(wn).wny*sinth*sinph + svt_geom(wn).wnz*costh
         cd = svt_geom(wn).wdx*sinth*cosph + svt_geom(wn).wdy*sinth*sinph + svt_geom(wn).wdz*costh

         if (cn.gt.1.) then
            svt_hit(ihit).thn = 0.
         else if (cn.lt.-1.) then
            svt_hit(ihit).thn = 3.1415927
         else
            svt_hit(ihit).thn = acos(cn)
         end if

         if (cd.gt.1.) then
            svt_hit(ihit).thd = 0.
         else if (cd.lt.-1.) then
            svt_hit(ihit).thd = 3.1415927
         else
            svt_hit(ihit).thd = acos(cd)
         end if

      end do

      svt_gtol = 0

      return
      end

c==============================================================================================

      integer function svt_ltog(svt_hit,svt_hit_n)
      implicit none

c     description: local to global coordinate mapping of all space points, acts on structure svt_hit
c     to produce x,y,z from lx, ly. Assume wafer #wn is hit. 
c     
c     return  0 if status is OK - normal completion

c     argument declaration
c     ====================
      include 'geant_st.f'

      record /svt_hit_st/svt_hit(svt_hit_nmax)

c     local declaration
c     =================
      integer ihit, wn
      real    xx, yy, zz

c     executable code
c     ===============

      do ihit = 1, svt_hit_n

         wn = svt_hit(ihit).id

         xx = svt_geom(wn).x + svt_hit(ihit).lx*svt_geom(wn).wtx + svt_hit(ihit).ly*svt_geom(wn).wdx
         yy = svt_geom(wn).y + svt_hit(ihit).lx*svt_geom(wn).wty + svt_hit(ihit).ly*svt_geom(wn).wdy
         zz = svt_geom(wn).z + svt_hit(ihit).lx*svt_geom(wn).wtz + svt_hit(ihit).ly*svt_geom(wn).wdz

         svt_hit(ihit).x = xx
         svt_hit(ihit).y = yy
         svt_hit(ihit).z = zz

      end do

      svt_ltog = 0

      return
      end 

   subroutine geometry
*
   Call cavegeo
   Call pipegeo
   Call svttgeo
   Call tpcegeo
   Call ftpcgeo
   Call vpddgeo
   Call btofgeo
   Call calbgeo
   Call ecalgeo
   Call magpgeo
   Call mfldgeo
   Call ggclos
   call agphysi
*
   end

   subroutine geometry_init
   external   geometry
   call this_is_amodule('geometry'//char(0),geometry)
   call geometry
   end

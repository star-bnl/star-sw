*****************************************************************
      function g2t_volume_id(Csys,numbv)
* 
*****************************************************************
      implicit none
      integer  g2t_volume_id
* 
      Character*4      Csys
      Integer          NUMBV(15)
      Integer          innout,sector,sub_sector,volume_id
      Integer          rileft,eta,phi,phi_sub,superl,forw_back,strip
      Integer          endcap,zslice,innour,lnumber,wafer,phi_30d
      Integer          section,tpgv,tpss,tpad,sector,isdet
*
*    this is an internal agfhit/digi information - need a better access.
      integer          idigi
      common /AgCdigi/ idigi(15)
      Integer          Iprin,Nvb
      Character*4                   cs,cd
      COMMON /AGCHITV/ Iprin,Nvb(8),cs,cd
*c - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*
      volume_id = 0
*
      If    (Csys.eq.'svt') then
*1*                                          Ken Wilson
        lnumber    = numbv(1)
        phi        = numbv(2)
        wafer      = numbv(3)
        If (lnumber.le.2) then
          wafer    = 5-wafer
        else If (lnumber.le.4) then
          wafer    = 7-wafer     
        else If (lnumber.le.6) then
          wafer    = 8-wafer     
        else
          print *,' G2T warning: layer number ',lnumber,
     *            '     in svt hits not found' 
        endif

        volume_id  = 100*lnumber+10*phi+wafer

      else If (Csys.eq.'tpc') then
*2*                                        Peter M. Jacobs
        tpgv  = numbv(1)
        tpss  = numbv(2)
        tpad  = numbv(3)
        sector= tpss+12*(tpgv-1) 

        isdet = 0
        If (cd.eq.'TPAI')  isdet=1
        If (cd.eq.'TPAO')  isdet=2

        volume_id=100000*isdet+100*sector+tpad

      else If (Csys.eq.'mwc') then
*3*
        rileft    = numbv(1)
        sector    = numbv(2) 
        innout    = numbv(3)
        innour    = numbv(4)
        volume_id = 1000*rileft+100*innout+10*innour+sector

      else If (Csys.eq.'tof') then
*4*
        rileft     = numbv(1)
        sector     = numbv(2)
        sub_sector = numbv(3) 
        innout     = numbv(4)
        volume_id  = 100000*rileft+1000*innout+10*sector+
     +                                         sub_sector   

      else If (Csys.eq.'ctb') then
*5*
        rileft    = numbv(1)
        sector    = numbv(2)
        innout    = numbv(3)
        volume_id = 1000*rileft+100*innout+sector

*   ------------------  calorimetry  ---------------------

      else If (Csys.eq.'emc') then
*6*                                barrel calorimeter - K.Shester
        rileft=numbv(1)
        eta=idigi(1)+1
        If (rileft.eq.1) then
          phi=60-numbv(2)+1
          phi_sub=idigi(2)
          If (phi_sub.eq.0) phi_sub=2 
        else
          phi=60+numbv(2)
          phi_sub=idigi(2)+1
        endif     
        superl=numbv(3)
        volume_id=10000000*rileft+100000*eta+100*phi+
     +                            10*phi_sub+superl

      else If (Csys.eq.'smd') then
*7*
        rileft=numbv(1) 
        eta=idigi(2)+1
        If (rileft.eq.1) then
          phi=60-numbv(2)+1
        else
          phi=60+numbv(2)
        endif     
        forw_back=numbv(3)
        If (forw_back.eq.4) forw_back=3
        strip=idigi(3)+1
        volume_id=100000000*rileft+1000000*eta+1000*phi+
     +                             100*forw_back+strip

      else If (Csys.eq.'esm') then
*8*                                 end-cap calorimeter - Rashid
        rileft    = numbv(1)
        phi       = numbv(2)
        phi_sub   = numbv(4)
        eta       = numbv(2) 
        volume_id = 10000000*rileft+100000*phi+100*phi_sub+eta

      else If (Csys.eq.'eem') then
*9*
        rileft    = numbv(1)
        phi_30d   = numbv(2)-1
        section   = numbv(3)
        eta       = numbv(6)
        phi       = numbv(4)
        volume_id = 100000*rileft+5000*phi_30d+1000*phi+10*eta+
     +              section
 
*   ------------------ forward region ---------------------

      else If (Csys.eq.'ftp') then
*10*
        endcap    = numbv(1)
        zslice    = numbv(2)
        volume_id = 100*endcap+zslice

      else If (Csys.eq.'vpd') then
*11*
        rileft    = numbv(1)
        innout    = numbv(2)
        sector    = numbv(3)
        volume_id = 1000*rileft+100*innout+sector

      else If (Csys.eq.'pgc') then
*12*
      else If (Csys.eq.'psc') then
*13*
      else
        print *,' G2T warning: volume  ',Csys,'  not found '  
      endif

      g2t_volume_id=volume_id

      end
      

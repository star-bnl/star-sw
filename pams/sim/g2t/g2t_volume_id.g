********************************************************************
      function g2t_volume_id(Csys,numbv)
* 
* Modification history:                                            *
* PN 28.12.99: use structure control access to avoid warnings      *
* PN 28.12.99: make decision on CALB 2/3 level numbering based on  *
*              CALB_Nmodule(1) and (2), not on RICH presence !     *
********************************************************************
      implicit none
      integer  g2t_volume_id
* 
      Character*3      Csys
      Integer          NUMBV(15),itpc/0/,ibtf/0/,ical/0/
      Integer          innout,sector,sub_sector,volume_id
      Integer          rileft,eta,phi,phi_sub,superl,forw_back,strip
      Integer          endcap,zslice,innour,lnumber,wafer,phi_30d
      Integer          section,tpgv,tpss,tpad,isdet,ladder,is
*
*    this is an internal agfhit/digi information - need a better access.
      integer          idigi
      common /AgCdigi/ idigi(15)
      Integer          Iprin,Nvb
      Character*4                   cs,cd
      COMMON /AGCHITV/ Iprin,Nvb(8),cs,cd
      Structure  TPCG  {version}
      Structure  BTOG  {version, Rmin, Rmax, dz, choice, posit1}
      Structure  CALG  {version, int Nmodule(2), int NetaT, int MaxModule, int Nsub,
     +                  int NetaSMDp, int NPhistr, int Netfirst, int Netsecon}

      logical          first/.true./
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
*
      if (First) then
          first=.false.
          call RBPUSHD
          USE  /DETM/TPCE/TPCG  stat=itpc
          USE  /DETM/BTOF/BTOG  stat=ibtf
          USE  /DETM/CALB/CALG  stat=ical

          call RBPOPD
          if (itpc>=0) print *,' g2t_volume_id: TPC version =',tpcg_version
          if (ibtf>=0) print *,'              : TOF version =',btog_version,
     >                         ' choice  =',btog_choice,btog_posit1
          if (ical>=0) print *,'              : CALB patch  =',calg_nmodule
      endif

      volume_id = 0
*
      If    (Csys=='svt') then
*1*                                          Ken Wilson
** Helen altered SVT volume IDs so agrees with hardware defs.
        If  (Cd=='SVTD') then
           lnumber    = numbv(1)
           ladder     = numbv(2)
           wafer      = numbv(3)
        
           If ( ladder .eq. 0 .and. wafer .eq. 0) then
* This is the year 1 ladder
                wafer = lnumber
                ladder = 12
                lnumber = 4

                                                                               * Set First barrel ids            
           else If (lnumber.le.2) then
*             wafer    = 5-wafer 
             If (lnumber .eq. 1) then
               If( ladder .eq. 1) then
                   ladder = 2
               else If(ladder .eq.2) then 
                  ladder= 8
               else If(ladder .eq.3) then
                  ladder= 6
               else If(ladder .eq.4) then
                  ladder = 4
               endif
             else
               If( ladder .eq. 1) then
                   ladder = 1
               else If(ladder .eq.2) then 
                  ladder= 7
               else If(ladder .eq.3) then
                  ladder= 5
               else If(ladder .eq.4) then
                  ladder = 3
               endif
             endif 
* Set 2nd barrel ids
           else If (lnumber.le.4) then
*             wafer    = 7-wafer   
             If (lnumber .eq. 4) then
               If( ladder .eq.1) then 
                  ladder= 2
               else If(ladder .eq.2) then 
                  ladder= 12
               else If(ladder .eq.3) then
                  ladder= 10
               else If(ladder .eq.4) then
                  ladder= 8
               else If(ladder .eq.5) then
                  ladder= 6
               else If(ladder .eq.6) then 
                  ladder= 4
               endif
             else
               If( ladder .eq.1) then 
                  ladder= 3
               else If(ladder .eq.2) then 
                  ladder= 1
               else If(ladder .eq.3) then
                  ladder= 11
               else If(ladder .eq.4) then
                  ladder= 9
               else If(ladder .eq.5) then
                  ladder= 7
               else If(ladder .eq.6) then 
                  ladder= 5
               endif


             endif

* Set 3rd barrel ids

           else If (lnumber.le.6) then
*             wafer    = 8-wafer
             If (lnumber .eq. 6)  then
               If( ladder .eq.1) then 
                   ladder= 3
               else If(ladder .eq.2) then 
                   ladder= 1
               else If(ladder .eq.3) then
                   ladder= 15
               else If(ladder .eq.4) then
                   ladder= 13
               else If(ladder .eq.5) then 
                   ladder= 11
               else If(ladder .eq.6) then
                    ladder= 9
               else If(ladder .eq.7) then
                    ladder= 7
               else If(ladder .eq.8) then
                    ladder= 5
               endif
             else
               If( ladder .eq.1) then 
                  ladder= 4
               else If(ladder .eq.2) then 
                  ladder= 2
               else If(ladder .eq.3) then
                  ladder= 16
               else If(ladder .eq.4) then
                  ladder= 14
               else If(ladder .eq.5) then
                  ladder= 12
               else If(ladder .eq.6) then 
                  ladder= 10
               else If(ladder .eq.7) then
                  ladder= 8
               else If(ladder .eq.8) then
                  ladder= 6
               endif
             endif 
           else
             print *,' G2T warning: layer number ',lnumber,
     >               '     in svt hits not found' 
           endif
           volume_id  = 1000*lnumber+100*wafer+ladder
        else If (Cd=='SFSD') then
           volume_id =  7000+100*numbv(2)+numbv(1)
        endif


      else If (Csys=='tpc') then
*2*                                        Peter M. Jacobs
        tpgv  = numbv(1)
        tpss  = numbv(2)
        sector= tpss+12*(tpgv-1) 
        tpad  = numbv(3)
        isdet = 0
        If  (tpcg_version==1) then
          If (cd=='TPAI')  isdet=1
          If (cd=='TPAO')  isdet=2
*PN:      outermost pseudopadrow:
          If (cd=='TPAO' & tpad==14) tpad=45
        else
          If (tpad<41) then
             isdet = mod(41-tpad,3)
             tpad  = (tpad+2)/3
          else If (tpad<73) then
             tpad=tpad-41+14
          else
             isdet = 2
             tpad  = 45
          endif
        endif

        volume_id=100000*isdet+100*sector+tpad

      else If (Csys=='mwc') then
*3*
        rileft    = numbv(1)
        sector    = numbv(2) 
        innout    = numbv(3)
        innour    = numbv(4)
        volume_id = 1000*rileft+100*innout+10*innour+sector

      else If (Csys=='tof') then
*4*                                             Frank Geurts
        If (btog_version==1) then
           rileft     = numbv(1)
           sector     = numbv(2)
           sub_sector = numbv(3) 
           innout     = numbv(4)
           volume_id  = 100000*rileft+1000*innout+10*sector+sub_sector   
        elseif (btog_version==2) then
           if (btog_choice==4) then
              rileft     = 1
              sector     = btog_posit1
              sub_sector = numbv(1) 
              innout     = numbv(2)
              volume_id  = 100000*rileft+1000*innout+10*sector+sub_sector   
           else
              print *,' g2t_volume_id : choice not coded yet '
           endif        
        elseif (btog_version==3) then
           if (btog_choice==2) then      ! Full TOF
             rileft     = numbv(1)       !     west(1)/east(2)
             sector     = numbv(2)       !     tray(1-60)
             innout     = numbv(3)       !     4wide(1)/5wide(2) sections
             sub_sector = numbv(4)       !     theta-tray(4w:1-4, 5w:1-5)
             section    = numbv(5)       !     phi-tray(4w:1-9,5w:1)
           elseif (btog_choice==3) then  ! ~25% TOF (only on west side)
             rileft     = 1              !     west (pre-set)
             sector     = numbv(1)       !     tray
             innout     = numbv(2)       !     4wide/5wide section
             sub_sector = numbv(3)       !     theta-tray
             section    = numbv(4)       !     phi-tray
           elseif (btog_choice==4) then  !  TOFp (single tray)
             rileft     = 1              !     west (pre-set)
             sector     = btog_posit1    !     tray (pre-set)
             innout     = numbv(1)       !     4wide/5wide section
             sub_sector = numbv(2)       !     theta-tray
             section    = numbv(3)       !     phi-tray
           else
             print *,' g2t_volume_id: unknown TOF choice.'
             print *,' g2t_volume_id: btog_choice=',btog_choice
           endif
*  -------- sanity check ---------
           if ((rileft.lt.1).OR.(rileft.gt. 2).or.
     +        (sector.lt.1).OR.(sector.gt.60).or.
     +        (innout.lt.1).OR.(innout.gt. 2).or.
     +        (sub_sector.lt.1).or.
     +         ((innout==1).AND.(sub_sector.gt.4)).or.
     +         ((innout==2).AND.(sub_sector.gt.5)).or.
     +        (section.lt.1).OR.(section.gt.9).or.
     +         ((innout.eq.2).AND.(section.ne.1)))
     +    print *,' g2t_volume_id: TOF sanity check failed.',
     +              rileft,sector,innout,sub_sector,section
*  -------- combine 4wide and 5wide sections ---------
          if (innout==1) then
             section = section+1  !  phi-tray (4wide-sections)
          endif
*  -------- encode volume_id ---------
          volume_id = 100000*rileft+1000*sector+100*sub_sector+section
        else
          print *,' g2t_volume_id : TOF version not coded yet'
          print *,' g2t_volume_id : btog_version=',btog_version
        endif

      else If (Csys=='ctb') then
*5*
        rileft    = numbv(1)
        sector    = numbv(2)
        innout    = numbv(3)
        volume_id = 1000*rileft+100*innout+sector

*   ------------------  calorimetry  ---------------------

      else If (Csys=='emc') then
*6*                                barrel calorimeter - K.Shester
        if (CALG_Nmodule(1)*CALG_Nmodule(2)>0) then
*          both left and right barrels:
           rileft = numbv(1)
           phi    = numbv(2)
           superl = numbv(3)
	else                   
*          only one barrel - left or write 
	   if(CALG_Nmodule(1)>0) then
              rileft=1
           else
              rileft=2
	   endif
           phi    = numbv(1)
           superl = numbv(2)
        endif
*
        eta=idigi(1)+1
        phi_sub=idigi(2)
        If (rileft==1) then
          phi=60-phi+1
          If (phi_sub==0) phi_sub=2
        else
          phi=60+phi
          phi_sub=phi_sub+1
        endif

	if(rileft<1 .or. rileft>2) then            
	  print *,'**ERROR at g2t_volume_id: emc rl ',rileft
*	elseif(eta<1 .or. eta>CALG_NetaT)  then                 
*	  print *,'**ERROR at g2t_volume_id: emc eta ',eta
*	elseif(phi<1 .or. phi>CALG_MaxModule)  then            
*	  print *,'**ERROR at g2t_volume_id: emc phi ',phi
*	elseif(superl<1 .or. superl>CALG_NSub) then            
*	  print *,'**ERROR at g2t_volume_id: emc superl ',superl
	else 
	  volume_id=10000000*rileft+100000*eta+100*phi+
     +	              +10*phi_sub+superl
	endif

      else If (Csys=='smd') then
*7*
        if (CALG_Nmodule(1)*CALG_Nmodule(2)>0) then
           rileft   =numbv(1)
           phi      =numbv(2)
           forw_back=numbv(3)
        else
           if (CALG_Nmodule(1)>0) then
              rileft=1
           else
              rileft=2
           endif
           phi      =numbv(1)
           forw_back=numbv(2)
        endif

        eta  =idigi(2)+1
        strip=idigi(3)+1

        If (forw_back==4) forw_back=3
        If (rileft==1) then
          phi=60-phi+1
        else
          phi=60+phi
        endif     

	if(rileft<1 .or. rileft>2)  then                        
	  print *,'**ERROR at g2t_volume_id: smd rl ',rileft
*	elseif(eta<1 .or. eta>calg_NetaSMDp) then                  
*	  print *,'**ERROR at g2t_volume_id: smd eta ',eta
*	elseif(phi<1 .or. phi>CALG_MaxModule) then            
*	  print *,'**ERROR at g2t_volume_id: smd phi ',phi
	elseif(forw_back<1 .or. forw_back>3) then            
	  print *,'**ERROR at g2t_volume_id: smd forw_back ',forw_back
	elseif(strip<1) then            
	  print *,'**ERROR at g2t_volume_id: smd strip ',strip
	elseif(forw_back=1.and.strip>calg_Netfirst) then            
	  print *,'**ERROR at g2t_volume_id: smd strip ',strip, forw_back
	elseif(forw_back=2.and.strip>calg_Netsecon)  then            
	  print *,'**ERROR at g2t_volume_id: smd strip ',strip, forw_back
	elseif(forw_back=3.and.strip>calg_NPhistr) then            
	  print *,'**ERROR at g2t_volume_id: smd strip ',strip, forw_back
	else 
          volume_id=100000000*rileft+1000000*eta+1000*phi+
     +              100*forw_back+strip
	endif

      else If (Csys=='esm') then
*8*                                 end-cap calorimeter - Rashid
        rileft    = numbv(1)
        phi       = numbv(2)
        phi_sub   = numbv(4)
        eta       = numbv(2) 
        volume_id = 10000000*rileft+100000*phi+100*phi_sub+eta

      else If (Csys=='eem') then
*9*
        rileft    = numbv(1)
        phi_30d   = numbv(2)-1
        section   = numbv(3)
        eta       = numbv(6)
        phi       = numbv(4)
        volume_id = 100000*rileft+5000*phi_30d+1000*phi+10*eta+
     +              section
 
*   ------------------ forward region ---------------------

      else If (Csys=='ftp') then
*10*
        endcap    = numbv(1)
        zslice    = numbv(2)
        volume_id = 100*endcap+zslice

      else If (Csys=='vpd') then
*11*
        rileft    = numbv(1)
        innout    = numbv(2)
        sector    = numbv(3)
        volume_id = 1000*rileft+100*innout+sector

      else If (Csys=='pgc') then
*12*
      else If (Csys=='psc') then
*13*
      else If (Csys=='rch') then
        volume_id = numbv(1)

      else If (Csys=='zdc') then
        volume_id = numbv(1)*1000+numbv(2)
*14*
      else
        print *,' G2T warning: volume  ',Csys,'  not found '  
      endif

      g2t_volume_id=volume_id

      end
      

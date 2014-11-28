c     File:- evt_zebra.inc
c     ======================
c     Include file used to access the ZEBRA data banks in connection with
c     Event Generator reading and writing. It is intended to be sufficient
c     general so that it can be used for reading with all the different event
c     generators which are considered.
c     This include file is independent of the program used to generate the
c     files.
c
c
!
!DIVISION DATA STRUCTURES ---change when adding a division

! identifiers for zebra divisions
! note that these idents are used as indices to an array and therefore
! MUST be sequential integers
        integer max_zebra_divisions
        parameter (max_zebra_divisions=3)

        integer  code_div
        integer  run_div
        integer  event_div
        parameter (code_div=1)
        parameter (run_div =2)
        parameter (event_div =3)

! options used in calling mzdiv
! each option is implemented as an array dimensioned(max_zebra_divisions)
! so we can call mzdiv in a loop
! div_index  ---   the index of the division within zebra store
! div_label  ---  identifier for the division
! div_nw     --   number of words
! div_nw_max --   max number of words , div can grow from nw to nw_max
! div_opt    --  short or long term

        integer div_index(max_zebra_divisions)   ! an output array

        character*8 div_label(max_zebra_divisions)

        integer  div_nw(max_zebra_divisions)
        integer  div_nw_max(max_zebra_divisions)

        character*4 div_opt(max_zebra_divisions)

!LINK INFO --change when adding a link

        integer max_down_links(max_zebra_divisions)

        integer total_down_links
        parameter (total_down_links = 3) ! sum of all max_down_links

! name of common area for zebra structural links
! the common area contains links for all banks
! so that we can call mzlink in a loop
! the link for each zebra division starts at START_LINK(div)in DIV_LINKS and
! ends at END_LINK(div)  in DIV_LINKS

        character*8 div_links_common    !/'div_lnk'/
        integer  link_addr(total_down_links)

        integer start_link(max_zebra_divisions)
        integer end_link(max_zebra_divisions)

!DATA STRUCTURES FOR LINKS  -- change when adding a link

!link identifiers so that we can loop over all down links in a division
! these MUST be sequential integers
! CODE
        integer code_head
        parameter (code_head=1)
! RUN
        integer run_head
        parameter (run_head=2)
!EVENT
        integer event_dat
        parameter (event_dat = 3)

! parameters for mzbook when creating top level banks
        character*4 top_ident(max_zebra_divisions)
        integer top_num_links(max_zebra_divisions)
        integer top_num_str_links(max_zebra_divisions )
        integer top_link_bias(max_zebra_divisions)
        integer top_bank_subr_addr(max_zebra_divisions)


! parameters for calling mzbook for each link in a particular division

        integer bank_address(total_down_links,max_zebra_divisions)
        integer link_bias(total_down_links,max_zebra_divisions)
        character*4 ident(total_down_links,max_zebra_divisions)
        integer num_links(total_down_links,max_zebra_divisions)
        integer num_str_links(total_down_links,max_zebra_divisions)

        common/zebra_simulation/div_links_common,
     >                     bank_address, link_bias, ident,
     >                     num_links, num_str_links

! parameters used in mzbook subroutine calls
        integer zebra_integer_flag          !all data is integer
        integer zebra_zero_bank
        integer zebra_stand_alone_bank
        integer zebra_form_event

        common/zebra_params/zebra_integer_flag,zebra_zero_bank,
     >   zebra_stand_alone_bank, zebra_form_event

!communication with ZEBRA
        integer zstore_index
        integer size_zstore
        parameter (size_zstore= 400000)

        integer num_extra,num_ref          !extra links for future expansion
        parameter (num_extra=5,num_ref=9)

        integer iqq(size_zstore),lqq(size_zstore),zstore(size_zstore)
        real qq(size_zstore)
        equivalence (top_link_addr(1),lqq(1) )
        equivalence (lqq(9),iqq(1),qq(1))
        integer fence(40),top_link_addr(max_zebra_divisions),
     >    lextra(num_extra),lref(num_ref),
     >    endzs
        character*8 zebra_common_name
        common/zstore/ zebra_common_name,
     >     zstore_index,fence,top_link_addr,lextra,lref,
     >     zstore,endzs

        common/zebra_div/div_index,div_label,div_nw,div_nw_max,
     >    div_opt,max_down_links,start_link,end_link,
     >    top_link_bias,
     >    top_ident,
     >    top_num_links,
     >    top_num_str_links,
     >    top_bank_subr_addr

        common /div_lnk/ link_addr      ! the sum of values in max_down_links
*
! I/O params
        logical z_input
        integer z_lun_out
        integer z_lun_in
        integer part_head_size
        integer part_data_size
        logical*4 z_double_out
        integer z_blocksize
        character*4 z_fz_option_out
        character*4 z_fz_option_in
        integer z_fzlogl
        common /zebra_io/ z_input, z_lun_out, z_lun_in,
     1                 part_head_size, part_data_size,
     1                 z_double_out, z_blocksize, z_fz_option_out,
     2                 z_fz_option_in, z_fzlogl
c
        integer*4 iquest
        common /quest/iquest(100)

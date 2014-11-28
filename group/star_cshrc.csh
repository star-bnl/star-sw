#
# What is here is equivalent to what should be excuted in a cshrc
# i.e. aliases, terminal settings and key bindings.
#
# Note that this file is executed at EACH processes so DO NOT add
# unecessary instructions here.
#
# J.Lauret May 2001
#
set self="star_cshrc_csh"

# have a way to force reloading full login
if ( $?FLOGIN || $?GNOME_DESKTOP_SESSION_ID ) then
   unsetenv star_login_csh
endif

if ( $?DECHO ) echo "$self :: begin - we are setting ourselves as defined"
setenv star_cshrc_csh 1

if ( ! $?star_login_csh ) then
    #
    # OK. Missing environment variables ...
    # This is actually the case in 'r' or 's'-service calls or even
    # some shells ...
    #
    if ( $?DECHO ) echo "$self :: star_login_csh not defined, trying"

    # in case this is installed locally, check path
    if( ! $?AFS_RHIC) then
	if( $?GROUP_DIR ) then
	    if ( $?DECHO ) echo "$self :: GROUP_DIR is defined"
	    if ( -x $GROUP_DIR/chkread ) then
		if ( $?DECHO ) then
		    setenv chkread_debug 1
		    echo "$self :: Checking AFS path"
		endif
		# we have this added in 2009
		$GROUP_DIR/chkread /afs/rhic.bnl.gov/
		if ( ! $status ) then
		    if ( $?DECHO ) echo "$self :: AFS seems to be readable"
		    setenv AFS_RHIC  /afs/rhic.bnl.gov
		else
		    if ( $?DECHO ) echo "$self :: Error while checking AFS"
		endif
	    else
		# define it as-is and move on
		setenv AFS_RHIC  /afs/rhic.bnl.gov
	    endif
	    # if still undefined, assume $status!=0 or
	    # we did not find chkread. Fail login by a set
	    # attempt to some fantasy path ... ATTENTION
	    # This value will be checked in  group_env
	    if ( ! $?AFS_RHIC ) setenv AFS_RHIC /Path_Not_Found_STAR_Login_Failure
        else
	    # old mode - GROUP_DIR not defined prior
	    setenv AFS_RHIC  /afs/rhic.bnl.gov
	endif
    else
        if ( $?DECHO ) echo "$self :: AFS path pre-defined"
    endif
    if( ! $?GROUP_DIR ) setenv GROUP_DIR $AFS_RHIC/star/group/

    if( -r $GROUP_DIR/star_login.csh ) then
	if ( $?DECHO ) echo "$self :: Sourcing star_login.csh"
    	source $GROUP_DIR/star_login.csh
    else
	if ( $?DECHO ) echo "$self :: Unreadable star_login.csh"
    endif
endif


# --------------------------------------------------------------------------
# Aliases. Backward compatibility only.
# Somehow, I am sure that most of it is unused ...
# I have eliminated the following non-portable commands

# --------------------------------------------------------------------------
unalias ls
alias   nman       '/usr/bin/nroff -man \!* | $PAGER'
alias   ll         'ls -lA'
alias   pwd        'echo $cwd'
alias   h          'history'
alias   l          'ls -lt'
alias   lf         'ls -CF'
alias   terminal   'setenv TERM `/usr/bin/tset - \!*`'

# YP fix
if( ! $?DOMAINNAME) then
    if ( -x "/bin/domainname" ) then
	setenv DOMAINNAME `/bin/domainname`
    else
	setenv DOMAINNAME "(none)"
    endif

    # Fake it
    if ( "$DOMAINNAME" == "(none)") then 
       setenv DOMAINNAME `/bin/hostname | sed 's/^[^\.]*\.//'`
    endif
endif

#
# ATTENTION - This support for $SITE need extending
# at each new site.
#
# Each Grid site should have an entry.
# Only sites having local DB rules could have an entry.
# 
switch ($DOMAINNAME)
    case "nersc.gov":    # <--- or whatever domainame returns
	setenv SITE "LBL"
	breaksw
 
    case "rhic.bnl.gov":
    case "rcf.bnl.gov":
	setenv SITE "BNL"
	breaksw

    case "if.usp.br":
	setenv SITE "USP"
	breaksw

    case "cluster.phy.uic.edu":
	setenv SITE "UIC"
	breaksw

   default:
	# Not implemented
	setenv SITE "generic"
	breaksw
endsw




# This group file might be merged later on.
if( -r $GROUP_DIR/group_aliases.csh ) then
    if ( $?DECHO ) echo "$self :: Sourcing group_aliases.csh"
    source $GROUP_DIR/group_aliases.csh
else
    if ( $?DECHO ) echo "$self :: Un-readable group_aliases.csh"
endif



# -------------------------------------------------
# Now, this is the time to set the tty
# There will be last ultimate call to a group_login
# script which is a file containing commands NOT
# requird in batch mode.
# -------------------------------------------------


# support for several su mode
if ( -o /bin/su ) then
    if($USER == "root") then
	# root prompt
	alias setprompt 'set prompt="%s%b%m#%.04/> "'
    else
	# to another user ...
	alias setprompt 'set prompt="%s%b%m|%.04/> "'
    endif
else
    # user - bold %S
    alias setprompt 'set prompt="%B%S[%m]%s%b %.04/> "'
endif


# support csh/tcsh
set filec
set fignore=( .o .dvi .aux .toc .log .blg .bbl .bak .BAK .sav .old .trace)
set noclobber
set ignoreeof
# set notify
set savehist=50
set history=100

switch ($shell)
    case "/usr/local/bin/tcsh":
    case "/bin/tcsh":
	if ( ! -e  $HOME/history) then
	    /bin/mkdir -p $HOME/history
	endif
	set correct = cmd
	set autolist=on
	set listjobs=long
	set showdots=on
	set ellispis=1
	set histfile=$HOME/history/.$HOST
	set autologout=2160
	breaksw
    default:
	alias cd 'chdir \!* && setprompt'
	breaksw
endsw

# key bindings.
if ($?tcsh) then
    bindkey '^[[1~'  exchange-point-and-mark
    bindkey '^[[2~'  overwrite-mode
    bindkey '^[[3~'  delete-char-or-list
    bindkey '^[[4~'  set-mark-command
    bindkey '^[[5~'  history-search-backward
    bindkey '^[[6~'  history-search-forward
    bindkey '^[Ol'   kill-line
    bindkey '^[Om'   yank
    bindkey '^[On'   set-mark-command
    bindkey '^[Op'   exchange-point-and-mark
    bindkey '^[Oq'   forward-word
    bindkey '^[Or'   spell-line
    bindkey '^[Os'   copy-prev-word
    bindkey '^[Ot'   beginning-of-line
    bindkey '^[Ou'   which-command
    bindkey '^[Ov'   end-of-line
    bindkey '^[Ow'   backward-word
    bindkey '^[Ox'   yank
    bindkey '^[Oy'   kill-region
endif

# This was taken from HEPIX 100 % as-is
# Make sure csh.login will be sourced on IRIX
if ( ! $?SHLVL ) then
    setenv SHLVL 1
endif

setprompt


if ( $?DECHO ) echo "$self :: end"


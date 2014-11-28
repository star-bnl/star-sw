#########################################################################
#									#
# Script:	sxml.tcl (namespace sxml)				#
#									#
# Started:	20th October, 2000.					#
#									#
# Completed:	5th November, 2000.					#
#									#
# Author:	Simon Edwards, (C) Proprius Consulting Ltd		#
#									#
# Provides:	sxml namespace (init end register_routine parse)	#
#									#
#		These commands provide an event driven method of	#
#		parsing simple XML files.				#
#									#
# Credits:	Various sources on the web, such as devshed, where	#
#		I've learn't a little about XML.			#
#									#
# Limitations:	This is the initial version - it offers only basic	#
#		parsing functionality, though is good enough, if slow.	#
#									#
# Version:	@(#)0.8.0 Initially released version. (SE)>		#
# 		@(#)0.8.1 Empty element parse error resolved (SE)>	#
# 		@(#)0.8.2 Procedure args, CDATA support (SE)>		#
#									#
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#									#
# Version:	0.8.2							#
# Date:		7th August, 2001.					#
# Author:	Simon Edwards, Proprius Consulting Ltd.			#
# Change:	Added support for optional command line arguments to	#
#		pass to a registered routine (Thanks to Jim Garrison 	#
#		(garrison@qualcomm.com).				#
#		Added support for CDATA attributes meaning that these	#
#		routines can handle "binary" data.			#
#		It expects the start of the block to have the following	#
#		definition:						#
#									#
#		<![CDATA[						#
#		At the end of the block the following definition is	#
#		expected:						#
#									#
#		]]>							#
#									#
#		If you need to include that sequence of characters in	#
#		the data then the following should be used instead:	#
#									#
#		]]&gt;							#
#									#
#		WARNING:	The details between the deliminators	#
#				for CDATA mean that EVERY BYTE is 	#
#				including in the element. This 		#
#				is as the XML specification (at least	#
#				as far as my basic understanding).	#
#									#
#		Support has also been added for the set_attr function	#
#		which is an interface to be able to set various 	#
#		functions which will affect the parsing of files.	#
#		The attributes are particular to each file to parse	#
#		and need to be set using the parse descriptor for a	#
#		particular file. All attributes are given suitable 	#
#		defaults.						#
#									#
#		Attribute	Values	Purpose				#
#		=========	======	=============================== #
#		trace		0/1/2	Whether to show trace info	#
#					when parsing a particular file.	#
#					Default is 0 (no).		#
#					2 Is the highest level of trace	#
#					whilst 1 gives more details 	#
#					than most people want anyway!	#
#		silent		0/1	Whether or not any errors 	#
#					found during the parse are sent	#
#					to the "stderr" channel. 	#
#					default is 0 (show errors).	#
#		pedantic	0/1	Whether to check when a proc is	#
#					registered whether it actually	#
#					exists and has the correct	#
#					number of arguments. Default is	#
#					0 (not pedantic).		#
#		extended	0/1	If set to 1 will look to see if	#
#					a event has been registered for	#
#					a "wildcard" at the latest	#
#					level of a element. Slower, but	#
#					allows improved parsing. Default#
#					is 0.				#
#									#
# Version:	0.8.1							#
# Date:		26th November, 2000.					#
# Author:	Simon Edwards, Proprius Consulting Ltd.			#
# Change:	Corrected assumption that an empty element tag		#
#		cannot have attributes, so now correctly handles	#
#		cases such as:						#
#		<testelement name="fred"/>				#
#									#
#########################################################################

###############################################################################
#    sxml.tcl - tcl module to provide basis XML parsing			      #
#    Copyright (C) 2000-2001 Simon Edwards, Proprius Consulting Ltd           #
#                                                                             #
#    This program is free software; you can redistribute it and/or modify     #
#    it under the terms of the GNU General Public License as published by     #
#    the Free Software Foundation; either version 2 of the License, or        #
#    (at your option) any later version.                                      #
#                                                                             #
#    This program is distributed in the hope that it will be useful,          #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of           #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            #
#    GNU General Public License for more details.                             #
#                                                                             #
#    You should have received a copy of the GNU General Public License        #
#    along with this program; if not, write to the Free Software              #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA  #
#                                                                             #
#    If you wish to contact the author the following details can be used;     #
#                                                                             #
#    simon.edwards@proprius.co.uk                                             #
###############################################################################

namespace eval sxml {

namespace export init end register_routine parse set_attr

variable xml_invoc
variable xml_stack
variable xml_file
variable xml_procs
variable xml_hadtoplevel
variable xml_cdata_parse
variable xml_cdata_start
variable xml_cdata_end
variable xml_cdata_sub
variable xml_attrs

set xml_cdata_start {<![CDATA[}
set xml_cdata_end {]]>}
set xml_invoc 0

proc init {file} {
variable xml_invoc
variable xml_stack
variable xml_file
variable xml_procs
variable xml_attrs_stack
variable xml_hadtoplevel
variable xml_cdata_parse
variable xml_attrs

	if { [catch {set fd [open $file]}]  } {
		return -1
	}
	incr xml_invoc
	set xml_cdata_parse($xml_invoc) 0
	set xml_stack($xml_invoc) {}
	set xml_procs($xml_invoc) {}
	set xml_file($xml_invoc) $fd
	set xml_hadtoplevel($xml_invoc) 0

	#################################################################
	# Default the attributes currently supported.			#
	#################################################################
	set xml_attrs(${xml_invoc}_trace) 0
	set xml_attrs(${xml_invoc}_silent) 0
	set xml_attrs(${xml_invoc}_pedantic) 0
	set xml_attrs(${xml_invoc}_extended) 0

	return $xml_invoc
}

proc set_attr {id attr val} {
variable xml_attrs
variable xml_stack

	if { ! [info exists xml_stack(${id})] } {
		return -1
	}
	if { ! [info exists xml_attrs(${id}_$attr)] } {
		return -2
	}
	set xml_attrs(${id}_$attr) $val
	return 0
}

proc end {id} {
variable xml_stack
variable xml_file
variable xml_procs
variable xml_attrs_stack
variable xml_cdata_parse
variable xml_attrs

	if { ! [info exists xml_stack($id)] } {
		return -1
	}
	catch {close $xml_file($id)}
	unset xml_file($id)
	unset xml_stack($id)
	unset xml_procs($id)
	unset xml_cdata_parse($id)

	unset xml_attrs(${id}_trace)
	unset xml_attrs(${id}_silent)
	unset xml_attrs(${id}_pedantic)
	unset xml_attrs(${id}_extended)
	return 0
}

#########################################################################
# Construct an array containing all attributes details for all          #
# sub-elements in the current stack.                                    #
#########################################################################

proc xml_construct_attr_list {stack myarr} {
variable xml_attrs_stack
upvar $myarr xx

	catch {unset xx}
	set xx(_dummy_) ""
	if { ! [info exists xml_attrs_stack] } {
		return 0
	}

	foreach xxx [array names xml_attrs_stack "$stack:*"] {
		set xx($xxx) $xml_attrs_stack($xxx)
	}
	return 0
}

proc xml_construct_data_list {stack myarr} {
variable xml_data_stack
upvar $myarr xx

	catch {unset xx}
	set xx(_dummy_) ""

	if { ! [info exists xml_data_stack]} {
		return
	}
	foreach xxx [array names xml_data_stack "$stack:*"] {
		set xx($xxx) $xml_data_stack($xxx)
	}
}

proc register_routine {id path proc args} {
variable xml_stack
variable xml_procs
variable xml_attrs

	if { ! [info exists xml_stack($id)]} {
		return -1
	}
	#################################################################
	# We should also make sure that the specifed routine already	#
	# exists, and has the correct number of arguments.		#
	# But only if pedantic has been turned on...			#
	#################################################################

	if { $xml_attrs(${id}_pedantic) == 1 } {
		set valid [info procs $proc]
		if { "$valid" == "" } {
			if { $xml_attrs(${id}_silent) == 0 } {
				puts stderr "Error: Specified procedure to register \"$proc\" is not defined."
			}
			return -2
		}
		set args [info args $proc]
		if { [llength $args] != 6 } {
			if { $xml_attrs(${id}_silent) == 0 } {
				puts stderr "Error: Specified procedure to register \"$proc\" does not have valid argument count."
			}
			return -3
		}
	}

	set path [string tolower $path]
	set x [lsearch -exact $xml_procs($id) $path]
	if { $x !=-1 } {
		set xml_procs($id) [lreplace $xml_procs($id) $x [expr $x + 1] $path $proc]
		return 0
	}

	lappend xml_procs($id) $path $proc $args
	return 0
}

proc xml_tag_attrs_to_str {attrstr empty_t incdata} {
upvar $empty_t empty_tag

	set empty_tag 0
	set oattr $attrstr
	set attrs {}
	set x 1
	while {1} {
		set x [regexp { ([[:alnum:]\-_]+)="([^"]*)"} $attrstr junk a1 a2]
		if {$x == 0} break
		if { ! [validate_name $a1] } {
			puts stderr "Error: Invalid attribute name: $a1"
			return -code error 1
		}
		set a2 [xml_handle_bs_string $a2 $incdata]
		lappend attrs $a1 $a2
		regsub {([[:alnum:]\-_]+)="([^"]*)"} $attrstr {} xx
		set attrstr $xx
	}

	if { "[string trim "$attrstr"]" == "/" } {
		set empty_tag 1
	} elseif {[llength $attrstr] != 0} {
		puts stderr "Malformed attributes: $attrstr found in:\n$oattr"
		return -code error 1
	}
	return $attrs
}

#########################################################################
# The function below is used to valid the format of attribute and	#
# element names. These must begin with a letter or underscore and	#
# can consist of letters, underscore, dash and numbers only.		#
# Will return 1 if name is valid, 0 otherwise.				#
#########################################################################

proc validate_name {n} {
	set x [regexp {^[_\-[:alpha:]][_\-[:alnum:]]*} $n matched]
	if { $x == 0 } {
		return 0
	}
	if { "$matched" == "$n" } {
		return 1
	}
	return 0
}

#########################################################################
#									#
# The below function, when called with the current position and the	#
# being interpretted will return the character to place on the string	#
# and the pointer will be updated to the next character position.	#
#									#
# This routine handles escape characters and & special symbols.		#
#									#
# 1.8.2		When this routine is called it is now passed the	#
#		value of xml_cdata_parse as well. If set none of the	#
#		standard substitutions are performed. However the 	#
#		special CDATA case of "]]&gt;" will change to		#
#		"]]>" in such cases.					#
#									#
#########################################################################

proc xml_handle_bs {cptr cline incdata} {
upvar $cptr c

	set tcline [string range $cline $c end]
	if { $incdata == 1 } {
		if { [string first {]]&gt;} $tcline] == 0 } {
			incr c 6
			return {]]>}
		}
		set ch [string index $cline $c]
		incr c
		return $ch
	}

	if { [string first "&amp;" $tcline] == 0 } {
		incr c 5
		return {&}
	}
	if { [string first "&lt;" $tcline] == 0 } {
		incr c 4
		return {<}
	} 
	if { [string first "&quot;" $tcline] == 0 } {
		incr c 6
		return {"}
	} 
	if { [string first "&apos;" $tcline] == 0 } {
		incr c 6
		return {'}
	} 

	set ch [string index $cline $c]
	incr c
	return $ch
}

proc xml_handle_bs_string {str incdata} {
	set c 0
	set x [string length $str]
	set nstr ""
	while { $c <= $x } {
		append nstr [xml_handle_bs c $str $incdata]
	}
 	return $nstr
}

proc do_extended_proc_search {proclist cstack} {

	# We need to get the current stack and chop off the most 	#
	# recent...							#
	set els [split $cstack :]
	set y [llength $els]
	if { $y < 1 } {
		return -1
	}
	set searchfor [join [lrange $els 0 [expr $y - 2]] ":"]
	if { "$searchfor" == "" } {
		set searchfor "*"
	} else {
		append searchfor ":*"
	}
	#################################################################
	# If we have a stack then we generate the the possible global	#
	# function to handle this level of stack.			#
	#################################################################

	set x [lsearch -exact $proclist $searchfor]
	return $x
}

# xml_parse return codes:
# -1:	Id specified is not recognised.
# -2:	End of line encountered before end of current tag
# -3:	Current end tag does not match latest start tag
# -4:	An error occured whilst calling user-procedure
# -5:	End of file encountered during comment reading
# -6:	End of file encountered with non-empty stack
# -7:	More than one top level entity encountered in file
# -8:	Entity name is not well formed.
# -9:	Attribute name is not well formed.
# 

proc parse {id} {
variable xml_invoc
variable xml_stack
variable xml_file
variable xml_procs
variable xml_attrs_stack
variable xml_data_stack
variable xml_hadtoplevel
variable xml_cdata_parse
variable xml_cdata_start
variable xml_cdata_end
variable xml_attrs

	#################################################################
	# If the trace attribute has been set, then set the local	#
	# variable to indicate this fact.				#
	#################################################################

	set trace $xml_attrs(${id}_trace)

	set xml_s_len [expr [string length $xml_cdata_start] - 1]
	set xml_e_len [expr [string length $xml_cdata_end] - 1]

	#################################################################
	# Check for the special _default_ handler and ensure this	#
	# is made available during the parse.				#
	#################################################################

	set have_default [lsearch -exact $xml_procs($id) _default_]
	if { $have_default == -1 || [expr $have_default % 3] != 0 } {
		set defproc ""
	} else {
		incr have_default
		set defproc [lindex $xml_procs($id) $have_default]
		# Call back arguments, if defined
		set args [lindex $xml_procs($id) [expr {$have_default + 1}]]
	}

	set have_default 0
	if {! [info exists xml_stack($id)]} {
		return -1
	}
	
	#################################################################
	# Loop for all lines in the file to parse			#
	# Status is used to indicate current status of element... 	#
	# 0 - waiting to start element....				#
	# 1 - reading element start tag					#
	# 2 - Reading data for attribute...				#
	# 3 - Reading tag (end tag if 1st ch!= "/")			#
	# 4 - Reading embedded comment/code, ignoring...		#
	#								#
	# 1.8.2	Note:	When reading CDATA the status must be 2, since	#
	#		CDATA is only accepted in data.			#
	#################################################################

	set cstack {}
	set cfileline 0
	set status 0
	while {! [eof $xml_file($id)]} {
		gets $xml_file($id) cline
		incr cfileline
		if { "$trace" >= 1 } {
			puts "Trace: Read line ($status): $cline"
		}
		append cline "\n"
		#########################################################
		# The processing of <tag/> is performed by		#
		# pre-processing each line to convert <tag/> to		#
		# <tag></tag>.						#
		#########################################################
		if {[regsub -all {<([A-Za-z0-9_]+)/>} $cline {<\1></\1>} xx]} {
			set cline $xx
		}

		set c 0
		set ll [string length $cline]
		while { $c < $ll } {
			set ch [string index $cline $c]
			if { $status == 4 } {
				#################################################
				# Cope with --> end comments...			#
				#################################################
				set ch3 [string range $cline $c [expr $c + 2]]
				if { "$ch3" == "-->" } {
					set status $prev_status
					incr c 3; continue
				}
				if { "$ch" != "?" } {
					incr c; continue
				}
				set ch2 [string index $cline [expr $c + 1] ]
				if { "$ch2" == ">" } {
					set status $prev_status
					incr c 2; continue
				}
				incr c; continue
			}
			if { $status == 0 } {
				if { "$ch" == "<" } {
					set status 1
					set tag ""
					incr c; continue
				}
				#################################################
				# Ignore all other characters. . .		#
				#################################################
				incr c; continue
			}

			if { $status == 1 } {
				set empty_ctag 0
				#################################################
				# Look for <? which is treated as a		#
				# comment at the moment...			#
				#################################################
				if { "$tag" == "" } {
					if { "$ch" == "?" } {
						set status 4
						set prev_status 0
						set comstart $cfileline
						incr c; continue
					}

					#########################################
					# Look for <!-- comment start tag	#
					#########################################
					set ch3 [string range $cline $c [expr $c + 2]]
					if { "$ch3" == "!--" } {
						set status 4
						set prev_status 0
						set comstart $cfileline
						incr c 3; continue
					}

					if { "$ch" == "/" } {
						#################################
						# We've got an embedded end tag	#
						# rather than item start tag.   #
						#################################
						set status 3
						set etag $ch
						incr c; continue
					}
				}

				if { "$ch" != ">" } {
					#########################################
					# If we've got a space, then we expect  #
					# attributes - if tag is not empty. - - #
					#########################################
					if { "$tag" != "" && ("$ch" == " " || "$ch" == "\t") } {
						set xx [string range $cline $c end]
						set xx2 [string first ">" $xx]
						if { $xx2 == -1 } {
							if { $xml_attrs(${id}_silent) == 0 } {
								puts stderr "Error: End of line before end of tag encountered."
								puts stderr "Current line below: \n$cline"
							}
							return -2
						}
						set xx [string range $xx 0 [expr $xx2 - 1] ]
						if { [catch {set ctag_attrs [xml_tag_attrs_to_str $xx empty_ctag $xml_cdata_parse($id)]}] } {
							if { $xml_attrs(${id}_silent) == 0 } {
								puts stderr "Current stack: $cstack:$tag"
							}
							return -9
						}
						incr c $xx2
						# Move past tag and deal with it...
					} else {
						set ctag_attrs ""
						append tag $ch
						incr c; continue
					}
				}
				#################################################
				# We have ended the current tag...		#
				#################################################
				set tag [string tolower $tag]
				if { "$trace" >= 1 } {
					puts "Trace: Start-tag: $tag"
				}
				if { [string length $cstack] == 0 } {
					#################################################################
					# If we have ended the current tag and the stack is empty	#
					# then we should check to ensure this is the first top-level	#
					# entity and abort with an error if not.			#
					#################################################################
					if { $xml_hadtoplevel($id) == 1 } {
						if { $xml_attrs(${id}_silent) == 0 } {
							puts stderr "Error: Second top-level entity found! ($tag)"
						}
						return -7
					}
					if { ! [validate_name $tag] } {
						if { $xml_attrs(${id}_silent) == 0 } {
							puts stderr "Error: Entity name malformed: $tag"
						}
						return -8
					}
					set cstack $tag
					set xml_hadtoplevel($id) 1
				} else {
					if { ! [validate_name $tag] } {
						if { $xml_attrs(${id}_silent) == 0 } {
							puts stderr "Error: Entity name malformed: $tag"
						}
						return -8
					}
					append cstack ":$tag"
				}
				#################################################
				# Remove any elements that were sub-elements	#		
				# of an older element of the same stack level.	#
				#################################################
				foreach oldstent [array names xml_attrs_stack "$cstack:*"] {
					unset xml_attrs_stack($oldstent)
					unset xml_data_stack($oldstent)
				}
				set status 2
				set cdata ""
				set xml_attrs_stack($cstack) $ctag_attrs

				#################################################
				# If we have empty_ctag set, then we don't need	#
				# need to search for an end-tag, since it has	#
				# been provived with the trailing /...		#
				#################################################

				if { $empty_ctag } {
					set status 3
					set etag "/$tag"
					continue
				}

				incr c; continue
			}
			if { $status == 2 } {
				#################################################
				# 1.8.2						#
				# See if we have the special CDATA start symbol	#
				# at current position, or the end tag...	#
				#################################################

				if { "[string range $cline $c [expr $c + $xml_s_len]]" == $xml_cdata_start } {
					set xml_cdata_parse($id) 1
					incr c [expr $xml_s_len + 1]
					continue
				}

				if { "[string range $cline $c [expr $c + $xml_e_len]]" == $xml_cdata_end } {
					set xml_cdata_parse($id) 0
					incr c [expr $xml_e_len + 1]
					continue
				}

				if { $xml_cdata_parse($id) == 1 } {
					append cdata [xml_handle_bs c $cline $xml_cdata_parse($id)]
					continue
				}

				#################################################
				# 1.8.2		END OF DATA ALTERATIONS		#
				#################################################

				if { "$ch" == "<" } {
					set status 3
					set etag ""
					if { "$trace" >= 1 } {
						puts "Trace: Saving $cstack: $cdata"
					}
					if {[info exists xml_data_stack($cstack)]} {
						append xml_data_stack($cstack) $cdata
					} else {
						set xml_data_stack($cstack) $cdata
					}
					incr c; continue
				}
				append cdata [xml_handle_bs c $cline $xml_cdata_parse($id)]
				continue
			}
			if { $status == 3 } {
				if { "$etag" == "" && "$ch" == "?" } {
					set status 4
					set comstart $cfileline
					set prev_status 2
					incr c; continue
				}
				if { "$etag" == "" && "$ch" != "/" } {
				#########################################
				# We've got an embedded tag     	#
				# rather than item end tag.     	#
				#########################################

					set status 1
					set tag $ch
					incr c; continue
				}
				if { "$ch" != ">" } {
					append etag $ch
					incr c; continue
				}
				# if { "$ch" == "/" } { }
				#	incr c; continue
				# { }
				#########################################
				# Ok we've got a ">" indicating end of  #
				# deliminator tag, so see if we are	#
				# able to run call back for this	#
				# element.				#
				#########################################
				set etag [string tolower $etag]
				if { "$trace" >= 1 } {
					puts "Trace: End-tag: $etag"
				}
				set xx_el [llength [split $cstack :]]
				incr xx_el -1
				set xx_ll [lindex [split $cstack :] $xx_el]
				if { "$etag" != "/$xx_ll" } {
					if { $xml_attrs(${id}_silent) == 0 } {
						puts stderr "Error: End tag mismatch ($xx_ll -> $etag)"
						puts stderr "Current stack: $cstack"
					}
					return -3
				}
				set x [lsearch -exact $xml_procs($id) $cstack]
				if { "$trace" >= 2 } {
					puts "Trace: Searched $xml_procs($id) for $cstack - Result = $x"
				}
				if { [info exists xml_data_stack($cstack)] } {
					set cdata "$xml_data_stack($cstack)"
				}
				set xml_data_stack($cstack) "$cdata"
				if { ( $x ==-1 || [expr $x % 3] != 0 ) && $xml_attrs(${id}_extended) == 1 } {
					#################################
					# If wild carding has been 	#
					# enabled then perform an extra	#
					# check...			#
					#################################
					set x [do_extended_proc_search $xml_procs($id) $cstack]
				}
				if { $x ==-1 || [expr $x % 3] != 0 } {
					#################################
					# Check for default handler...	#
					#################################
					if { "$defproc" != "" } {
						xml_construct_attr_list $cstack myarr
						xml_construct_data_list $cstack myarr2
						set r [$defproc "$cstack" "$cdata" myarr2 "$xml_attrs_stack($cstack)" myarr $args]
						#########################
						# We need to purge or	#
						# error if required.	#
						#########################
						if { $r == "SXML_PURGE" } {
							set cdata {}
							set xml_data_stack($cstack) {}
						} elseif { ($r != "0" && $r != "SXML_OK") || $r == "SXML_ERROR" } {
							if { $xml_attrs(${id}_silent) == 0 } {
								puts stderr "Error: Returned error when calling: $defproc"
								puts stderr "Current stack: $cstack"
							}
							return -4
						}
					} else {
					#################################
					# Since we do not have a tag	#
					# for this element append it to #
					# the saved data....		#
					#################################
						if { "$trace" >= 1 } {
							puts "Trace: Added-saved: $tag=$cdata"
						}
					}
				} else {
					set proc [lindex $xml_procs($id) [expr $x + 1]]
					if { "$trace" >= 1 } {
						puts "Trace: Calling proc $proc"
					}
					xml_construct_attr_list $cstack myarr
					xml_construct_data_list $cstack myarr2
					set r [$proc "$cstack" "$cdata" myarr2 "$xml_attrs_stack($cstack)" myarr]
					#########################
					# We need to purge or	#
					# error if required.	#
					#########################
					if { $r == "SXML_PURGE" } {
						set cdata {}
						set xml_data_stack($cstack) {}
					} elseif { ($r != "0" && $r != "SXML_OK") || $r == "SXML_ERROR" } {
						if { $xml_attrs(${id}_silent) == 0 } {
							puts stderr "Error: Returned error when calling: $proc"
							puts stderr "Current stack: $cstack"
						}
						return -4
					}
				}
				set stack_split [split $cstack :]
				set e1 [llength $stack_split]
				incr e1 -2
				set cstack [join [lrange $stack_split 0 $e1] :]
				set cdata ""
				set ctag ""
				set etag ""
				set status 0
				incr c; continue
			}
			puts "stderr: Warning invalid state encountered!"
			incr c
		}
	}
	if { $status == 4 } {
		if { $xml_attrs(${id}_silent) == 0 } {
			puts stderr "Error: End of file during comment - comment started on line $comstart."
		}
		return -5
	}
	if { $status != 0 || [string length $cstack] > 0 } {
		if { $xml_attrs(${id}_silent) == 0 } {
			puts stderr "Error: Data exhausted, format not satisfied."
			puts stderr "Current stack: $cstack"
		}
		return -6
	}
	return 0
}

}

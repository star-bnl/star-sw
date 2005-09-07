 package require tdom

 proc validateFile {filename {schema ""}} {
     set doc [file2list $filename]
     foreach {name atts -} $doc break
     if {$schema eq ""} {
	 set schema [keyget $atts xsi:noNamespaceSchemaLocation]
     }
     if {$schema eq ""} {error "no schema given"}
     unset -nocomplain ::def
     readSchema $schema
     is-a $name $doc
 }
 proc file2list filename {
    set doc [dom parse [readfile $filename]]
    set root [$doc documentElement]
    K [$root asList] [$doc delete]
 }

if 0 {This returns the value of a key from a {key val key val...} dict-like 
      list, or else the specified default:
}

 proc keyget {list key {default ""}} {
    foreach {k v} $list {if {$k eq $key} {return $v}}
    return $default
 }

#-- Contents of a file as a string

 proc readfile filename {
    set fp [open $filename]
    K [read $fp] [close $fp]
 }

if 0 {Reading a schema involves keeping track of pathnames, as a schema might
      include another one, addressed with relative path. I use a global stack 
      for paths, and implement the XSD elements as Tcl procs, so they can be 
      directly eval-led.
}

 proc readSchema filename {
    lappend ::Stack [file join [pwd] [file dir $filename]]
    eval [file2list $filename]
    set ::Stack [lrange $::Stack 0 end-1]
 }
 proc K {a b} {set a}

if 0 {This is the core of validation: it retrieves the definitions from the 
      global def array, and sees whether they're met by the input data. For 
      detecting undefined attributes, I place all into an array, and cancel 
      out those that were checked by xs:attribute (or are allowed in general, 
      by xmlns bureaucracy...) This however is done only if "is-a" isn't just 
      recursing from element to type name. 
}

 proc is-a {typename data} {
    if [info exists ::def($typename)] {
        set typename $::def($typename)
    }
    match? $typename $data
 }
 proc match? {definition data} {
    if {[lindex $definition 0 0] eq "is-a"} {
        return [is-a [lindex $definition 0 1] $data]
    }
    if {[llength [lindex $data 1]] % 2 == 0} {
        array set atts [lindex $data 1]
        foreach i {xmlns xmlns:xsi xsi:noNamespaceSchemaLocation} {
            array unset atts $i
        }
    } else {set atts {}}
    foreach condition $definition {
        if {[lindex $condition 0] eq "#comment"} continue
        if {[lindex $condition 0] eq "string"} {
            lset condition 0 xs:string
        }
        if ![eval $condition [list $data]] {
            error "bad data '$data' for $condition"
        }
        if [string match *attribute [lindex $condition 0]] {
            array unset atts [keyget [lindex $condition 1] name]
        }
        #-- Attributes can also be inside a complexType
        if [string match *complexType [lindex $condition 0]]  {
            foreach part [lindex $condition 2] {
                if [string match *attribute [lindex $part 0]] {
                    array unset atts [keyget [lindex $part 1] name]
                }
            }
        }
    }
    if {[array get atts] ne ""} {
        error "undefined attribute(s) '[array get atts]' in $data"
    }
    return 1
 }

if 0 {The asList format makes it easy to implement XSD elements as Tcl commands.
      As in XML just a single colon indicates namespace, this is not in 
      conflict with Tcl namespaces, but is part of the name. Here they are, 
      in roughly alphabetic order: 
}

 proc xs:annotation args {}

 proc xs:attribute {atts kids data} {
    set name [keyget $atts name]
    set dataatts [lindex $data 1]
    if {[keyget $dataatts $name -none-] eq "-none-"} {
        if {[keyget $atts use] eq "required"} {
            error "missing required attribute '$name' in $data"
        } else {
            return 1
        }
    }
    is-a [keyget $atts type] [keyget $dataatts $name]
 }
 proc xs:attributeGroup {atts kids {data -}} {
    set name [keyget $atts name]
    if {$name ne ""} {
        set ::def($name) $kids
    } else {
        #is-a [keyget $atts ref] $data ;# TODO
        return 1
    }
 }
 proc xs:boolean x {in {true false 0 1} $x}
 proc xs:complexContent {atts kids data} {
    foreach kid $kids {eval $kid [list $data]}
    return 1
 }
 proc xs:element {atts kids} {
    set name [keyget $atts name]
    if [llength $kids] {
        set ::def($name) $kids
    } else {
        set ::def($name) [list [list is-a [keyget $atts type]]]
    }
 }
 proc xs:enumeration {atts kids data} {
    set value [keyget $atts value]
    expr {[string equal $value $data]
        || [string equal $value [lindex $data 2 0 1]]}
 }
 proc xs:extension {atts kids data} {
    set base [keyget $atts base]
    set xkids [concat [lindex $::def($base) 0 2] [lindex $kids 0 2]]
    xs:sequence {} $xkids $data
 }
 proc xs:include {atts kids} {
     set path [lindex $::Stack end]
     set loc [keyget $atts xs:schemaLocation]
     if {$loc eq ""} {set loc [keyget $atts schemaLocation]}
    readSchema [file join $path $loc]
 }
 proc xs:fractionDigits {atts - data} {
    regexp {(\.([0-9]*))?$} [lindex $data 2 0 1] - - digits
    expr [string length $digits] <= [keyget $atts value]
 }

if 0 {Some restrictions are so similar that I found it better to define a 
      generic handler, and specialize that by aliasing in the comparison 
      operator:
}

 proc generic'length'bound {op atts - data} {
    expr [string length [lindex $data 2 0 1]] $op [keyget $atts value]
 }
 interp alias {} xs:length    {} generic'length'bound ==
 interp alias {} xs:maxLength {} generic'length'bound <=
 interp alias {} xs:minLength {} generic'length'bound >=

 proc generic'numeric'bound {op atts - data} {
    expr [lindex $data 2 0 1] $op [keyget $atts value]
 }
 interp alias {} xs:maxExclusive {} generic'numeric'bound <
 interp alias {} xs:maxInclusive {} generic'numeric'bound <=
 interp alias {} xs:minExclusive {} generic'numeric'bound >
 interp alias {} xs:minInclusive {} generic'numeric'bound >=

 proc generic'string'test {type data} {
     expr {[string is $type -strict [lindex $data 2 0 1]]
     || [string is $type -strict $data]}
 }
# interp alias {} xs:boolean {} generic'string'test double
 interp alias {} xs:decimal {} generic'string'test double
 interp alias {} xs:double  {} generic'string'test double
 interp alias {} xs:float   {} generic'string'test double
 interp alias {} xs:int     {} generic'string'test int
 foreach type {xs:string xs:anyURI xs:language xs:NMTOKEN xs:date} {
     interp alias {} $type {} K 1 ;# always true :-)
 }
 proc xs:integer x         {expr {![catch {incr x 0}]}}
 proc xs:positiveInteger x {expr {[incr x 0]>0}}

if 0 {So far I've not seen cases where XSD regular expressions differ from 
      Tcl's, so this quick shot is sufficient for now:
}

 proc xs:pattern {atts kids data} {
    set re ^[keyget $atts value]$
    expr {[regexp $re $data] || [regexp $re [lindex $data 2 0 1]]}
 }

if 0 {The restriction code got a bit bloated by the fact that most
      restrictions are to be combined with "and" (i.e. all must match), 
      while enumeration restrictions go by "or" (i.e. one must match):
}

 proc xs:restriction {atts kids data} {
     set base [keyget $atts base]
     set x [lindex $data 2 0 1]
     set r [expr {$base eq "string"? 1: [is-a $base $x]}]
     if !$r {return 0}
     set enum [expr {![string match *enumeration* $kids]}]
     foreach condition $kids {
        if [string match "*enumeration" [lindex $condition 0]] {
            incr enum [eval $condition [list $data]]
        } else {
            if {![eval $condition [list $data]]} {return 0}
        }
    }
    return $enum
 }
 proc in {list element} {expr {[lsearch -exact $list $element]>=0}}

 proc xs:schema {atts kids} {
     set namespace [extract'namespace $atts]
     if {$namespace ne ""} {
	 set ::def(_ns_) $namespace:
     } else {set ::def(_ns_) ""}
     foreach kid $kids {eval $kid}
 }
 proc extract'namespace atts {
     foreach {key value} $atts {
	 if [string match xmlns:* $key] {return [string range $key 6 end]}
     }
 }

if 0 {For validating sequences of elements, I convert the XSD data to a regular 
      expression, so that

  regexp {(foo ){1,1}(bar ){1,}(grill ){0,1}} {foo bar bar } -> 1

}

 proc xs:sequence {atts kids data} {
    set re ""
    foreach kid $kids {
        set name [lindex $kid 0]
        if { $name eq "#comment"} continue
        if ![string match *element $name] {
	            error "bad kid $kid"
	    }
        set atts [lindex $kid 1]
        set name [keyget $atts name]
        if {$name eq ""} {set name [keyget $atts ref]}
        if ![info exists ::def($name)] {eval $kid}

        set type($name) [keyget $atts type]
        if {$type($name) eq ""} {set type($name) $name}
        set min [keyget $atts minOccurs 1]
        set max [string map {unbounded ""} [keyget $atts maxOccurs 1]]
        append re "($name ){$min,$max}"
    }
    set skimmed [skim [lindex $data 2]]
    if ![regexp ^$re$ "$skimmed "] {
        error "sequence mismatch $re - $skimmed"
    }
    foreach testkid [lindex $data 2] {
        set name [lindex $testkid 0]
        if {$name eq "#comment"} continue
        is-a $type($name) $testkid
    }
    return 1
 }
 # Based on xs:sequence.
 # Want all elements, but they can be in any order.
 proc xs:all {atts kids data} {
    set kidl [list]
    foreach kid $kids {
        set name [lindex $kid 0]
        if { $name eq "#comment"} continue
        if ![string match *element $name] {
	            error "bad kid $kid"
	    }
        set atts [lindex $kid 1]
        set name [keyget $atts name]
        if {$name eq ""} {set name [keyget $atts ref]}
        if ![info exists ::def($name)] {eval $kid}

        set type($name) [keyget $atts type]
        if {$type($name) eq ""} {set type($name) $name}
        set min [keyget $atts minOccurs 1]
        set max [string map {unbounded "999999"} [keyget $atts maxOccurs 1]]
        if {[info exists kidName($name)]} {
            error "$name occurs more than once in block."
        }
        lappend kidl $name $min $max
    }
    set skimmed [skim [lindex $data 2]]
    foreach {kid min max} $kidl {
        set num [regsub -all $kid $skimmed "" skimmed]
        if {($num < $min) || ($max < $num)} {
            error "expected between $min and $max occurences of $kid. Found $num"
        }
    }
    foreach left $skimmed {
        error "$left not defined"
    }
    foreach testkid [lindex $data 2] {
        set name [lindex $testkid 0]
        if {$name eq "#comment"} continue
        is-a $type($name) $testkid
    }
    return 1
 }
 # Based on xs:sequence.
 # Want only one element. We can allow minOcurrs and maxOcurrs without
 # too much difficulty and I guess that makes sense in a choice.
 proc xs:choice {atts kids data} {
    set re [list]
    foreach kid $kids {
        set name [lindex $kid 0]
        if { $name eq "#comment"} continue
        if ![string match *element $name] {
	            error "bad kid $kid"
	    }
        set atts [lindex $kid 1]
        set name [keyget $atts name]
        if {$name eq ""} {set name [keyget $atts ref]}
        if ![info exists ::def($name)] {eval $kid}

        set type($name) [keyget $atts type]
        if {$type($name) eq ""} {set type($name) $name}
        set min [keyget $atts minOccurs 1]
        set max [string map {unbounded "999999"} [keyget $atts maxOccurs 1]]
        lappend re "($name){$min,$max}"
    }
    set skimmed [skim [lindex $data 2]]
    set match 0
    foreach r $re {
        if {[regexp $r $skimmed]} {
            incr match
        }
    }
    if {$match == 0} {
        error "No match for $skimmed found in $re."
    } elseif {$match > 1} {
        error "Found more than one choice for $skimmed in $re"
    }
    foreach testkid [lindex $data 2] {
        set name [lindex $testkid 0]
        if {$name eq "#comment"} continue
        is-a $type($name) $testkid
    }
    return 1
 }
 # assume list has no kids. (Is this valid?)
 # Data is a list of values. Check each one satisfies atts.
 proc xs:list {atts kids data} {
     set base [lindex $atts 1]
     foreach d $data {
         match? $base $d
     }
     return 1
 }


 proc xs:simpleType {atts kids {data -none-}} {
    set name [keyget $atts name]
    if {$name ne ""} {
        set ::def($name) $kids
        if {$::def(_ns_) ne ""} {set ::def($::def(_ns_)$name) $kids}
        return 1
    } else {
        match? $kids $data
    }
 }
 # For complex type check if mixed=true. If so we strip out all text then
 # validate in the same way as simpletype.
 proc xs:complexType {atts kids {data -none-}} {
    set m [keyget $atts mixed]
    if {($m ne "") && $m} {
        set data2 [list]
        foreach d [lindex $data 2] {
            if {![string equal [lindex $d 0] "#text"]} {
                lappend data2 $d
            }
        }
        set data [concat [lrange $data 0 1] [list $data2]]
    }
    xs:simpleType $atts $kids $data
 }

 proc xs:totalDigits {atts kids data} {
    set value [keyget $atts value]
    set x [lindex $data 2 0 1]
    expr {[string is digit $x] && [string length $x] == $value}
 }
 proc xs:unsignedByte x {
     expr {$x*1>=0 && $x<256}
 }
 proc xs:unsignedInt x {
     if {[llength $x]==3} {set x [lindex $x 2 0 1]}
     expr {[incr x 0]>=0}
 }
 proc #comment args {} ;#-- just ignore them

if 0 {This returns the first elements (names) of a list of lists, e.g.

 skim {{a 1} {b 2 3 4} {c 5 6}} -> {a b c}

}

 proc skim list {
    set res {}
    foreach element $list {
        set name [lindex $element 0]
        if {$name eq "#comment"} continue
        lappend res $name
    }
    set res
 }

#-- Debug helper

 proc ! args {
    puts !!$args
    for {set i [info level]} {$i>=0} {incr i -1} {
        puts $i:[info level $i]
    }
 }

#-- This prepares for schemas using the xsd: or no namespace:

 foreach cmd [info commands xs:*] {
     interp alias {} xsd:[string range $cmd 3 end] {} $cmd
     if {$cmd eq "xs:string"} continue
     if {$cmd eq "xs:list"} continue
     interp alias {} [string range $cmd 3 end] {} $cmd
 }

 #foreach test {po.xml ship.xml greeting.xml directory.xml} {
 #   puts $test:[validateFile $test]
 #}

# validateFile [lindex $argv 0] [lindex $argv 1]

if 0 {

SEH 10/7/04 -- I tried to validate an xml document using the generic schema at 
http://www.w3.org/2001/XMLSchema.xsd, and I got an 
error "invalid command name xs:import"

RS: Well, I didn't claim completeness, and obviously there is no implementation 
for xs:import yet. The W3C Schema has so many nooks and crannies that I didn't 
bother with all... just being happy that simple examples worked. But I'll look 
at xs:import later today.

}

proc getTowerPedestal { pedestal } {
    global PedestalShift

    # Comment from Alex Stolpovsky:
    # For JetPatch trigger details see:
    # http://www.star.bnl.gov/HyperNews-star/protected/get/triggerboard/39/2/1/2/1/1.html
    # 1) We start out with 12-bit single tower ADC.
    # 2) We chop off 2 least significant bits and end up with 10-bit single tower
    # ADC.
    # 3) Subtract pedestal. In doing so, we want the resulting value to be
    # away from the multiples of 16
    # (multiples of 4 on 10-bit level) to avoid "jumping bits" that are
    # responsible for noise in the JetPatch sum. An outcome from some earlier
    # discussion was that the goal is to set the value at 24, half-way between 16
    # and 32, as opposed to 8, to distinguish live channels from the ones that are
    #.masked out (pedestal of 24 will result in trigger pedestal 1).
    #  The maximum value that can be subtracted is 60 (15 on 10-bit
    # level). Some pedestals are too high to be subtracted down to 24. In this
    # case I make them oddNumber*8.
    # 4) Lose 2 more bits; now we have 8-bit ADC. [1 ADC at this level is worth
    # ~8MeV (our calibration const.) times 16 = ~125MeV]
    # 5) Sum 16 towers to form a 4x4 patch sum. The sum is a 12-bit number.
    # 6) Apply LUT, so that 4x4 sums with values under Ped4x4 become 0; values
    # from Ped4x4 to (Ped4x4 + 60) are shifted by Ped4x4 to take values from 0 to
    # 60, and everything above (4x4Ped + 60) is 60.
    # 7) Sum 25 patch sums of 4x4. This is JetPatch ADC.
    #
    ###########################################################################
    # From the manual I understood that the pedestal subtraction is
    # done over the 10 bits ADC value, not the 12 bits raw data.
    # this is the reason we divide the pedestal by 4. This will convert
    # the number from a 12 bits to a 10 bits chopped scale
    # the offset value is a shift on the pedestal to put all of them in
    # a single place. If offset is set as 0 it will put all pedestals at 0
    # level. Remember that this offset in in the 12 bits value, before we
    # we convert them to 10 bits
    # AAPSUAIDE
    ###########################################################################
    ###########################################################################
    # pedestal values that are loaded is a 5 bits number where the MSB is
    # a signal (1 means subtract pedestal while 0 means add).
    # so, we have only 4 bits of that allowing us to subtract a value up
    # to 15 only (this means a raw pedestal value up to 60 ADC counts in
    # 12 bits data)
    # AAPSUAIDE
    ###########################################################################
    #
    # Alex Stolpovsky:
    # present algorithm produces stable values for pedestal values
    # from (measuredPed-6) to (measuredPed+5), including bounds.
    # If a pedestal deviates from measuredPed by more than 6 counts
    # there will be a "jumping bit" for certain values of measuredPed
    #

    set scale10bits 4
    set operationBit 1
    # operationBit == 1 means subtract (default)
    # operationBit == 0 means add

    set pedestal1 [expr $pedestal - $PedestalShift]
    if ($pedestal1<0) then {
        set pedestal1 [expr $pedestal1*(-1)]
        set operationBit 0
    }
    set operationBit [format "%1.0f" $operationBit]
    set value2 [expr $pedestal1/$scale10bits]
    set value1 [format "%3.0f" $value2]
    set value2 [expr $pedestal1 - $value1*$scale10bits]
    if ($value2>2) then {
        set value2 [expr $value1 + 1]
        set value1 [format "%3.0f" $value2]
    }
    if ($value1>15) then {
        set value3 [expr ($value1-11)/$scale10bits]
        set value3 [format "%3.0f" $value3]
        set value3 [expr $value3*$scale10bits]
        set value2 [expr $value1-$value3]
        set value1 [format "%3.0f" $value2]
    }
    if {$operationBit == 1} {set value [expr ($value1&0x0F)|(0x10)]}
    if {$operationBit == 0} {set value [expr ($value1&0x0F)]}
    return $value
}

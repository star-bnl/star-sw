proc getLUTped { board patch } {
    global lutScale lutPed lutSigma lutUseMask lutUsePowerup
    global triggermask1 triggermask2 PedestalShift

    set ped $lutPed([expr $patch+1],$board)
    if {$lutUsePowerup} {set ped [expr $ped + 15]}
    if {$lutUseMask == 2} {
        set triggerMask 0xffff
        if {$patch == 0} {set triggerMask $triggermask1($board)}
        if {$patch == 1} {set triggerMask $triggermask2($board)}
        set numberOfMaskedChannels 0
        for {set bit 1} {$bit <= 0xffff} {set bit [expr $bit * 2]} {
            if {[expr ($triggerMask & $bit)] == 0} {incr numberOfMaskedChannels}
        }
        set ped [expr $ped - ($numberOfMaskedChannels) * (($PedestalShift - 8) / 16)]
    }
    return $ped
}

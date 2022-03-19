;Localisation based functions live here

getsetSwitchChar:  ;ah = 37h, allows changing default switch from / to anything
getsetCountryInfo: ;ah = 38h, localisation info
getExtLocalInfo:   ;ah = 65h, Get Extended Country Info
getsetGlobalCP:    ;ah = 66h, Get/Set Global Codepage, reserved
    ret
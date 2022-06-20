# SCPDOS
Some notes on documentation

## Various interfaces:
Int 41h = DOS API (DAPI)

Int 4Fh AX=11xxh = DOS Redirector Interface (DRI)

Int 4Fh AX=12xxh = Internal DOS Interface (IDI)

## API function states
- Proposed (A proposed new API function)
- Accepted (A accepted but not yet implemented API function)
- Preliminary (Documented but subject to change, such as the IDI, DRI functions and Multitasking hooks as these are more or less undocumented in the DOS days)
- Current (Standard DOS API functions. Will be supported. If ever depreciated, then they return an error code and their function numbers are NOT recycled)
- Obsolete (Use an alternative function for everything EXCEPT ensuring compatibility. The FCB functions fall here)
- Removed (This function henceforth will only ever return an error)
